#CALLED FUNCTIONS-------------
#data reads
ReadPlateMap <- function(plate_map_address){
  #read whole map
  plate_map <- read.xlsx(plate_map_address, 1, header=T)
  
  #separate drug list
  drugList <- plate_map[c(7:14), c(2:13)] %>% t() %>% as.vector()
  if(drugList[1]=="Drug name"){
    drugList <- plate_map[c(10:17), c(2:13)] %>% t() %>% as.vector()
    concList <- plate_map[c(21:28), c(2:13)] %>% t() %>% as.vector()
    mediumList <- plate_map[c(33:40), c(2:13)] %>% t() %>% as.vector()
    strainList <- plate_map[c(45:52), c(2:13)] %>% t() %>% as.vector()
  }else{
    drugList <- plate_map[c(7:14), c(2:13)] %>% t() %>% as.vector()
    concList <- plate_map[c(17:24), c(2:13)] %>% t() %>% as.vector()
    mediumList <- plate_map[c(27:34), c(2:13)] %>% t() %>% as.vector()
    strainList <- plate_map[c(37:44), c(2:13)] %>% t() %>% as.vector()
  }
  
  slotList <- sapply(LETTERS[c(1:8)], function(x) paste(x, c(1:12), sep="")) %>% as.vector()
  plate_map <- cbind.data.frame(slotList, drugList, concList, mediumList, strainList)
  colnames(plate_map) <- c("Slot", "Drug", "Conc", "Medium", "Strain")
  
  #add WellID
  plate_map$WellId <- paste(plate_map$Drug, plate_map$Conc, plate_map$Medium, plate_map$Strain, sep="-")
  return(plate_map)
}
Extract_oneMeasFile <- function(meas_address_single){
  #read whole csv file
  measRes <- read.csv(meas_address_single, header=T)
  
  #extract measurement timestamp nad measurement results
  measTime <- measRes[1,1]
  measRes <- measRes[c(8:103),]
  
  #process measurement time
  measTime <- gsub("Date:", "", measTime)
  measTime <- gsub("Time:", "-", measTime)
  measTime <- gsub(" ", "", measTime)
  
  #process measurement results
  measRes <- gsub(" ", "", measRes)
  measRes <- do.call(rbind, strsplit(measRes, split=":")) %>% data.frame()
  colnames(measRes) <- c("Slot", toString(measTime))
  measRes$Slot <- sapply(measRes$Slot, 
                         function(x) if(substring(x, 2, 2)=="0"){paste(substring(x, 1, 1), substring(x, 3, 3), sep="")}else{x})
  
  return(list(measRes))
}
Read_allMeasFile <- function(input_wd){
  #get list of files
  input_files <- list.files(input_wd)
  input_files <- paste(input_wd, input_files, sep="/")
  meas_res <- sapply(input_files, function(x) Extract_oneMeasFile(x))
  
  measR <- c()
  for(i in c(1:length(meas_res))){
    if(i==1){
      measR <- meas_res[[i]]
    }else{
      measR <- left_join(measR, meas_res[[i]], by="Slot")
    }
  }
  return(measR)
}
Create_LongFormat <- function(all_res){
  #extract measurement and slots
  dat_colnames <- colnames(all_res)
  dat_colnames <- dat_colnames[!(dat_colnames %in% c("Drug", "Conc", "Medium", "Strain", "WellId"))]
  nmDat <- all_res[dat_colnames]
  
  #convert to long-format
  nmDat <- melt(nmDat, id.vars="Slot")
  colnames(nmDat) <- c("Slot", "Time", "Measurement")
  
  #convert time to chron format
  nmDat <- cbind.data.frame(nmDat, t(sapply(nmDat$Time, function(x) strsplit(toString(x), split="-")[[1]])))
  colnames(nmDat)[c(4:5)] <- c("Date", "Hours")
  nmDat$Time <- chron(dates=nmDat$Date, times=nmDat$Hours, format=c(dates='d/m/y', times='h:m:s'))
  nmDat <- nmDat[1:3]
  nmDat$Time <- as.numeric(nmDat$Time - min(nmDat$Time)) * 24 #standardize to first measurement timepoint (as zero); convert to hours
  
  #add well ID
  nmDat$WellId <- sapply(nmDat$Slot, function(x) all_res$WellId[all_res$Slot==x])
  
  #convert measurement to numeric
  nmDat$Measurement <- as.numeric(nmDat$Measurement)
  return(nmDat)
}

#processing
Proc_noControl <- function(raw_NM, control=F){
  procData <- raw_NM[,c("WellId", "Time")] %>% distinct()
  procData$timeID <- paste(procData$WellId, procData$Time, sep="_")
  raw_NM$timeID <- paste(raw_NM$WellId, raw_NM$Time, sep="_")
  
  #expand well info
  procData <- cbind.data.frame(procData,
                               do.call(rbind, strsplit(procData$WellId, split="-")))
  
  #get mean and sd
  procData <- cbind.data.frame(procData,
                               t(sapply(procData$timeID, function(x) c(mean(raw_NM$Measurement[raw_NM$timeID==x]),
                                                                       sd(raw_NM$Measurement[raw_NM$timeID==x]),
                                                                       length(raw_NM$Measurement[raw_NM$timeID==x])))))
  colnames(procData)[4:10] <- c("Drug", "Conc", "Medium", "Strain", "AvgMeas", "SDMeas", "nReplicates")
  procData$SDMeas[is.na(procData$SDMeas)] <- 0 #remove NA from single measurements
  
  #remove no drug asssigned rows
  if(!control){
    procData <- subset(procData, Drug!="NA")
  }
  
  return(procData)
}
Proc_getControl <- function(control_address, raw_NM){
  #read control input
  contInp <- read.csv(control_address, header=T)[,c(2:13)] %>% t() %>% as.vector()
  slotList <- sapply(LETTERS[c(1:8)], function(x) paste(x, c(1:12), sep="")) %>% as.vector()
  contInp <- cbind.data.frame(slotList, contInp)
  colnames(contInp) <- c("Slot", "ControlType")
  
  #append control type to raw data
  proc_NMc <- left_join(raw_NM, contInp, by="Slot")
  
  #remove ignored columns
  proc_NMc <- proc_NMc[!grepl("ignore", proc_NMc$ControlType, ignore.case=T),]
  
  #separate type and group
  proc_NMc <- cbind.data.frame(proc_NMc, 
                               do.call(rbind, strsplit(proc_NMc$ControlType, split="_")))
  colnames(proc_NMc)[c(6,7)] <- c("Group", "Type")
  proc_NMc$timeTypeID <- paste(proc_NMc$Time, proc_NMc$ControlType, sep="-")
  
  #get controls matrix
  contMat <- proc_NMc[,c("timeTypeID", "ControlType", "Group", "Type", "Time")] %>% distinct()
  contMat <- contMat[grepl("control", contMat$Type, ignore.case=T),]
  contMat$AvgMeas <- vapply(contMat$timeTypeID, FUN.VALUE=1,
                            function(x) mean(proc_NMc$Measurement[proc_NMc$timeTypeID==x]))
  
  #normalize values to baseline
  baseline <- c()
  for(i in c(1:nrow(proc_NMc))){
    baseline <- c(baseline, contMat$AvgMeas[contMat$Group==proc_NMc$Group[i] & contMat$Time==proc_NMc$Time[i]])
  }
  proc_NMc$Baseline <- baseline
  proc_NMc$Measurement_normalized <- proc_NMc$Measurement - proc_NMc$Baseline
  
  #remove controls
  proc_NMc <- proc_NMc[!grepl("control", proc_NMc$Type, ignore.case=T),]
  
  return(proc_NMc)
}

#MAIN FUNCTION------------
mainFun <- function(platemap_address, inputwd, control_map=NULL){
  #read platemap and measurement results
  plateMap <<- ReadPlateMap(platemap_address)
  measResults <<- Read_allMeasFile(inputwd)
  
  #combine raw data
  rawData_matrix <<- left_join(plateMap, measResults, by="Slot")
  rawData_NM <<- Create_LongFormat(rawData_matrix) #create long-format
  
  #process data
  if(is.null(control_map)){
    frplt <- Proc_noControl(rawData_NM)
    proc_NM <<- frplt
  }else{
    proc_NM <- Proc_getControl(control_map, rawData_NM)
    frplt <- Proc_noControl(proc_NM, control=T)
    proc_NM <<- frplt
  }
  
  #create plot
  plt <<- ggplot(data=frplt, aes(x=Time, y=AvgMeas))+
    geom_point()+geom_line()+facet_wrap(~WellId)+theme_bw()+
    geom_errorbar(aes(ymin=AvgMeas - SDMeas, ymax=AvgMeas + SDMeas), width=0.5)+
    xlab("Time / hours")
  
  return(proc_NM)
}

#TROUBLESHOOTING-----------------
#platemap_wd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator"
#inputwd <- "TestInput"

#### Read Plate Map ####
#extract address
#plateMap_address <- list.files(platemap_wd)
#plateMap_address <- plateMap_address[grepl("xlsx", plateMap_address)]
#plateMap_address <- paste(platemap_wd, plateMap_address, sep="\\")
#inputwd <- paste(platemap_wd, inputwd, sep="\\")
#extract address for control input
#controlInput_address <- paste(platemap_wd, "ControlMap.csv", sep="\\")

#call main
#dis <- mainFun(plateMap_address, inputwd, control_map = controlInput_address)
#dis <- mainFun(plateMap_address, inputwd)
