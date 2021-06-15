#LIBRARIES---------
#library(readxl)
#library(dplyr)
#CALLED FUNCTIONS-------------
#data reads
ReadPlateMap <- function(plate_map_address){
  #read whole map
  #plate_map <- read.xlsx(plate_map_address, 1, header=T)
  plate_map <- read_xlsx(plate_map_address, sheet=1, col_names=T) %>% data.frame()
  plate_map <- plate_map[(!apply(plate_map, 1, function(x) all(is.na(x)))),]
  
  #separate drug list
  drugList <- plate_map[c(7:14), c(2:13)] %>% t() %>% as.vector()
  if(drugList[1]=="Drug name"){
    drugList <- plate_map[c(9:16), c(2:13)] %>% t() %>% as.vector()
    concList <- plate_map[c(19:26), c(2:13)] %>% t() %>% as.vector() %>% as.numeric()
    mediumList <- plate_map[c(29:36), c(2:13)] %>% t() %>% as.vector()
    strainList <- plate_map[c(39:46), c(2:13)] %>% t() %>% as.vector()
  }else{
    drugList <- plate_map[c(7:14), c(2:13)] %>% t() %>% as.vector()
    concList <- plate_map[c(17:24), c(2:13)] %>% t() %>% as.vector() %>% as.numeric()
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
Create_LongFormat <- function(all_res, coordinate_control=F){
  #extract measurement and slots
  dat_colnames <- colnames(all_res)
  dat_colnames <- dat_colnames[!(dat_colnames %in% c("Drug", "Conc", "Medium", "Strain", "WellId", "ControlType"))]
  nmDat <- all_res[dat_colnames]
  
  #convert to long-format
  nmDat <- melt(nmDat, id.vars="Slot")
  colnames(nmDat) <- c("Slot", "Time", "Measurement")
  
  #convert time to chron format
  nmDat$Time <- gsub("\\(.*?\\)", "", nmDat$Time)
  nmDat <- cbind.data.frame(nmDat, t(sapply(nmDat$Time, function(x) strsplit(toString(x), split="-")[[1]])))
  colnames(nmDat)[c(4:5)] <- c("Date", "Hours")
  nmDat$Time <- chron(dates=nmDat$Date, times=nmDat$Hours, format=c(dates='d/m/y', times='h:m:s'))
  nmDat <- nmDat[1:3]
  nmDat$Time <- as.numeric(nmDat$Time - min(nmDat$Time)) * 24  %>% #standardize to first measurement timepoint (as zero); convert to hours
    round(digits=2) #round to two decimal places
  
  #add well ID
  if(coordinate_control){
    nmDat$WellId <- all_res$ControlType
  }else{
    nmDat$WellId <- sapply(nmDat$Slot, function(x) all_res$WellId[all_res$Slot==x])
  }
  
  #convert measurement to numeric
  nmDat$Measurement <- as.numeric(nmDat$Measurement)
  
  return(nmDat)
}

#processing
avgControl <- function(all_controls, cur_control_spec){
  cur_data <- subset(all_controls, controlSpec==cur_control_spec)
  #res : average, wells, timepoint, drug, drug concentration, medium, strain
  res <- c(mean(cur_data$Measurement), paste(cur_data$Slot, collapse=", "), cur_data$Time[1],
           cur_data$Drug[1], cur_data$DrugConc[1], cur_data$Medium[1], cur_data$Strain[1]) 
  return(res)
}
avgControl_coord <- function(all_data, cur_group){
  cur_data <- subset(all_data, GroupTimeID==cur_group & CtrlType=="control")
  
  res <- c(mean(cur_data$Measurement), paste(cur_data$Slot, collapse=", "), cur_data$Time[1],
           cur_data$Drug[1], cur_data$DrugConc[1], cur_data$Medium[1], cur_data$Strain[1]) 
  
  return(res)
}
createControlList <- function(current_control_plate, ctrl_selection){
  #Task 1 | Identify wells without inoculum WITH 0 drug concentration
  # also remove non-drug specified wells
  control_list <- subset(current_control_plate, (Strain=="NA" | is.na(Strain)) & !(Drug=="NA" | is.na(Drug)))
  
  #Task 2 | Assign possible controls
  # add time-medium ID
  control_list$timeMediumID <- paste(control_list$Time, control_list$Medium, sep="_")
  
  # add time-medium-drug ID
  control_list$timeMedDrugID <- paste(control_list$Time, control_list$Medium, control_list$Drug, sep="_")
  
  # add time-medium-drug-conc ID
  control_list$timeMedDrugConcID <- paste(control_list$Time, control_list$Medium, control_list$Drug, control_list$DrugConc, sep="_")
  
  #Task 3 | take average
  if(ctrl_selection==1){
    #medium only
    control_list <- subset(control_list, DrugConc==0)
    control_list$controlSpec <- control_list$timeMediumID
    control_list <- do.call(rbind, lapply(unique(control_list$controlSpec), avgControl, all_controls=control_list)) %>% data.frame()
  }else if(ctrl_selection==2){
    control_list$controlSpec <- control_list$timeMedDrugID
    control_list <- subset(control_list, DrugConc==0)
    control_list <- do.call(rbind, lapply(unique(control_list$controlSpec), avgControl, all_controls=control_list)) %>% data.frame()
  }else{
    control_list$controlSpec <- control_list$timeMedDrugConcID
    control_list <- do.call(rbind, lapply(unique(control_list$controlSpec), avgControl, all_controls=control_list)) %>% data.frame()
  }
  
  #naming
  colnames(control_list) <- c("ControlVal", "ControlSlot", "Time", "Drug", "DrugConc", "Medium", "Strain")
  control_list$Time <- as.numeric(control_list$Time)
  return(control_list)
}
assignControl <- function(x, ctrlList){
  # get control with the same controlSpec
  select_ctrl <- subset(ctrlList, controlSpec==x["controlSpec"])
  if(nrow(select_ctrl)==0){
    select_ctrl <- c() #dummy blanks if none found
  }else{
    # get one with closest time
    select_ctrl$dTime <- (select_ctrl$Time - as.numeric(x["Time"]))^2
    select_ctrl <- select_ctrl[select_ctrl$dTime == min(select_ctrl$dTime),] %>% as.vector()
  }
  return(select_ctrl)
}
normalize_to_Control <- function(raw_data, ctrl_data, separate_control=F, control_selection=0){
  # STEP 0 - Expand WellID
  raw_data <- cbind.data.frame(raw_data, do.call(rbind, strsplit(raw_data$WellId, split="-")))
  colnames(raw_data)[5:8] <- c("Drug", "DrugConc", "Medium", "Strain")
  
  # SELECTION: coordinate control OR else
  if(control_selection==5){
    # STEP 1 - Bind controls information to wells
    prc_NM <- left_join(raw_data, ctrl_data, by="Slot")
    prc_NM <- cbind.data.frame(prc_NM, do.call(rbind, strsplit(prc_NM$ControlType, split="_")))
    colnames(prc_NM)[10:11] <- c("Group", "CtrlType")
    prc_NM$GroupTimeID <- paste(prc_NM$Time, prc_NM$Group, sep="-")
    
    # STEP 2 - Extract control list
    control_list_names <- unique(prc_NM$GroupTimeID)
    control_list_names <- control_list_names[!grepl("ignore", control_list_names)]
    control_list <- lapply(control_list_names, avgControl_coord, all_data=prc_NM)
    
    control_list <- do.call(rbind, control_list) %>% data.frame()
    
    #res : average, wells, timepoint, drug, drug concentration, medium, strain
    colnames(control_list) <- c("AvgControl", "ControlSlots", "Time", "Drug", "DrugConc", "Medium", "Strain") 
    control_list$ControlPlate <- "On-plate control"
    control_list$GroupTimeID <- control_list_names
    control_list$ControlSlots <- paste(control_list$ControlPlate, " - ", 
                                       round(as.numeric(control_list$Time),2), "h - ", 
                                       control_list$ControlSlots, sep="")
    
    # STEP 3 - Bind controls to measurement
    control_list <- control_list[,c(1, 2, 9)]
    prc_NM <- left_join(prc_NM, control_list, by="GroupTimeID")
    
    # STEP 4 - Apply controls
    prc_NM$AvgControl[is.na(prc_NM$AvgControl)] <- -99
    prc_NM$correctedVal <- prc_NM$Measurement - as.numeric(prc_NM$AvgControl)
    prc_NM$correctedVal[prc_NM$AvgControl==-99] <- "-"
    prc_NM$ControlSlots[prc_NM$AvgControl==-99] <- "-"
    prc_NM$AvgControl[prc_NM$AvgControl==-99] <- "-"
    
  }else{
    if(!is.null(ctrl_data)){
      ctrl_data <- cbind.data.frame(ctrl_data, do.call(rbind, strsplit(ctrl_data$WellId, split="-")))
      colnames(ctrl_data)[5:8] <- c("Drug", "DrugConc", "Medium", "Strain")
    }
    
    if(control_selection!=0){
      # STEP 1 - Create Control List
      if(separate_control){
        #create control list
        control_list <- createControlList(ctrl_data, control_selection)
        #specify plate used for control
        control_list$ControlPlate <- "Separate control plate"
      }else{
        #create control list
        control_list <- createControlList(raw_data, control_selection)
        #specify plate used for control
        control_list$ControlPlate <- "On-plate control"
      }
      
      # STEP 2 - Create Control Spec on both measurement and control datasets
      if(control_selection==1){
        #medium only
        raw_data$controlSpec <- raw_data$Medium
        control_list$controlSpec <- control_list$Medium
      }else if(control_selection==2){
        #medium+drug only
        raw_data$controlSpec <- paste(raw_data$Medium, raw_data$Drug, sep="_")
        control_list$controlSpec <- paste(control_list$Medium, control_list$Drug, sep="_")
      }else{
        #medium+drug+concentration only
        raw_data$controlSpec <- paste(raw_data$Medium, raw_data$Drug, raw_data$DrugConc, sep="_")
        control_list$controlSpec <- paste(control_list$Medium, control_list$Drug, control_list$DrugConc, sep="_")
      }
      
      # STEP 3 - Assign Control Measurements
      prc_list <- apply(raw_data, 1, assignControl, ctrlList=control_list)
      for(i in c(1:length(prc_list))){
        #dummy filling for no-control wells
        if(is.null(prc_list[[i]])){
          prc_list[[i]] <- rep("-", 9)
        }
      }
      prc_NM <- do.call(rbind, prc_list)[,c(1:3, 8)]
      colnames(prc_NM)[3] <- c("ControlTime")
      prc_NM <- cbind.data.frame(raw_data, prc_NM)
      
      # STEP 4 - Calculate Normalized Value
      prc_NM$ControlVal[prc_NM$ControlVal=="-"] <- -99
      prc_NM$corrected_value <- prc_NM$Measurement - as.numeric(prc_NM$ControlVal)
      prc_NM$corrected_value[prc_NM$ControlVal==-99] <- "-"
      prc_NM$ControlVal[prc_NM$ControlVal==-99] <- "-"
    }else{
      prc_NM <- raw_data
      prc_NM$controlSpec <- "-"
      prc_NM$ControlVal <- "-"
      prc_NM$ControlSlot <- "-"
      prc_NM$ControlTime <- "-"
      prc_NM$ControlPlate <- "No control"
      prc_NM$corrected_value <- "-"
    }
  }
  
  return(prc_NM)
}
assignReplicateID_oneID <- function(x, all_data){
  #subset current data
  all_data <- subset(all_data, timeWellID==x)
  all_data$replicate_ID <- c(1:nrow(all_data))
  return(all_data)
}

#MAIN FUNCTION------------
mainFun <- function(platemap_address, inputwd, control_selection, 
                    control_map_address=NULL, control_meas_wd=NULL, separate_control=F){
  #EXTRACTION---------------------------
  #read platemap and measurement results
  plateMap <- ReadPlateMap(platemap_address)
  measResults <- Read_allMeasFile(inputwd)
  
  #combine raw data
  rawData_matrix <- left_join(plateMap, measResults, by="Slot")
  rawData_NM <- Create_LongFormat(rawData_matrix) #create long-format
  
  #REPEAT EXTRACTION FOR CONTROL PLATE (if prompted)------------------------
  controlData_NM <- NULL
  if(!is.null(control_map_address) & !is.null(control_meas_wd) & separate_control){
    #read platemap and measurement results
    control_Map <- ReadPlateMap(control_map_address)
    control_Res <- Read_allMeasFile(control_meas_wd)
    
    #combine raw controls
    controlData_matrix <- left_join(control_Map, control_Res, by="Slot")
    controlData_NM <- Create_LongFormat(controlData_matrix) #create long-format
  }else if(!separate_control & !is.null(control_map_address) & control_selection==5){
    #IF control selection == 5 (coordinate controls)
    #Read plate map
    controlData_NM <- read.csv(control_map_address, header=T)[,2:13] %>% t() %>% unlist() %>% as.vector()
    controlData_NM <- cbind.data.frame(as.vector(sapply(LETTERS[1:8], function(x) paste(x, c(1:12), sep=""))), 
                                       controlData_NM)
    colnames(controlData_NM) <- c("Slot", "ControlType")
    
  }else if(separate_control){
    errMessage <- "Control data missing!"
    control_selection <- 1
  }
  
  #PROCESSING-----------------------
  # Normalize to control
  prc_NM <- normalize_to_Control(rawData_NM, controlData_NM, separate_control, control_selection)
  
  # Assign replicate ID
  prc_NM$timeWellID <- paste(prc_NM$Time, prc_NM$WellId, sep="_")
  prc_NM <- do.call(rbind, lapply(unique(prc_NM$timeWellID), assignReplicateID_oneID, all_data=prc_NM))
  
  # Separating slot ID
  prc_NM$well_row <- substring(prc_NM$Slot, 1, 1)
  prc_NM$well_column <- substring(prc_NM$Slot, 2, nchar(prc_NM$Slot))
  
  # Ordering
  prc_NM <- prc_NM[order(as.numeric(prc_NM$well_column)),]
  prc_NM <- prc_NM[order(prc_NM$well_row),]
  prc_NM <- prc_NM[order(prc_NM$Time),]
 
  # Adding time specification to control
  if(control_selection!=5){
    prc_NM$ControlTime <- sapply(prc_NM$ControlTime, function(x) if(x!="-"){round(as.numeric(x), 2)}else{""})
    prc_NM$ControlSlot <- paste(prc_NM$ControlPlate, " - ", prc_NM$ControlTime, " hours - ", prc_NM$ControlSlot, sep="")
    
    # selecting columns
    prc_NM <- prc_NM[,c(5, 7, 8, 2, 6, 17, 18, 
                        16, 10, 11, 3, 14)]
  }else{
    prc_NM <- prc_NM[,c(5, 7, 8, 2, 6, 18, 19,
                        17, 13, 14, 3, 15)]
  }
  
 
  colnames(prc_NM) <- c("drug_name", "media_name", "strain_name", "time", "drug_concentration", "well_row", "well_column",
                        "replicate_ID", "correction_value", "correction_wells", "raw_measurement", "corrected_measurement")
  
  #blank correction well assignment
  prc_NM$correction_wells[prc_NM$correction_value=="-"] <- "-"
  
  #pass to global
  prc_NM <<- prc_NM
  rawData_NM <<- prc_NM
  controlData_NM <<- controlData_NM
  
  return(prc_NM)
}

#TROUBLESHOOTING-----------------
#measurement
#platemapAddress <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Analysis_studentTrials\\New CSV data + platemap\\20210506_JN_P001_E036_Cef_Mutants1947_imputtemplate.xlsx"

#measurement_wd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Analysis_studentTrials\\New CSV data + platemap\\inputs"

#controls
#controlMap_address <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Analysis_studentTrials\\New CSV data + platemap\\b_test.csv"

#dis <- mainFun(platemapAddress, measurement_wd, 1, 
#               NULL, NULL, separate_control=F)