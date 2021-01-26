#FUNCTIONS--------
#raw data readers
ExtractRawDat <- function(csv_dataset, readerID){
  if(readerID==1){
    #if fluostar omega without robot arm (newton)
    current_data <- as.vector(unlist(csv_dataset[c(10:17),c(2:13)]))
    if(typeof(current_data[1])=='character'){
      current_data <- as.numeric(gsub(",", ".", current_data))
    }
    
  }else if(readerID==2){
    #if fluostar omega WITH robot arm (newton)
    current_data <- csv_dataset[c(9:104),1]
    current_data <- gsub(" ", "", current_data)
    current_data <- sapply(current_data, function(x) as.numeric(substring(x, 5, nchar(x))))
    names(current_data) <- c()
  }
  
  #return final value
  return(current_data)
}

#times stamp readers
ExtractTimeStamp <- function(csv_dataset, readerID){
  if(readerID==1){
    #if fluostar omega without robot arm (newton)
    time_point <- c(csv_dataset[2,2], csv_dataset[2,3])
    time_point[1] <- substring(time_point[1], 7, nchar(time_point[1]))
    time_point[2] <- substring(time_point[2], 7, nchar(time_point[2]))
  }else if(readerID==2){
    #if fluostar omega WITH robot arm (newton)
    time_point <- strsplit(csv_dataset[2,], split=' ')[[1]][c(2, 5)]
    time_point[1] <- gsub("/", "-", time_point[1])
  }
  
  #return final value
  return(time_point)
}

#supporting functions
ControlSelector <- function(grand_res){
  #main iteration to re-parse name
  gres <- data.frame()
  for(i in c(1:length(grand_res[,1]))){
    nexItem <- strsplit(as.character(grand_res$variable[i]), split=" ", fixed=T)[[1]]
    if(length(nexItem)<3){
      #if the current item is an absolute blank
      nexItem <- c(nexItem, 
                   replicate(4-length(nexItem), "absolute_blank"))
    }else if(length(nexItem)==3){
      if(nexItem[2]==0){
        nexItem <- c(nexItem, "drug_blank")
      }else{
        nexItem <- c(nexItem, "conc_blank")
      }
    }
    
    gres <- rbind.data.frame(gres,
                             nexItem)
  }
  
  #rename column
  colnames(gres) <- c("DrugName", "Conc", "Medium", "Inoculum")
  
  #remove empty wells (no medium wells)
  grand_res <- grand_res[(gres$Medium != ""),]
  gres <- gres[(gres$Medium != ""),]
  
  #select "negatives" as concentration blanks
  #gres$Inoculum[grepl("neg", gres$Inoculum, ignore.case=T)] <- "conc_blank"
  
  #concatenate to grand result
  grand_res <- cbind.data.frame(grand_res$time, gres, grand_res$Absorbance)
  
  #rename grand result
  colnames(grand_res)[c(1,6)] <- c("time", "Absorbance")
  
  #make string unfortunate drug names (nalidixic acid)
  grand_res$DrugName <- sapply(grand_res$DrugName, function(x) toString(x))
  
  return(grand_res)
}
Blank_Substract_timepoint <- function(grand_res_time, blank){
  if(blank==1){
    #if choice is absolute blank
    #calculate mean absolute blank
    mean_blank <- mean(grand_res_time$Absorbance[grand_res_time$Inoculum=='absolute_blank'])
    #subtract blank from all absorbance values
    sub_Val <- grand_res_time$Absorbance - mean_blank
    
  }else if(blank==2){
    #if choice is blank per-drug+medium
    drugs <- unique(grand_res_time$DrugName)
    mediums <- unique(grand_res_time$Medium)
    #initiate subtracted value
    sub_Val <- grand_res_time$Absorbance
    #iterate through all drug types
    for(i in c(1:length(drugs))){
      for(j in c(1:length(mediums))){
        #subset current drug
        cur_data <- subset(grand_res_time, DrugName==drugs[i] & Medium==mediums[j])
        
        #calculate current mean absolute blank
        if(length(cur_data$Absorbance[cur_data$Inoculum=='drug_blank'])>0){
          cur_mean_blank <- mean(cur_data$Absorbance[cur_data$Inoculum=='drug_blank'])
        }else{
          cur_mean_blank <- 0
        }
        
        #subtract from absorbance values
        sub_Val[(grand_res_time$DrugName==drugs[i] & grand_res_time$Medium==mediums[j])] <- 
          sub_Val[(grand_res_time$DrugName==drugs[i] & grand_res_time$Medium==mediums[j])] - cur_mean_blank
      }
    }
  }else if(blank==3){
    #Specific Blanks
    main_data <- grand_res_time[!grepl("blank", grand_res_time$Inoculum),]
    main_blank <- grand_res_time[grepl("blank", grand_res_time$Inoculum),]
    
    #initiate subtracted value
    sub_Val <- grand_res_time$Absorbance
    
    #get main variable names
    main_names <- unique(main_blank$DrugName)
    main_concs <- unique(main_blank$Conc)
    for(i in c(1:length(main_names))){
      for(j in c(1:length(main_concs))){
        #subset blank dataset
        cur_data <- subset(main_blank, Conc==main_concs[j] & DrugName==main_names[i])
        if(length(cur_data)>0){
          #calculate mean blank at the current drug-concentration combination
          cur_mean_blank <- mean(cur_data$Absorbance)
          #subtract from subtracted value
          sub_Val[(grand_res_time$Conc==main_concs[j] & grand_res_time$DrugName==main_names[i])] <- 
            sub_Val[(grand_res_time$Conc==main_concs[j] & grand_res_time$DrugName==main_names[i])] - cur_mean_blank
        }
      }
    }
  }else{
    sub_Val <- grand_res_time$Absorbance
  }
  
  #concatenate result
  grand_res_time <- cbind.data.frame(grand_res_time, sub_Val)
  return(grand_res_time)
}
Blank_Substract_main <- function(grand_res, blank){
  #initiate new grand result
  new_grandRes <- data.frame()
  #iterate through all time points
  times <- unique(grand_res$time)
  for(i in c(1:length(times))){
    curSet <- grand_res[(grand_res$time==times[i]),]
    curRes <- Blank_Substract_timepoint(curSet, blank)
    #concatenate result
    new_grandRes <- rbind.data.frame(new_grandRes, curRes)
  }
  colnames(new_grandRes)[length(new_grandRes[1,])] <- "NormalizedAbsorbance"
  return(new_grandRes)
}

#MAIN FUNCTION-------
main <- function(directory, first_measurement, reader_id, blank_selection){
  
  #### PRE-PROCESSING ######
  #GET FILE NAMES AND PLATE MAP------
  files_in_dir <- list.files(directory)
  
  #get plate map; ensure one file selected
  plateMap_file <- files_in_dir[grepl("platemap", files_in_dir, ignore.case=T)]
  
  if(length(plateMap_file)!=1){
    if(length(plateMap_file)>1){
      errMessage <<- "Multiple plate maps found - make sure measurement data are in .csv format!"
    }else{
      errMessage <<- "Plate map not found!"
    }
  }
  #get read data
  absData_files <- files_in_dir[!grepl("PlateMap", files_in_dir, ignore.case=T)]
  
  #MAIN----------
  #getting time stamps
  first_measurement <- as.numeric(strsplit(first_measurement, split=':')[[1]])
  first_measurement <- first_measurement[1] + first_measurement[2]/60 + first_measurement[3]/60^2 #standardize to hours
  #read plate map
  plateMap <- paste(directory, "/", plateMap_file, sep='')
  plateMap <- if(grepl("xlsx", plateMap, fixed=T)){
    absData_files <- files_in_dir[!grepl("xlsx", files_in_dir, fixed=T)]
    empty_wells <- as.vector(unlist(read.xlsx(plateMap, sheetIndex=1, rowIndex=c(33:40), 
                                              colIndex=c(2:13), header=F)))
    #returned:
    as.vector(unlist(t(read.xlsx(plateMap, sheetIndex=1, rowIndex=c(57:64), colIndex=c(2:13), header=F))))
    
  }else{
    as.vector(unlist(read.csv(plateMap, header=T, as.is=T)))
  }
  
  #get index of empty wells
  
  ##
  #iterate through measurement reads
  mainData <- c()
  timeStamps <- c()
  for(i in c(1:length(absData_files))){
    #read current data
    curData <- paste(directory, "/", absData_files[i], sep='')
    curData <- read.csv(curData, sep=',', header=F)
    if(dim(curData)[2]==1){
      curData <- paste(directory, "/", absData_files[i], sep='')
      curData <- read.csv(curData, sep=';', header=F)
    }
    
    ## Format-specific
    #extract raw read data
    nex_rawData <- ExtractRawDat(curData, reader_id)
    mainData <- rbind.data.frame(mainData, nex_rawData)
    
    #extract time information
    timePoint <- ExtractTimeStamp(curData, reader_id)
    timeStamps <- rbind(timeStamps, timePoint)
    ## Format-specific
  }
  
  #STANDARDIZE TIME STAMP------
  #recap time stamps
  timeStamps <- apply(timeStamps, 1, 
                      function(x) chron(dates=x[1], times=x[2], format = c('d-m-y', 'h:m:s')))
  timeStamps <- (timeStamps - min(timeStamps)) * 24
  names(timeStamps) <- c()
  
  #add delta time for first measurement
  timeStamps <- timeStamps + first_measurement
  
  #combining time stamps
  colnames(mainData) <- plateMap
  
  #FILTERING-------
  #selecting non-empty wells (as columns in mainData)
  parseID <- c()
  for(i in c(1:length(plateMap))){
    curParsed <- strsplit(plateMap[i], split=' ')[[1]]
    if(curParsed[1]!=''){
      parseID <- c(parseID, T)
    }else{
      parseID <- c(parseID, F)
    }
  }
  
  mainData <- mainData[,parseID]
  
  #READJUSTING MAIN DATA-------
  #combining time stamps
  mainData <- cbind.data.frame(timeStamps, mainData)
  colnames(mainData)[1] <- "time.hours"
  rownames(mainData) <- c()
  
  #sort for troubleshooting
  mainData <- mainData[order(mainData$time.hours),]
  
  ###### MAIN PROCESSING #######
  grandRes <- data.frame()
  grandRes <- cbind.data.frame(timeStamps)
  grandErr <- data.frame()
  grandErr <- cbind.data.frame(timeStamps)
  
  #iterate through all unique replicates
  ids <- unique(colnames(mainData)[2:length(mainData[1,])])
  
  for(i in c(1:length(ids))){
    curData <- mainData[,(colnames(mainData)==ids[i])]
    
    #calculate mean and confidence interval distance from mean
    if(is.null(dim(curData))){
      grandRes <- cbind.data.frame(grandRes, curData)
      grandErr <- cbind.data.frame(grandErr, replicate(length(curData), 0))
    }else{
      grandRes <- cbind.data.frame(grandRes, 
                                   apply(curData, 1, function(x) mean(x)))
      grandErr <- cbind.data.frame(grandErr,
                                   apply(curData, 1, function(x) (sd(x)/sqrt(dim(curData)[2]))*
                                           qt(0.975, df=(dim(curData)[2]-1))))
    }
  }
  
  #renaming
  colnames(grandRes) <- c('time', ids)
  colnames(grandErr) <- colnames(grandRes)
  
  
  #ReFormat---------
  grandRes <- melt(grandRes, id='time', value.name='Absorbance')
  grandErr <- melt(grandErr, id='time', value.name='Err')
  
  #removing empty wells
  grandErr <- grandErr[(grandRes$variable != "0"),]
  grandRes <- grandRes[(grandRes$variable != "0"),]
  #add information about control
  grandRes <- tryCatch({
    grand_res <- ControlSelector(grandRes)
    grand_res <- Blank_Substract_main(grand_res, blank_selection)
    return(grand_res)
  },
  error=function(cond){
    if(errMessage=='SUCCESS'){
      errMessage <<- "Failed to subtract blank - please select the appropriate blank mode"
    }
    return(NULL)
  })
  
  return(grandRes)
}

#TROUBLESHOOTING-----------------
#main(directory, first_measurement, reader_id, blank_selection)
#directory <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Analysis_studentTrials"
#first_measurement <- "00:00:00"
#reader_id <- 2 #with robot arm
#blank_selection <- 4 #else; no blank
#errMessage <- ""
#dis <- main(dir, first, reader_type, blank)