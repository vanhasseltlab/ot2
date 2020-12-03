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


#MAIN FUNCTION-------
main <- function(directory, first_measurement, reader_id){
  #### PRE-PROCESSING ######
  #GET FILE NAMES AND PLATE MAP------
  files_in_dir <- list.files(directory)
  
  #get plate map; ensure one file selected
  plateMap_file <- files_in_dir[grepl("PlateMap", files_in_dir, fixed=T)]
  if(length(plateMap_file)==0){
    #otherwise use xlsx as plate map
    plateMap_file <- files_in_dir[grepl("xlsx", files_in_dir, fixed=T)]
  }
  
  if(length(plateMap_file)!=1){
    if(length(plateMap_file)>1){
      errMessage <<- "Multiple plate maps found - make sure measurement data are in .csv format!"
    }else{
      errMessage <<- "Plate map not found!"
    }
  }
  #get read data
  absData_files <- files_in_dir[!grepl("PlateMap", files_in_dir, fixed=T)]
  
  #MAIN----------
  #getting time stamps
  first_measurement <- as.numeric(strsplit(first_measurement, split=':')[[1]])
  
  #read plate map
  plateMap <- paste(directory, "/", plateMap_file, sep='')
  plateMap <- if(grepl("xlsx", plateMap, fixed=T)){
    absData_files <- files_in_dir[!grepl("xlsx", files_in_dir, fixed=T)]
    #returned:
    as.vector(unlist(read.xlsx(plateMap, sheetIndex=1, rowIndex=c(57:64), 
                               colIndex=c(2:13), header=F)))
  }else{
    as.vector(unlist(read.csv(plateMap, header=T, as.is=T)))
  }
  
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
  timeStamps <- chron(dates=timeStamps[,1], times=timeStamps[,2],
                      format = c('d-m-y', 'h:m:s'))
  names(timeStamps) <- c()
  
  #standardize to first measurement
  timeStamps <- timeStamps - min(timeStamps)
  
  #add delta time for first measurement
  timeStamps <- as.character(timeStamps)
  new_timeStamps <- c()
  for(i in c(1:length(timeStamps))){
    nex_timeStamps <- as.numeric(strsplit(timeStamps[i], split=':')[[1]])
    nex_timeStamps <- nex_timeStamps + first_measurement
    nex_timeStamps <- nex_timeStamps[3] + 60*(nex_timeStamps[2] + nex_timeStamps[1]*60)
    new_timeStamps <- rbind(new_timeStamps, nex_timeStamps)
  }
  
  #FILTERING-------
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
  mainData <- cbind.data.frame(new_timeStamps, mainData)
  #renaming columns
  colnames(mainData) <- c("time.seconds", plateMap[parseID])
  rownames(mainData) <- c()
  
  ###### MAIN PROCESSING #######
  grandRes <- data.frame()
  grandRes <- cbind.data.frame(new_timeStamps)
  grandErr <- data.frame()
  grandErr <- cbind.data.frame(new_timeStamps)
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
  colnames(grandRes) <- c('time', ids)
  colnames(grandErr) <- colnames(grandRes)
  
  #GRAPH---------
  grandRes <- melt(grandRes, id='time', value.name='Absorbance')
  grandErr <- melt(grandErr, id='time', value.name='Err')
  minVal <- grandRes$Absorbance - grandErr$Err
  minVal <- sapply(minVal, function(x) max(0, x))
  maxVal <- grandRes$Absorbance + grandErr$Err
  upper_bound_axis <- round(max(maxVal), 1)
  lower_bound_axis <- round((min(grandRes$Absorbance)), 2)
  minVal <- sapply(minVal, function(x) max(lower_bound_axis, x))
  grandRes <- cbind.data.frame(grandRes, 
                               minVal,
                               maxVal)
  colnames(grandRes) <- c('time', 'variable', 'Absorbance', 'minVal', 'maxVal')
  
  grandRes$time <- grandRes$time / 3600 #convert to hours
  return(grandRes)
}
