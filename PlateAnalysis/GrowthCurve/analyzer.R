main <- function(directory, first_measurement){
  #### PRE-PROCESSING ######
  #GET FILE NAMES AND PLATE MAP------
  files_in_dir <- list.files(directory)
  
  #get plate map; ensure one file selected
  plateMap_file <- files_in_dir[grepl("PlateMap", files_in_dir, fixed=T)]
  if(length(plateMap_file)!=1){
    if(length(plateMap_file)>1){
      errMessage <- "Multiple plate maps found"
    }else{
      errMessage <- "Plate map not found!"
    }
  }
  
  #get read data
  absData_files <- files_in_dir[!grepl("PlateMap", files_in_dir, fixed=T)]
  
  
  #MAIN----------
  first_measurement <- as.numeric(strsplit(first_measurement, split=':')[[1]])
  #read plate map
  plateMap <- paste(directory, "\\", plateMap_file, sep='')
  plateMap <- as.vector(unlist(read.csv(plateMap, header=T, as.is=T)))
  
  #iterate through measurement reads
  mainData <- c()
  timeStamps <- c()
  for(i in c(1:length(absData_files))){
    #read current data
    curData <- paste(directory, "\\", absData_files[i], sep='')
    curData <- read.csv(curData, sep=',', header=F)
    if(dim(curData)[2]==1){
      curData <- paste(directory, "\\", absData_files[i], sep='')
      curData <- read.csv(curData, sep=';', header=F)
    }
    
    #extract raw read data
    nex_rawData <- as.vector(unlist(curData[c(10:17),c(2:13)]))
    if(typeof(nex_rawData[1])=='character'){
      nex_rawData <- as.numeric(gsub(",", ".", nex_rawData))
    }
    mainData <- rbind.data.frame(mainData, nex_rawData)
    
    #extract time information
    timePoint <- c(curData[2,2], curData[2,3])
    timePoint[1] <- substring(timePoint[1], 7, nchar(timePoint[1]))
    timePoint[2] <- substring(timePoint[2], 7, nchar(timePoint[2]))
    
    timeStamps <- rbind(timeStamps, timePoint)
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

#plt <- ggplot(data=grandRes, aes(x=time, y=Absorbance))+
#  geom_point()+geom_line()+theme_bw()+
#  facet_wrap(~variable)+
#  scale_y_continuous(trans='log10', limits=c(lower_bound_axis, upper_bound_axis))+
#  geom_errorbar(aes(ymin=minVal, ymax=maxVal), width=0.5)
  
#plt