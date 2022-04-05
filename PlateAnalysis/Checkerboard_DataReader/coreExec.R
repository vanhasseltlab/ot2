#LIBRARIES----------
library(dplyr)
library(reshape2)
library(tidyr)
library(rlist)
library(readxl)
library(chron)

#FUNCTIONS-------------
readPlateMap <- function(main_wd){
  # distinguishing plate map
  files_list <- list.files(main_wd)
  plateMap <- files_list[grepl(".xlsx", files_list)]
  
  # reading plate map
  plateMap <- read_xlsx(paste0(main_wd, "/", plateMap), skip=55)
  plateMap <- plateMap[,c(2:ncol(plateMap))] %>% unlist()
  wells <- sapply(seq(1, 12, 1), function(i) paste0(LETTERS[seq(1, 8, 1)], i)) %>% as.vector()
  plateMap <- cbind.data.frame(Well = wells,
                               Fill = plateMap)
  
  # parse fill description
  fillDescription <- sapply(plateMap$Fill, function(x){
    parsed_fill <- strsplit(x, split=" ")[[1]]
    
    # check if fill is not empty
    if(length(parsed_fill) > 2){
      drugs <- strsplit(parsed_fill[1], split="_")[[1]]
      concentrations <- strsplit(parsed_fill[2], split="_")[[1]]
      medium <- parsed_fill[3]
      strain <- if(length(parsed_fill)==3){""}else{parsed_fill[4]}
      blank <- 0
    }else{
      drugs <- ""
      concentrations <- ""
      medium <- parsed_fill[2]
      strain <- ""
      blank <- 1
    }
    
    return(c(drugs, concentrations, medium, strain, blank))
  })
  
  # detect number of drugs
  nDrugs <- vapply(fillDescription, FUN.VALUE=1, length) %>% max()
  nDrugs <- (nDrugs - 2) / 2
  
  # re-parsing empty fill descriptions
  fillDescription <- lapply(fillDescription, function(x){
    if(length(x) < (nDrugs * 2) + 2){
      value <- c(replicate(nDrugs, x[1]), replicate(nDrugs, x[2]), x[3], x[4], x[5])
    }else{
      value <- x
    }
    return(value)
  }) %>% list.rbind()
  
  # creating column names
  column_names <- c(paste0("Drug_", seq(1, nDrugs, 1)), 
                    paste0("Concentration_", seq(1, nDrugs, 1)),
                    "Medium", "Strain", "Blank")
  colnames(fillDescription) <- column_names
  
  # combine plate map
  plateMap <- cbind.data.frame(plateMap, fillDescription)
  
  return(plateMap)
}
readMeasurementFile <- function(current_file, main_wd){
  # read measurement file
  meas_file <- read.csv(paste0(main_wd, "/", current_file))
  
  # parse measurement time
  meas_time <- strsplit(meas_file[1,], split=" ")[[1]][c(2, 5)]
  meas_time <- chron(dates=meas_time[1], times=meas_time[2], format=c(dates='d/m/y', times="h:m:s"))
  
  # extract measurement values and well index
  meas_file <- meas_file[c(8:nrow(meas_file)),] %>% strsplit(split=": ") %>% 
    list.rbind()
  meas_vals <- gsub(" ", "", meas_file[,2]) %>% as.numeric()
  meas_well <- sapply(meas_file[,1], function(x) if(substring(x, 2, 2)=="0"){gsub("0", "", x)}else{x})
  
  # combine measurement output
  meas_output <- data.frame(Well=meas_well, Value=meas_vals)
  meas_output$Time <- meas_time
  
  return(meas_output)
}

#MAIN FUNCTION-----------
mainExec <- function(main_dir, first_measurement = 0, use_control="1"){
  # read plate map
  plateMap <- readPlateMap(main_dir)
  
  # read other files
  files_list <- list.files(main_dir)
  files_list <- files_list[grepl(".csv", files_list)]
  
  # read all measurement files
  measOutput <- lapply(files_list, readMeasurementFile, main_wd=main_dir) %>% list.rbind()
  measOutput <- left_join(plateMap, measOutput, by="Well")
  
  # parse time
  measOutput$ActualTime <- as.numeric(measOutput$Time - min(measOutput$Time) + first_measurement)
  
  # control handling
  if(use_control=="1"){
    #remove uneccesary rows
    measOutput <- filter(measOutput, Blank=="0") %>%
      select(-Blank)
  }else if(use_control=='2'){
    # remove blanks
    measOutput <- filter(measOutput, Blank=="0")
    
    # get control labels
    drug_columns <- colnames(measOutput)[grepl("Drug", colnames(measOutput))]
    measOutput$ControlLabel <- apply(measOutput, 1, function(x){
      paste0(c(x[drug_columns], x['ActualTime']), collapse="_")
    })
    
    # isolate controls
    control_table <- filter(measOutput, Strain=="")
    control_columns <- colnames(control_table)[grepl("oncentration", colnames(control_table))]
    control_table$control <- apply(control_table, 1, function(x){
      x[control_columns] %>% as.numeric() %>% sum()
    })
    
    # get control values; take means if necessary
    control_table <- subset(control_table, control==0) %>%
      select(ControlLabel, Value) %>%
      rename("ControlVal"=Value)
    
    control_table <- lapply(unique(control_table$ControlLabel), function(x){
      current_control <- subset(control_table, ControlLabel==x)
      
      # generating output
      current_line <- current_control[1,]
      current_line$ControlVal <- current_control$ControlVal %>% mean()
      return(current_line)
    }) %>% list.rbind()
    
    # bind to measOutput
    measOutput <- left_join(measOutput, control_table, by="ControlLabel") %>%
      mutate(Value = Value - ControlVal) 
  }
  
  return(measOutput)
}

#MAIN EXEC----------
#dqs <- mainExec(main_dir)