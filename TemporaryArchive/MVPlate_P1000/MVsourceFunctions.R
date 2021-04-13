options(stringsAsFactors = F)
#FUNCTIONS LIBRARY------------
# read input
GetStockList <- function(file_name){
  res <- read_xlsx(file_name, range="C1:M2") %>% data.frame() %>%
    select_if(function(x) any(!is.na(x)))
  sl_res <- unlist(res)
  if(is.character(sl_res[1])){
    sl_res <- gsub(",", ".", sl_res) %>% as.numeric()
  }
  
  names(sl_res) <- colnames(res)
  
  return(sl_res)
}
GetWellVols <- function(file_name){
  res <- read_xlsx(file_name, sheet=1, range="C5:C6", col_names=F) %>% unlist()
  names(res) <- c("TotalVol", "FillVol")
  return(res)
}
Get_nPlate <- function(file_name){
  res <- read_xlsx(file_name, sheet=1, range="F6", col_names=F) %>% unlist()
  return(res)
}
GetPlateMap <- function(file_name){
  #read
  res <- read_xlsx(file_name, 1, range="B57:M64", col_names=F) %>% data.frame()
  rownames(res) <- LETTERS[1:8]
  colnames(res) <- sapply(c(1:12), toString)
  
  #parse to vector
  map <- c()
  for(row in c(1:8)){
    #subset
    curRow <- unlist(res[row,])
    
    #get info
    well_id <- sapply(c(1:12), function(x) paste(LETTERS[row], toString(x), sep=''))
    curRow <- cbind(well_id, curRow)
    
    #concatenate results
    map <- rbind(map, curRow)
  }
  
  #parse names
  fin_map <- c()
  parsed_names <- sapply(map[,2], function(x) strsplit(x, ' ', fixed=T))
  
  for(i in c(1:length(parsed_names))){
    #if well is empty
    if(map[i,2]!="0" & map[i,2]!=""){
      if(parsed_names[[i]][1]=="0"){
        #if both drug name and inoculum is not filled, then it is a blank fill well (which might as well be blank control)
        nex_info <- c("FILL",
                      "NA",                 #drug name
                      parsed_names[[i]][1], #concentration
                      parsed_names[[i]][2], #solvent
                      "NA")                 #inoculum
      }else{
        #if all info is complete OR inoculum not added
        nex_info <- c(paste(parsed_names[[i]][1], parsed_names[[i]][2], parsed_names[[i]][3], sep=' '),
                      parsed_names[[i]][1], #drug name
                      parsed_names[[i]][2], #concentration
                      parsed_names[[i]][3], #solvent
                      parsed_names[[i]][4]) #inoculum
      }
      #concatenate well
      fin_map <- rbind(fin_map, nex_info)
      rownames(fin_map) <- c()
    }
  }
  
  #remove blanks from map
  map <- map[(map[,2]!=""),]
  map <- map[(map[,2]!="0"),]
  
  #concatenate info
  fin_map <- cbind.data.frame(map, fin_map)
  colnames(fin_map) <- c('Well', 'fillID', 'solID', 'DrugType', 'DrugConc', 'Solvent', 'Inoc')
  fin_map$Inoc[is.na(fin_map$Inoc)] <- "NA"
  
  #expecting dot/comma decimal separator
  if(is.character(fin_map$DrugConc[1])){
    fin_map$DrugConc <- gsub(",", ".", fin_map$DrugConc) %>% as.numeric()
  }
  
  #dropping factor
  fin_map[] <- lapply(fin_map, as.character)
  return(fin_map)
}

#preparation
CreateSolList <- function(plate_map, total_vol_well, inoc_vol, stock_list, n_plate){
  
  plate_map$solID <- gsub(",", ".", plate_map$solID)
  
  #get occurence
  occ <- table(plate_map$solID)
  occurences <- cbind.data.frame(names(occ), as.numeric(occ)* as.numeric(n_plate)) 
  colnames(occurences) <- c("solID", "Occ")
  
  #combine data frames
  fin_list <- plate_map[,c(3:6)] %>% distinct() %>% left_join(occurences, by="solID") %>% filter(solID!="FILL")
  colnames(fin_list) <- c('SolID', 'DrugType', 'DrugConc', 'Solvent', 'Occurence')
  fin_list[] <- lapply(fin_list, as.character) #convert to character
  
  #calculating required dilution volume
  fin_list$Occurence <- as.numeric(fin_list$Occurence)
  fin_list$DrugConc <- as.numeric(fin_list$DrugConc)
  fin_list <- CalculateDilVolume(fin_list, total_vol_well, inoc_vol, stock_list)
  
  #dropping factors
  fin_list[] <- lapply(fin_list, as.character)
  
  return(fin_list)
}
CalculateDilVolume <- function(sol_list, total_vol_well, inoc_vol, stock_list){
  #calculate initially required amount
  drugSol_well <- total_vol_well - inoc_vol
  solAmt <- sol_list$Occurence * drugSol_well + 150 #adds 10 uL excess
  sol_list <- cbind.data.frame(sol_list, solAmt)
  
  #recalculate amount to adjust with the incoming inoculum
  sol_list$DrugConc <- as.numeric(sol_list$DrugConc) * 
    total_vol_well / (total_vol_well - inoc_vol)
  
  #remove no-drug solutions from the list
  sol_list <- sol_list[(sol_list$DrugType != ""),]
  
  #initiate new list
  new_solList <- c()
  #iterate through all drug and solvent types
  solvents <- unique(sol_list$Solvent)
  drugs <- unique(sol_list$DrugType)
  
  for(i in c(1:length(solvents))){
    for(j in c(1:length(drugs))){
      
      #subset the current drug type
      curList <- subset(sol_list, DrugType==drugs[j] & Solvent==solvents[i])
      
      #perform following actions only if not null
      if(nrow(curList)>0){
        #order according to concentration
        curList <- curList[order(as.numeric(curList$DrugConc)),]
        
        ####################################################################
        #add items if additional pre-dilutions is required
        new_curList <- c()
        for(q in c(1:length(curList[,1]))){
          new_curList <- rbind.data.frame(new_curList, curList[q,])
          
          #get the current dilution factor
          if(q == length(curList[,1])){
            conc_hi <- stock_list[curList$DrugType[q]]
            curDilFac <- conc_hi/curList$DrugConc[q]
          }else{
            conc_hi <- curList$DrugConc[q+1]
            curDilFac <- conc_hi/curList$DrugConc[q]
          }
          
          #check if the current dilution factor is more than 10
          if(curList$DrugConc[q] > 0 & curDilFac > 10){
            
            ## further dilution required
            iterator <- T
            #iterate while dilution factor higher than 10
            while(curDilFac > 10){
              if(iterator){
                nex_newCurList <- curList[q,]
                iterator <- F
              }
              
              #update the item list
              nex_newCurList$DrugConc <- conc_hi/10
              nex_newCurList$Occurence <- 0
              nex_newCurList$SolID <- paste(nex_newCurList$DrugType, 
                                            nex_newCurList$DrugConc, 
                                            nex_newCurList$Solvent,
                                            sep=' ')
              
              #concatenate dilution to list
              new_curList <- rbind.data.frame(new_curList, nex_newCurList)
              
              #re-calculate current dilution factor
              conc_hi <- nex_newCurList$DrugConc
              if(q == length(curList[,1])){
                curDilFac <- conc_hi/curList$DrugConc[q]
              }else{
                curDilFac <- conc_hi/curList$DrugConc[q]
              }
              
            }
          }
        }
        
        curList <- new_curList
        curList$solAmt <- as.numeric(curList$solAmt)
        curList$DrugConc <- as.numeric(curList$DrugConc)
        curList <- curList[order(curList$DrugConc),]
        
        #####################################################################
        
        #check amount needed from above
        needed_from_above <- c()
        for(m in c(1:length(curList[,1]))){
          
          if(m<length(curList[,1])){ 
            #usual dilution from pre-diluted stock
            amt_needed <- curList$solAmt[m]*curList$DrugConc[m]/curList$DrugConc[m+1]
            
            #check if it is lower than the minimum pipette volume
            if(amt_needed < 30 & amt_needed > 0){
              #set amount from above to 30
              curList$solAmt[m] <- curList$solAmt[m] * 30 / amt_needed
              amt_needed <- 30 
            }
            
            needed_from_above <- c(needed_from_above, amt_needed)
            
            #add amount to higher concentration
            curList$solAmt[m+1] <- curList$solAmt[m+1] + amt_needed
            
          }else{
            
            #calculate amount for initial dilution
            amt_needed <- curList$solAmt[m]*curList$DrugConc[m]/stock_list[curList$DrugType[m]]
            #check if it is lower than the minimum pipette volume
            
            if(amt_needed < 30 & amt_needed > 0){
              #set amount from above to 30
              amt_needed <- amt_needed
              curList$solAmt[m] <- curList$solAmt[m] * 30 / amt_needed
              amt_needed <- 30 
            }
            
            needed_from_above <- c(needed_from_above, amt_needed)
          }
        }
      }
      
      nexItem <- cbind(curList, unlist(needed_from_above))
      new_solList <- rbind(new_solList, nexItem)
    }
  }
  
  #renaming
  colnames(new_solList)[length(new_solList[1,])] <- 'AmtHi'
  
  #calculate required solvent amount
  solventAmt <- as.numeric(new_solList$solAmt) - as.numeric(new_solList$AmtHi)
  
  #check required tube size
  reqTube <- replicate(length(new_solList[,1]), "15_Falcon")
  if(max(new_solList$solAmt) >= 14*1000){
    createNull <- T
  }else{createNull <- F}
  
  
  #concatenate Info
  new_solList <- cbind.data.frame(new_solList, solventAmt, reqTube)
  
  #dropping factor
  new_solList[] <- lapply(new_solList, as.character)
  
  if(createNull){
    new_solList <- NULL
    errMessage <<- "OVER CAPACITY!"
  }
  return(new_solList)
}
CreateDilMap <- function(sol_list, deckMap, stock_list){
  #initiate map
  small_coords <- c(1, 1)
  large_coords <- c(1, 1)
  spare_coords <- c(1, 1)
  
  small_dilMap <- cbind()
  large_dilMap <- cbind()
  spare_dilMap <- cbind()
  
  #pre-filling small coordinates
  for(i in c(1:length(stock_list))){
    small_coords[2] <- small_coords[2] + 1
    if(small_coords[2]>5){ #is a falcon tube rack (15 mL)
      small_coords[1] <- small_coords[1] + 1
      small_coords[2] <- 1
    }
  } 
  
  #iterate through all items in solution list
  for(i in c(1:length(sol_list[,1]))){
    if(as.numeric(sol_list$solAmt[i])<=1300){
      #check if the main rack is full
      if(small_coords[1]<5){
        #if the main rack is still available
        nexItem <- c(paste(LETTERS[small_coords[1]], toString(small_coords[2]), sep=''),
                     toString(sol_list$SolID[i]))
        #concatenate item
        small_dilMap <- rbind(small_dilMap, nexItem)
        
        #update coordinates
        small_coords[2] <- small_coords[2]+1
        if(small_coords[2]>6){
          small_coords[2] <- 1
          small_coords[1] <- small_coords[1] + 1
        }
      }else{
        #if the main rack is full
        nexItem <- c(paste(LETTERS[spare_coords[1]], toString(spare_coords[2]), sep=''),
                     toString(sol_list$SolID[i]))
        
        #concatenate item
        spare_dilMap <- rbind(spare_dilMap, nexItem)
        
        #update coordinates
        spare_coords[2] <- spare_coords[2]+1
        if(spare_coords[2]>5){
          spare_coords[2] <- 1
          spare_coords[1] <- spare_coords[1] + 1
        }
      }
    }else{
      #if solution amount is larger
      if(large_coords[1]<4){
        #if the main rack is available
        nexItem <- c(paste(LETTERS[large_coords[1]], toString(large_coords[2]), sep=''),
                     toString(sol_list$SolID[i]))
        
        #concatenate item
        large_dilMap <- rbind(large_dilMap, nexItem)
        
        #update coordinates
        large_coords[2] <- large_coords[2]+1
        if(large_coords[2]>5){
          large_coords[2] <- 1
          large_coords[1] <- large_coords[1] + 1
        }
      }else{
        #if the main rack is full
        nexItem <- c(paste(LETTERS[spare_coords[1]], toString(spare_coords[2]), sep=''),
                     toString(sol_list$SolID[i]))
        
        #concatenate item
        spare_dilMap <- rbind(spare_dilMap, nexItem)
        
        #update coordinates
        spare_coords[2] <- spare_coords[2]+1
        if(spare_coords[2]>5){
          spare_coords[2] <- 1
          spare_coords[1] <- spare_coords[1] + 1
        }
      }
    }
  }
  
  #get labware locations
  small_dilMap <- cbind(small_dilMap, replicate(length(small_dilMap[,1]),
                                                names(deckMap)[match("15ml_Falcon_stock", deckMap)]))
  spare_dilMap <- cbind(spare_dilMap, replicate(length(spare_dilMap[,1]),
                                                names(deckMap)[match("15_Falcon_spare", deckMap)]))
  large_dilMap <- cbind(large_dilMap, replicate(length(large_dilMap[,1]),
                                                names(deckMap)[match("15_Falcon_main", deckMap)]))
  
  #check if racks are empty
  if(length(small_dilMap[,1])==0){
    small_dilMap <- cbind(c('m'), c('m'), c('m'))
  }
  if(length(large_dilMap[,1])==0){
    large_dilMap <- cbind(c('m'), c('m'), c('m'))
  }
  if(length(spare_dilMap[,1])==0){
    spare_dilMap <- cbind(c('m'), c('m'), c('m'))
  }
  
  #re-assign names
  dil_map <- rbind(small_dilMap, large_dilMap, spare_dilMap)
  colnames(dil_map) <- c('Slot', 'Fill', 'Labware')
  dil_map <- data.frame(dil_map)
  dil_map <- dil_map[(dil_map$Slot != 'm'),]
  
  #dropping factor
  dil_map[] <- lapply(dil_map, as.character)
  
  return(dil_map)
}

#commands
Cmd_InitDist <- function(deck_map, sol_list, solvent_map, dil_map){
  #INITIATE COMMAND LIST
  cmd_list <- c()
  tipID <- 1
  
  ###########
  #iterate through all solvent types in solution list
  solvents <- unique(sol_list$Solvent)
  for(j in c(1:length(solvents))){
    cur_sol_list <- subset(sol_list, Solvent==toString(solvents[j]))
    #iterate through all item in the current list
    for(i in c(1:length(cur_sol_list[,1]))){
      #create next command list
      nexCmd <- c(names(deck_map)[match('Solvent', deck_map)], #source ware = solvent rack
                  solvent_map[solvent_map[,2]==toString(cur_sol_list$Solvent[i]),1], #source slot = solvent tube
                  dil_map$Labware[dil_map$Fill==toString(cur_sol_list$SolID[i])], #target ware = dilution rack
                  dil_map$Slot[dil_map$Fill==toString(cur_sol_list$SolID[i])], #target slot = dilution tube
                  cur_sol_list$solventAmt[i], #amount transferred
                  "0", tipID, 'Initial solvent distribution')
      
      #concatenate command
      cmd_list <- rbind(cmd_list, nexCmd)
    }
    
    #update tip id
    tipID <- tipID + 1
  }
  ##############
  return(cmd_list)
}
Cmd_HiDrug <- function(cmd_list, sol_list, stock_map, deck_map, dil_map){
  #get tip id
  tipID <- as.numeric(cmd_list[length(cmd_list[,1]),7]) + 1
  
  #iterate through all drug types
  for(i in c(1:length(stock_map[,1]))){
    #subset
    crSolList <- subset(sol_list, DrugType==stock_map[i,2])
    
    #iterate through all solvent types
    solvents <- unique(crSolList$Solvent)
    for(j in c(1:length(solvents))){
      #subset highest concentration only
      curSolList <- subset(crSolList, Solvent==solvents[j])
      curSolList <- subset(curSolList, DrugConc==max(as.numeric(curSolList$DrugConc)))
      
      #create command
      nexCmd <- c(names(deck_map)[match("15ml_Falcon_stock", deck_map)], #source ware = stock rack
                  stock_map[stock_map[,2]==curSolList$DrugType[1],1], #source slot = solvent tube
                  dil_map$Labware[dil_map$Fill==curSolList$SolID[1]], #target ware = dilution rack
                  dil_map$Slot[dil_map$Fill==curSolList$SolID[1]], #target slot = dilution tube
                  curSolList$AmtHi,
                  curSolList$AmtHi, tipID, 'Initial stock dilution')
      
      #update tip ID
      tipID <- tipID + 1
      
      #concatenate command
      cmd_list <- rbind(cmd_list, nexCmd)
    }
  }
  return(cmd_list)
}
Cmd_SerialDil <- function(cmd_list, sol_list, dil_map){
  tipID <- max(as.numeric(cmd_list[,7]), na.rm=T) + 1
  #iterate through all drug and solvents
  drugs <- unique(sol_list$DrugType)
  solvents <- unique(sol_list$Solvent)
  for(i in c(1:length(drugs))){
    for(j in c(1:length(solvents))){
      #subset current list
      cur_SolList <- subset(sol_list, DrugType==drugs[i] & Solvent==solvents[j])
      #check if not null
      if(length(cur_SolList[,1])>0){
        #order list in descending concentration
        cur_SolList <- cur_SolList[order(as.numeric(cur_SolList$DrugConc), decreasing=T), ]
        
        #iterate through the list
        for(m in c(1:(length(cur_SolList[,1])-1))){
          nexCmd <- c(dil_map$Labware[dil_map$Fill==cur_SolList$SolID[m]], #select source labware
                      dil_map$Slot[dil_map$Fill==cur_SolList$SolID[m]], #select source slot
                      dil_map$Labware[dil_map$Fill==cur_SolList$SolID[m+1]],#select target labware
                      dil_map$Slot[dil_map$Fill==cur_SolList$SolID[m+1]],  #select target slot
                      cur_SolList$AmtHi[m+1], #amount to transfer
                      cur_SolList$AmtHi[m+1], tipID, paste('Serially diluting to ', cur_SolList$SolID[m+1]))
          #concatenate command
          if(cur_SolList$AmtHi[m+1]!=0){
            cmd_list <- rbind(cmd_list, nexCmd)
            #update tip ID
            tipID <- tipID+1
          }
        }
      }
    }
  }
  return(cmd_list)
}
Cmd_DrugSolDist <- function(cmd_list, dil_map, plate_map, deck_map, well_info, n_plates){
  tipID <- max(as.numeric(cmd_list[,7]), na.rm=T) + 1
  transV <- well_info[1] - well_info[2]
  
  #standardize decimal separator
  plate_map$solID <- sapply(plate_map$solID, function(x) gsub(",", ".", x))
  dil_map$Fill <- sapply(dil_map$Fill, function(x) gsub(",", ".", x))
  
  #get plate names
  plates <- sapply(LETTERS[c(1:n_plates)], function(x) paste("96-well", x, sep="_"))
  
  #iterate through all items in the dilution map
  for(i in c(1:length(dil_map[,1]))){
    target_wells <- plate_map$Well[plate_map$solID==dil_map$Fill[i]]
    
    #iterate through all target plates
    for(j in c(1:length(plates))){
      nexCmd <- c(dil_map$Labware[i],
                  dil_map$Slot[i],
                  names(deck_map)[match(plates[j], deck_map)],
                  paste(target_wells, collapse=', '),
                  transV, 0, tipID, paste('Distributing ', dil_map$Fill[i]))
      #concatenate result
      cmd_list <- rbind(cmd_list, nexCmd)
    }
    
    
    #update tip ID only at the end of the current solution
    tipID <- tipID + 1
  }
  
  return(cmd_list)
}
Cmd_FillOuter <- function(plate_map, deck_map, solvent_map, well_info, cmd_list, n_plates){
  tipID <- max(as.numeric(cmd_list[,7]), na.rm=T) + 1
  
  #rename water if needed
  solvent_map[(solvent_map[,2]=='water' | solvent_map[,2]=='Water'),2] <- 'WATER'
  
  #subset current plate map
  cur_plate_map <- subset(plate_map, solID=='FILL')
  
  #get plate names
  plates <- sapply(LETTERS[c(1:n_plates)], function(x) paste("96-well", x, sep="_"))
  
  #iterate through all solvent types
  solvents <- unique(solvent_map[,2])
  for(i in c(1:length(solvents))){
    #select targets
    target_wells <- paste(cur_plate_map$Well[cur_plate_map$Solvent==solvents[i]], collapse=', ')
    
    #iterate through all target plates
    for(j in c(1:length(plates))){
      #creating command list
      nexCmd <- c(names(deck_map)[match('Solvent', deck_map)],
                  solvent_map[solvent_map[,2]==solvents[i],1],
                  names(deck_map)[match(plates[j], deck_map)],
                  target_wells,
                  well_info[1], 0, tipID, 'Filling outer wells with WATER')
      
      #concatenate result
      cmd_list <- rbind(cmd_list, nexCmd)
      
      #update tip after every plate
      tipID <- tipID + 1
    }
  }
  
  return(cmd_list)
}
Cmd_SeparateLong <- function(cmd_list){
  #initiate new command list
  new_cmd_list <- c()
  #iterate through all command lines
  for(i in c(1:length(cmd_list[,1]))){
    d_tip <- 0
    
    #select current wells
    rem_wells <- strsplit(cmd_list[i,4], split=', ', fixed=T)[[1]]
    while(length(rem_wells)>0){
      n_wells <- min(8, length(rem_wells))
      nex_wells <- rem_wells[1:n_wells]
      
      #update rem_wells
      rem_wells <- rem_wells[-c(1:n_wells)]
      
      #create next command
      nex_command <- cmd_list[i,]
      nex_command[,4] <- paste(nex_wells, collapse=', ')
      nex_command[,7] <- as.numeric(cmd_list[i,7]) + d_tip
      d_tip <- d_tip + 1
      
      #concatenate command
      new_cmd_list <- rbind(new_cmd_list, nex_command)
    }
    #correct tip id
    cmd_list[c(i:length(cmd_list[,1])),7] <- as.numeric(cmd_list[c(i:length(cmd_list[,1])),7]) + d_tip - 1
  }
  
  return(new_cmd_list)
}

#counters
Cal_SolAmt <- function(deck_map, solvent_map, cmd_list){
  req_amt <- c()
  rack_position <- names(deck_map)[match('Solvent', deck_map)]
  for(i in c(1:length(solvent_map[,1]))){
    relCmdList <- subset(cmd_list, SourceLabware==rack_position & SourceSlot==solvent_map[i,1])
    
    #get number of target wells per-row
    n_well <- unlist(sapply(relCmdList$TargetSlot, function(x) length(strsplit(x, split=', ', fixed=T)[[1]])))
    
    #calculate required amount
    solvent_amt <- sum(as.numeric(relCmdList$TransAmt) * n_well)
    
    #concatenate amount
    req_amt <- c(req_amt, solvent_amt)
  }
  
  #put excess of 2 mL
  req_amt <- req_amt + 2000
  #place minimum of 10 mL
  req_amt[req_amt<10000] <- 10000
  #translate to mL
  req_amt <- req_amt/1000
  #round up
  req_amt <- ceiling(req_amt)
  
  #concatenate result
  solvent_map <- cbind(solvent_map, req_amt)
  
  #naming solvent map
  solvent_map <- data.frame(solvent_map)
  names(solvent_map) <- c('Slot', 'Name', 'RequiredAmount')
  
  #Place labware information
  Labware <- replicate(length(solvent_map[,1]), names(deck_map)[match('Solvent', deck_map)])
  Unit <- replicate(length(solvent_map[,1]), "mL")
  Type <- replicate(length(solvent_map[,1]), "50 mL Falcon Tube")
  Category <- replicate(length(solvent_map[,1]), "SOLVENT")
  
  #integrate result
  solvent_map <- cbind.data.frame(Category, Labware, Type, solvent_map, Unit)
  rownames(solvent_map) <- c()
  
  #removing factors
  solvent_map[] <- lapply(solvent_map, as.character)
  return(solvent_map)
}
Cal_StockAmt <- function(sol_list, stock_list, stock_map, deck_map){
  #initiate amount list
  amt_list <- replicate(length(stock_list), 0)
  
  #iterate
  drugs <- unique(sol_list$DrugType)
  solvents <- unique(sol_list$Solvent)
  #iterate through all possible combinations of the two
  for(i in c(1:length(drugs))){
    for(j in c(1:length(solvents))){
      #subset
      curList <- subset(sol_list, DrugType==drugs[i] & Solvent==solvents[j])
      
      #perform if not null
      if(length(curList)>0){
        #get required amount
        cur_reqAmt <- as.numeric(curList$AmtHi[as.numeric(curList$DrugConc)==max(as.numeric(curList$DrugConc))])
        
        #add to amount list
        amt_list[names(stock_list)==curList$DrugType[1]] <- as.numeric(amt_list[names(stock_list)==curList$DrugType[1]]) + cur_reqAmt
      }
    }
  }
  
  #make output
  stock_list <- cbind.data.frame(names(stock_list), unlist(stock_list), amt_list)
  colnames(stock_list) <- c('Name', 'Conc', 'RequiredAmount')
  rownames(stock_list) <- c()
  
  #add excess
  stock_list$RequiredAmount <- as.numeric(stock_list$RequiredAmount) + 300
  #place minimum
  stock_list$RequiredAmount[as.numeric(stock_list$RequiredAmount)<700] <- 700
  #round up
  stock_list$RequiredAmount <- ceiling(as.numeric(stock_list$RequiredAmount)/100)*100
  
  #get well locations
  well_loc <- stock_map[stock_map[,2]==stock_list$Name,1]
  stock_map <- cbind.data.frame(well_loc, stock_list)
  colnames(stock_map)[1] <- 'Slot'
  stock_map <- stock_map[,c(1, 2, 4)]
  
  #additional informations
  Labware <- replicate(length(stock_map[,1]), names(deck_map)[match("15ml_Falcon_stock", deck_map)])
  Unit <- replicate(length(stock_map[,1]), "uL")
  Type <- replicate(length(stock_map[,1]), "15 mL Falcon Tube")
  Category <- replicate(length(stock_map[,1]), "DRUG STOCK")
  
  #integrate results
  stock_map <- cbind.data.frame(Category, Labware, Type, stock_map, Unit)
  rownames(stock_map) <- c()
  
  #removing factors
  stock_map[] <- lapply(stock_map, as.character)
  
  return(stock_map)
}
Cal_DilTubes <- function(dil_map){
  #get number of occurence
  occs <- table(dil_map$Labware)
  
  outputMap <- cbind(names(occs), occs, replicate(length(occs), '15_Falcon'))
  #outputMap[outputMap[,1]=='labware_5',3] <- '1.5_Eppendorf' #removed in this version (MV plate)
  outputMap <- data.frame(outputMap)
  colnames(outputMap) <- c('Labware', 'RequiredAmount', 'Name')
  rownames(outputMap) <- c()
  
  #additional informations
  Category <- replicate(length(occs), 'EMPTY TUBES FOR DILUTION')
  Type <- replicate(length(occs), '-')
  Slot <- replicate(length(occs), '-')
  Unit <- replicate(length(occs), 'tubes')
  
  #integrate
  outputMap <- cbind.data.frame(Category, outputMap$Labware, Type, Slot, outputMap$Name,
                                outputMap$RequiredAmount, Unit)
  colnames(outputMap) <- c('Category', 'Labware', 'Type', 'Slot', 'Name', 'RequiredAmount', 'Unit')
  return(outputMap)
}
Cal_DeckAdjustment <- function(cmd_list, deck_map, dil_tubes, n_plate){
  dil_tubes <- dil_tubes
  deck <- matrix(as.character(c(12:1)), ncol=3, byrow=T)
  deck <- deck[,c(3, 2, 1)]
  deck_map <- matrix(deck_map, ncol=3, byrow=T)
  
  fin_deck <- c()
  for(i in c(1:length(deck_map[,1]))){
    fin_deck <- rbind(fin_deck, deck[i,], deck_map[(length(deck_map[,1])-i+1),])
  }
  
  
  #check if spare tube rack was required
  if(!('labware_8' %in% dil_tubes$Labware)){
    fin_deck[4,2] <- '(empty)'
  }
  
  #check if plates required
  if(n_plate < 6){
    fin_deck[8,3] <- "(empty)"
    if(n_plate < 5){
      fin_deck[8,2] <- "(empty)"
      if(n_plate < 4){
        fin_deck[8,1] <- "(empty)"
        if(n_plate < 3){
          fin_deck[6,3] <- "(empty)"
          if(n_plate < 2){
            fin_deck[6,2] <- "(empty)"
          }
        }
      }
    }
  } 
  return(fin_deck)
}
Int_CreateCmdList <- function(deck_map, sol_list, solvent_map, inoc_map,
                              dil_map, stock_map, well_info, plate_map, n_plate){
  
  #1. DISTRIBUTE SOLVENT
  cmdlist <- tryCatch({
    Cmd_InitDist(deck_map, sol_list, solvent_map, dil_map)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Initial distribuion failed"
    }
    return(NA)
  })
  
  #2. DISTRIBUTE HIGHEST DRUG CONCENTRATION
  cmdlist <- tryCatch({
    Cmd_HiDrug(cmdlist, sol_list, stock_map, deck_map, dil_map)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Preliminary/First stock dilution failed to initiate"
    }
    return(NA)
  })
  
  #3. SERIAL DILUTION
  cmdlist <- tryCatch({
    Cmd_SerialDil(cmdlist, sol_list, dil_map)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Serial dilution error"
    }
    return(NA)
  })
  
  #4. DISTRIBUTING DRUG SOLUTION
  cmdlist <- tryCatch({
    Cmd_DrugSolDist(cmdlist, dil_map, plate_map, deck_map, well_info, n_plate)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Failed to distribute solutions to 96-well plates"
    }
    return(NA)
  })
  
  #5. FILLING OUTER WELLS
  cmdlist <- tryCatch({
    Cmd_FillOuter(plate_map, deck_map, solvent_map, well_info, cmdlist, n_plate)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Failed to fill outer/no-drug wells"
    }
    return(NA)
  })
  
  
  #NAMING
  cmdlist <- data.frame(cmdlist)
  colnames(cmdlist) <- c('SourceLabware', 'SourceSlot', 'TargetLabware',
                         'TargetSlot', 'TransAmt', 'MixAmt', 'TipID', 'Comment')
  
  #removing factors
  cmdlist[] <- lapply(cmdlist, as.character)
  return(cmdlist)
}

#adjustments
ConvertAmtList_MVtoMC <- function(amt_list){
  new_amtList <- cbind.data.frame(amt_list$Labware, amt_list$Slot, amt_list$Name,
                                  replicate(length(amt_list[,1]), 0), amt_list$RequiredAmount)
  colnames(new_amtList) <- c('Labware', 'Slot', 'Fill', 'Conc', 'Vol')
  return(new_amtList)
}
cal_amtList_Excess <- function(amt_list, cmd_list, deck_map){
  tubes <- amt_list
  tubes$Vol <- 0
  
  for(i in c(1:length(cmd_list[,1]))){
    #if current tube+slot is sourced
    if(cmd_list$SourceLabware[i] %in% tubes$Labware){
      if(cmd_list$SourceSlot[i] %in% tubes$Slot[tubes$Labware == cmd_list$SourceLabware[i]]){
        #check volume used after transfer
        volUsed_after <- tubes$Vol[tubes$Labware == cmd_list$SourceLabware[i] & tubes$Slot == cmd_list$SourceSlot[i]] + 
          as.numeric(cmd_list$TransAmt[i])*length(strsplit(cmd_list$TargetSlot[i], split=', ')[[1]])
        
        deltaV <- as.numeric(cmd_list$TransAmt[i])*length(strsplit(cmd_list$TargetSlot[i], split=', ')[[1]])
        
        if((volUsed_after <= 45000 &  #switch limit to 45000 so that maximum + excess is 48 mL
            cmd_list$SourceLabware[i] == 'labware_11') | 
           (volUsed_after <= 14000 & 
            cmd_list$SourceLabware[i] == 'labware_10')){
          #if volume is still acceptable
          #calculate volume
          tubes$Vol[tubes$Labware == cmd_list$SourceLabware[i] & tubes$Slot == cmd_list$SourceSlot[i]] <- tubes$Vol[tubes$Labware == cmd_list$SourceLabware[i] & tubes$Slot == cmd_list$SourceSlot[i]] + 
            as.numeric(cmd_list$TransAmt[i])*length(strsplit(cmd_list$TargetSlot[i], split=', ')[[1]])
        }else{
          #if volume exceeded limit
          #place new tube
          if(cmd_list$SourceLabware[i] == 'labware_11'){
            sol_or_stock <- 'tock'
          }else{
            sol_or_stock <- 'olvent'
          }
          
          solvent_map <- subset(tubes, Labware==toString(deck_map[grepl(sol_or_stock, deck_map[,2]),1]))
          #get last filled tube
          last_filled <- solvent_map$Slot[length(solvent_map[,1])]
          new_slot <- c(substring(last_filled, 1, 1), substring(last_filled, 2, 2))
          new_slot[2] <- as.numeric(new_slot[2])+1
          if(as.numeric(new_slot[2])>3){
            new_slot[1] <- LETTERS[which(LETTERS==new_slot[1])+1]
            new_slot[2] <- 1
          }
          new_slot <- paste(new_slot, collapse='')
          #assign new tube in 'tubes'
          nexDat <- cbind.data.frame(cmd_list$SourceLabware[i], 
                                     new_slot,
                                     tubes$Fill[tubes$Labware==cmd_list$SourceLabware[i] & tubes$Slot==cmd_list$SourceSlot[i]],
                                     tubes$Conc[tubes$Labware==cmd_list$SourceLabware[i] & tubes$Slot==cmd_list$SourceSlot[i]],
                                     deltaV, stringsAsFactors=F)
          colnames(nexDat) <- colnames(tubes)
          tubes <- rbind.data.frame(tubes, nexDat, stringsAsFactors=F)
          #update command lines
          old_slots <- cmd_list[c(i:length(cmd_list[,1])),]
          
          new_slots <- old_slots
          new_slots$SourceSlot[old_slots$SourceLabware == cmd_list$SourceLabware[i] &
                                 old_slots$SourceSlot == cmd_list$SourceSlot[i]] <- new_slot
          cmd_list$SourceSlot[c(i:length(cmd_list[,1]))] <- new_slots$SourceSlot
        }
      }
    }
  }
  
  #order items
  amt_list <- tubes[order(tubes$Labware),]
  
  for(i in c(1:length(amt_list[,1]))){
    
  }
  #round up; add excess
  amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('olvent',deck_map[,2]),1])] <- ceiling(amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('olvent',deck_map[,2]),1])]/1000) + 3 #3 mL excess
  amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('olvent',deck_map[,2]),1])] <- sapply(amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('olvent',deck_map[,2]),1])], function(x) max(x, 5)) #5 mL minimum
  amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('olvent',deck_map[,2]),1])] <- sapply(amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('olvent',deck_map[,2]),1])], function(x) min(x, 48))
  
  amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('tock',deck_map[,2]),1])] <- ceiling(amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('tock',deck_map[,2]),1])]/100)*100 + 300
  amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('tock',deck_map[,2]),1])] <- sapply(amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('tock',deck_map[,2]),1])], function(x) max(x, 700)) #placing a minimum of 700 uL
  amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('tock',deck_map[,2]),1])] <- sapply(amt_list$Vol[amt_list$Labware==toString(deck_map[grepl('tock',deck_map[,2]),1])], function(x) min(x, 14000))
  
  #return result
  res <- list(cmd_list, amt_list)
  return(res)
}
ConvertAmtList_MCtoMV <- function(new_allAmt){
  Category <- sapply(new_allAmt$Labware, function(x) if(x=='labware_11'){"SOLVENT"}else{"STOCK"})
  
  Type <- sapply(new_allAmt$Labware, 
                 function(x) if(x=='labware_11'){"50 mL Falcon Tube"}else{"15 mL Falcon Tube"})
  Unit <- sapply(new_allAmt$Labware, function(x) if(x=='labware_11'){"mL"}else{"uL"})
  
  fin_allAmt <- cbind.data.frame(Category, new_allAmt$Labware, Type,
                                 new_allAmt$Slot, new_allAmt$Fill, new_allAmt$Vol, Unit)
  colnames(fin_allAmt) <- c('Category', 'Labware', 'Type', 'Slot', 'Name', 'RequiredAmount', 'Unit')
  return(fin_allAmt)
}

selectPipette_oneGroup <- function(cmd_list, current_tipGroup){
  #subset
  current_group <- subset(cmd_list, TipID==current_tipGroup)
  
  #check if transfer amount are all above 100
  if(sum(as.numeric(current_group$TransAmt)<100)==0){
    #operation if each operations in group are above 100 uL
    group_operation <- current_group
    group_operation$pipette <- replicate(nrow(group_operation), "P1000")
  }else{
    #operation if not all operations in group are above 100 uL
    p1000_operation <- subset(current_group, as.numeric(TransAmt)>300)
    p1000_operation$pipette <- replicate(nrow(p1000_operation), "P1000")
    p300_operation <- subset(current_group, as.numeric(TransAmt)<=300)
    p300_operation$pipette <- replicate(nrow(p300_operation), "P300")
    
    #bind
    group_operation <- rbind.data.frame(p1000_operation, p300_operation)
  }
  return(group_operation)
}

#MAIN---------
main <- function(file_path, file_name=""){
  #READ PLATE------
  stockList <- tryCatch({
    GetStockList(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Input file error - stockList"
    }
    return(NA)
  })
  
  wellInfo <- tryCatch({
    GetWellVols(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Input file error - wellInfo"
    }
    return(NA)
  })
  
  plateMap <- tryCatch({
    GetPlateMap(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Input file error - plateMap"
    }
    return(NA)
  })
  
  plateNum <- tryCatch({
    Get_nPlate(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Input file error - plate number"
    }
    return(NA)
  })
  
  #GET SOLUTION LIST AND DILUTION SCHEME-----------
  solList <- tryCatch({
    CreateSolList(plateMap, wellInfo["TotalVol"], wellInfo["FillVol"], stockList, plateNum)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Failed to initiate solution list"
    }
    return(NA)
  })
  
  #-1. LOADING DECK MAP-----------
  ## error not expected
  if(errMessage==""){
    deckMap <- c('96-well_D', '96-well_E', '96-well_F',
                 '96-well_A', '96-well_B', '96-well_C',
                 'tip', '15_Falcon_spare', '15_Falcon_main',
                 '15ml_Falcon_stock', 'Solvent', 'TRASH')
    names(deckMap) <- sapply(c(1:12), function(x) paste('labware', toString(x), sep='_'))
    
    # 0. LOAD LABWARES--------
    #solvents
    #initiate map
    coords <- c(1, 1)
    solventMap <- c()
    #iterate through all solvents in platemap
    solvents <- unique(plateMap$Solvent)
    for(i in c(1:length(solvents))){
      nexItem <- c(paste(LETTERS[coords[1]], toString(coords[2]), sep=''), toString(solvents[i]))
      #place to map
      solventMap <- rbind(solventMap, nexItem)
      
      #update fill coordinates
      coords[2] <- coords[2]+1
      if(coords[2]>3){
        coords[2] <- 1
        coords[1] <- coords[1] + 1
      }
    }
    
    #stock
    #initiate map
    coords <- c(1, 1)
    stockMap <- c()
    #iterate through all items in stockList
    for(i in c(1:length(stockList))){
      nexItem <- c(paste(LETTERS[coords[1]], toString(coords[2]), sep=''), names(stockList)[i])
      
      #place to map
      stockMap <- rbind(stockMap, nexItem)
      
      #update fill coordinates
      coords[2] <- coords[2]+1
      if(coords[2]>5){ #as 15 mL Falcon tube racks
        coords[2] <- 1
        coords[1] <- coords[1] + 1
      }
    }
  }
  
  #assign slots for diluted solutions
  dilMap <<- tryCatch({
    CreateDilMap(solList, deckMap, stockList)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Failed to create dilution map"
    }
    return(NA)
  })
  # 1. CREATE COMMAND LIST-----------
  if(errMessage==""){
    #initiate map
    coords <- c(1, 1)
    inocMap <- c() #assume rack is 15 mL Falcon tube rack
    #iterate through all inoculum types in plate
    inocs <- unique(plateMap$Inoc)
    inocs <- inocs[inocs!='NA']
    for(i in c(1:length(inocs))){
      nexItem <- c(paste(LETTERS[coords[1]], toString(coords[2]), sep=''), toString(inocs[i]))
      inocMap <- rbind(inocMap, nexItem)
      #update coordinate
      coords[2] <- coords[2]+1
      if(coords[2]>5){
        coords[2] <- 1
        coords[1] <- coords[1] + 1
      }
    }
    
    cmdList <- Int_CreateCmdList(deckMap, solList, solventMap, inocMap,
                                  dilMap, stockMap, wellInfo, plateMap, plateNum)
    
    cmdList <- Cmd_SeparateLong(cmdList)
    cmdList[] <- lapply(cmdList, as.character)
  }
  # 2. BUNDLING OUTPUT-------
  if(errMessage==""){
    allAmt <- rbind.data.frame(Cal_SolAmt(deckMap, solventMap, cmdList),
                               Cal_StockAmt(solList, stockList, stockMap, deckMap))
    allAmt[] <- lapply(allAmt, as.character)
    
    # 3. CALCULATE REQUIRED NUMBER OF ITEMS--------
    dilTubes <- Cal_DilTubes(dilMap) #error not expected?
    
    # 4. DECK LAYOUT FOR USER---------
    finDeck <- Cal_DeckAdjustment(cmdList, deckMap, dilTubes, plateNum) #error not expected?
    
    # 5. Adjusting Required Volume Amounts
    allAmt2 <- ConvertAmtList_MVtoMC(allAmt)
    deckMap2 <- cbind(sapply(c(1:12), function(x) paste("labware_", toString(x), sep='')),
                      as.vector(deckMap))
    
    adjustment <- cal_amtList_Excess(allAmt2, cmdList, deckMap2)
    allAmt <- ConvertAmtList_MCtoMV(adjustment[[2]])
    cmdList <- adjustment[[1]]
    
    
    #handling P1000
    ## due to deck space limitation, all operations in current protocol is altered to P300
    #cmdList <- do.call(rbind, lapply(unique(cmdList$TipID), selectPipette_oneGroup, cmd_list = cmdList))
    cmdList$pipette <- "P300"
    
    #################
    #CREATING OUTPUT#
    #################
    #Command List-------
    dis <- replicate(length(allAmt[,1]), "NA")
    all_amt <- cbind.data.frame(allAmt[,c(2, 4, 5)], dis, allAmt[,6], dis, dis, dis, dis, stringsAsFactors=F)
    colnames(all_amt) <- colnames(cmdList)
    
    ware_num <- unlist(finDeck[c(1, 3, 5, 7),])
    ware_fil <- unlist(finDeck[c(2, 4, 6, 8),])
    ware_fil <- ware_fil[order(as.numeric(ware_num))]
    ware_num <- ware_num[order(as.numeric(ware_num))]
    ware_num <- sapply(ware_num, function(x) paste('labware_', toString(x), sep=''))
    
    dis <- replicate(length(ware_num), "NA")
    fin_deck <- cbind.data.frame(ware_num, ware_fil, 
                                 dis, dis, dis, dis, dis, dis, dis)
    
    cmdList_output <<- list(c(">Amount List"), all_amt,
                            c('>CommandLines'), cmdList,
                            c(">PlateMap"), fin_deck)
    
    #User Commands-----------
    #adjusting file name
    allAmt <- rbind.data.frame(allAmt, dilTubes)
    usercmd_output <<- list(finDeck, allAmt)
  }else{
    allAmt <- errMessage
  }
  
  return(allAmt)
}

#TROUBLESHOOTING---------
errMessage <<- ""
fpath <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\MVPlate"
#dataName <- "20210412_IM_P001_E025_Ciprofloxacin_IC90_InputTemplate.xlsx"
dataName <- "MV_InputTemplate.xlsx"
main(paste(fpath, dataName, sep="//"))