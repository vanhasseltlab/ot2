#FUNCTIONS--------------
Sum_and_Round <- function(q){
  minAmt <- ceiling((sum(q)/100)) * 100
  if(minAmt<1200){minAmt <- max(minAmt + 200, 300) #200 uL excess for eppendorfs; minimum of 300 uL
  }else if(minAmt<13000){minAmt <- max(ceiling((minAmt + 1000)/1000)*1000, 2000) #1 mL excess for 15 mL Falcon tubes; minimum of 2 mL
  }else{minAmt <- max(ceiling((minAmt + 3000)/1000)*1000, 3000)} #3 mL excess and minimum amount for 15 mL Falcon tubes
  return(minAmt)
}
ReadInputs <- function(input_file){
  #reading stock information
  stock_info <<- read_xlsx(input_file, sheet=1, skip=1, col_names=T)[,1:4] %>% data.frame()
  
  #reading volume information
  vol_info <- read_xlsx(input_file, sheet=1, n_max=2, col_names=F)
  vol_info <- vol_info[9:ncol(vol_info)] %>% data.frame() %>% t()
  
  colnames(vol_info) <- vol_info[1,]
  vol_info <- vol_info[-1,]
  vol_info <- vol_info[(!is.na(vol_info[,1]) & 
                          !is.null(vol_info[,1])), ]  #removing volInfo empty row name
  
  #reading solution details
  sol_details <- read_xlsx(input_file, sheet=1, col_names=T) %>% data.frame()
  sol_details <- sol_details[,c(10:ncol(sol_details))] 
  
  sol_details <- t(cbind(stock_info[,3], sol_details[-1,]))
  colnames(sol_details) <- stock_info[,2]
  sol_details <- sol_details[(!is.na(sol_details[,1]) & 
                                !is.null(sol_details[,1])), ] #remove empty row names
  
  #reading manual tube fill
  manual_fill <- read_xlsx(input_file, sheet=1, col_names=T, skip=1)[,1:6] %>% data.frame()
  
  #return
  return(list(stock_info, vol_info, sol_details, manual_fill))
}
Calculate_ReqStockAmount <- function(conc_details, amount_details){
  reqStockVol <- c()
  #iterate through all solution types
  for(i in c(2:length(conc_details[,1]))){
    #calculate required amounts
    nex_reqStockAmt <- as.numeric(conc_details[i,]) * 
      as.numeric(amount_details$FinAmt[i-1]) / as.numeric(conc_details[1,])
    #concatenate result
    reqStockVol <- rbind(reqStockVol, nex_reqStockAmt)
  }
  reqStockVol <- reqStockVol*1000 #switch to uL
  #rename rows and columns
  colnames(reqStockVol) <- colnames(conc_details)
  rownames(reqStockVol) <- rownames(conc_details[-1,])
  reqStockVol <- data.frame(reqStockVol)
  return(reqStockVol)
}
AssignTubes <- function(vol_list){
  tube_type <- replicate(length(vol_list), "15 Falcon")
  tube_type[vol_list <= 1350] <- "1.5 Eppendorf"
  tube_type[vol_list > 13500] <- "50 Falcon"
  return(tube_type)
}
AssignSlots <- function(amt_list, deck_map, manual_fill){
  #ASSIGNING MANUAL FILLING
  #combine with manual filling info
  amt_list <- cbind.data.frame(amt_list, manual_fill)
  
  #check if amount is enough
  amt_list$VolCheck <- amt_list$Amt <= amt_list$ManualVol
  amt_list$VolCheck[!amt_list$VolCheck] <- "Volume insufficient - tube automatically re-assigned"
  amt_list$VolCheck[amt_list$VolCheck=="TRUE"] <- ""
  
  #replace with manual filling if required volume is satisfied
  amt_list$Amt <- apply(amt_list, 1, 
                        function(x) if(x["VolCheck"]==""){x["ManualVol"]}else{x["Amt"]})
  amt_list$TubeType <- apply(amt_list, 1, 
                             function(x) if(x["VolCheck"]==""){x["ManualTube"]}else{x["TubeType"]})
  
  #LOCATION ASSIGNMENT
  #initiate labware location
  falcon15_wares <- sapply(which(grepl("15", deck_map)), 
                           function(x) paste("labware_", x, sep=""))
  falcon50_wares <- sapply(which(grepl("50Falcon_A", deck_map, ignore.case = T)), 
                           function(x) paste("labware_", x, sep=""))
  epp_wares <- sapply(which(grepl("1.5", deck_map)), 
                      function(x) paste("labware_", x, sep=""))
  
  #initiate tube racks
  falcon15_rack <- c(1, 1, 1)
  falcon50_rack <- c(1, 1, 1)
  epp_rack <- c(1, 1, 1)
  
  #initiate slot array
  slotArr <- c()
  wareArr <- c()
  
  #iterate through all items in amt_list
  for(i in c(1:length(amt_list[,1]))){
    if(grepl("15", amt_list$TubeType[i])){
      #if 15 mL Falcon
      #assign slot
      slotArr <- c(slotArr,
                   paste(LETTERS[falcon15_rack[2]], toString(falcon15_rack[3]), sep=""))
      #assign ware
      wareArr <- c(wareArr, falcon15_wares[falcon15_rack[1]])
      
      #update rack information
      if(falcon15_rack[3] < 5){
        #if space still available in current row
        falcon15_rack[3] <- falcon15_rack[3] + 1
      }else{
        #if space not available in current row
        falcon15_rack[3] <- 1 #reset column count in row
        if(falcon15_rack[2] < 3){
          #if space still available in current rack
          falcon15_rack[2] <- falcon15_rack[2] + 1 #move row
        }else{
          #if space no longer available in the current rack
          falcon15_rack[1] <- falcon15_rack[1] + 1 #move rack
          falcon15_rack[2] <- 1 #reset row count in plate 
        }
      }
    }else if(grepl("50", amt_list$TubeType[i])){
      #if 50 mL Falcon
      #assign slot
      slotArr <- c(slotArr,
                   paste(LETTERS[falcon50_rack[2]], toString(falcon50_rack[3]), sep=""))
      #assign ware
      wareArr <- c(wareArr, falcon50_wares[falcon50_rack[1]])
      
      #update rack information
      if(falcon50_rack[3] < 3){
        falcon50_rack[3] <- falcon50_rack[3] + 1
      }else{
        if(falcon50_rack[2] < 2){
          falcon50_rack[2] <- falcon50_rack[2] + 1
        }else{
          falcon50_rack[1] <- falcon50_rack[1] + 1
          falcon50_rack[2] <- 1
        }
        falcon50_rack[3] <- 1
      }
    }else{
      #if Eppendorf
      #assign slot
      slotArr <- c(slotArr,
                   paste(LETTERS[epp_rack[2]], toString(epp_rack[3]), sep=""))
      #assign ware
      wareArr <- c(wareArr, epp_wares[epp_rack[1]])
      
      #update rack information
      if(epp_rack[3] < 6){
        epp_rack[3] <- epp_rack[3] + 1
      }else{
        if(epp_rack[2] < 4){
          epp_rack[2] <- epp_rack[2] + 1
        }else{
          epp_rack[1] <- epp_rack[1] + 1
          epp_rack[2] <- 1
        }
        epp_rack[3] <- 1
      }
    } 
  }
  
  #concatenate result
  amt_list <- amt_list[,c(1, 2, 6)]
  columnNames <- colnames(amt_list)
  amt_list <- cbind.data.frame(rownames(amt_list), amt_list,  wareArr, slotArr)
  colnames(amt_list)[1] <- "Solution"
  colnames(amt_list)[(ncol(amt_list)-1):ncol(amt_list)] <-  c("Labware", "Slot")
  amt_list <- amt_list[,c(1:3, 5, 6, 4)]
  
  return(amt_list)
}
Assign_MediumSlot <- function(dqs){
  slotnum <- c(1:length(dqs[,1]))
  slotlet <- replicate(length(slotnum), "A")
  slotlet[slotnum>3] <- "B"
  slotnum[slotnum>3] <- slotnum[slotnum>3] - 3
  slot <- paste(slotlet, slotnum, sep="")
  dqs <- cbind.data.frame(dqs, slot)
  return(dqs)
}
Create_Commands <- function(transfer_amounts, solution_list){
  #initiate command list
  cmd_list <- c()
  
  #iterate through all items in the stock
  for(i in c(1:length(transfer_amounts[1,]))){
    #get source labware and slot location
    source_ware <- solution_list$Labware[solution_list$Solution==colnames(transfer_amounts)[i]]
    source_slot <- solution_list$Slot[solution_list$Solution==colnames(transfer_amounts)[i]]
    
    #get target labware and slot location
    target_ware <- sapply(row.names(transfer_amounts), 
                          function(x) solution_list$Labware[solution_list$Solution==x])
    target_slot <- sapply(row.names(transfer_amounts), 
                          function(x) solution_list$Slot[solution_list$Solution==x])
    
    
    #assign new command lines
    nexLines <- cbind(replicate(length(target_ware), source_ware),
                      replicate(length(target_ware), source_slot),
                      target_ware, target_slot, 
                      transfer_amounts[,i], 0, #no mixing
                      c(1:length(target_ware)), "Dilution")
    
    #remove 0 transfer amounts; concatenate result
    cmd_list <- rbind(cmd_list, nexLines[nexLines[,5]!=0,])
  }
  colnames(cmd_list) <- c("SourceLabware", "SourceSlot",	"TargetLabware",
                         "TargetSlot",	"TransAmt",	"MixAmt",	"TipID",	"Comment")
  cmdList <- data.frame(cmd_list)
  #recalculate tip ID
  cmdList$TipID <- c(1:length(cmd_list[,1]))+1
  return(cmdList)
}
CreateRobotCommands <- function(init_amt_list, command_lines, deck_map){
  #create amount list
  init_amt_list <- cbind.data.frame(init_amt_list$Labware, init_amt_list$Slot,
                                    init_amt_list$Solution, 
                                    replicate(length(init_amt_list[,1]), ""),
                                    init_amt_list$Amt,
                                    replicate(length(init_amt_list[,1]), ""),
                                    replicate(length(init_amt_list[,1]), ""),
                                    replicate(length(init_amt_list[,1]), ""),
                                    replicate(length(init_amt_list[,1]), ""))
  colnames(init_amt_list) <- colnames(command_lines)
  
  #create deck map
  deck_map <- cbind.data.frame(sapply(c(1:12), function(x) paste("labware_", toString(x), sep="")), 
                               deck_map,
                               replicate(length(deck_map), ""),
                               replicate(length(deck_map), ""),
                               replicate(length(deck_map), ""),
                               replicate(length(deck_map), ""),
                               replicate(length(deck_map), ""),
                               replicate(length(deck_map), ""),
                               replicate(length(deck_map), ""))
  colnames(deck_map) <- colnames(command_lines)
  
  #concatenate items
  command_lines <- rbind.data.frame(c(">AmountList", replicate(8, "")),
                                    init_amt_list,
                                    c(">CommandLines", replicate(8, "")),
                                    command_lines,
                                    c(">PlateMap", replicate(8, "")),
                                    deck_map) 
  
  return(command_lines)
}
Adjust_Unit <- function(deck_map, amt_req){
  sol_rack <- c(paste("labware_", which(grepl("solvent", deck_map, ignore.case=T)), sep=""),
                paste("labware_", which(grepl("50Falcon_A", deck_map, ignore.case=T)), sep=""))
  amt_req$Unit[amt_req$Labware %in% sol_rack] <- "mL"
  amt_req$Amt[amt_req$Labware %in% sol_rack] <- as.numeric(amt_req$Amt[amt_req$Labware %in% sol_rack])/1000
  return(amt_req)
}
Distribute_WaterRack <- function(deck_map, amt_req, cmd_list){
  solvent_rack <- cbind.data.frame(as.vector(sapply(c(1:2), function(x) paste(LETTERS[x], c("1", "2", "3"), sep=""))),
                                   replicate(6, 0))
  colnames(solvent_rack) <- c("Slot", "FillAmt")
  solvent_loc <- paste("labware", which(grepl("solvent", deck_map, ignore.case=T)), sep="_")
  
  #remove already filled slots
  filled_slots <- subset(amt_req, Labware==solvent_loc)$Slot
  solvent_rack <- solvent_rack[!(solvent_rack$Slot %in% filled_slots),]
  
  #iterate through all command list
  for(i in c(1:length(cmd_list[,1]))){
    if(cmd_list$SourceLabware[i]==solvent_loc & 
       grepl("filling water", cmd_list$Comment[i], ignore.case = T)){
      cur_transAmt <- as.numeric(cmd_list$TransAmt[i]) * length(strsplit(cmd_list$TargetSlot[i], split=", ")[[1]])
      try_slot <- 1
      while((solvent_rack$FillAmt[try_slot] + cur_transAmt) > 45000){
        try_slot <- try_slot + 1 #move to next slot
      }
      
      #append value to the current slot
      solvent_rack$FillAmt[try_slot] <- solvent_rack$FillAmt[try_slot] + cur_transAmt
      cmd_list$SourceSlot[i] <- solvent_rack$Slot[try_slot]
    }
  }
  
  #concatenate result to amt req
  solvent_rack <- solvent_rack[solvent_rack$FillAmt>0,]
  solvent_rack$FillAmt <- ceiling(solvent_rack$FillAmt/1000)*1000 + 3000 #excess of 3 mL
  solvent_rack <- cbind.data.frame(replicate(length(solvent_rack[,1]), "Water"),
                                   solvent_rack$FillAmt,
                                   replicate(length(solvent_rack[,1]), "50 Falcon"),
                                   replicate(length(solvent_rack[,1]), solvent_loc),
                                   solvent_rack$Slot,
                                   replicate(nrow(solvent_rack), ""))
  
  colnames(solvent_rack) <- colnames(amt_req)
  amt_req <- rbind.data.frame(amt_req, solvent_rack)
  return(list(cmd_list, amt_req))
}
Cal_manualFilling <- function(cmd_list){
  #subset water filling sections
  filling_commands <- cmd_list[grepl("filling", cmd_list$Comment, ignore.case=T),]
  other_commands <- cmd_list[!(grepl("filling", cmd_list$Comment, ignore.case=T)),]
  
  #separate automatic and manual filling
  preFill_amt <- floor(as.numeric(filling_commands$TransAmt)/1000)
  robotFill_amt <- as.numeric(filling_commands$TransAmt) - (preFill_amt*1000)
  filling_commands$TransAmt <- robotFill_amt
  
  #assign tube locations to manual fill
  preFill_amt <- cbind.data.frame(filling_commands$TargetLabware, filling_commands$TargetSlot, preFill_amt)
  colnames(preFill_amt) <- c("TargetLabware", "TargetSlot", "AmountPreFill")
  #concatenate result
  cmd_list <- rbind.data.frame(filling_commands, other_commands)
  rownames(cmd_list) <- c()
  
  return(list(cmd_list, preFill_amt))
}
check_OverFlow <- function(cmd_list, deck_map, vol_info){
  mainRack_commands <- subset(cmd_list, 
                              TargetLabware==paste("labware_", which(grepl("main", deck_map, ignore.case=T)), sep=""))
  slots <- unique(mainRack_commands$TargetSlot)
  checks <- sapply(slots, function(x) sum(as.numeric(subset(mainRack_commands, TargetSlot==x)$TransAmt))>vol_info$FinAmt[vol_info$slot==x])
  
  return(sum(checks)>0)
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
#MAIN-----------
M9_complex <- function(file_loc){ #main run function
  #READ INPUT FILE
  inputFiles <- ReadInputs(file_loc)
  stockInfo <- inputFiles[[1]]
  volInfo <- data.frame(inputFiles[[2]])
  if(length(volInfo[1,])==1){volInfo <- cbind.data.frame(volInfo[1,], volInfo[2,])}
  colnames(volInfo) <- c("Solutions", "FinAmt")
  concDetails <- inputFiles[[3]]
  
  #manual fill info and preparation
  manualFill <- inputFiles[[4]]
  #pick relevant columns from manual fill
  manualFill <- manualFill[,c("Item", "Stock.Volume..uL.", "Tube.type")]
  colnames(manualFill) <- c("Name", "ManualVol", "ManualTube")
  manualFill <- manualFill[!is.na(manualFill$Name),] #remove empty rows
  manualFill$ManualVol <- as.numeric(manualFill$ManualVol)
  
  #INITIATE DECK MAP
  deckMap <- c("1.5Eppendorf_A", "50Falcon_Main", "50Falcon_A",
               "tip300", "15Falcon_A", "15Falcon_B", 
               "tip300_spare", "SOLVENT", "tip_1000",
               replicate(2, "empty"), "TRASH")
  
  #PREPARATION
  #a. Calculate amount required per-stock per-solution
  transAmts <- Calculate_ReqStockAmount(concDetails, volInfo)
  rownames(transAmts) <- volInfo$Solutions
  
  #b. Calculate amount required in each stock
  amtRequired <- apply(transAmts, 2, function(x) Sum_and_Round(x))
  
  #c. Assign Tubes and Slots; compare with manual input
  amtRequired <- cbind.data.frame(amtRequired,
                                  AssignTubes(amtRequired))
  colnames(amtRequired) <- c("Amt", "TubeType")
  amtRequired <- AssignSlots(amtRequired, deckMap, manualFill)
 
  #d. Assign slots for mediums
  volInfo <- Assign_MediumSlot(volInfo)
  
  #e. Create solutions list
  solList <- cbind.data.frame(volInfo$Solutions,
                              0, #begin with fresh tubes (empty)
                              replicate(length(volInfo[,1]), "50 Falcon"),
                              replicate(length(volInfo[,1]), 
                                        paste("labware_",which(grepl("ain", deckMap)), sep="")),
                              volInfo$slot,
                              replicate(nrow(volInfo), ""))
  colnames(solList) <- c("Solution", "Amt", "TubeType", "Labware", "Slot", "VolCheck")
  solList <- rbind.data.frame(solList, amtRequired)
  solList$Amt <- as.numeric(solList$Amt)
  
  #CREATING COMMAND LIST
  cmdList <- Create_Commands(transAmts, solList)
  
  #VOLUME ADJUSTMENT
  #updating solutions list
  for(i in c(1:length(cmdList[,1]))){
    #reduce aspirated amount
    solList$Amt[solList$Labware==cmdList$SourceLabware[i] & solList$Slot==cmdList$SourceSlot[i]] <- as.numeric(solList$Amt[solList$Labware==cmdList$SourceLabware[i] & solList$Slot==cmdList$SourceSlot[i]]) - as.numeric(cmdList$TransAmt[i])
    
    #add dispensed amount
    solList$Amt[solList$Labware==cmdList$TargetLabware[i] & solList$Slot==cmdList$TargetSlot[i]] <- as.numeric(solList$Amt[solList$Labware==cmdList$TargetLabware[i] & solList$Slot==cmdList$TargetSlot[i]]) + as.numeric(cmdList$TransAmt[i])
  }
  
  #calculate volume of water to include
  extra_amt_src <- solList[solList$Solution %in% volInfo$Solutions, ]
  extra_amt <- cbind.data.frame(extra_amt_src,
                                max(ceiling(extra_amt_src$Amt/5000)*5000) - 
                                  extra_amt_src$Amt)
  colnames(extra_amt)[length(extra_amt[1,])] <- "ExtraAmt"
  
  #calculate extra amount based on final volume
  extra_amt$ExtraAmt2 <- as.numeric(volInfo$FinAmt)*1000 - extra_amt$Amt
  
  #minimum
  extra_amt$ExtraUsed <- apply(extra_amt, 1, function(x) min(x["ExtraAmt2"], x["ExtraAmt"]))
  
  #create commands to pre-fill tubes
  fillCommands <- c()
  for(i in c(1:length(extra_amt[,1]))){
    nexCommand <- c(paste("labware_", which(grepl("olvent", deckMap, ignore.case=T)), sep=""),
                    "A1", extra_amt$Labware[i], extra_amt$Slot[i],
                    extra_amt$ExtraUsed[i], 0, #no mixing
                    1, "Filling Water")
    fillCommands <- rbind(fillCommands, nexCommand)
  }
  colnames(fillCommands) <- colnames(cmdList)
  
  #concatenate to command list
  cmdList <- rbind(fillCommands, cmdList)
  
  #FINAL POST-CALCULATION
  #adding water to output amount
  add_water <- Distribute_WaterRack(deckMap, amtRequired, cmdList)
  output_amtRequired <- add_water[[2]]
  cmdList <- add_water[[1]]
  
  #determine amount water for manual filling
  manualFill <- Cal_manualFilling(cmdList)
  cmdList <- manualFill[[1]]
  manualFill <- manualFill[[2]]
  
  #Separating P300 and P1000 operations
  cmdList <- do.call(rbind, lapply(unique(cmdList$TipID), selectPipette_oneGroup, cmd_list = cmdList))
  
  #check over-volume
  overFLow <- check_OverFlow(cmdList, deckMap, volInfo)
  
  #CREATE ROBOT COMMANDS
  output_amtRequired$Amt <- as.numeric(output_amtRequired$Amt)
  RobotCommands <<- CreateRobotCommands(output_amtRequired, cmdList, deckMap)
  
  #CREATE USER COMMANDS
  output_amtRequired$Unit <- "uL"
  output_amtRequired <- Adjust_Unit(deckMap, output_amtRequired)
  UsrCommands <- cbind.data.frame(output_amtRequired$Labware, output_amtRequired$Slot,
                                  output_amtRequired$TubeType, output_amtRequired$Solution,
                                  output_amtRequired$Amt, output_amtRequired$Unit, output_amtRequired$VolCheck)
  colnames(UsrCommands) <- c("Labware", "Slot", "Type", "Name", "Amount", "Unit", "VolumeCheck")
  UsrCommands <- UsrCommands[order(UsrCommands$Labware),]
  
  #add extra tubes; assign pre-fill amounts
  extraTubes <- c()
  for(p in c(1:length(manualFill[,1]))){
    nexTubes <- c(manualFill$TargetLabware[p], manualFill$TargetSlot[p], "50 mL Falcon Tube",
                  paste(solList$Solution[solList$Labware==manualFill$TargetLabware[p] & solList$Slot==manualFill$TargetSlot[p]], "Fill with water/solvent", sep="_"),
                  manualFill$AmountPreFill[p], "mL")
    #concatenate result
    extraTubes <- rbind(extraTubes, nexTubes)
  }
 
  extraTubes <- cbind(extraTubes, replicate(nrow(extraTubes), ""))
  colnames(extraTubes) <- colnames(UsrCommands)
  
  #create main user command
  UsrCommands <- rbind.data.frame(UsrCommands, extraTubes)
  
  #attach deck map
  deckMap <- rbind(c(10:12), deckMap[10:12], c(7:9), deckMap[7:9],
                   c(4:6), deckMap[4:6], c(1:3), deckMap[1:3])
  deckMap <- cbind(deckMap, replicate(8, ""), replicate(8, ""), replicate(8, ""), replicate(8, ""))
  colnames(deckMap) <- colnames(UsrCommands)
  
  UsrCommands <- rbind(UsrCommands, 
                       c(">>> OT2 DECK MAP <<<", replicate(6, "")),
                       deckMap)
  
  rownames(UsrCommands) <- c()
  
  if(overFLow){
    RobotCommands <- c()
    UsrCommands <- c("Required volume for dilution exceeded targeted amount - please use a more concentrated stock")
  }
  return(UsrCommands)
}
#TROUBLESHOOTING------------
#mainwd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\M9MixR"
#inputName <- "M9MixR_InputTemplate.xlsx"
#fpath <- paste(mainwd, inputName, sep='\\')
#dis <- M9_complex(paste(mainwd, inputName, sep='\\'))