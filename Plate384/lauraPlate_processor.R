#LIBRARIES--------
library(readxl)
library(writexl)
library(dplyr)
library(rlist)

#GENERAL PREPARATION-------
wellOrder <- sapply(c(1:24), function(x) paste0(LETTERS[1:16], x)) %>% as.vector()

#FUNCTIONS-----------
read_InputFile <- function(file_name){
  #Stock concentration
  stockInfo <- read_xlsx(file_name, sheet=1)[,1:3]
  
  #general information
  generalInfo <- read_xlsx(file_name, sheet=1, range="F2:F4", col_names=F) %>% unlist()
  names(generalInfo) <- c()
  generalInfo <- c(nPlates=generalInfo[1], Vtotal=generalInfo[2], Vinoc=generalInfo[3])
  
  #plate layout
  read_range <- cbind.data.frame(ranges = c("B3:Y18", "B22:Y37", "B41:Y56"),
                                 type = c("Fill", "Concentration", "Solvent"))
  
  plateInfo <- lapply(read_range$ranges, function(x){
    cbind.data.frame(Well = wellOrder,
                     Info = unlist(read_xlsx(file_name, 
                                             sheet=2, range=x, col_names=F)))})
  plateInfo <- left_join(plateInfo[[1]], plateInfo[[2]], by="Well") %>% 
    left_join(plateInfo[[3]], by="Well")
  colnames(plateInfo) <- c("Well", read_range$type)
  
  return(list(stockInfo, generalInfo, plateInfo))
}
create_dilScheme <- function(current_dilID, sol_list, stock_info){
  #dilution function
  current_dil <- subset(sol_list, dilutionID==current_dilID) %>%
    arrange(Concentration)
  
  # calculate amount needed from previous step
  current_dil$V_forDilution <- 0
  current_dil$V_total <- 0
  for(i in c(1:nrow(current_dil))){
    if(i == 1){
      current_dil$V_forDilution[i] <- 0
    }else{
      current_dil$V_forDilution[i] <- current_dil$V_total[i-1] * 
        current_dil$Concentration[i-1] / current_dil$Concentration[i]
    }
    
    current_dil$V_total[i] <- current_dil$volNeeded[i] + current_dil$V_forDilution[i] + 300 #add excess; reduce to 150
  }
  
  #round initial dilution for manual handling
  current_dil$V_total[nrow(current_dil)] <- round(current_dil$V_total[nrow(current_dil)]/100, 0) * 100
  
  #additional info for row-wise dilution
  current_dil$V_fromStock <- c(rep(0, nrow(current_dil)-1), 
                               current_dil$Concentration[nrow(current_dil)] * 
                                 current_dil$V_total[nrow(current_dil)] / 
                                 stock_info$`Stock Concentration`[stock_info$`Drug Name`==current_dil$Fill[1]])
  
  current_dil$V_solvent <- c(current_dil$V_total[1:(nrow(current_dil)-1)] - 
                               current_dil$V_forDilution[2:(nrow(current_dil))],
                             current_dil$V_total[nrow(current_dil)] - 
                               current_dil$V_fromStock[nrow(current_dil)])
  return(current_dil)
}
create_commandList_solventDistribution <- function(solution_list, solvent_map){
  initial_solvent_dil <- dplyr::select(subset(solution_list, V_fromStock==0), 
                                       Solvent, V_solvent, slot, deck)
  
  #select pipette
  initial_solvent_dil <- lapply(unique(initial_solvent_dil$Solvent), function(x){
    current_set <- subset(initial_solvent_dil, Solvent==x)
    if(sum(current_set$V_solvent<100)>0){
      current_set$pipette="p300"
    }else{current_set$pipette="p1000"}
    return(current_set)}) %>% list.rbind()
  
  #set aspiration set for each solvent
  running_aspID <- 1
  running_aspV <- initial_solvent_dil$V_solvent[1]
  initial_solvent_dil$asp_set <- 1
  for(i in c(2:nrow(initial_solvent_dil))){
    #set current threshold
    current_threshold <- if(initial_solvent_dil$pipette[i]=="p1000"){1000}else{300}
    
    #check if space is still available in the current aspirate set
    if(initial_solvent_dil$Solvent[i]==initial_solvent_dil$Solvent[i-1] & 
       (running_aspV+initial_solvent_dil$V_solvent[i])<=current_threshold){
      
      # add amount to the current running volume
      running_aspV <- running_aspV + initial_solvent_dil$V_solvent[i]
      
      # set aspirate ID to the current  row
      initial_solvent_dil$asp_set[i] <- running_aspID
      
    }else{
      # execute if current aspirate set is full
      running_aspID <- running_aspID + 1 #update running aspirate ID
      running_aspV <- initial_solvent_dil$V_solvent[i] #update running aspirate volume
      initial_solvent_dil$asp_set[i] <- running_aspID
    }
  }
  
  cmd_list <- data.frame(from_deck = sapply(initial_solvent_dil$Solvent, 
                                            function(x) solvent_map$deck[solvent_map$fill==x]),
                         from_slot = sapply(initial_solvent_dil$Solvent, 
                                            function(x) solvent_map$slot[solvent_map$fill==x]),
                         to_deck = initial_solvent_dil$deck, to_slot = initial_solvent_dil$slot,
                         amt = initial_solvent_dil$V_solvent, mix=0,
                         tip_n = 1, asp_set = initial_solvent_dil$asp_set, 
                         pipette=initial_solvent_dil$pipette, comment="initial solvent distribution")
  
  #setup tip counter
  tip_counter <- unique(cmd_list$from_slot)
  cmd_list$tip_n <- vapply(cmd_list$from_slot, FUN.VALUE=1, function(x) which(tip_counter==x))
  
  return(cmd_list)
}
create_commandList_serialDilution <- function(command_list, current_id, sol_list, deck_map){
  current_set <- subset(sol_list, dilutionID == current_id) %>% arrange(desc(Concentration))
  
  current_cmd <- sapply(c(2:nrow(current_set)), function(x){
    c(current_set$deck[x-1], current_set$slot[x-1], 
      current_set$deck[x], current_set$slot[x],
      current_set$V_forDilution[x-1], current_set$V_total[x]/2,
      1, -1, 
      if((current_set$V_forDilution[x-1]>300) & 
         !grepl("96", deck_map$fill[deck_map$deck==current_set$deck[x-1]])){"p1000"}else{"p300"}, #tip type
      "Serial dilution")
  }) %>% t() %>% data.frame()
  
  colnames(current_cmd) <- colnames(command_list)
  
  current_cmd$asp_set <- c((max(command_list$asp_set)+1):(max(command_list$asp_set)+nrow(current_cmd)))
  return(current_cmd)
}
create_commandList_finalDistribution <- function(sol_list, gen_info, 
                                                 sol_map,plate_map, cmdList_serial, deck_map){
  cmdList_final_distribution <- lapply(sol_list$solutionID, function(x){
    lapply(paste0("384_", c(1:gen_info["nPlates"])), function(xi){
      c(from_deck = sol_map$deck[which(sol_map$solutionID==x)],
        from_slot = sol_map$slot[which(sol_map$solutionID==x)],
        to_deck = which(deck_map$fill==xi), 
        to_slot = paste(plate_map$Well[plate_map$solutionID==x], collapse=", "),
        amount = gen_info["Vtotal"]-gen_info["Vinoc"],
        mix = 0, tip_n = -5, tipID=0,
        pipette=if(gen_info["Vtotal"]-gen_info["Vinoc"] <= 300){"p300"}else{"p1000"},
        comment="Final distribution")
    }) %>% list.rbind()
  }) %>% list.rbind() %>% data.frame()
  
  #setup tip id
  tip_setup <- dplyr::select(cmdList_final_distribution, from_deck, from_slot) %>% distinct()
  tip_setup$tip_n <- c((max(cmdList_serial$tip_n)+1):(max(cmdList_serial$tip_n)+nrow(tip_setup)))
  
  cmdList_final_distribution$tip_n <- apply(cmdList_final_distribution, 1, function(x){
    subset(tip_setup, from_deck==as.numeric(x["from_deck"]) & from_slot==x["from_slot"])$tip_n})
  colnames(cmdList_final_distribution) <- colnames(cmdList_serial)
  
  cmdList_final_distribution$asp_set <- c((max(cmdList_serial$asp_set)+1):(max(cmdList_serial$asp_set)+nrow(cmdList_final_distribution)))
  
  return(cmdList_final_distribution)
}
calculate_emptyTubes <- function(all_list, current_deck_ware){
  current_set <- subset(all_list, deckWareID==current_deck_ware)
  
  #decide if item is a 96-well plate
  if(grepl("96", current_deck_ware)){
    current_name <- "96 Deep Well Plate (2000 uL)"
    n <- 1
    slot_names <- ""
  }else{
    if(grepl("15", current_deck_ware)){
      current_name <- "15 mL Falcon Tube"
    }else{
      current_name <- "1.5 mL Eppendorf Tube"
    }
    n <- nrow(current_set)
    slot_names <- paste(current_set$slot, collapse=", ")
  }
  return(c(current_set$deck[1], 
           slot_names,
           current_name, n))
}
distribute_outerWells <- function(plate_info, cmd_list_checkpoint, solvent_map, deck_map, general_info){
  # subset
  fill_wells <- subset(plate_info, is.na(Concentration))
  
  # create command; separate per-solvent
  current_command <- lapply(unique(fill_wells$Solvent), function(x){
    #subset wells to fill
    current_fill_wells <- subset(fill_wells, Solvent==x)
    #create commands; separate for each aspirate group
    command_current_solvent <- data.frame()
    well_targets <- current_fill_wells$Well
    n_well_perAspirate <- floor(300 / general_info["Vtotal"]) #use p300 only!
    n_aspirate <- floor(length(well_targets)/n_well_perAspirate) + ceiling((length(well_targets)%%n_well_perAspirate)/n_well_perAspirate)
    for(i in c(1:n_aspirate)){
      start_index <- ((i-1)*n_well_perAspirate) + 1
      end_index <- (start_index+n_well_perAspirate-1)
      if(end_index > length(well_targets)){end_index <- length(well_targets)}
      
      # assign current commands to current wells
      command_current_solvent <- rbind.data.frame(command_current_solvent,
                                                  c(from_deck = deck_map$deck[which(deck_map$fill=="solvent")],
                                                    from_slot = solvent_map$slot[which(solvent_map$fill==x)],
                                                    to_deck = 1, 
                                                    to_slot = paste(well_targets[start_index:end_index], collapse=", "),
                                                    amt = general_info["Vtotal"], mix=0, tip_n=1, asp_set = i, pipette="p300", comment="Filling outer wells"))
    }
    
    #add column name
    colnames(command_current_solvent) <- colnames(cmd_list_checkpoint)
    
    #replicate for each plates included
    command_current_solvent <- lapply(c(1:general_info["nPlates"]), function(x){
      add_current <- command_current_solvent
      add_current[,colnames(cmd_list_checkpoint)=="to_deck"] <- deck_map$deck[which(deck_map$fill==paste0("384_", x))]
      return(add_current)
    }) %>% list.rbind()
    
    return(command_current_solvent)}) %>% list.rbind()
  
  
  #fix aspirate set counter
  current_command$asp_set <- max(cmd_list_checkpoint$asp_set) + c(1:nrow(current_command))
  
  #fix tip counter
  tip_counter_matrix <- data.frame(Solvent = unique(fill_wells$Solvent),
                                   from_slot = sapply(unique(fill_wells$Solvent), function(xi) solvent_map$slot[which(solvent_map$fill==xi)]))
  tip_counter_matrix$tip_n_true <- c(1:nrow(tip_counter_matrix)) + max(cmd_list_checkpoint$tip_n)
  tip_counter_matrix <- dplyr::select(tip_counter_matrix, from_slot, tip_n_true)
  
  
  current_command <- left_join(current_command, tip_counter_matrix, by="from_slot") %>%
    mutate(tip_n = tip_n_true) %>% dplyr::select(-tip_n_true)
  
  #combine to cmd list
  cmd_list_checkpoint <- rbind.data.frame(cmd_list_checkpoint, current_command)
  return(cmd_list_checkpoint)
}

#MAIN EXEC----------
mainExec <- function(input_file_name, fill_outer){
  # A | Read input files; re-parse
  inputFiles <- read_InputFile(input_file_name)
  stockInfo <- inputFiles[[1]]
  generalInfo <- inputFiles[[2]]
  plateInfo <- inputFiles[[3]]
 
  # B | Parse out solution list\
  plateInfo$solutionID <- paste(plateInfo$Fill, plateInfo$Concentration, plateInfo$Solvent, sep="_")
  solList <- dplyr::select(plateInfo, -Well) %>% 
    distinct() %>% filter(!is.na(Fill)) %>%
    arrange(Concentration) %>% arrange(Fill) %>% arrange(Solvent) 
  solList$dilutionID <- paste(solList$Fill, solList$Solvent, sep="_")
  solList$nWell <- sapply(solList$solutionID, function(x) length(which(plateInfo$solutionID==x)))
  solList$volNeeded <- solList$nWell * generalInfo["nPlates"] * 
    (generalInfo["Vtotal"] - generalInfo["Vinoc"])
 
  # C | Setup deck and dilution maps
  deckMap <- data.frame(deck=c(1:12),
                        fill = c("solvent", "p1000", "p300", "dilution_96_A", "dilution_96_B", "dilution_15falcon_C",
                                 "384_3", "dilution_15falcon_D", "dilution_15falcon_E", "384_1", "384_2", "trash"))
  solutionMap_96 <-data.frame(slot = rep(sapply(c(1:8), function(x) paste0(LETTERS[x], c(1:12))), 
                                           length(which(grepl("dilution_96", deckMap$fill)))),
                              deck = as.vector(sapply(which(grepl("dilution_96", deckMap$fill)), 
                                                        function(x) rep(x, 96))),
                              solutionID = replicate((96*length(which(grepl("dilution_96", deckMap$fill)))), ""))
  
  solutionMap_15 <-data.frame(slot = rep(sapply(c(1:3), function(x) paste0(LETTERS[x], c(1:5))), 
                                         length(which(grepl("dilution_15", deckMap$fill)))),
                              deck = as.vector(sapply(which(grepl("dilution_15", deckMap$fill)), 
                                                      function(x) rep(x, 15))),
                              solutionID = replicate((15*length(which(grepl("dilution_15", deckMap$fill)))), ""))
  
  solventMap <- data.frame(slot= as.vector(sapply(c(1:2), function(x) paste0(LETTERS[x], c(1:3)))),
                           deck = which(grepl("solvent", deckMap$fill)),
                           fill = "")

  # Filling solvent rack; max. 5 different solvents (6 if no outer fill)
  if(fill_outer){
    solventMap$fill[1:length(unique(plateInfo$Solvent))] <- unique(plateInfo$Solvent)
  }else{
    solventMap$fill[1:length(unique(solList$Solvent))] <- unique(solList$Solvent) 
  }
  
  # D | Create dilution scheme
  solList <- lapply(unique(solList$dilutionID), create_dilScheme, 
                    sol_list=solList, stock_info=stockInfo) %>% list.rbind()
 
  # E | Assign dilution slot
  V_limit_DeepWell <- 1750
  if(nrow(subset(solList, V_total <= V_limit_DeepWell))>0){
    solutionMap_96$solutionID[1:nrow(subset(solList, V_total <= V_limit_DeepWell))] <- subset(solList, V_total <= V_limit_DeepWell)$solutionID
  }
  
  if(nrow(subset(solList, V_total > V_limit_DeepWell))>0){
    solutionMap_15$solutionID[1:nrow(subset(solList, V_total > V_limit_DeepWell))] <- subset(solList, V_total > V_limit_DeepWell)$solutionID
  }
  
  solutionMap <- rbind.data.frame(solutionMap_96, solutionMap_15)
  
  solList <- left_join(solList, solutionMap, by="solutionID")
  
  # F | Command list
  #   Solvent Distribution
  cmdList_solventDistribution <- create_commandList_solventDistribution(solList, solventMap)
  
  #   Serial Dilution
  cmdList_serialDilution <- lapply(unique(solList$dilutionID), function(x)
    create_commandList_serialDilution(cmdList_solventDistribution, x, solList, deckMap)) %>% list.rbind()
  cmdList_serialDilution$tip_n <- c((max(cmdList_solventDistribution$tip_n)+1):
                                      (max(cmdList_solventDistribution$tip_n)+nrow(cmdList_serialDilution)))
  cmdList_serialDilution$asp_set <- c((max(cmdList_solventDistribution$asp_set)+1):
                                        (max(cmdList_solventDistribution$asp_set)+nrow(cmdList_serialDilution)))
  #   Final Distribution
  cmdList_finalDistribution <- create_commandList_finalDistribution(solList, generalInfo, solutionMap,
                                                                    plateInfo, cmdList_serialDilution, deckMap)
  
  cmdList <- rbind.data.frame(cmdList_solventDistribution, cmdList_serialDilution,
                              cmdList_finalDistribution)
  
  #   Filling outer wells
  if(fill_outer){
    cmdList <- distribute_outerWells(plateInfo, cmdList, solventMap, deckMap, generalInfo)
  }
  
  # G | Calculate required solvent amounts
  solvent_map_prep <- dplyr::select(solventMap, slot, fill) %>% rename(from_slot=slot)
  solvent_commands <- subset(cmdList, from_deck==deckMap$deck[deckMap$fill=='solvent']) %>%
    left_join(solvent_map_prep, by="from_slot")
  solvent_operations <- data.frame(slot = unique(solvent_commands$from_slot),
                                   volume = sapply(unique(solvent_commands$from_slot), 
                                                   function(x) sum(as.numeric(solvent_commands$amt[solvent_commands$from_slot==x])))) %>%
    left_join(solventMap, by="slot")
  
  solvent_operations$volume <- round(solvent_operations$volume / 500) * 500 + 3000 #excess
  
  # manage solvent tube overfill
  overFilled <- solvent_operations$volume > 50000
  if(sum(overFilled)>0){
    cmdList$operationRowIndex <- c(1:nrow(cmdList))
    overFilled <- solvent_operations[overFilled,]
    for(i in c(1:nrow(overFilled))){
      #iterate through all over-filled tubes
      current_operations <- subset(cmdList, from_deck==deckMap$deck[deckMap$fill=="solvent"] & from_slot==overFilled$slot[i])
      
      #calculate cumulative amount
      current_operations$actualAmt <- apply(current_operations, 1, function(x) as.numeric(x['amt']) * length(strsplit(x["target_slot"], split=", ")[[1]]))
      current_operations$cumulativeAmount <- sapply(c(1:nrow(current_operations)), function(x) sum(current_operations$actualAmt[1:x]))
      current_operations$effectiveCumulativeAmount <- round(current_operations$cumulativeAmount / 500) * 500 + 3000
      
      #separate to a different tube
      available_slot <- which(solventMap$fill=="") %>% min()
      solventMap$fill[available_slot] <- overFilled$fill[i]
      current_operations$from_slot[current_operations$effectiveCumulativeAmount > 49000] <- solventMap$slot[available_slot]
      
      #push back to command list
      cmdList$from_slot[cmdList$operationRowIndex %in% current_operations$operationRowIndex] <- current_operations$from_slot
    }
    
    # re-fix command list
    cmdList <- dplyr::select(cmdList, -operationRowIndex)
    
    # re-calculate required solvent amounts
    solvent_map_prep <- dplyr::select(solventMap, slot, fill) %>% rename(from_slot=slot)
    solvent_commands <- subset(cmdList, from_deck==deckMap$deck[deckMap$fill=='solvent']) %>%
      left_join(solvent_map_prep, by="from_slot")
    solvent_commands$actualAmt <- apply(solvent_commands, 1, function(x) as.numeric(x["amt"]) * length(strsplit(x["to_slot"], split=", ")[[1]]))
    solvent_operations <- data.frame(slot = unique(solvent_commands$from_slot),
                                     volume = sapply(unique(solvent_commands$from_slot), 
                                                     function(x) sum(as.numeric(solvent_commands$amt[solvent_commands$from_slot==x])))) %>%
      left_join(solventMap, by="slot")
    
    solvent_operations$volume <- round(solvent_operations$volume / 500) * 500 + 3000 #excess
  }

  solvent_operations <- cbind.data.frame(solvent_operations[c("deck", "slot", "fill")], 
                                         rep("", nrow(solvent_operations)),
                                         rep("", nrow(solvent_operations)),
                                         solvent_operations["volume"],
                                         rep("", nrow(solvent_operations)),
                                         rep("", nrow(solvent_operations)))
  colnames(solvent_operations) <- c("slot", "deck", "fill", "concentration", 'solvent', "V_total", "V_stock", "V_solvent")
  
  #   prepare solution list
  prepared_solList <- subset(solList, V_fromStock!=0) %>% 
    dplyr::select(slot, deck, Fill, Concentration, Solvent, V_total, V_fromStock, V_solvent) %>%
    arrange(Concentration) %>% arrange(Solvent) %>% arrange(Fill)
  prepared_solList[,c(1,2)] <- prepared_solList[,c(2,1)]
  colnames(prepared_solList) <- colnames(solvent_operations)
  prepared_solList <- rbind.data.frame(prepared_solList, solvent_operations)
  
  #  separate table for robot
  prepared_solList_robot <- dplyr::select(prepared_solList, slot, deck, V_total)
  prepared_solList_robot <- cbind.data.frame(prepared_solList_robot, replicate(7, rep("", nrow(prepared_solList_robot))))
  colnames(prepared_solList_robot) <- colnames(cmdList)
  
  #update deck map
  used <- c(cmdList$from_deck, cmdList$to_deck) %>% unique() %>% as.numeric()
  deckMap$fill[!(deckMap$deck %in% used) & !(deckMap$fill %in% c("trash", "p300", "p1000"))] <- ""
  
  #combine output
  deckMap <- cbind.data.frame(deckMap, replicate(8, rep("", 12)))
  colnames(deckMap) <- colnames(cmdList)
  finOutput <- rbind.data.frame(c("> SolutionList", replicate(9, "")),
                                prepared_solList_robot,
                                c("> CmdList", replicate(9, "")),
                                cmdList, 
                                c("> DeckMap", replicate(9, "")),
                                deckMap)
  
  # H | Prepare setup guide
  #    primary dilution guide
  colnames(stockInfo) <- c("Fill", "StockConcentration", "Unit")
  colnames(deckMap)[1:2] <- c("deck", "Tube")
  preDilution <- filter(solList, V_fromStock>0) %>%
    dplyr::select(deck, slot, Fill, Concentration, Solvent, V_fromStock, V_solvent, V_total) %>%
    left_join(stockInfo, by="Fill") %>%
    rename(Drug=Fill, StockVolume=V_fromStock, SolventVolume=V_solvent, TotalVolume=V_total) %>%
    left_join(deckMap[,c(1:2)], by="deck") %>%
    mutate(Tube = sapply(Tube, function(x) if(grepl("dilution_96", x)){"Deep Well"}else if(grepl("15falcon", x)){"15 mL Falcon Tube"}else{"1.5 mL Eppendorf tube"}))
  
  #    solvent
  solventPrep <- filter(prepared_solList, concentration=="") %>% 
    dplyr::select(slot, deck, fill, V_total) %>%
    rename(deck=slot, slot=deck, Solvent=fill, SolventVolume=V_total)
  solventPrep <- cbind.data.frame(solventPrep[,c(1,2)],
                                  rep("", nrow(solventPrep)),
                                  rep("", nrow(solventPrep)),
                                  solventPrep[,"Solvent"],
                                  rep("", nrow(solventPrep)),
                                  solventPrep[,"SolventVolume"],
                                  rep("", nrow(solventPrep)),
                                  rep("", nrow(solventPrep)),
                                  rep("", nrow(solventPrep)),
                                  rep("50 mL Falcon Tube", nrow(solventPrep)))
  colnames(solventPrep) <- colnames(preDilution)
  
  #     empty tubes/deep wells
  emptyLabwares <- subset(solList, V_fromStock==0) %>%
    dplyr::select(deck, slot) %>%
    left_join(deckMap, by="deck")
  emptyLabwares$deckWareID <- paste(emptyLabwares$deck, emptyLabwares$Tube, sep="_")
  emptyLabwares <- lapply(unique(emptyLabwares$deckWareID), calculate_emptyTubes, 
                          all_list = emptyLabwares) %>% list.rbind()
  
  emptyLabwares <- cbind.data.frame(emptyLabwares[,1],
                                    emptyLabwares[,2],
                                    rep("", nrow(emptyLabwares)),
                                    rep("", nrow(emptyLabwares)),
                                    rep("", nrow(emptyLabwares)),
                                    rep("", nrow(emptyLabwares)),
                                    rep("", nrow(emptyLabwares)),
                                    emptyLabwares[,4],
                                    rep("", nrow(emptyLabwares)),
                                    rep("", nrow(emptyLabwares)),
                                    emptyLabwares[,3])
  colnames(emptyLabwares) <- colnames(preDilution)
  preDilution <- rbind.data.frame(preDilution, solventPrep, emptyLabwares) %>%
    arrange(slot) %>% arrange(deck)
  
  #     deck map final prep
  deckMap <- deckMap[,c(1:2)]
  colnames(deckMap) <- c("Deck", "Item")
  deckMap$Item <- sapply(deckMap$Item,
                         function(x){
                           if(grepl("solvent", x)){
                             c_name <- "Solvent Rack - 50 mL Falcon tubes"
                           }else if(substring(x, 1, 1)=="p"){
                             c_name <- paste0("P", substring(x, 2, nchar(x)), " tip rack")
                           }else if(grepl("dilution", x)){
                             if(grepl("96", x)){
                               c_name <- "Dilution Rack - 96 Deep Well plate"
                             }else{
                               c_name <- "Dilution Rack - 15 mL Falcon tubes"
                             }
                           }else if(grepl("384", x)){
                             c_name <- "Main Target - 384-well plate"
                           }else if(x==""){
                             c_name <- "empty"
                           }else{
                             c_name <- "Fixed trash"
                           }
                         })
  userGuide <- list(DeckMap=data.frame(deckMap),
                    DeckPreparation = data.frame(preDilution))
  return(list(finOutput, userGuide))
}

#TEST--------------
# input 
#fileName <- "20220103_MIC384_DOX_MIN_TOB_GEN_STR_TET_CIP_FAEC_FAEM.xlsx"
#mainwd <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator"
#input_file_name <- paste0(mainwd, "\\", fileName)

#output <- mainExec(input_file_name, T)

#write output
#write.csv(output[[1]], paste0(mainwd, "/SMURF_CommandList.csv"), row.names=F)
#write_xlsx(output[[2]], paste0(mainwd, "/SMURF_UserGuide.xlsx"))