#LIBRARIES-------
library(dplyr)
library(readxl)
library(reshape2)
library(tidyr)
library(rlist)

#FUNCTION LIBRARY-----------
# Input read
readInputFile <- function(file_name){
  # primary read
  wellFill <- read_xlsx(file_name, sheet=2, skip=37)
  colnames(wellFill)[1] <- "row"
  wellFill <- melt(wellFill, id.vars='row', variable.name = 'column', value.name='Fill') %>%
    unite("Well", row, column, sep="")
  
  # parse fill
  parsedInfo <- sapply(wellFill$Fill, function(x){
    parsed <- strsplit(x, split="_")[[1]]
    if(length(parsed)==4){parsed <- c(parsed[1:2], "", parsed[3:4])}
    return(parsed)}) %>% t() %>% data.frame()
  colnames(parsedInfo) <- c("Drug", "Concentration", "Rep", "Strain", "Medium")
  parsedInfo <- unite(parsedInfo, "StrainRep", Strain, Rep)
  wellFill <- cbind.data.frame(wellFill, parsedInfo)
  
  # primary read - run info
  input_read <- read_xlsx(file_name, sheet=1)
  stockInfo <- input_read[,c(1:3)]
  runInfo <- input_read[,c(5:6)]
  
  return(list(wellFill, stockInfo, runInfo))
}

# Dilution Scheme Preparation
calculate_dilutionSet <- function(current_drug_type, solution_required, stock_info){
  if(substring(current_drug_type, 1, 1)!="-"){
    current_set <- subset(solution_required, DrugType==current_drug_type)
    
    # loop through
    for(i in c(1:nrow(current_set))){
      # get one step higher concentration
      concentration_above <- if(i != nrow(current_set)){current_set$Concentration[i+1]}else{
        stock_info$`Stock Concentration`[stock_info$`Drug Name`==strsplit(current_drug_type, split="-")[[1]][1]]}
      
      # get one step below volume
      current_set$V_forBelow[i] <- if(i > 1){current_set$V_fromAbove[i-1]}else{0}
      
      # calculate dilution
      current_set$Volume[i] <- current_set$Volume[i] +  current_set$V_forBelow[i]
      current_set$V_fromAbove[i] <- current_set$Volume[i] * current_set$Concentration[i] / concentration_above
      current_set$V_medium[i] <- current_set$Volume[i] - current_set$V_fromAbove[i]
    }
    
    return(current_set)
  }
}
calculate_dilutionScheme <- function(plate_map, stock_information, run_info){
  # plate_map <<- plate_map
  # stock_information <<- stock_information
  # run_info <<- run_info
  
  # calculate required amount (volume)
  plate_map <- plate_map %>% unite("solType", Drug, Concentration, Medium, remove=F)
  solutionRequired <- select(plate_map, Drug, Concentration, Medium, solType) %>% 
    distinct() %>% unite("DrugType", Drug, Medium, sep="-", remove=F) %>%
    mutate(nWell = sapply(solType, function(x){
      subset(plate_map, solType==x) %>% nrow()}), Concentration = as.numeric(Concentration)) %>%
    arrange(Drug, Concentration)
  
  solutionRequired$Volume <- 1000 + # 1 mL excess
    solutionRequired$nWell * as.numeric(run_info[1,2]) * as.numeric(run_info[2,2])
  
  solutionRequired$V_fromAbove <- 0
  solutionRequired$V_forBelow <- 0
  solutionRequired$V_medium <- 0
  
  # calculate per-drug type
  drug_types <- unique(solutionRequired$DrugType)
  
  # main dilution scheme
  dil_scheme <- lapply(drug_types, calculate_dilutionSet, 
                       solution_required = solutionRequired, stock_info=stock_information) %>% 
    list.rbind()
  
  return(dil_scheme)
}
createSolventMap <- function(tube_type, deck_map){
  if(grepl("15", tube_type)){
    tube_type <- "15"
    n_row <- 3
    n_col <- 5
  }else if(grepl("50", tube_type)){
    tube_type <- "50"
    n_row <- 2
    n_col <- 3
  }else{
    tube_type <- "Epp"
    n_row <- 4
    n_col <- 6
  }
  
  solutionMap_15 <- lapply(deck_map$Fill[grepl(tube_type, deck_map$Fill)], function(x){
    wellIDs <- sapply(c(1:n_row), function(xi){
      sapply(c(1:n_col), function(xii) paste0(LETTERS[xi], xii))
    }) %>% as.vector()
    return(data.frame(DeckID = x, Deck = which(deck_map$Fill==x), Slot=wellIDs, Fill=""))
  }) %>% list.rbind()
}
initiateSolutionMap <- function(dil_scheme, deck_map, stock_info){
  # dil_scheme <<- dil_scheme
  # deck_map <<- deck_map
  # stock_info <<- stock_info
  
  # initiate empty solution map
  solution_map <- lapply(c("Falcon15", "Falcon50", "Epp_1_5"), 
                         function(x) createSolventMap(x, deck_map=deck_map)) %>% 
    list.rbind() %>% unite('Address', Deck, Slot, remove=F)
  
  # assign final solution to a dilution tube
  for(i in c(1:nrow(dil_scheme))){
    tube_type <- if(dil_scheme$Volume[i] <= 13500){"15"}else{"50"}
    
    # get empty slot
    current_available <- subset(solution_map, grepl(tube_type, DeckID) & Fill=="" &
                                  !grepl("Medium", DeckID))
    
    # assign location
    if(nrow(current_available)>0){
      index_fill <- which(solution_map$Address==current_available$Address[1])
      solution_map$Fill[index_fill] <- dil_scheme$solType[i]
    }
  }
  
  # assign medium location
  mediums <- unique(dil_scheme$Medium)
  for(i in c(1:length(mediums))){
    # get empty slot
    current_available <- subset(solution_map, grepl("Medium", DeckID) & Fill=="")
    
    # assign location
    if(nrow(current_available)>0){
      index_fill <- which(solution_map$Address==current_available$Address[1])
      solution_map$Fill[index_fill] <- mediums[i]
    }
  }
  
  # assign stock location
  for(i in c(1:nrow(stock_info))){
    # get empty slot
    current_available <- subset(solution_map, grepl("Epp", DeckID) & Fill=="")
    
    # assign location
    if(nrow(current_available)>0){
      index_fill <- which(solution_map$Address==current_available$Address[1])
      solution_map$Fill[index_fill] <- stock_info$'Drug Name'[i]
    }
  }
  
  return(solution_map)
}
correct_deckMap <- function(deck_map, run_info){
  plates_needed <- sapply(c(1:as.numeric(run_info[1,2])), function(x) paste0("Plate_48_", LETTERS[x]))
  deck_map$Fill[grepl("48", deck_map$Fill) & !(deck_map$Fill %in% plates_needed)] <- ""
  return(deck_map)
}

# Command List Generation
cmd_oneSolventDistribution <- function(current_set, last_cmd_list, solution_map){
  # current_set <<- current_set
  # last_cmd_list <<- last_cmd_list
  # solution_map <<- solution_map
  
  # commands: distributing solvent
  current_tip_id <- if(nrow(last_cmd_list)==0){0}else{max(as.numeric(last_cmd_list[,7]))}
  current_asp_id <- if(nrow(last_cmd_list)==0){0}else{max(as.numeric(last_cmd_list[,8]))}
  current_pipette_type <- ''
  
  # loop through
  current_operation <- sapply(c(1:nrow(current_set)), function(i){
    from_deck <- solution_map$Deck[which(solution_map$Fill==current_set$Medium[i])]
    from_slot <- solution_map$Slot[which(solution_map$Fill==current_set$Medium[i])]
    to_deck <- solution_map$Deck[which(solution_map$Fill==current_set$solType[i])]
    to_slot <- solution_map$Slot[which(solution_map$Fill==current_set$solType[i])]
    amt <- current_set$V_medium[i]
    pipette_type <- if(amt < 200){'p300'}else{'p1000'}
    if(pipette_type != current_pipette_type){current_tip_id <- current_tip_id + 1}
    
    return(c(from_deck, from_slot, to_deck, to_slot, amt, 0, 
             current_tip_id, current_asp_id+i, 
             pipette_type, "initial solvent distribution"))
  }) %>% t() %>% data.frame()
  
  return(current_operation)
}
cmd_solventDistribution <- function(dil_scheme, solution_map){
  # dil_scheme <<- dil_scheme
  # solution_map <<- solution_map
  
  cmd_solventPreDistribution <- data.frame()
  mediums <- unique(dil_scheme$Medium)
  for(j in c(1:length(mediums))){
    current_set <- subset(dil_scheme, Medium==mediums[j])
    if(nrow(current_set)>0){
      current_operation <- cmd_oneSolventDistribution(current_set, cmd_solventPreDistribution, solution_map)
      
      cmd_solventPreDistribution <- rbind.data.frame(cmd_solventPreDistribution, current_operation)
    }
  }
  
  colnames(cmd_solventPreDistribution) <- c("from_deck", "from_slot", "to_deck",
                                            "to_slot", 'amt', "mix", 'tip_n', 
                                            'asp_set', 'pipette', 'comment')
  
  return(cmd_solventPreDistribution)
}
cmd_oneSerialDilution <- function(last_tip_n, last_asp_set, solution_map, dil_scheme, current_drug_type){
  # subset current drug type
  current_set <- subset(dil_scheme, DrugType==current_drug_type) %>%
    arrange(desc(Concentration))
  
  cmd_serial <- c()
  for(i in c(1:nrow(current_set))){
    if(i==1){
      from_deck <- solution_map$Deck[solution_map$Fill==current_set$Drug[i]]
      from_slot <- solution_map$Slot[solution_map$Fill==current_set$Drug[i]]
    }else{
      from_deck <- solution_map$Deck[solution_map$Fill==current_set$solType[i-1]]
      from_slot <- solution_map$Slot[solution_map$Fill==current_set$solType[i-1]]
    }
    
    to_deck <- solution_map$Deck[solution_map$Fill==current_set$solType[i]]
    to_slot <- solution_map$Slot[solution_map$Fill==current_set$solType[i]]
    amt <- current_set$V_fromAbove[i]
    mix <- min(amt, 1000)
    tip_n <- last_tip_n + i
    asp_set <- last_asp_set + i
    pipette <- if(amt <= 300){'p300'}else{'p1000'}
    
    cmd_serial <- rbind(cmd_serial,
                        c(from_deck, from_slot, to_deck, to_slot, amt, mix,
                          tip_n, asp_set, pipette, paste0('Serially diluting to ', current_set$solType[i])))
  }
  
  return(cmd_serial)
}
cmd_serialDilution <- function(dil_scheme, last_cmd_list, solution_map){
  drug_types <- unique(dil_scheme$DrugType)
  cmd_serialDistribution <- data.frame()
  
  for(i in c(1:length(drug_types))){
    if(i==1){
      last_tipN <- last_cmd_list$tip_n %>% as.numeric() %>% max()
      last_aspN <- last_cmd_list$asp_set %>% as.numeric() %>% max()
    }else{
      last_tipN <- cmd_serialDistribution$tip_n %>% as.numeric() %>% max()
      last_aspN <- cmd_serialDistribution$asp_set %>% as.numeric() %>% max()
    }
    current_cmd <- cmd_oneSerialDilution(last_tipN, last_aspN, solution_map,
                                         dil_scheme, drug_types[i])
    
    colnames(current_cmd) <- colnames(last_cmd_list)
    cmd_serialDistribution <- rbind.data.frame(cmd_serialDistribution, current_cmd)
  }
  
  return(cmd_serialDistribution)
}
cmd_MediumDistributionPlate <- function(last_cmdList, plate_map, solution_map, deck_map, run_info){
  last_tipN <- last_cmdList$tip_n %>% as.numeric() %>% max()
  last_aspN <- last_cmdList$asp_set %>% as.numeric() %>% max()
  
  # iterate through all medium types
  medium_types <- unique(plate_map$Medium)
  
  cmd_mediumDist <- c()
  for(x in c(1:length(medium_types))){
    # for current medium, get source
    from_deck <- solution_map$Deck[solution_map$Fill==medium_types[x]]
    from_slot <- solution_map$Slot[solution_map$Fill==medium_types[x]]
    
    # for current medium, get targets
    targetDecks <- deck_map$Slot[grepl("48", deck_map$Fill)]
    to_slot <- subset(plate_map, (Drug=="" & Medium==medium_types[x]))$Well %>%
      paste(collapse=', ')
    
    # track tip number
    last_tipN <- last_tipN + 1
    
    # iterate through all target decks
    for(j in c(1:length(targetDecks))){
      last_aspN <- last_aspN + 1
      
      current_set <- c(from_deck, from_slot, targetDecks[j], to_slot, 
                       as.numeric(run_info[2,2]), 0, 
                       last_tipN, last_aspN, 'p1000', 
                       paste0('Distributing ', medium_types[x], " to Deck ", targetDecks[j]))
      
      cmd_mediumDist <- rbind.data.frame(cmd_mediumDist, current_set)
    }
  }
  
  colnames(cmd_mediumDist) <- colnames(last_cmdList)
  cmd_listhere <<- cmd_mediumDist
  return(cmd_mediumDist)
}
cmd_SolutionDistributionPlate <- function(last_cmdList, plate_map, dil_scheme, solution_map, deck_map, run_info){
  # track tip and aspirate counter
  tipN <- last_cmdList$tip_n %>% as.numeric() %>% max()
  aspN <- last_cmdList$asp_set %>% as.numeric() %>% max()
  
  # initiate empty command list
  cmd_list <- c()
  
  # iterate through all solutions
  for(i in c(1:nrow(dil_scheme))){
    # get source
    from_deck <- solution_map$Deck[solution_map$Fill==dil_scheme$solType[i]]
    from_slot <- solution_map$Slot[solution_map$Fill==dil_scheme$solType[i]]
    
    # get target slot
    to_slot <- plate_map$Well[which((plate_map$Drug==dil_scheme$Drug[i]) & 
                                      (as.numeric(plate_map$Concentration)==as.numeric(dil_scheme$Concentration[i])))] %>%
      paste(collapse=', ')
    
    # track tip number
    tipN <- tipN + 1
    
    # iterate through all target decks
    target_decks <- deck_map$Slot[grepl("48", deck_map$Fill)]
    for(j in c(1:length(target_decks))){
      # get current target deck
      to_deck <- target_decks[j]
      
      # track aspirate number
      aspN <- aspN + 1
      
      current_set <- c(from_deck, from_slot, to_deck, to_slot, as.numeric(run_info[2,2]), 0,
                       tipN, aspN, 'p1000', 
                       paste0('Distributing ', dil_scheme$solType[i], ' to ', to_deck))
      
      cmd_list <- rbind.data.frame(cmd_list, current_set)
    }
  }
  colnames(cmd_list) <- colnames(last_cmdList)
  return(cmd_list)
}

# Post-calculation
check_extraMedium <- function(plate_map, solution_map, cmd_list){
  solventTypes <- unique(plate_map$Medium)
  check_modifications <- F
  
  # iteate through all solvent types
  for(i in c(1:length(solventTypes))){
    # get medium source location
    current_medium_location <- c(solution_map$Deck[solution_map$Fill==solventTypes[i]], 
                                 solution_map$Slot[solution_map$Fill==solventTypes[i]])
    
    # calculate required medium amount per-operation
    current_medium_commands <- subset(cmd_list, from_deck==current_medium_location[1] &
                                        from_slot == current_medium_location[2])
    current_medium_commands$amountOperation <- apply(current_medium_commands, 1, function(x){
      as.numeric(x['amt']) * length(strsplit(x['to_slot'], split=", ")[[1]])})
    
    # calculate cumulative medium amount
    current_medium_commands$amountCumulative <- 0
    for(j in c(1:nrow(current_medium_commands))){
      prev_amount <- if(j==1){0}else{current_medium_commands$amountCumulative[j-1]}
      current_medium_commands$amountCumulative[j] <- current_medium_commands$amountOperation[j] + prev_amount
    }
    
    # assign limit per-tube
    limitVolume <- 45000 # uL
    if(current_medium_commands$amountCumulative[nrow(current_medium_commands)] > limitVolume){
      # perform only if volume of the solvent exceeded the pre-detemined limit
      
      # assign new solvent rack
      new_slot <- subset(solution_map, grepl("Medium", DeckID) & Fill=="")[1,]
      
      # assign extra solvent to slot
      name_extension <- if(!grepl("#", solventTypes[i])){paste0(solventTypes[i], "_#A")}else{
        last_indexer <- which(LETTERS==substring(solventTypes[i], nchar(solventTypes[i]), nchar(solventTypes[i])))+1
        return(paste0(substring(solventTypes[i], 1, nchar(solventTypes[i])-1), LETTERS[last_indexer]))
      }
      solution_map$Fill[solution_map$Address==new_slot$Address] <- paste0(name_extension)
      
      # redirect to new slot
      redirected_operations <- subset(current_medium_commands, amountCumulative > limitVolume)$asp_set
      cmd_list$from_deck[cmd_list$asp_set %in% redirected_operations] <- new_slot$Deck
      cmd_list$from_slot[cmd_list$asp_set %in% redirected_operations] <- new_slot$Slot
      
      # marker if modifications were made
      check_modifications <- T
    }
  }
  
  return(list(solution_map, cmd_list, check_modifications))
}
calculate_startingSolutions <- function(solution_map, plate_map, cmd_list){
  requiredSolutions <- subset(solution_map, Fill!="" & 
                                Fill %in% c(unique(plate_map$Medium), unique(plate_map$Drug), unique(subset(solution_map, grepl("#", Fill))$Fill)))
  requiredSolutions$Volume <- 0
  
  for(xi in c(1:nrow(requiredSolutions))){
    current_set <- subset(cmd_list, from_deck == requiredSolutions$Deck[xi] & from_slot == requiredSolutions$Slot[xi])
    requiredSolutions$Volume[xi] <- apply(current_set, 1, function(x){
      as.numeric(x['amt']) * length(strsplit(x['to_slot'], split=", ")[[1]])
    }) %>% sum()
  }
  
  requiredSolutions$Volume <- vapply(c(1:nrow(requiredSolutions)), FUN.VALUE=1, function(x){
    added_amount <- if(grepl("50", requiredSolutions$DeckID[x])){3000}else if(grepl("15", requiredSolutions$DeckID[x])){500}else{250}
    final_amount <- if(added_amount == 3000){
      max(5000, ceiling((requiredSolutions$Volume[x] + added_amount)/1000)*1000)
    }else if(added_amount==500){
      max(ceiling((requiredSolutions$Volume[x] + added_amount)/100)*100, 1500)
    }else{
      max(ceiling((requiredSolutions$Volume[x] + added_amount)/100)*100, 500)
    }
    return(final_amount)
  })
  
  return(requiredSolutions)
}
correct_cmd_list_Aspirate <- function(cmd_list){
  cmd_list_corrected <- data.frame()
  
  for(i in c(1:nrow(cmd_list))){
    # get aspirate amount and max. pipette aspirate amount
    current_aspirate_amount <- as.numeric(cmd_list$amt[i]) * length(strsplit(cmd_list$to_slot[i], split=", ")[[1]])
    current_aspirate_max <- if(cmd_list$pipette[i]=="p1000"){1000}else{300}
    remaining_slots <- strsplit(cmd_list$to_slot[i], split=", ")[[1]]
    
    # perform only if amount exceeded max
    if(current_aspirate_amount > current_aspirate_max & length(remaining_slots)>1){
     
      # get number of aspirate sets
      nWell_per_aspirate <- current_aspirate_max / as.numeric(cmd_list$amt[i])
      
      # separate into aspirate chunks
      slot_separations <- c()
      while(length(remaining_slots)>0){
        if(length(remaining_slots)>=nWell_per_aspirate){
          current_set <- remaining_slots[1:nWell_per_aspirate]
          remaining_slots <- remaining_slots[-c(1:nWell_per_aspirate)]
        }else{
          current_set <- remaining_slots
          remaining_slots <- c()
        }
        
        # bind to list
        slot_separations <- c(slot_separations, paste(current_set, collapse=", "))
      }
      
      # create aspirate set
      next_set <- do.call("rbind", replicate(length(slot_separations), cmd_list[i,], simplify = FALSE))
      next_set$to_slot <- slot_separations
      next_set$asp_set <- c(1:nrow(next_set)) + max(as.numeric(cmd_list_corrected$asp_set))
      cmd_list_corrected <- rbind.data.frame(cmd_list_corrected, next_set)
    }else{
      next_set <- cmd_list[i,]
      if(i > 1){
        next_set$asp_set <- max(as.numeric(cmd_list_corrected$asp_set)) + 1
      }
      
      cmd_list_corrected <- rbind.data.frame(cmd_list_corrected, next_set)
    }
  }
  
  return(cmd_list_corrected)
}

# output setup
tableExpander <- function(core_table, reference_table){
  extra_columns <- ncol(reference_table) - ncol(core_table)
  extra_rows <- nrow(core_table)
  extra_matrix <- matrix(data=replicate(extra_columns * extra_rows, ""), ncol=extra_columns)
  core_table <- cbind.data.frame(core_table, extra_matrix)
  colnames(core_table) <- colnames(reference_table)
  
  return(core_table)
}
add_tableSeparator <- function(reference_table, separator_comment){
  comment_row <- c(separator_comment, replicate(ncol(reference_table)-1, ""))
  reference_table <- rbind.data.frame(comment_row, reference_table)
  return(reference_table)
}
create_userGuide <- function(deck_map, initial_solutions, empty_tubewares){
  # prepare solution guide
  solution_guide <- dplyr::select(initial_solutions, Deck, Slot, Fill, Volume)
  solution_guide$Unit <- sapply(solution_guide$Volume, function(x) if(x > 2000){"mL"}else{"uL"})
  solution_guide$Volume <- vapply(solution_guide$Volume, FUN.VALUE=1, function(x) if(x>2000){x/1000}else{x})
  solution_guide$Fill <- sapply(solution_guide$Fill, function(x){
    if(grepl("_#", x)){
      splitted <- strsplit(x, split="_")[[1]]
      return(paste(splitted[1:(length(splitted)-1)], sep="_"))
    }else{
      return(x)
    }
  })
  colnames(solution_guide)[4] <- "Amount"
  
  # prepare labware guide
  labware_guide <- empty_tubewares[,c(1, 3, 4, 2)]
  labware_guide$Unit <- "tubes"
  colnames(labware_guide) <- colnames(solution_guide)
  
  # combine
  user_guide <- rbind.data.frame(
    solution_guide, labware_guide,
    add_tableSeparator(tableExpander(deck_map, solution_guide), "> Deck Map")
  )
  
  return(user_guide)
}

# CORE
mainExec <- function(file_address){
  # PREPARATION------------
  # reading input files
  plateInput <- readInputFile(file_address)
  plateMap <- plateInput[[1]]
  stockInfo <- plateInput[[2]]
  runInfo <- plateInput[[3]]
  
  # create dilution scheme
  dilScheme <- calculate_dilutionScheme(plateMap, stockInfo, runInfo)
  
  # initiate solution and deck map
  deckMap <- data.frame(Slot = c(1:12),
                        Fill = c(sapply(c(1:3), function(x) paste0("Plate_48_", LETTERS[x])),
                                 "tip300", "tip1000", "Epp_1_5", 
                                 "Falcon50_A", "Falcon15_A", "Falcon15_B",
                                 "Falcon50_Medium", "Falcon50_B", 'trash'))
  deckMap <- correct_deckMap(deckMap, runInfo)
  solutionMap <- initiateSolutionMap(dilScheme, deckMap, stockInfo)
  
  # GENERATE COMMAND LIST---------------
  # CMD - Solvent Distribution
  cmdList_solventDistribution <- cmd_solventDistribution(dilScheme, solutionMap)
  
  # CMD - Serial Dilution
  cmdList_serialDilution <- cmd_serialDilution(dilScheme, cmdList_solventDistribution, solutionMap)
  
  # CMD - Medium Distribution
  cmdList_mediumDist <- cmd_MediumDistributionPlate(cmdList_serialDilution, plateMap, solutionMap, deckMap, runInfo)
  
  # CMD - Main Solution Distribution
  cmdList_solutionDistribution <- cmd_SolutionDistributionPlate(cmdList_mediumDist, plateMap, dilScheme, solutionMap, deckMap, runInfo) 
  
  # CMD - Combine
  if(cmdList_mediumDist$to_slot == ""){
    cmdList <- rbind.data.frame(cmdList_solventDistribution, cmdList_serialDilution, cmdList_solutionDistribution)
  }else{
    cmdList <- rbind.data.frame(cmdList_solventDistribution, cmdList_serialDilution, cmdList_mediumDist, cmdList_solutionDistribution)
  }
  # POST-CALCULATIONS AND OUTPUT GENERATION-------------
  # separate multi-dispense aspirate set
  cmdList <- correct_cmd_list_Aspirate(cmdList)
  
  # extra solvent
  checkMedium <- check_extraMedium(plateMap, solutionMap, cmdList)
  while(checkMedium[[3]]==T){
    checkMedium <- check_extraMedium(plateMap, checkMedium[[1]], checkMedium[[2]])
  }
  solutionMap <- checkMedium[[1]]
  cmdList <- checkMedium[[2]]
  
  # empty tube wares
  emptyTubeWares <- subset(solutionMap, Fill != "" & !(Fill %in% c(unique(plateMap$Medium), unique(plateMap$Drug), unique(subset(solutionMap, grepl("#", Fill))$Fill))))
  emptyTubeWares <- sapply(unique(emptyTubeWares$Deck), function(x){
    current_set <- subset(emptyTubeWares, Deck==x)
    tube_type <- if(grepl("15", current_set$DeckID[1])){"15 mL Falcon"}else if(grepl("50", current_set$DeckID[1])){"50 mL Falcon"}else{"1.5 mL Eppendorf"}
    return(c(x, nrow(current_set), paste(current_set$Slot, collapse=', '), tube_type))}) %>% t() %>% data.frame()
  colnames(emptyTubeWares) <- c("Deck", "Number", "Slots", "Tube")
  
  # required solutions
  initialSolutions <- calculate_startingSolutions(solutionMap, plateMap, cmdList)
  
  # removed unused labwares
  unused_wares <- unique(solutionMap$Deck)[sapply(unique(solutionMap$Deck), function(x){
    nrow(subset(solutionMap, Deck==x & Fill != ""))==0})]
  deckMap$Fill[deckMap$Slot %in% unused_wares] <- ""
  
  # prepare output (robot handler and command list)
  final_commandList <- rbind.data.frame(
    add_tableSeparator(tableExpander(initialSolutions[,c(3, 4, 6)], cmdList), "> SolutionList"),
    add_tableSeparator(cmdList, "> CmdList"),
    add_tableSeparator(tableExpander(deckMap, cmdList), "> DeckMap")
  )
  
  # user guide
  final_userGuide <- create_userGuide(deckMap, initialSolutions, emptyTubeWares)
  
  # OUTPUT-----------
  return(list(final_commandList, final_userGuide))
}

#TROUBLESHOOTING---------
#mainwd <- "C:/Users/jornb/Documents/GitHub/ot2new/Execution code for OT2/Incubator/Test User inputs"
#plateInput <- "48Well_InputTemplate.xlsx"
#dqs <- mainExec(paste0(mainwd, "/", plateInput))

#write.csv(dqs[[1]], paste0(mainwd, "/48WellTrialCMDList_exp2.csv"), row.names = F)
#write.csv(dqs[[2]], paste0(mainwd, "/48WellTrialUSRGuide_exp2.csv"), row.names = F)

# mainwd <- "C:\\Users\\sebas\\Documents\\GitHub\\ot2\\Plate48"
# plateInput <- "InputTest.xlsx"
# dqs <- mainExec(paste0(mainwd, "/", plateInput))