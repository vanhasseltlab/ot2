#### META ####
# S. T. Tandar - 2021-1-1
# Universal CQ processor
#LIBRARIES------
library(dplyr)
library(readxl)
library(rlist)
library(tidyr)

# ---------- SECTION A - Read and Preparation -------------
ReadInput <- function(data_path){
  #initial read
  input_file <- read_xlsx(data_path, sheet=1) %>% data.frame()
  input_file <- input_file[(!apply(input_file, 1, function(x) all(is.na(x)))),] #removing na-only rows
  
  #A. gather stock information
  stock_info <- input_file[c(1:2),]
  stock_info <- stock_info[ , colSums(is.na(stock_info)) == 0]
  rownames(stock_info) <- stock_info[,1]
  stock_info <- stock_info[,-c(1, length(stock_info[1,]))]
  
  #B. gather volume information
  vol_info <- as.numeric(input_file[c(4:5), c(3)])
  names(vol_info) <- c("Total", "Inoculum")
  
  #C. gather info about number of plate
  n_plate <- as.numeric(input_file[5,6])
  
  #D. gather info about drug and solution map
  ## create plate map
  slot_vec <- c(sapply(c(1:12), function(x) paste(LETTERS[1:8], x, sep="")))
  
  ## read drug name on plate
  drug_name <- unlist(input_file[c(9:16), c(2:13)])
  
  ## read drug concentration on plate
  drug_conc <- unlist(input_file[c(19:26), c(2:13)])
  
  ## read solution map
  medium_map <- unlist(input_file[c(29:36), c(2:13)])
  
  #combine to drug map
  drug_map <- cbind.data.frame(slot_vec, drug_name, drug_conc, medium_map)
  colnames(drug_map) <- c("Slot", "DrugName", "Conc", "Medium")
  rownames(drug_map) <- c()
  drug_map$DrugName[is.na(drug_map$DrugName)] <- "mediumfill"
  
  return(list(stock_info, vol_info, drug_map, n_plate))
}
ParseDrugCombination <- function(slot_info){
  curdrugs <- strsplit(toString(slot_info[2]), split="_")[[1]]
  curconcs <- strsplit(toString(slot_info[3]), split="_")[[1]]
  curdata <- cbind(replicate(length(curdrugs), slot_info[1]),
                   replicate(length(curdrugs), slot_info[4]),
                   curdrugs, curconcs)
  colnames(curdata) <- c("Slot", "Medium", "DrugName", "Conc")
  return(curdata)
}
GetSolutionInfo <- function(drug_map){
  drug_map <- subset(drug_map, DrugName!="mediumfill")
  
  #iterate through all slots in drug map
  complete_info <- c()
  for(i in c(1:length(drug_map[,1]))){
    complete_info <- rbind.data.frame(complete_info, ParseDrugCombination(drug_map[i,]))
  }
  
  #get unique drug+conc+medium identifier
  complete_info$solID <- paste(complete_info$DrugName, complete_info$Conc, complete_info$Medium, sep="-")
  
  ## Truncate List ##
  #iterate through all solution ids
  solIDs <- unique(complete_info$solID)
  sol_list <- c()
  for(i in c(1:length(solIDs))){
    #subset
    curInfo <- subset(complete_info, solID==solIDs[i])
    
    #calculate number of wells inquired
    curInfo$nWell <- length(curInfo[,1])
    
    #concatenate result
    sol_list <- rbind.data.frame(sol_list, curInfo[1,])
  }
  rownames(sol_list) <- c()
  
  #order
  sol_list$Conc <- as.numeric(sol_list$Conc)
  
  sol_list <- sol_list[order(sol_list$Conc),]
  sol_list <- sol_list[order(as.character(sol_list$DrugName)),] %>%
    dplyr::select(Medium, DrugName, Conc, solID, nWell) # remove column slot
  
  return(sol_list)
}

## NEW DIL SCHEME
CalculateRequired_ConcVol <- function(drug_map, vol_info, sol_list, n_plate){
  well_totalVol <- vol_info["Total"] - vol_info["Inoculum"]
  sol_separate <- max(sapply(drug_map$DrugName, function(x) length(strsplit(x, split="_")[[1]])))
  vol_per_drugSol <- well_totalVol / sol_separate
  
  sol_list$ReqVolume <- sol_list$nWell * vol_per_drugSol * n_plate
  sol_list$ReqConc <- sol_list$Conc * vol_info["Total"] / vol_per_drugSol
  
  return(sol_list)
}
cal_dilScheme_MedID <- function(current_set_id, solution_list, stock_info){
  # subset
  current_set <- subset(solution_list, medDrugID==current_set_id) %>% filter(Conc>0) %>%
    mutate(finVolumes=0, volAbove=0, volMedium=0, vol_forBelow=0)
  
  # get stock concentration
  stock_conc <- stock_info[1,which(colnames(stock_info)==current_set$DrugName[1])] %>% as.numeric()
  
  # main dilutions
  for(i in c(1:nrow(current_set))){
    # assign required volume
    current_set$finVolumes[i] <- current_set$ReqVolume[i] + 200 # excess
    
    if(i>1){
      current_set$finVolumes[i] <- current_set$ReqVolume[i] + current_set$volAbove[i-1] + 200 # excess
    }
    
    # calculate amount required from above
    if(i < nrow(current_set)){
      current_set$volAbove[i] <- current_set$ReqConc[i] * current_set$finVolumes[i] / current_set$ReqConc[i+1]
      current_set$vol_forBelow[i+1] <- current_set$volAbove[i]
    }else{
      if(stock_conc > 10*max(current_set$ReqConc)){
        current_set$volAbove[i] <- current_set$finVolumes[i]/10
      }else{
        current_set$volAbove[i] <- current_set$finVolumes[i] * current_set$ReqConc[i] / stock_conc
      }
    }
    
    # calculate required medium
    current_set$volMedium[i] <- current_set$finVolumes[i] - current_set$volAbove[i]
  }
  
  # if pre-dilution if required
  if(stock_conc >= 10*max(current_set$ReqConc)){
    #pre-dilute stock
    predilution_conc <- 10*max(current_set$ReqConc)
    req_volume <- max(current_set$ReqVolume[nrow(current_set)]/10 + 200, 300)
    stock_predilution <- c(current_set_id,
                           unique(current_set$Medium), unique(current_set$DrugName),
                           10*max(current_set$Conc), 
                           paste(unique(current_set$DrugName), 10*max(current_set$Conc), unique(current_set$Medium), sep="-"),
                           0, req_volume, predilution_conc,
                           req_volume, req_volume*predilution_conc/stock_conc,
                           req_volume*(1-predilution_conc/stock_conc), current_set$volAbove[nrow(current_set)]) %>% unlist()
    current_set <- rbind.data.frame(current_set, stock_predilution)
  }
  
  return(current_set)
}
##

getHighestAmount <- function(medDrug_ID, solution_list){
  solution_list <- subset(solution_list, medDrugID==medDrug_ID)
  return( solution_list$volAbove[ solution_list$ReqConc==max(solution_list$ReqConc) ] )
}
CalculateStockAmt <- function(sol_list, stock_info){
  #calculate required stock amount
  reqStockAmt <- sapply(unique(sol_list$medDrugID), function(x) getHighestAmount(x, sol_list))
  drugnames <- sapply(names(reqStockAmt), function(x) strsplit(x, split="_")[[1]][2])
  
  #clump values per-drug type
  amounts <- sapply(unique(drugnames), function(x) max(ceiling(sum(reqStockAmt[drugnames==x])/100)*100, 300))
  
  #concatenate results; create new stock list
  stock_info <- data.frame(DrugName = colnames(stock_info), StockConc=unlist(stock_info[1,]))
  stock_info$AmtRequired <- sapply(stock_info$DrugName, function(x) amounts[names(amounts)==x])
  
  return(stock_info)
}

# ---------- SECTION B - Deck Preparation -------------
InitiateRacks <- function(deck_map){
  #initiate rack map
  rack_map <- c()
  #iterate through all labwares
  for(i in c(1:length(deck_map[,1]))){
    if(grepl("rack", deck_map$Item[i], ignore.case=T)){
      #if the current ware is a rack; get rack size
      if(grepl("50", deck_map$Item[i])){
        rack_size <- c(2,3)
      }else if(grepl("15", deck_map$Item[i])){
        rack_size <- c(3,5)
      }else if(grepl("1.5", deck_map$Item[i])){
        rack_size <- c(4,6)
      }else{
        rack_size <- c(0,0)
      }
      
      #create slots
      current_slots <- as.vector(sapply(c(1:rack_size[1]), 
                                        function(x) paste(LETTERS[x], c(1:rack_size[2]), sep="")))
      current_slots <- cbind.data.frame(replicate(length(current_slots), deck_map$Labware[i]),
                                        replicate(length(current_slots), deck_map$Item[i]),
                                        current_slots,
                                        replicate(length(current_slots), "(empty)"),
                                        replicate(length(current_slots), 0))
      colnames(current_slots) <- c("Labware", "Item", "Slot", "FillSolution", "FillVolume")
      
      #concatenate results
      if(length(rack_map)>0){rack_map <- rbind.data.frame(rack_map, current_slots)}else{rack_map <- data.frame(current_slots)}
    }
  }
  return(rack_map)
}
DistributeStock <- function(rack_map, stock_info){
  #separate stock rack from rack map
  stock_rack <- rack_map[grepl("Stock", rack_map$Item, ignore.case=T),]
  spare_15 <- rack_map[grepl("15", rack_map$Item),]
  remaining_racks <- rack_map[(!(grepl("Stock", rack_map$Item, ignore.case=T)) & !grepl("15", rack_map$Item)),]
  
  #rack_map <- rack_map[!(grepl("Stock", rack_map$Item, ignore.case=T)),]
  
  #distributing stock solutions to racks; iterate through all drugs
  for(i in c(1:length(stock_info[,1]))){
    # check fill amount
    current_fill_amount <- stock_info$AmtRequired[i]
    if(current_fill_amount <= 1000){
      # fill to eppendorf if volume is less than or equal to 1000
      fill_loc <- which(stock_rack$FillSolution=="(empty)")[1]
      stock_rack$FillSolution[fill_loc] <- stock_info$DrugName[i]
      stock_rack$FillVolume[fill_loc] <- current_fill_amount + 200 # 200 uL excess
    }else{
      if(current_fill_amount <= 13700){
        # fill to a single 15 mL Falcon tube if amount <= 13700
        fill_loc <- which(spare_15$FillSolution=="(empty)")[1]
        spare_15$FillSolution[fill_loc] <- stock_info$DrugName[i]
        spare_15$FillVolume[fill_loc] <- current_fill_amount + 300 # 300 uL excess for falcon tubes
      }else{
        # otherwise, distribute to multiple falcon tubes
        remaining_fill <- current_fill_amount
        while(remaining_fill>0){
          #calculate current fill volume
          current_fill <- min(remaining_fill, 13700)
          
          #update remaining fill volume
          remaining_fill <- remaining_fill - current_fill
          
          #fill the stock rack
          fill_loc <- which(spare_15$FillSolution=="(empty)")[1]
          spare_15$FillSolution[fill_loc] <- stock_info$DrugName[i]
          spare_15$FillVolume[fill_loc] <- current_fill + 300 # 300 uL excess for falcon tubes
        }
      }
      
    }
    
  }
  
  #append back to rack map
  rack_map <- rbind.data.frame(remaining_racks, stock_rack, spare_15)
  
  return(rack_map)
}
DistributeSolvent <- function(sol_list, rack_map){
  solvents <- unlist(unique(sol_list$Medium))
  #separate stock rack from rack map
  solvent_rack <- rack_map[grepl("Solvent", rack_map$Item, ignore.case=T),]
  rack_map <- rack_map[!(grepl("Solvent", rack_map$Item, ignore.case=T)),]
  for(i in c(1:length(solvents))){
    #fill the stock rack
    fill_loc <- which(solvent_rack$FillSolution=="(empty)")[1]
    solvent_rack$FillSolution[fill_loc] <- solvents[i]
    solvent_rack$FillVolume[fill_loc] <- 50000 #set to max for now
  }
  
  #concatenate back to rack map
  rack_map <- rbind.data.frame(rack_map, solvent_rack)
  return(rack_map)
}
AssignSolutions <- function(sol_list, rack_map){
  #separate dilution rack from the rest
  dil_rack_falcon <- rack_map[grepl("15", rack_map$Item),]
  dil_rack_Epp <- rack_map[grepl("1.5", rack_map$Item),]
  rack_map <- rack_map[!grepl("15", rack_map$Item),]
  rack_map <- rack_map[!grepl("1.5", rack_map$Item),]
  
  #iterate through all items in solution list
  for(i in c(1:length(sol_list[,1]))){
    #select rack
    if(sol_list$finVolumes[i]<=1200){
      #fill 1.5 mL rack (same rack as stock rack)
      fill_loc <- which(dil_rack_Epp$FillSolution=="(empty)")[1]
      dil_rack_Epp$FillSolution[fill_loc] <- sol_list$solID[i]
      dil_rack_Epp$FillVolume[fill_loc] <- sol_list$finVolumes[i]
    }else{
      #fill 15 mL Falcon tube rack
      fill_loc <- which(dil_rack_falcon$FillSolution=="(empty)")[1]
      dil_rack_falcon$FillSolution[fill_loc] <- sol_list$solID[i]
      dil_rack_falcon$FillVolume[fill_loc] <- sol_list$finVolumes[i]
    }
  }
  #concatenate back
  rack_map <- rbind.data.frame(rack_map, dil_rack_Epp, dil_rack_falcon)
  
  return(rack_map)
}

# ---------- SECTION C - Main Operations -------------
InitSolventDist <- function(sol_list, deck_map, rack_map){
  cmd_list <- c()   #initiate result
  medium_amounts <- sol_list$finVolumes - sol_list$volAbove #calculate required medium amounts
  
  #clump together all mediums with one pipette tip
  medtypes <- unlist(unique(sol_list$Medium))
  for(w in c(1:length(medtypes))){
    #subset
    cur_solList <- subset(sol_list, Medium==medtypes[w])
    
    #iterate through all items in the current solution list
    for(q in c(1:length(cur_solList[,1]))){
      #extract current information
      source_labware <- deck_map$Labware[grepl("solvent", deck_map$Item, ignore.case=T)]
      source_slot <- rack_map$Slot[which(rack_map$FillSolution == cur_solList$Medium[q])]
      target_labware <- rack_map$Labware[which(rack_map$FillSolution==sol_list$solID[q])]
      target_slot <- rack_map$Slot[which(rack_map$FillSolution==cur_solList$solID[q])]
      
      #calculate transfer informations
      trans_amt <- medium_amounts[q]
      comment <- paste("Distributing", cur_solList$Medium[q], "to", target_labware, target_slot, sep=" ")
      
      #concatenate result
      nex_command <- c(source_labware, source_slot, target_labware, target_slot, 
                       trans_amt, 0, w, comment)
      cmd_list <- rbind(cmd_list, nex_command)
    }
  }
  
  colnames(cmd_list) <- c("SourceLabware", "SourceSlot", "TargetLabware", "TargetSlot",
                          "TransAmt", "MixAmt", "TipID", "Comment")
  return(cmd_list)
}
InitialStockDilution <- function(cmd_list, sol_list, rack_map){
  #get tip id
  last_tip <- max(as.numeric(cmd_list[,"TipID"])) + 1
  
  #iterate through all drug-medium ID
  medDrugIDs <- unique(sol_list$medDrugID)
  for(q in c(1:length(medDrugIDs))){
    #subset
    cur_data <- subset(sol_list, medDrugID==medDrugIDs[q])
    cur_data <- cur_data[cur_data$ReqConc==max(cur_data$ReqConc),]
    
    #create command
    nex_command <- c(rack_map$Labware[rack_map$FillSolution==cur_data$DrugName], 
                     rack_map$Slot[rack_map$FillSolution==cur_data$DrugName],
                     rack_map$Labware[rack_map$FillSolution==cur_data$solID],
                     rack_map$Slot[rack_map$FillSolution==cur_data$solID],
                     cur_data$volAbove, cur_data$volAbove, last_tip, "Initial Dilution")
    
    #update tip number
    last_tip <- last_tip + 1
    
    #concatenate result
    cmd_list <- rbind(cmd_list, nex_command)                
  }
  cmd_list <- data.frame(cmd_list)
  return(cmd_list)
}
MainDilution <- function(cmd_list, sol_list, rack_map){
  #get latest tip id
  tip_latest <- max(as.numeric(cmd_list$TipID))
  #iterate through all medium-drug id
  medDrugIDs <- unique(sol_list$medDrugID)
  for(i in c(1:length(medDrugIDs))){
    #subset
    cur_data <- subset(sol_list, medDrugID==medDrugIDs[i])
    cur_data <- cur_data[order(cur_data$ReqConc, decreasing=T),]
    
    #iterate through all items in the current medium-drug combintation
    for(j in c(1:(length(cur_data[,1])-1))){
      #extract current informations
      source_labware <- rack_map$Labware[rack_map$FillSolution==cur_data$solID[j]]
      source_slot <- rack_map$Slot[rack_map$FillSolution==cur_data$solID[j]]
      target_labware <- rack_map$Labware[rack_map$FillSolution==cur_data$solID[j+1]]
      target_slot <- rack_map$Slot[rack_map$FillSolution==cur_data$solID[j+1]]
      trans_amt <- cur_data$volAbove[j+1]
      
      #create next command line
      cmd_line <- c(rack_map$Labware[rack_map$FillSolution==cur_data$solID[j]],
                    rack_map$Slot[rack_map$FillSolution==cur_data$solID[j]],
                    rack_map$Labware[rack_map$FillSolution==cur_data$solID[j+1]],
                    rack_map$Slot[rack_map$FillSolution==cur_data$solID[j+1]],
                    cur_data$volAbove[j+1], cur_data$volAbove[j+1], tip_latest,
                    paste("Serial dilution to ", cur_data$solID[j+1]))
      
      #concatenate result
      cmd_list <- rbind.data.frame(cmd_list, cmd_line)
      tip_latest <- tip_latest + 1 #update latest tip
    }
  }
  return(cmd_list)
}
recreate_solID <- function(row){
  row <- unlist(row)
  solIDs <- paste(strsplit(row[2], split="_")[[1]], 
                  strsplit(row[3], split="_")[[1]],
                  replicate(length(strsplit(row[3], split="_")[[1]]), row[4]),
                  sep="-")
  return(solIDs)
}
MainDistribution <- function(vol_info, sol_list, rack_map, drug_map, n_plate, deck_map, cmd_list){
  #get current tip id
  cur_tip <- max(as.numeric(cmd_list$TipID)) + 1
  
  #preparing drug map
  drug_map2 <- subset(drug_map, DrugName != "mediumfill")
  solID_info <- c()
  for(qq in c(1:length(drug_map2[,1]))){
    nexDat <- recreate_solID(drug_map2[qq,])
    
    if(length(solID_info)>0){solID_info <- rbind(solID_info, nexDat)}else{solID_info <- nexDat}
  }
  drug_map3 <- cbind.data.frame(drug_map2$Slot, solID_info)
  colnames(drug_map3)[1] <- "Slot"
  
  #calculate amount to transfer per-drugID
  amt_per_well <- (vol_info[1] - vol_info[2]) / (length(drug_map3[1,])-1)
  
  #initiate plate command
  plate_distribute <- c()
  #iterate through all items in the solution list
  for(q in c(1:length(sol_list[,1]))){
    #select wells
    choose_well <- drug_map3$Slot[which(apply(drug_map3, 1, function(x) sol_list$solID[q] %in% x))]
    if(length(choose_well)>0){
      #create command
      nex_cmd <- c(rack_map$Labware[rack_map$FillSolution == sol_list$solID[q]],
                   rack_map$Slot[rack_map$FillSolution == sol_list$solID[q]],
                   "", paste(choose_well, collapse=", "), amt_per_well, 0, cur_tip,
                   paste("Distributing", sol_list$solID[q], "to"))
      #concatenate command line
      plate_distribute <- rbind(plate_distribute, nex_cmd)
      
      #update iteration
      cur_tip <- cur_tip + 1 #iterate tip id
    }
  }
  
  #fill 0 ng/uL solutions with respective mediums
  control_code <- paste("0", unique(drug_map$Medium), sep="-")
  med_names <- unique(drug_map$Medium)
  
  for(j in c(1:length(control_code))){
    choose_well <- drug_map3$Slot[apply(drug_map3, 1, function(x) sum(grepl(control_code[j], x)))>0]
    times <- apply(drug_map3, 1, function(x) sum(grepl(control_code[j], x)))[which(drug_map3$Slot %in% choose_well)]
    
    if(length(choose_well)>0){
      nex_cmd <- c()
      for(q in c(1:length(choose_well))){
        nex_cmd <- rbind(nex_cmd, 
                         c(rack_map$Labware[rack_map$FillSolution == med_names[j]],
                           rack_map$Slot[rack_map$FillSolution == med_names[j]],
                           "", choose_well[q], amt_per_well*times[q], 0, cur_tip,
                           paste("Distributing", med_names[j], "to")))
      }
    }else{
      nex_cmd <- NULL
    }
    
    
    #fill outer wells
    choose_well <- drug_map$Slot[drug_map$DrugName=="mediumfill" & drug_map$Medium == med_names[j]]
    if(length(choose_well)>0){
      nex_cmd2 <- c(rack_map$Labware[rack_map$FillSolution == med_names[j]],
                    rack_map$Slot[rack_map$FillSolution == med_names[j]],
                    "", paste(choose_well, collapse=", "), vol_info[1], 0, cur_tip + 1,
                    paste(med_names[j], "fillings for"))
    }else{
      nex_cmd2 <- NULL
    }
    
    #concatenate; update tip
    plate_distribute <- rbind(plate_distribute, nex_cmd, nex_cmd2)
    cur_tip <- cur_tip + 2
  }
  
  #turn to data frame
  plate_distribute <- data.frame(plate_distribute)
  colnames(plate_distribute) <- colnames(cmd_list)
  
  #repeat for all plates
  grand_plateDis <- c()
  for(i in c(1:n_plate)){
    nex_cmd <- plate_distribute
    nex_cmd$TargetLabware <- deck_map$Labware[deck_map$Item==paste("96-well", LETTERS[i], sep="_")] #update target labware
    nex_cmd$Comment <- paste(nex_cmd$Comment,  nex_cmd$TargetLabware, sep=" ") #update comments
    if(i==1){grand_plateDis <- data.frame(nex_cmd)}else{grand_plateDis <- rbind.data.frame(grand_plateDis, nex_cmd)}
  }
  
  grand_plateDis <- grand_plateDis[order(grand_plateDis$TipID, decreasing=F),]
  
  #concatenate to command list
  cmd_list <- rbind.data.frame(cmd_list, grand_plateDis)
  return(cmd_list)
}

# ---------- SECTION D - Output Preparations -------------
Calculate_SolventAmt <- function(rack_map, cmd_list){
  #extract solvent rack
  solvent_rack <- rack_map[grepl("solvent", rack_map$Item, ignore.case=T),]
  rack_map <- rack_map[!grepl("solvent", rack_map$Item, ignore.case=T),]
  
  #zero in
  solvent_rack$FillVolume <- 0
  
  #iterate through command list
  for(i in c(1:length(cmd_list[,1]))){
    if(cmd_list$SourceLabware[i] == solvent_rack$Labware[1]){
      #get volumes
      tube_fill <- solvent_rack$FillVolume[solvent_rack$Slot == cmd_list$SourceSlot[i]]
      nex_trans_amt <- as.numeric(cmd_list$TransAmt[i]) * 
        length(strsplit(cmd_list$TargetSlot[i], split=", ")[[1]])
      
      if(tube_fill + nex_trans_amt < 46000){
        #add volume
        solvent_rack$FillVolume[solvent_rack$Slot==cmd_list$SourceSlot[i]] <- solvent_rack$FillVolume[solvent_rack$Slot==cmd_list$SourceSlot[i]] + nex_trans_amt
      }else{
        #if volume exceeded limit (48 mL)
        
        #create new solvent tube
        fill_loc <- which(solvent_rack$FillSolution=="(empty)")[1]
        solvent_rack$FillSolution[fill_loc] <- solvent_rack$FillSolution[solvent_rack$Labware==cmd_list$SourceLabware[i] &
                                                                           solvent_rack$Slot==cmd_list$SourceSlot[i]]
        
        #assign volume
        solvent_rack$FillVolume[fill_loc] <- nex_trans_amt
        
        #reassign solvent slot commands
        olds <- cmd_list[c(1:i-1),]
        news <- cmd_list[c(i:length(cmd_list[,1])),]
        
        #change new parts
        news$SourceSlot[news$SourceLabware==solvent_rack$Labware[1] & news$SourceSlot==cmd_list$SourceSlot[i]] <- 
          solvent_rack$Slot[fill_loc]
        
        #reassign command list
        cmd_list <- rbind.data.frame(olds, news)
      }
    }
  }
  #round-up solvent rack fills
  solvent_rack$FillVolume[solvent_rack$FillSolution!="(empty)"] <- 
    ceiling(as.numeric(solvent_rack$FillVolume[solvent_rack$FillSolution!="(empty)"])/1000)*1000 + 2000
  
  #re-combine rack map
  rack_map <- rbind.data.frame(rack_map, solvent_rack)
  
  return(list(rack_map, cmd_list))
}
Output_PrepSolutions <- function(stock_info, rack_map, deck_map, sol_list){
  #extract original solutions
  original_solutions <- c(stock_info$DrugName, unique(sol_list$Medium))
  prep_items <- subset(rack_map, FillSolution %in% original_solutions)
  
  #get solvent location
  solvent_loc <- deck_map$Labware[grepl("solvent", deck_map$Item, ignore.case=T)]
  
  #add category; fix volume and units for solvents (to mL); add tube type; rearrange
  prep_items$Category <- sapply(prep_items$Labware, function(x) if(x==solvent_loc){
    "SOLVENT"}else{"STOCK"})
  prep_items$FillVolume[prep_items$Category=="SOLVENT"] <- prep_items$FillVolume[prep_items$Category=="SOLVENT"]/1000
  prep_items$Unit <- sapply(prep_items$Category, function(x) if(x=="SOLVENT"){"mL"}else{"uL"})
  prep_items$TubeType <- sapply(prep_items$Labware, function(x){
    labware_item <- deck_map$Item[deck_map$Labware==x]
    tubeType <- if(grepl("50", labware_item)){"50 mL Falcon"}else if(grepl("15", labware_item)){"15 mL Falcon"}else{"1.5 mL Eppendorf"}
    return(tubeType)
  })
  prep_items <- prep_items[,c("Category", "Labware", "Slot", "TubeType", "FillSolution", "FillVolume", "Unit")]
  
  return(prep_items)
}
Count_DilTubes <- function(stock_info, sol_list, rack_map, prep_sols){
  #select items
  original_solutions <- unlist(c(stock_info$DrugName, unique(sol_list$Medium), "(empty)"))
  select_racks <- rack_map[(!(rack_map$FillSolution %in% original_solutions)),]
  
  #iterate through all selected racks
  racks <- unique(select_racks$Labware)
  for(i in c(1:length(racks))){
    #subset
    current_rack <- subset(select_racks, Labware==racks[i])
    #gather information
    slots <- paste(current_rack$Slot, collapse=", ")
    tube_type <- if(grepl("15", current_rack$Item[1])){"15 mL Falcon"}else{"1.5 mL Eppendorf"}
    n_tube <- length(current_rack[,1])
    
    #append to prepSols
    prep_sols <- rbind.data.frame(prep_sols,
                                  c("EMPTY TUBE", current_rack$Labware[1],
                                    slots, tube_type, "-", n_tube, "tubes"))
  }
  return(prep_sols)
}
AdjustDeck <- function(deck_map, rack_map, n_plate){
  #adjusting number of plates
  required_plates <- sapply(c(1:n_plate), function(x) paste("96-well", LETTERS[x], sep="_"))
  deck_map$Item[grepl("96-well", deck_map$Item, ignore.case=T) & 
                  !(deck_map$Item %in% required_plates)] <- "(empty)"
  
  #removing empty tube racks
  ##iterate through all rack labwares
  racks <- unique(rack_map$Labware)
  unused_racks <- c()
  for(i in c(1:length(racks))){
    #subset
    cur_rack <- subset(rack_map, Labware==racks[i])
    
    #check fill
    if(sum(!grepl("empty", cur_rack$FillSolution)) == 0){ #if none is not empty (none is filled)
      unused_racks <- c(unused_racks, racks[i])
    }
  }
  deck_map$Item[deck_map$Labware %in% unused_racks] <- "(empty)"
  
  return(deck_map)
}

#MAIN--------
mainExec <- function(file_name){
  # ---------- SECTION A - Read and Preparation -------------
  allInput <- ReadInput(file_name)
  stockInfo <- allInput[[1]]
  volInfo <- allInput[[2]]
  drugMap <- allInput[[3]]
  nPlate <- allInput[[4]]
  
  #A. Parsing solutions map
  solList <- GetSolutionInfo(drugMap)
  
  #B. Pre-calculating solution amounts required (for wells)
  solList <- CalculateRequired_ConcVol(drugMap, volInfo, solList, nPlate)
  
  #C. Serial Dilution Pre-Calculation
  solList <- solList %>% unite("medDrugID", Medium, DrugName, remove=F)
  
  solList <- lapply(unique(solList$medDrugID), cal_dilScheme_MedID, solution_list=solList, stock_info=stockInfo) %>%
    list.rbind() %>%
    mutate(Conc=as.numeric(Conc), nWell=as.numeric(nWell), ReqVolume=as.numeric(ReqVolume),
           ReqConc = as.numeric(ReqConc), finVolumes=as.numeric(finVolumes), 
           volAbove=as.numeric(volAbove)) %>% 
    dplyr::select(Medium, DrugName, Conc, solID, nWell, ReqVolume, 
                  ReqConc, medDrugID, finVolumes, volAbove)
  
  #D. Calculate required stock amounts
  stockInfo <- CalculateStockAmt(solList, stockInfo)
    
  # ---------- SECTION B - Deck Preparation -------------
  #E. Initiate Deck Map
  deckMap <- c("96-well_D", "96-well_E", "96-well_F",
               "96-well_A", "96-well_B", "96-well_C",
               "tip", "Rack_50_Solvent", "Rack_15",  
               "Rack_15_B", "Rack_1.5_Stock", "TRASH")
  deckMap <- cbind.data.frame(sapply(c(1:12), function(x) paste("labware_", x, sep="")),
                              deckMap)
  colnames(deckMap) <- c("Labware", "Item")
  
  #F. Initiate Rack
  rackMap <- InitiateRacks(deckMap)
  
  #E. Distributing Stock and Solvent to Rack
  rackMap <- DistributeStock(rackMap, stockInfo)
  rackMap <- DistributeSolvent(solList, rackMap)
  
  #F. Assigning Solution Slots
  rackMap <- AssignSolutions(solList, rackMap)
  
  # ---------- SECTION C - Main Operations -------------
  #x1. Initial Solvent Distribution for Dilution
  cmdList <- InitSolventDist(solList, deckMap, rackMap)
  
  #x2. Initial Stock Dilution
  cmdList <- InitialStockDilution(cmdList, solList, rackMap)
  
  #x3. Serial Dilutions
  cmdList <- MainDilution(cmdList, solList, rackMap) # here
  
  #x4. Distribution
  cmdList <- MainDistribution(volInfo, solList, rackMap, 
                              drugMap, nPlate, deckMap, cmdList)
  
  # ---------- SECTION D - Output Preparations -------------
  #q1. Calculate Required Solvent Amount
  rackMap <- Calculate_SolventAmt(rackMap, cmdList) #pool
  cmdList <- rackMap[[2]] #extract command list
  rackMap <- rackMap[[1]] #extract rack map info
  
  #q2. Solutions and Tubes to Prepare
  prepSols <- Output_PrepSolutions(stockInfo, rackMap, deckMap, solList)
  prepSols <- Count_DilTubes(stockInfo, solList, rackMap, prepSols)
  
  #q3. Deck Map Show
  deckMap <- AdjustDeck(deckMap, rackMap, nPlate) #remove unecessary labwares
  usrDeck <- rbind(c(10:12),
                   deckMap$Item[10:12],
                   c(7:9),
                   deckMap$Item[7:9],
                   c(4:6),
                   deckMap$Item[4:6],
                   c(1:3),
                   deckMap$Item[1:3])
  usrDeck <- cbind(usrDeck, replicate(length(usrDeck[,1]), ""),
                   replicate(length(usrDeck[,1]), ""),
                   replicate(length(usrDeck[,1]), ""), replicate(length(usrDeck[,1]), ""))
  usrDeck <- rbind(c(">>>OT2 DECK MAP<<<", replicate(length(prepSols[1,])-1, "")),
                   usrDeck)
  colnames(usrDeck) <- colnames(prepSols)
  
  #q4. Robot Amount List
  robotAmtList <- cbind.data.frame(prepSols$Labware, prepSols$Slot, prepSols$FillSolution, 
                                   replicate(length(prepSols[,1]), ""), prepSols$FillVolume,
                                   replicate(length(prepSols[,1]), ""),
                                   replicate(length(prepSols[,1]), ""),
                                   replicate(length(prepSols[,1]), ""))
  robotAmtList <- rbind(c(">AmountList", replicate(length(cmdList[1,])-1, "")), robotAmtList)
  colnames(robotAmtList) <- colnames(cmdList)
  
  #q5. Main for Robot Handler
  robotHandler <- rbind.data.frame(prepSols, usrDeck)
  
  #q6. Prepare deck map for robot input
  robot_deckMap <- cbind(deckMap,
                          replicate(length(deckMap[,1]), ""),
                          replicate(length(deckMap[,1]), ""),
                          replicate(length(deckMap[,1]), ""),
                          replicate(length(deckMap[,1]), ""),
                          replicate(length(deckMap[,1]), ""),
                          replicate(length(deckMap[,1]), ""))
  robot_deckMap <- rbind(c(">PlateMap", replicate(length(cmdList[1,])-1, "")),
                         robot_deckMap)
  colnames(robot_deckMap) <- colnames(cmdList)
  
  #q7. Integrate robot commands
  robotCommands <- rbind.data.frame(robotAmtList,
                                    c(">CommandLines", replicate(length(cmdList[1,])-1, "")),
                                    cmdList,
                                    robot_deckMap)
  
  #q8. Remove row names
  rownames(robotHandler) <- c()
  rownames(robotCommands) <- c()
  
  # ---------- RETURN -------------
  robotHandler <<- robotHandler
  robotCommands <<- robotCommands
  
  return(robotHandler)
}

#TROUBLESHOOTING--------------
# mainwd <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\ot2\\CQ_Plate"
# inputFile <- "Test CQ plate_jorn_muc_AZT.xlsx"
# dqs <- mainExec(paste(mainwd, inputFile, sep="\\"))

#write.csv(robotCommands, paste0(mainwd, "/CommandList_test.csv"), row.names=F)