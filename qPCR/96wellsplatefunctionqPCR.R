options(stringsAsFactors = F)
library(dplyr)
library(rlist)
library(readxl)
library(tidyr)

##Start Functions---------------------------
#supportive function:
# Function: split_larger_rows
# Splits rows where the total volume exceeds a threshold
split_larger_rows <- function(df, max_volume =1000){
  i <- 1  # Start index
  while (i <= nrow(df)) {
    # Check if the total volume exceeds the limit
    if (df$totalvol[i] > max_volume) {
      # Divide the row values by 2
      original_row <- df[i, -ncol(df)] / 2

      # Update the current row with halved values
      df$totalvol[i] <- sum(original_row)
      
      # Create a new row with the same halved values
      new_row <- original_row
      new_row$totalvol <- sum(original_row)
      row_name <- paste(rownames(df)[i], 'splitted' , sep = "_")
      df <- rbind(df, new_row)
      rownames(df)[nrow(df)] <- row_name
    } else {
      i <- i + 1  # Move to the next row
    }
  }
  return(df)
}

# Function: GetPlateMap
# Reads a plate map from an Excel file and processes it into a well ID format
GetPlateMap <- function(file_name){
  res <- read_xlsx(file_name, 1, range= "B32:M39", col_names= F)%>% data.frame()
  rownames(res) <- LETTERS[1:8] # A-H for plate rows
  colnames(res) <- sapply(c(1:12), toString) # Plate columns 1-12
  
  #Vector
  map <- c()
  for(row in c(1:8)){
    #sub-setting
    curRow <- unlist(res[row,])
    #get info
    well_id <- sapply(c(1:12), function(x) paste(LETTERS[row], toString(x), sep='')) # Generate well IDs
    curRow <- cbind(well_id, curRow)   # Combine well IDs with data
    map <- rbind(map, curRow)
  }
  #parsing names
  fin_map <- c()
  parsed_names <- sapply(map[,2], function(x) strsplit(x, ' ', fixed=T))
  
  # Filter genes and sample names
  Filtered_genes <- lapply(parsed_names, function(x) {
    if (length(x) >= 2) { # Check if the element has at least two values
      x[2]  # No explicit return, R automatically returns the last expression
    } else {
      NA    # This is the last expression if the condition is false
    }
  })
  
  Filtered_names <- lapply(parsed_names, function(x) {
    if (length(x) >= 1) { # Check if the element has at least two values
      x[1]  # No explicit return, R automatically returns the last expression
    } else {
      NA    # This is the last expression if the condition is false
    }
  })
  res3 <- as.data.frame(do.call(rbind, Filtered_names))
  res3$V2 <- Filtered_genes
  res3$well_name <- sapply(LETTERS[1:8], function(x) paste0(x, c(1:12))) %>% as.vector()
  rownames(res3) <- res3[,3]
  res3[,3]<- NULL
  colnames(res3)<- c("Sample_name", "Gene_name")
  map <- res3
  return(map)
}

# Function: Getsamplequestion
# Extracts a specific cell value from the Excel sheet
Getsamplequestion<- function(file_name){
  res <- read_xlsx(file_name, 1, range= "H2", col_names= F)
  res <- as.character(res[[1,1]])
  return (res)
}

# Function: GetreactionNum
# Counts the occurrences of gene names in a plate map and returns a count table
GetreactionNum <- function(file_name){
  res <- read_xlsx(file_name, 1, range= "B32:M39", col_names= F)%>% data.frame()
  rownames(res) <- LETTERS[1:8]
  colnames(res) <- sapply(c(1:12), toString)
  
  #Vector
  map <- c()
  for(row in c(1:8)){
    #sub-setting
    curRow <- unlist(res[row,])
    #get info
    well_id <- sapply(c(1:12), function(x) paste(LETTERS[row], toString(x), sep=''))
    curRow <- cbind(well_id, curRow)
    map <- rbind(map, curRow)
  }
  
  #parsing names
  fin_map <- c()
  parsed_names <- sapply(map[,2], function(x) strsplit(x, ' ', fixed=T))

  parsed_names <- parsed_names[!is.na(parsed_names)]
  
  #filtering the primers out of it
  Filtered_genes <- lapply(parsed_names, function(x) {
    if (length(x) >= 2) { # Check if the element has at least two values
      x[2]  # No explicit return, R automatically returns the last expression
    } else {
      NA    # This is the last expression if the condition is false
    }
  })
  res2 <- unlist(Filtered_genes)
  
  # Create a data frame with the original order of genes
  gene_list <- data.frame(Gene = res2, stringsAsFactors = FALSE)
  
  # Count the occurrences of each gene while preserving their original order
  gene_list$Count <- ave(seq_along(gene_list$Gene), gene_list$Gene, FUN = length)

  # Remove duplicates, keeping only the first appearance of each gene
  gene_list <- gene_list[!duplicated(gene_list$Gene), ]

  # Set row names and return the data frame
  rownames(gene_list) <- gene_list$Gene
 
  gene_list$Gene <- NULL
  
  return(gene_list)
}

# Function: MMscheme
# Calculates the volumes of components needed for each mastermix
MMscheme <- function(R_num){
  #first parameters Mastermix
  sensi <- 1.5
  FW <- 0.36
  RV <- 0.36
  Buff <- 0.5
  MgCl2 <- 0.36
  H2O <- 2.92
  
  mastermix <- R_num
  mastermix$ExcessReactions <- mastermix$Count + 20
  mastermix$Sensimix <- mastermix$ExcessReactions * sensi
  mastermix$FWprimer <- mastermix$ExcessReactions * FW
  mastermix$RVprimer <- mastermix$ExcessReactions * RV
  mastermix$buffer <- mastermix$ExcessReactions * Buff
  mastermix$MgCl2 <- mastermix$ExcessReactions * MgCl2
  mastermix$H2O <- mastermix$ExcessReactions * H2O
  mastermix$Count <- NULL
  mastermix$ExcessReactions <- NULL
  mastermix$totalvol <- rowSums(mastermix)
  
  # Rename rows and handle volume exceeding threshold
  rownames(mastermix) <- paste("Mastermix", seq_len(nrow(mastermix)))
  mmxtest2 <<- mastermix
  allmix <- split_larger_rows(mastermix)
  mmxtest <<- allmix
  allmix$totalvol <- NULL

  return (allmix)
}

#Sollist
SolList_fill <- function(mastermix){

  #time to generate the volume of the needed items
  sensimixvol <- sum(mastermix$Sensimix)
  buffervol <- sum(mastermix$buffer)
  MgCl2vol <- sum(mastermix$MgCl2)
  H2Ovol <- sum(mastermix$H2O)
  track <- 0
  if(H2Ovol > 1000){
    H2Ovol <- H2Ovol/2
    H2Ovol <- data.frame(H2Ovol)
    H2Ovol <- rbind(H2Ovol, H2Ovol)
    rownames(H2Ovol) <- paste("H2O", seq_len(nrow(H2Ovol)))
    colnames(H2Ovol) <- c("ul")
    track <- 1
  }

  #now prep the primers for dataframe
  FWprimer <- data.frame(mastermix$FWprimer)
  rownames(FWprimer) <- paste("Forward primer", seq_len(nrow(FWprimer)))
  colnames(FWprimer) <- c("ul")
  FWprimer[1] <- FWprimer[1] + 100
  for (i in seq_len(nrow(FWprimer))){
    if(FWprimer$ul[i] > 200){
      FWprimer$ul[i] <- 200
    }
  }
  
  Rvprimer <- data.frame(mastermix$RVprimer)
  rownames(Rvprimer) <- paste("Reverse primer", seq_len(nrow(Rvprimer)))
  colnames(Rvprimer) <- c("ul")
  Rvprimer[1] <- Rvprimer[1] + 100
  for (i in seq_len(nrow(Rvprimer))){
    if(Rvprimer$ul[i] > 200){
      Rvprimer$ul[i] <- 200
    }
  }
  
  #complete the vollist to sollist for later assignment
  vollist <- rbind(sensimixvol,buffervol,MgCl2vol,H2Ovol)
  SolList <- data.frame(vollist)
  colnames(SolList) <- c("ul")

  if(track == 1){
    rownames(SolList) <- c("sensimixvol", "buffervol", "MgCl2vol", rownames(H2Ovol))
  }
  
  colnames(SolList) <- c("ul")

  SolList[1] <- SolList[1] + 100 #100 ul excess
  SolList <- rbind(SolList, FWprimer, Rvprimer)
  
  #add location in eppy
  rows <- LETTERS[1:4]  # Rows A-D
  cols <- 1:6           # Columns 1-6
  slots <- c()
  for (row in rows) {
    slots <- c(slots, paste0(row, cols))
  }
  
  #formatting SolList for future Solution List
  SolList$Slot_Id <- slots[seq_len(nrow(SolList))]
  SolList$Deck_Id <- 7
  SolList <- SolList %>% select(Slot_Id, everything())
  SolList <- SolList %>% select(Deck_Id, everything())

  return(SolList)
}

#Sample Location
Samplecoll <- function(file_name){
  res <- read_xlsx(file_name, 1, range= "B32:M39", col_names= F)%>% data.frame()
  rownames(res) <- LETTERS[1:8]
  colnames(res) <- sapply(c(1:12), toString)
  
  #Vector
  map <- c()
  for(row in c(1:8)){
    #sub-setting
    curRow <- unlist(res[row,])
    #get info
    well_id <- sapply(c(1:12), function(x) paste(LETTERS[row], toString(x), sep=''))
    curRow <- cbind(well_id, curRow)
    map <- rbind(map, curRow)
  }
  
  #parsing names
  fin_map <- c()
  parsed_names <- sapply(map[,2], function(x) strsplit(x, ' ', fixed=T))
  parsed_names <- parsed_names[!is.na(parsed_names)]
  
  #filtering the primers out of it
  Filtered_genes <- lapply(parsed_names, function(x) {
    if (length(x) >= 2) { # Check if the element has at least two values
      x[1]  # No explicit return, R automatically returns the last expression
    } else {
      NA    # This is the last expression if the condition is false
    }
  })
  res2 <- unlist(Filtered_genes)
  
  # Create a data frame with the original order of genes
  gene_list <- data.frame(Gene = res2, stringsAsFactors = FALSE)
  
  # Count the occurrences of each gene while preserving their original order
  gene_list$Count <- ave(seq_along(gene_list$Gene), gene_list$Gene, FUN = length)
  
  # Remove duplicates, keeping only the first appearance of each gene
  gene_list <- gene_list[!duplicated(gene_list$Gene), ]
  
  # Set row names and return the data frame
  rownames(gene_list) <- gene_list$Gene
  
  gene_list$Gene <- NULL
  
  rows <- LETTERS[1:4]  # Rows A-D
  cols <- 1:6           # Columns 1-6
  slots <- c()
  for (row in rows) {
    slots <- c(slots, paste0(row, cols))
  }
  
  gene_list <- data.frame(gene_list)
  gene_list$Deck_Id <- 9
  gene_list$Slot_Id <- slots[seq_len(nrow(gene_list))]

  na_index <- which(is.na(gene_list$Slot_Id))
  
  if (length(na_index) > 0) {
    # Update Deck_Id starting from the first NA
    gene_list$Deck_Id[na_index:length(gene_list$Deck_Id)] <- 10
    
    # Reset Slot_Id starting from A1
    reset_slots <- slots[seq_len(nrow(gene_list) - na_index[1] + 1)]
    gene_list$Slot_Id[na_index:nrow(gene_list)] <- reset_slots[seq_along(na_index:nrow(gene_list))]
  }
  
  #moving the slotid to front (easier read)
  gene_list <- gene_list %>% select(Slot_Id, everything())
  gene_list <- gene_list %>% select(Deck_Id, everything())
  gene_list$Count <- NULL
  return(gene_list)
}

#CMDlist parts------------------------------
cmd_mmprep <- function(SolList, Mastermix){
  #remove unnessesarry stuff
  Amountsollist <- SolList$ul

  SolList$ul <- NULL
  
  #make patern for recognision
  primer_pattern <- grepl("Forward primer \\d+|Reverse primer \\d+", rownames(SolList))
  
  #Truncated List for primer selection
  primerres <- SolList[primer_pattern,]
  SolList <- SolList[!primer_pattern,]
  rownames(SolList)<- c('Sensimix', 'buffer', 'MgCl2', 'H2O')
  
  cmd_start <- c()
  cur_asp <- 1
  cur_tip <- 1
  mix <- 0
  #begin itteration
  for(i in 1:nrow(SolList)){
    curset <- SolList[i,]

    cursol <- c()
    cursol<- cbind(Mastermix[1], Mastermix[2], Mastermix[row.names(curset)])

    for (j in 1:nrow(cursol)){
      commentvariable <- colnames(cursol[j,])
      commentvariable <- commentvariable[!commentvariable %in% c("Deck_Id", "Slot_Id")]
      pipette <- "NVT"
      comment <- paste("Putting" , commentvariable, "into mix")
      cmd_cur <- bind_cols(curset, cursol[j,], mix, cur_tip, cur_asp, pipette, comment)
      rownames(cmd_cur)<- NULL
      colnames(cmd_cur) <- c("from_deck", "from_slot", "to_deck", "to_slot", "amt", "mix", "tip_n", "asp_set", "pipette","comment")
      cmd_start <- rbind(cmd_start, cmd_cur)
    }
    cur_asp <- cur_asp + 1
    cur_tip <- cur_tip + 1
  }
  
  
  #location of Mastermix
  mmloc <- Mastermix[1:2]
  volmm <- cbind(Mastermix['FWprimer'], Mastermix['RVprimer'])
  # Dynamically assign PrimerType
  num_mastermixes <- nrow(Mastermix)
  
  primer_names <- rownames(primerres)
  
  primer_types <- ifelse(grepl("Forward", primer_names), 'FWprimer', 'RVprimer')

  primer_info <- data.frame(
    Primer = primer_names,
    PrimerType = primer_types,
    Deck_ID_Primer = primerres$Deck_Id,
    Slot_ID_Primer = primerres$Slot_Id)

  # Correct PairID Assignment by ensuring FW primers are paired with RV primers
  # Separate FW primers and RV primers
  fw_primers <- primer_info[grepl("Forward", primer_info$Primer), ]
  rv_primers <- primer_info[grepl("Reverse", primer_info$Primer), ]
  
  # Sort them by Slot_ID (or any appropriate criteria for pairing) to ensure proper pairing order
  fw_primers <- fw_primers[order(fw_primers$Slot_ID_Primer), ]
  rv_primers <- rv_primers[order(rv_primers$Slot_ID_Primer), ]
  
  # Create PairID that ensures correct pairing
  # Pair each FW primer with its corresponding RV primer
  fw_primers$PairID <- 1:nrow(fw_primers)
  rv_primers$PairID <- fw_primers$PairID  # Assign the same PairID to the corresponding RV primer
  
  # Combine the FW and RV primers back together
  primer_info <- rbind(fw_primers, rv_primers)
  
  # Reshape volumes into long format with dynamic mapping
  # Reshape volumes into long format dynamically
  volume_long <- volmm %>%
    pivot_longer(
      cols = everything(),
      names_to = "PrimerType",
      values_to = "Volume"
    ) %>%
    mutate(
      Mastermix = rep(rownames(volmm), each = ncol(volmm)), # Correct Mastermix replication
      PairID = rep(1:nrow(volmm), each = ncol(volmm)) # Dynamically group volumes into pairs
    )

  # Merge primers and volumes by PrimerType and PairID
  combined <- merge(primer_info, volume_long, by = c("PrimerType", "PairID"))

  # Add mastermix location details
  final_combined <- merge(combined, Mastermix, by.x = "Mastermix", by.y = "row.names")


  # Create final table
  final_primertable <- data.frame(
    `Primer Deck` = final_combined$Deck_ID_Primer,
    `Primer Slot` = final_combined$Slot_ID_Primer,
    `Mastermix Deck` = final_combined$Deck_Id,
    `Mastermix Slot` = final_combined$Slot_Id,
    `Volume` = final_combined$Volume
  )
  
  cmd_primer <- c()

  for (j in 1:nrow(final_primertable)){
    mix <- 0
    comment <- "Adding primers to mastermix"
    pipette <- "NVT"
    cmd_cur <- bind_cols(final_primertable[j,], mix, cur_tip, cur_asp, pipette, comment)
    colnames(cmd_cur) <- c("from_deck", "from_slot", "to_deck", "to_slot", "amt", "mix", "tip_n", "asp_set", "pipette","comment")
    cmd_primer <- rbind(cmd_primer, cmd_cur)
    cur_asp <- cur_asp + 1
    cur_tip <- cur_tip + 1
  }
  cmd_listmm <- rbind(cmd_start, cmd_primer)
  return(cmd_listmm)
}

#CMDList plating
cmd_plate <- function(mastermix, platemap, cmd_listmm){
  #for this part only location is interesting
  locmm <- mastermix[1:2]
  print(platemap)
  
  #omit nas
  platemap <- platemap[!(is.na(platemap$Gene_name)),]
  
  #make positions a column and remove sample name
  platemap$slot <- rownames(platemap)
  platemap$Sample_name<- NULL
  
  # Convert factor columns to character
  platemap[] <- lapply(platemap, function(col) {
    if (is.factor(col)) as.character(col) else col
  })
  
  # Get unique Gene_names
  unique_genes <- unique(platemap$Gene_name)
  
  # Assign Mastermix labels to each unique Gene_name
  print(unique_genes)
  mastermix_map <- setNames(paste0("Mastermix ", seq_along(unique_genes)), unique_genes)
  
  # Map Mastermix labels back to the original plate_map
  print(mastermix_map)
  platemap$Mastermix <- sapply(platemap$Gene_name, function(gene) mastermix_map[[gene]])

  #change mmloc so we have something to map to
  locmm$Mastermix <- rownames(locmm)
  
  #combining
  combined <- merge(platemap, locmm, by = c("Mastermix"))
  
  #add location plate
  combined$decklocplate <- 5

  tip_n <- tail(cmd_listmm$tip_n, n=1)
  asp_set <- tail(cmd_listmm$asp_set, n=1)
  
  #Amount Mastermix 
  amt <- 6
  
  #And readymake the CMD
  res <- data.frame(
    from_deck <- combined$Deck_Id,
    from_slot <- combined$Slot_Id,
    to_deck <- combined$decklocplate,
    to_slot <- combined$slot,
    amt <- amt
  )
  
  # putting the columns correct
  colnames(res) <- c("from_deck", "from_slot", "to_deck", "to_slot", "amt")
  tip_n <- tip_n + 1
  asp_set<-asp_set + 1
  
  #get a ready frame
  cmdplating <- cmd_listmm
  #volume counter to prevent p1000 use aka p50 values
  camtmin <- 40
  camtmax <- 50
  
  #adding n_tip and asp_set
  for(i in 1:nrow(res)){
    cur <- res[i,]
    min <- res[i-1,]
    
    if(i != 1){
      if(cur$from_slot != min$from_slot & i != 1){
        camt = amt
        mix <- 2
        tip_n <- tip_n + 1
        asp_set <- asp_set + 1
        res2 <- res[i,]
        pipette <- "NVT"
        comment <- "Plating Mastermix to plate"
        res2 <- cbind(res2, mix, tip_n, asp_set, pipette, comment)
        cmdplating <- rbind(cmdplating, res2)
      }else{
        res2 <- res[i,]
        pipette <- "NVT"
        comment <- "Plating Mastermix to plate"
        camt <- camt + amt
        mix <- 1
        if (between(camt, camtmin, camtmax)){
          asp_set <- asp_set + 1
          camt = amt
        }else{
          print("")
        }
        res2 <- cbind(res2, mix, tip_n, asp_set, pipette, comment)
        cmdplating <- rbind(cmdplating, res2)
      }
      
    }else{
      res2 <- res[i,]
      mix <- 2
      pipette <- "NVT"
      comment <- "Plating Mastermix to plate"
      camt <- amt
      res2 <- cbind(res2, mix, tip_n, asp_set, pipette, comment)
      cmdplating <- rbind(cmdplating, res2)
    }
  }
  
  rownames(cmdplating) <- NULL
  cmdplating <- cmdplating[order(cmdplating$asp_set), ]
  
  return(cmdplating)
}

cmd_sample<-function(sampledeck, platemap, cmd_listp){
  #for this part only location is interesting
  locsamp <- sampledeck[1:2]
  
  #omit nas
  platemap <- platemap[!(is.na(platemap$Sample_name)),]
  
  #make positions a column and remove sample name
  platemap$slot <- rownames(platemap)
  mastermixtarget <- platemap
  mastermixtarget$Sample_name <- NULL
  platemap$Gene_name<- NULL
  
  # Convert factor columns to character
  platemap[] <- lapply(platemap, function(col) {
    if (is.factor(col)) as.character(col) else col
  })
  
  # Get unique Gene_names
  unique_samples <- unique(platemap$Sample_name)
  
  # Assign Sample labels to each unique Gene_name
  samplemap <- setNames(paste0("Sample ", seq_along(unique_samples)), unique_samples)
  
  # Map sample labels back to the original plate_map
  platemap$Sample <- sapply(platemap$Sample_name, function(sample) samplemap[[sample]])
  
  #hardcode the amt and mixing + plate deck
  platemap$to_deck <- 5
  
  #change mmloc so we have something to map to
  locsamp$samplenames <- rownames(locsamp)
  locsamp$Sample <- sapply(locsamp$samplenames, function(sample) samplemap[[sample]])
  locsamp$samplenames <- NULL
  colnames(locsamp) <- c("from_deck", "from_slot", "Sample")

  #combining
  combined <- merge(platemap, locsamp, by = c("Sample"))

  #next part until combined is to make sure samples are selected on both mastermix and sample name not just samplename. prefends scenarios where sample 1 goes into mmx 1 and 2 with its tips
  # Get unique Gene_names
  unique_genes <- unique(mastermixtarget$Gene_name)
  
  # Assign Mastermix labels to each unique Gene_name
  mastermix_map <- setNames(paste0("Mastermix ", seq_along(unique_genes)), unique_genes)
  print(mastermix_map)
  
  # Map Mastermix labels back to the original plate_map
  print(mastermix_map)
  mastermixtarget$Mastermix <- sapply(mastermixtarget$Gene_name, function(gene) mastermix_map[[gene]])
  res2 <- data.frame(
    to_slot <- mastermixtarget$slot,
    mastermixor <- mastermixtarget$Mastermix
  )
  
  colnames(res2)<- c("slot", "mastermixor")
  combined <- merge(combined, res2, by = c("slot"))
  
  #add a number to sort the sample numbers
  combined$Sample_numeric <- as.numeric(gsub("Sample ", "", combined$Sample))
  
  # Extract alphabetical prefix and numeric suffix from the slot column
  combined$slot_prefix <- gsub("[0-9]+", "", combined$slot)  # Extracts letters
  combined$slot_number <- as.numeric(gsub("[^0-9]", "", combined$slot))  # Extracts numbers
  
  # Sort the data frame by slot_prefix and then slot_number
  combined <- combined[order(combined$slot_prefix, combined$slot_number), ]
  
  # Sort the data frame by the numeric Sample column
  combined_sorted <- combined[order(combined$Sample_numeric), ]
  
  # Remove the helper column if not needed
  combined_sorted$Sample_numeric <- NULL
  combined$slot_prefix <- NULL
  combined$slot_number <- NULL
  
  #format correctly
  res <- data.frame(
    from_deck <- combined_sorted$from_deck,
    from_slot <- combined_sorted$from_slot,
    to_deck <- combined_sorted$to_deck,
    to_slot <- combined_sorted$slot,
    amt <- 4,
    mix <- 1,
    temp <- combined_sorted$mastermixor
  )
  
  colnames(res) <- c("from_deck", "from_slot", "to_deck", "to_slot", "amt", "mix", "originalmm")
  
  tip_n <- tail(cmd_listp$tip_n, n=1)
  asp_set <- tail(cmd_listp$asp_set, n=1)
  
  #set tip_n and asp_set correct for the next part
  tip_n <- tip_n + 1
  asp_set<-asp_set + 1
  
  #just renaming for readability
  cmd_comp <- cmd_listp
  
  #adding n_tip and asp_set
  for(i in 1:nrow(res)){
    cur <- res[i,]
    min <- res[i-1,]
    if(i != 1){
      if (cur$originalmm != min$originalmm | cur$from_slot != min$from_slot){
          tip_n <- tip_n + 1
          asp_set <- asp_set + 1
          res2 <- res[i,]
          pipette <- "NVT"
          comment <- "Plating Sample"
          res2 <- cbind(res2, tip_n, asp_set, pipette, comment)
          res2$originalmm <- NULL
          cmd_comp <- rbind(cmd_comp, res2)
          
        }else{
          res2 <- res[i,]
          pipette <- "NVT"
          comment <- "Plating Sample"
          res2 <- cbind(res2, tip_n, asp_set, pipette, comment)
          res2$originalmm <- NULL
          cmd_comp <- rbind(cmd_comp, res2)
      }
    }else{
      res2 <- res[i,]
      pipette <- "NVT"
      comment <- "Plating Sample"
      res2 <- cbind(res2, tip_n, asp_set, pipette, comment)
      res2$originalmm <- NULL
      cmd_comp <- rbind(cmd_comp, res2)
    }
  }
  rownames(cmd_comp) <- NULL
  
  #sorting because somehow the asp number does weird things
  col_order <- c("from_deck", "from_slot", "to_deck", "to_slot", "amt", "mix", "tip_n", "asp_set", "pipette", "comment")
  cmd_comp <- cmd_comp [,col_order]
  cmd_comp <- cmd_comp[order(cmd_comp$asp_set), ]

  return(cmd_comp)
}

#robot handling formatting
rhandcreate<-function(sollist, mastermix, samplelist){
  
  #part 1.1: sollist
  colnames(sollist) <- c("labware", "slot", "RequiredAmount")
  sollist$Name <- rownames(sollist)
  sollist$Catagory <- "STOCK"
  
  sollist$Unit <- "ul"
  rownames(sollist) <- NULL
  rhandler <- data.frame(
    "Catagory" <- sollist$Catagory,
    "Labware" <- sollist$labware,
    "Type" <- "1.5 Eppy",
    "Slot" <- sollist$slot,
    "Name" <- sollist$Name,
    "RequiredAmount" <- sollist["RequiredAmount"],
    "Unit" <- sollist$Unit
  )
  
  colnames(rhandler)<- c("Catagory", "Labware", "Type", "Slot", "Name", "RequiredAmount", "Unit")
  rhandler <- rhandler %>% mutate(Labware = paste0("labware_", Labware))
  
  #part 1.2 MMix tubes
  mmixtube <- data.frame(
    "Catagory" <- "EMPTY Tubes",
    "Labware"  <- 8,
    "Type" <- "-",
    "Slot" <- "-",
    "Name" <- "1.5 Eppy",
    "RequiredAmount" <- nrow(mastermix),
    "Unit" <- "tubes"
  )
  colnames(mmixtube) <- colnames(rhandler)
  mmixtube <- mmixtube %>% mutate(Labware = paste0("labware_", Labware))
  
  #part 1.3 samplelist
  sampler <- data.frame(
    "Catagory" <- "Sample",
    "Labware"  <- samplelist$Deck_Id,
    "Type" <- "-",
    "Slot" <- samplelist$Slot_Id,
    "Name" <- rownames(samplelist),
    "RequiredAmount" <- samplelist$ul,
    "Unit" <- "ul"
  )
  colnames(sampler) <- colnames(rhandler)
  sampler <- sampler %>% mutate(Labware = paste0("labware_", Labware))
  
  #list everything
  rhandler <- list(rhandler,
                   mmixtube,
                   sampler
                   )
  return(rhandler)
}

#MAIN -----------------
main <- function(file_path, filename = ""){
  #read excel------
  plateMap <- tryCatch({
    GetPlateMap(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Input file error - plateMap"
    }
    return(NA)
  })
  test<<- plateMap
  
  samplequestion <- tryCatch({
    Getsamplequestion(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Input file error - samplepipetting not clear"
    }
    return(NA)
  })
  
  ReactionNum <- tryCatch({
    GetreactionNum(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Input file error - Sample/Gene names not correct"
    }
    return(NA)
  })
  
  #Calc MM components
  Mastermix <- tryCatch({
    MMscheme(ReactionNum)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Mastermix could not be calculated"
    }
    return(NA)
  })
  
  #SolList assignment
  SolList <- tryCatch({
    SolList_fill(Mastermix)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Mastermix could not be calculated"
    }
    return(NA)
  })
  
  
  samplesdeck <- tryCatch({
    Samplecoll(file_path)
  },
  error = function(cond){
    if(errMessage == ""){
      errMessage <<- "Samples are misbehaving"
    }
    return(NA)
  })
  
  #sampledeck finishing
  samplesdeck$ul <- 100
  
  # 1. Make assignment for destination 1. mm
  #add location in posibility of eppy
  rows <- LETTERS[1:4]  # Rows A-D
  cols <- 1:6           # Columns 1-6
  slots <- c()
  for (row in rows) {
    slots <- c(slots, paste0(row, cols))
  }
  if(errMessage== ""){
    #Assign slot id to mastermix
    Mastermix$Deck_Id <- 8
    Mastermix$Slot_Id <- slots[seq_len(nrow(Mastermix))]
    
    #moving the slotid to front (easier read)
    Mastermix <- Mastermix %>% select(Slot_Id, everything())
    Mastermix <- Mastermix %>% select(Deck_Id, everything())
    
    deckmap <- data.frame(deck=c(1:12), fill = c("{empty}", "{empty}", "{empty}", "Tiprack_P1000", "Plate_A_96", "Tiprack_P50", "Epp_1_5_Stock", "Epp_1_5", "Epp_1_5_Sample", "Epp_1_5_Sample_spare", "Epp_1_5_stock_spare", "trash"))
    #cmd_list buildup
    cmd_mastermixprep<- cmd_mmprep(SolList, Mastermix)
    cmd_plateing <- cmd_plate(Mastermix, plateMap, cmd_mastermixprep)
    if (samplequestion == "Yes"){
      cmd_complete <- cmd_sample(samplesdeck, plateMap, cmd_plateing)
    }else{
      cmd_complete <- cmd_plateing
    }
    
    #Headers
    Sollisthead <- "> SolutionList"
    CMDlisthead <- "> CmdList"
    DECKlisthead <- "> DeckMap"
    
    #CMDlist output making
    rsollist <- SolList
    rsample <- samplesdeck
    if (samplequestion == "Yes"){
      SolList <- rbind.data.frame(SolList, samplesdeck)
    }else{
      print("sample plating is off")
    }
    
    #making Sollist and Deckmap same column size to bind them in a list
    dis <- replicate(length(SolList[,1]), "NA")
    x <- rownames(SolList)
    cmd_sollist <- cbind.data.frame(SolList[,c(1,2)], SolList[,3], x, dis, dis, dis, dis, dis, dis, stringsAsFactors=F)
    colnames(cmd_sollist) <- colnames(cmd_complete)
    
    dis <- replicate(length(deckmap[,1]), "NA")
    deckmap <- cbind.data.frame(deckmap, dis, dis, dis, dis, dis, dis, dis, dis)
    cmdList_output <<- list(Sollisthead, cmd_sollist,
                            CMDlisthead, cmd_complete,
                            DECKlisthead, deckmap
                            )
    #Robothandler/user commands ------
    Rhandlerp1 <- rhandcreate(rsollist, Mastermix, rsample)
    usercmd_output <<- Rhandlerp1
    
  }else{
    SolList <- errMessage
  }
  displaylist <- cmd_sollist[c(1,2,3,5)]
  colnames(displaylist) <- c("Deck location", "Slot", "What", "Required Amount")
  return(displaylist)
}

# #TROUBLESHOOTING---------
errMessage <<- ""
fpath <- "C:\\Users\\jornb\\NextCloud\\Jorn Brink\\01.Opentrons\\qPCR test case 96 wells\\Test scripts\\"
dataName <- "qPCR_template.xlsx"
dqs <- main(paste(fpath, dataName, sep="//"))