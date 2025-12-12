#META #####
#J. Brink Flex Controller -- 2025/01/08
# > 96 well plate for qPCR
# > Credits for S.T. Tandar for major help and contibutions

#Library ------------
library(shiny)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

options(stringsAsFactors = F)

#Server Main functions-------------
shinyServer(function(input, output){
  
  #first defining directorys --------- change if in troubleshooting or Servermode (not yet implemented)
  outputDir_cmdline <- "/home/shiny-ot2/ShinyApps/outputs_cmdlist"
  outputDir_usrGuide <- "/home/shiny-ot2/ShinyApps/outputs_usrguide"
  inputTemplate <- "/home/shiny-ot2/ShinyApps/ot2/qPCR/qPCR_template.xlsx"
  sourceDir <- "/home/shiny-ot2/ShinyApps/ot2/qPCR/96wellsplatefunctionqPCR.R"
  
  #Troubleshooting (local machine paths)
  #outputDir_cmdline <- "C:\\Users\\jornb\\ownCloud\\Jorn Brink\\01. Opentrons\\qPCR test case 96 wells\\Testoutput"
  #outputDir_usrGuide <- "C:\\Users\\jornb\\ownCloud\\Jorn Brink\\01. Opentrons\\qPCR test case 96 wells\\Testoutput"
  #inputTemplate <- "C:\\Users\\jornb\\ownCloud\\Jorn Brink\\01. Opentrons\\qPCR test case 96 wells\\Rscript\\qPCR_template.xlsx"
  #sourceDir <- "C:\\Users\\jornb\\ownCloud\\Jorn Brink\\01. Opentrons\\qPCR test case 96 wells\\Rscript\\96wellsplatefunction.R"
  
  #Loading source
  source(sourceDir)
  
  #Initializing err message
  errMessage <<- ""
  
  #Obtain names---------
  new_name <- reactive({
    if(is.null(input$pmid)){pmid <- ''}else{pmid <- input$pmid}
    if(is.null(input$exp_name)){expName <- ''}else{expName <- input$exp_name}
    if(is.null(input$exp_num)){expNum <- ''}else{expNum <- input$exp_num}
    if(is.null(input$f_name)){fName <- ''}else{fName <- input$f_name}
    if(is.null(input$l_name)){lName <- ''}else{lName <- input$l_name}
    
    #paste
    res <- paste("PMID-", pmid, "_EXPID-", 
                 expName, "-", expNum, "_", lName, ".", fName, sep='')
    
    return(res)
  })
  #Confirming Upload File-----------
  contents <- reactive({
    infile = input$file
    if(is.null(infile)){return(NULL)}
    
    #perform if file is loaded
    if(input$do==0){
      dis <- read_xlsx(infile$datapath, sheet=1, range = "B57:M64", col_names=F)
      pmap_output <<- dis
    }else{
      #rename files for safekeeping
      file_name <<- strsplit(infile$name, '.xl')[[1]][1]
      
      #update table view; perform main function note that dis is display
      dis <- main(infile$datapath, infile$name)
      if(errMessage==""){
        #generate appropriate output files bit of a behind the scene for me cmdList_output is created by the << aka global good to know
        #Robot Commands---------------
        sel_colnames <- colnames(cmdList_output[[4]])
        
        #initiate new command line output
        new_cmdLineOutput <- c()
        
        #add first item
        nex_item <- replicate(length(sel_colnames), "")
        nex_item[1] <- cmdList_output[[1]]
        nex_item <- data.frame(t(nex_item))
        
        colnames(nex_item) <- sel_colnames
        new_cmdLineOutput <- rbind(new_cmdLineOutput, nex_item)
        new_cmdLineOutput[] <- lapply(new_cmdLineOutput, as.character)
        
        #add second item
        cur_item <- cmdList_output[[2]]
        cur_item[] <- lapply(cur_item, as.character)
        new_item <- c()
        for(j in c(1:length(cur_item[,1]))){
          cur_itemRow <- cur_item[j,]
          nex_item <- replicate(length(sel_colnames), "")
          nex_item[1:length(cur_itemRow)] <- cur_itemRow
          new_item <- rbind(new_item, nex_item)
        }
        colnames(new_item) <- c(sel_colnames)
        new_item[] <- lapply(new_item, as.character)
        new_cmdLineOutput <- rbind(new_cmdLineOutput, new_item)
        
        #add third item
        nex_item <- replicate(length(sel_colnames), "")
        nex_item[1] <- cmdList_output[[3]]
        nex_item <- data.frame(t(nex_item))
        colnames(nex_item) <- sel_colnames
        
        new_cmdLineOutput <- rbind(new_cmdLineOutput, nex_item)
        
        #add fourth item
        nex_item <- cmdList_output[[4]]
        nex_item[] <- lapply(nex_item, as.character)
        new_cmdLineOutput <- rbind.data.frame(new_cmdLineOutput, cmdList_output[[4]])
        new_cmdLineOutput <- apply(new_cmdLineOutput,2,as.character)
        
        #add fifth item
        nex_item <- replicate(length(sel_colnames), "")
        nex_item[1] <- cmdList_output[[5]]
        nex_item <- data.frame(t(nex_item))
        colnames(nex_item) <- sel_colnames
        
        new_cmdLineOutput <- rbind(new_cmdLineOutput, nex_item)
        
        #add sixth item
        nex <- cmdList_output[[6]]
        colnames(nex) <- colnames(new_cmdLineOutput)
        new_cmdLineOutput <- rbind.data.frame(new_cmdLineOutput, nex, stringsAsFactors = F)
        
        #place to global
        new_cmdLineOutput[new_cmdLineOutput == "NA"] <- ""
        
        new_cmdLineOutput <<- new_cmdLineOutput
        
        #User Commands---------------
        sel_colnames <- colnames(usercmd_output[[2]])
        
        #initiate new command line output
        new_userGuideOutput <- c()
        
        #add first item--------------
        nex_item <- replicate(length(sel_colnames), "")
        nex_item[1] <- '>>> Stocklist <<<'
        nex_item <- data.frame(t(nex_item))
        
        colnames(nex_item) <- sel_colnames
        new_userGuideOutput <- rbind(new_userGuideOutput, nex_item)
        
        #add second item
        first_item <- c()
        curItem <- usercmd_output[[1]]
        for(i in 1:nrow(curItem)){
          curRow <- curItem[i,]
          nex_item <- replicate(length(sel_colnames), "")
          nex_item[1:length(curRow)] <- curRow
          first_item <- rbind(first_item, nex_item)
        }
        colnames(first_item) <- sel_colnames
        new_userGuideOutput <- rbind.data.frame(new_userGuideOutput, first_item)
        
        
        #Third part
        nex_item <- replicate(length(sel_colnames), "")
        nex_item[1] <- '>>> Empty tubes <<<'
        nex_item <- data.frame(t(nex_item))
        
        colnames(nex_item) <- sel_colnames
        new_userGuideOutput <- rbind(new_userGuideOutput, nex_item)
        
        
        #add fourth item
        first_item <- c()
        curItem <- usercmd_output[[2]]
        for(i in 1:nrow(curItem)){
          curRow <- curItem[i,]
          nex_item <- replicate(length(sel_colnames), "")
          nex_item[1:length(curRow)] <- curRow
          first_item <- rbind(first_item, nex_item)
        }
        colnames(first_item) <- sel_colnames
        new_userGuideOutput <- rbind.data.frame(new_userGuideOutput, first_item)
        
        #fifth part
        nex_item <- replicate(length(sel_colnames), "")
        nex_item[1] <- '>>> Samples <<<'
        nex_item <- data.frame(t(nex_item))
        
        colnames(nex_item) <- sel_colnames
        new_userGuideOutput <- rbind(new_userGuideOutput, nex_item)
        
        #add 6th item
        first_item <- c()
        curItem <- usercmd_output[[3]]
        for(i in 1:nrow(curItem)){
          curRow <- curItem[i,]
          nex_item <- replicate(length(sel_colnames), "")
          nex_item[1:length(curRow)] <- curRow
          first_item <- rbind(first_item, nex_item)
        }
        colnames(first_item) <- sel_colnames
        new_userGuideOutput <- rbind.data.frame(new_userGuideOutput, first_item)
        
        #lastpart of Rhandler
        FLEXdeck <- cmdList_output[[6]]
        FLEXdeck <- FLEXdeck[c(2)]
        FLEXdeck <- unlist(FLEXdeck)
        matrixversion <- matrix(FLEXdeck, nrow=4, ncol=3, byrow = TRUE)
        res3 <- as.data.frame(matrixversion)
        colnames(res3) <- c("[,1]", "[,2]", "[,3]")
        
        #Finally
        nex_item <- replicate(length(sel_colnames), "")
        nex_item[1] <- '>>> FLEX DECK <<<'
        nex_item <- data.frame(t(nex_item))
        
        colnames(nex_item) <- sel_colnames
        titlenex <- nex_item
        
        first_item <- c()
        curItem <- res3
        for(i in 1:nrow(curItem)){
          curRow <- curItem[i,]
          nex_item <- replicate(length(sel_colnames), "")
          nex_item[1:length(curRow)] <- curRow
          first_item <- rbind(first_item, nex_item)
        }
        
        colnames(first_item) <- sel_colnames
        
        new_userGuideOutput <- rbind.data.frame(first_item, new_userGuideOutput)
        new_userGuideOutput <- rbind.data.frame(nex_item, new_userGuideOutput)
        
        new_userGuideOutput <- apply(new_userGuideOutput,2,as.character) %>% data.frame()
        
        new_userGuideOutput <<- new_userGuideOutput 
        
        #savekeeping output files
        #command line
        cmdLine_name <- paste("CommandList_", new_name(), '.csv', sep='')
        write_dir <- paste(outputDir_cmdline, cmdLine_name, sep='/')
        write.csv(new_cmdLineOutput, write_dir, row.names = FALSE)
        
        #user guide
        usrGuide_name <- paste("RobotHandler_", new_name(), '.xlsx', sep='')
        write_dir <- paste(outputDir_usrGuide, usrGuide_name, sep='/')
        write_xlsx(new_userGuideOutput, write_dir, col_names=T)
      }
    }
    return(dis)
  })
  
  output$tab <- renderTable({contents()})
  
  #Enabling download button-------
  output$downloadData <- renderUI({
    req(input$do, contents())
    downloadButton("d_OT2", "Download Robot Commands")
  })
  output$downloadData2 <- renderUI({
    req(input$do, contents())
    downloadButton("guide", "Download Robot Setup Guide")
  })
  output$downloadData3 <- renderUI({
    req(input$do, contents())
    downloadButton("plateMap", "Download Plate Map")
  })
  
  #Sample File Name--------
  output$tex <- renderText({new_name()})
  
  #Defining download buttons--------
  output$d_OT2 <- downloadHandler(
    filename = function(){paste("CommandList_", new_name(), '.csv', sep='')},
    content = function(file) {
      write.csv(new_cmdLineOutput, file, row.names = FALSE)
    }
  )
  output$guide <- downloadHandler(
    filename = function(){paste("RobotHandler_", new_name(), '.xlsx', sep='')},
    content = function(file) {
      write_xlsx(new_userGuideOutput, file, col_names=T)
      #write.csv(new_userGuideOutput, file, row.names = FALSE)
    }
  )
  
  #download input template
  output$downloadTemplate <- downloadHandler(
    filename = "MV_InputTemplate.xlsx",
    content = function(file) {
      file.copy(inputTemplate, file)
    }
  )
})