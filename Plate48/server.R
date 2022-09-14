#META #####
#S.T.Tandar_384-well plate operator -- 2021/11/17

library(shiny)
library(readxl)
library(writexl)
library(dplyr)

options(stringsAsFactors = F)

#SERVER MAIN------------
shinyServer(function(input, output) {
  #defining directory-------
  outputDir_cmdline <- "/home/shiny-ot2/ShinyApps/outputs_cmdlist"
  outputDir_usrGuide <- "/home/shiny-ot2/ShinyApps/outputs_usrguide"
  inputTemplate <- "/home/shiny-ot2/ShinyApps/ot2/Plate48/48Well_InputTemplate.xlsx" 
  sourceDir <- "/home/shiny-ot2/ShinyApps/ot2/Plate48/48WellParse.R" 
  
  #outputDir_cmdline <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\48WellPlate"
  #outputDir_usrGuide <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\48WellPlate"
  #inputTemplate <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\48WellPlate\\48Well_InputTemplate.xlsx" 
  #sourceDir <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\48WellPlate\\48WellParse.R" 
  
  #loading functions--------
  source(sourceDir)
  
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
    infile <- input$file
    if(is.null(infile)){return(NULL)}
    
    if(input$do==0){
      # for show
      res <- read_xlsx(infile$datapath, sheet=2, range = "A38:I44")
    }else{
      res <- mainExec(infile$datapath)
      cmdList_output <<- res[[1]]
      usrGuide_output <<- res[[2]]
      
      #savekeeping output files
      #command line
      cmdLine_name <- paste("CommandList_", new_name(), '.csv', sep='')
      write_dir <- paste(outputDir_cmdline, cmdLine_name, sep='/')
      write.csv(cmdList_output, write_dir, row.names = FALSE)
      
      #user guide
      usrGuide_name <- paste("RobotHandler_", new_name(), '.xlsx', sep='')
      write_dir <- paste(outputDir_usrGuide, usrGuide_name, sep='/')
      write_xlsx(usrGuide_output, write_dir, col_names=T)
      
      # for show in table
      res <- usrGuide_output
    }
    
    return(res)
  })
  
  #DOWNLOAD INPUT TEMPLATE-------
  output$downloadTemplate <- downloadHandler(
    filename = "48Well_InputTemplate.xlsx",
    content = function(file) {
      file.copy(inputTemplate, file)
    }
  )
  
  
  #Sample File Name--------
  output$tex <- renderText({new_name()})
  
  #TABLE FOR SHOW---------
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
  
  #Defining download buttons--------
  output$d_OT2 <- downloadHandler(
    filename = function(){paste("CommandList_", new_name(), '.csv', sep='')},
    content = function(file) {
      write.csv(cmdList_output, file, row.names = FALSE)
    }
  )
  output$guide <- downloadHandler(
    filename = function(){paste("RobotHandler_", new_name(), '.xlsx', sep='')},
    content = function(file) {
      write_xlsx(usrGuide_output, file)
    }
  )
})