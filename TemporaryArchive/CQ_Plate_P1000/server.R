library(shiny)
library(readxl)
library(writexl)
library(dplyr)

options(stringsAsFactors = F)

#SERVER MAIN------------
shinyServer(function(input, output) {
  #defining directory-------
  #outputDir_cmdline <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\CQ_Plate"
  #outputDir_usrGuide <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\CQ_Plate"
  #inputTemplate <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\CQ_Plate\\CQ_InputTemplate.xlsx"
  #sourceDir <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\CQ_Plate\\ComboDrugs_source.R"
  
  outputDir_cmdline <- "/srv/shiny-server/files/Output_CmdList"
  outputDir_usrGuide <- "/srv/shiny-server/files/Output_UsrGuide"
  inputTemplate <- "/srv/shiny-server/ot2/CQ_Plate/CQ_InputTemplate.xlsx"
  sourceDir <- "/srv/shiny-server/ot2/CQ_Plate/ComboDrugs_source.R"
  
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
    infile = input$file
    if(is.null(infile)){return(NULL)}
    
    if(input$do==0){
      dis <- read_xlsx(infile$datapath, sheet=1,
                       range="B57:M64", col_names=F)
    }else{
      #rename files for safekeeping
      file_name <<- strsplit(infile$name, '.xl')[[1]][1]
      
      #update table view
      dis <- mainExec(infile$datapath)
      
      #savekeeping output files
      #command line
      cmdLine_name <- paste("CommandList_", new_name(), '.csv', sep='')
      write_dir <- paste(outputDir_cmdline, cmdLine_name, sep='/')
      write.csv(robotCommands, write_dir, row.names = FALSE)
      
      #user guide
      usrGuide_name <- paste("RobotHandler_", new_name(), '.xlsx', sep='')
      write_dir <- paste(outputDir_usrGuide, usrGuide_name, sep='/')
      write_xlsx(data.frame(robotHandler), write_dir, col_names=T)
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
  
  #Sample File Name--------
  output$tex <- renderText({new_name()})
  
  #Defining download buttons--------
  output$d_OT2 <- downloadHandler(
    filename = function(){paste("CommandList_", new_name(), '.csv', sep='')},
    content = function(file) {
      write.csv(robotCommands, file, row.names = FALSE)
    }
  )
  output$guide <- downloadHandler(
    filename = function(){paste("RobotHandler_", new_name(), '.xlsx', sep='')},
    content = function(file) {
      write_xlsx(data.frame(robotHandler), file, col_names=T)
    }
  )
  
  #download input template
  output$downloadTemplate <- downloadHandler(
    filename = "CQ_InputTemplate.xlsx",
    content = function(file) {
      file.copy(inputTemplate, file)
    }
  
  )
})
