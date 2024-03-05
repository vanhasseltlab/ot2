library(shiny)
library(readxl)
library(writexl)
library(dplyr)
library(chron)
library(reshape2)
library(ggplot2)
library(shinyjs)
options(stringsAsFactors = F)

errMessage <- ""
shinyServer(function(input, output) {
  disable("plot_tab")
  hide('download_plot')
  
  #### PREPARATION-------------
  #set directories, take source analyzer
  mainwd <- "/home/shiny-ot2/ShinyApps/ot2/files"
  sourcewd <- "/home/shiny-ot2/ShinyApps/ot2/PlateAnalysis/GrowthCurve_v2/srcPlateAnalyzer.R"
  templatewd <- "/home/shiny-ot2/ShinyApps/ot2/PlateAnalysis/GrowthCurve_v2/ControlMap.csv"
  
  #for troubleshooting
  # mainwd <- "C:\\Users\\sebas\\Documents\\GitHub\\ot2\\PlateAnalysis\\GrowthCurve_v2"
  # sourcewd <- paste0(mainwd, "/srcPlateAnalyzer.R")

  if(!("Analysis" %in% list.files(mainwd))){
    setwd(mainwd)
    dir.create("Analysis", recursive=T)
  }
  mainwd <- paste(mainwd, "/Analysis", sep='')
  
  source(sourcewd)
  
  #### MAIN----------
  contents <- reactive({
    infile = input$files
    if(is.null(infile)){return(list(NULL, NULL))}
    
    fileNames <- c(input$pMap$name, input$files$name)
    controlNames <- c(input$control_map$name, input$control_meas$name)
    
    if(input$do==0){
      return(list(fileNames, NULL))
    }else{
      #PROCESS
      ## File Safekeeping
      #create new directory
      folderName <- input$folderName
      nex <- F
      i <- 1
      while(folderName %in% list.files(mainwd)){
        if(nex){
          folderName <- substring(folderName, 1, (nchar(folderName)-(nchar(toString(i-1))+1)) )
        }
        folderName <- paste(folderName, "_", toString(i), sep='')
        
        i <- i+1
        nex <- T
      }
      
      mainwd <- paste(mainwd, folderName, sep="/")
      dir.create(mainwd, recursive=T)
      
      ########
      #COPY FILES TO MAIN DIRECTORY
      #plate map - main measurement
      pmaps <- input$pmap$datapath
      file.copy(input$pMap$datapath, mainwd)
      if('csv' %in% input$pMap$name){
        newName <- paste(mainwd, "/plateMap.csv", sep='')
        oldName <- paste(mainwd, "/0.csv", sep='')
        file.rename(oldName, newName)
      }else{
        newName <- paste(mainwd, "/plateMap.xlsx", sep='')
        oldName <- paste(mainwd, "/0.xlsx", sep='')
        file.rename(oldName, newName)
      }
      
      #measurement files - main measurement
      meas_path <- paste(mainwd, "files", sep="/")
      dir.create(meas_path)
      fileNames <- fileNames[2:length(fileNames)]
      for(i in c(1:length(fileNames))){
        file.copy(input$files$datapath[i], meas_path)
        print(input$files$datapath[i])
        #renaming
        oldName <- paste(meas_path, "/", toString(i-1), ".csv", sep='')
        newName <- paste(meas_path, "/", fileNames[i], sep='')
        file.rename(oldName, newName)
      }
      
      ## MAIN ##
      grandRes <- mainFun(paste(mainwd, "/plateMap.xlsx", sep=""), meas_path, 
                          control_selection=0)
      
      return(grandRes)
    }
  })
  
  #OUTPUT TABLE----------
  output$tab <- renderTable({contents()[[1]]})
  
  #OUTPUT PLOT------------
  observeEvent(input$do, {
    output$timecourse_plot <- renderPlot(contents()[[2]])
    disable("do")
    show('download_plot')
  })
  
  #RESULT DOWNLOAD BUTTONS------------
  #Pre-processed data
  output$download_prcNM <- renderUI({
    req(input$do, contents()[[1]])
    downloadButton('download_preprocessed', 'Download Preprocessed Data')
  })
  
  output$download_preprocessed <- downloadHandler(
    filename = paste('Preprocessed_', input$folderName, '.csv', sep=''),
    content = function(file) {
      write.csv(prc_NM, file, row.names=F)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename=function(){
      paste0("TimeCoursePlot_", input$folderName, ".png")
    },
    content = function(file) {
      ggsave(file, plot = contents()[[2]], device = 'png',
             width=400, height=300, units='mm')
    }
  )
  
  #ERROR MESSAGE----------
  err_report <- reactiveValues()
  observeEvent(input$do,{
    req(input$do, contents()[[1]])
    err_report$message <- errMessage
  })
  
  output$err_message <- renderText({err_report$message})
})