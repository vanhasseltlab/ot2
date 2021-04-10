library(shiny)
#.libPaths(c("/home/sebastian/R/x86_64-pc-linux-gnu-library/3.4"))
library(readxl)
library(writexl)
library(dplyr)
library(chron)
library(reshape2)
library(ggplot2)
options(stringsAsFactors = F)

errMessage <- ""
shinyServer(function(input, output) {
    #### PREPARATION-------------
    #set directories, take source analyzer
    mainwd <- "/srv/shiny-server/files"
    sourcewd <- "/srv/shiny-server/ot2/PlateAnalysis/GrowthCurve/srcPlateAnalyzer.R"
    templatewd <- "/srv/shiny-server/ot2/PlateAnalysis/GrowthCurve/ControlMap.csv"
    
    #for troubleshooting
    #mainwd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\PlateAnalysis\\GrowthCurve"
    #sourcewd <- paste(mainwd, "/srcPlateAnalyzer.R", sep='')
    #templatewd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\ot2\\PlateAnalysis\\GrowthCurve\\ControlMap.csv"
    
    if(!("Analysis" %in% list.files(mainwd))){
        setwd(mainwd)
        dir.create("Analysis", recursive=T)
    }
    mainwd <- paste(mainwd, "/Analysis", sep='')
    
    source(sourcewd)
    
    #### MAIN----------
    contents <- reactive({
        infile = input$files
        if(is.null(infile)){return(NULL)}
        
        fileNames <- c(input$pMap$name, input$files$name)
        controlNames <- c(input$control_map$name, input$control_meas$name)
        
        if(input$do==0){
            return(fileNames)
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
            
            #create new directory for control map and measurements
            if(!is.null(input$control_map) | !is.null(input$control_meas)){
                controlwd <- paste(mainwd, "controls", sep="/")
                dir.create(controlwd, recursive=T)
            }
            
            
            ########
            #COPY FILES TO MAIN DIRECTORY
                #copying plate map
                    
            #copy and rename
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
            
            #plate map - control map
            if(!is.null(input$control_map)){
                pmaps <- input$control_map$datapath
                file.copy(input$control_map$datapath, controlwd)
                if('csv' %in% input$control_map$name){
                    newName <- paste(controlwd, "/plateMap.csv", sep='')
                    oldName <- paste(controlwd, "/0.csv", sep='')
                    file.rename(oldName, newName)
                }else{
                    newName <- paste(controlwd, "/plateMap.xlsx", sep='')
                    oldName <- paste(controlwd, "/0.xlsx", sep='')
                    file.rename(oldName, newName)
                }
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
            
            #measurement files - controls
            if(!is.null(input$control_meas)){
                meas_path_control <- paste(controlwd, "files", sep="/")
                dir.create(meas_path_control)
                controlNames <- controlNames[2:length(controlNames)]
                for(i in c(1:length(controlNames))){
                    file.copy(input$control_meas$datapath[i], meas_path_control)
                    print(input$control_meas$datapath[i])
                    #renaming
                    oldName <- paste(meas_path_control, "/", toString(i-1), ".csv", sep='')
                    newName <- paste(meas_path_control, "/", controlNames[i], sep='')
                    file.rename(oldName, newName)
                }
            }
            
            
            ## MAIN ##
            if(!is.null(input$control_meas) & !is.null(input$control_map) & input$separate_control){
                grandRes <- mainFun(paste(mainwd, "/plateMap.xlsx", sep=""), meas_path, 
                                    control_selection=input$control_selection,
                                    paste(controlwd, "/plateMap.xlsx", sep=""), meas_path_control, separate_control=input$separate_control)
            }else if(input$control_selection==5){
                grandRes <- mainFun(paste(mainwd, "/plateMap.xlsx", sep=""), meas_path, 
                                    control_selection=input$control_selection, 
                                    control_map_address=paste(controlwd, "/0.csv", sep=""))
            }else{
                #if control plate not provided
                grandRes <- mainFun(paste(mainwd, "/plateMap.xlsx", sep=""), meas_path, 
                                    control_selection=input$control_selection)
            }
            
            
            return(grandRes)
        }
    })
    
    
    #OUTPUT TABLE----------
    output$tab <- renderTable({contents()})
    
    #CONTROL SELECTION UI------------
    output$ctrlSelectionUI <- renderUI({
        if(input$separate_control){
            radioButtons("control_selection", "Control Selection",
                         c("No control" = 1, 
                           "Medium control" = 2,
                           "Drug-Medium control" = 3,
                           "Drug-Medium-Concentration control" = 4))
        }else{
            radioButtons("control_selection", "Control Selection",
                         c("No control" = 1, 
                           "Medium control" = 2,
                           "Drug-Medium control" = 3,
                           "Drug-Medium-Concentration control" = 4,
                           "Coordinate/Custom control selection (on-plate)" = 5))
        }
    })
    
    #CONTROL UPLOAD UI------------
    output$control_map_upload <- renderUI({
        if(input$separate_control){
            fileInput("control_map", "Upload Control Map", accept=".xlsx")
        }else if(input$control_selection==5){
            fileInput("control_map", "Upload Control Map", accept=".csv")
        }
    })
    
    
    #upload for control measurements
    output$control_meas_upload <- renderUI({
        if(input$separate_control){
            fileInput('control_meas', 'Upload control measurements', accept=".csv", multiple=T)
        }
    })
    
    #download coordinate control map
    output$coord_map_download <- renderUI({
        if(input$control_selection==5){
            downloadButton('download_coord_control', 'Download Coordinate Control Template')
        }
    })
    
    output$download_coord_control <- downloadHandler(
        filename = "CoordinateControlTemplate.csv",
        content = function(file) {
            file.copy(templatewd, file)
        }
    )
    
    #RESULT DOWNLOAD BUTTONS------------
    #Pre-processed data
    output$download_prcNM <- renderUI({
        req(input$do, contents())
        downloadButton('download_preprocessed', 'Download Preprocessed Data')
    })
    
    output$download_preprocessed <- downloadHandler(
        filename = paste('Preprocessed_', input$folderName, '.csv', sep=''),
        content = function(file) {
            write.csv(prc_NM, file, row.names=F)
        }
    )
    
    #Raw control measurement data
    output$download_controlNM <- renderUI({
        req(input$do, contents())
        downloadButton('download_control', 'Download Raw Control Measurement Data')
    })
    
    output$download_control <- downloadHandler(
        filename = paste('Controls_', input$folderName, '.csv', sep=''),
        content = function(file) {
            write.csv(controlData_NM, file, row.names=F)
        }
    )
    
    #PRE-PROCESSOR DOWNLOAD----------
    output$downloadScript <- downloadHandler(
        filename = "PlatePreProcessor_v2021-03-12.R",
        content = function(file){
            file.copy(sourcewd, file)
        }
    )
    
    #ERROR MESSAGE----------
    err_report <- reactiveValues()
    observeEvent(input$do,{
        req(input$do, contents())
        err_report$message <- errMessage
    })
            
    output$err_message <- renderText({err_report$message})
})