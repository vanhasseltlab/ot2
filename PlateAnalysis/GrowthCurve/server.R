library(shiny)
.libPaths(c("/home/sebastian/R/x86_64-pc-linux-gnu-library/3.4"))
library(xlsx)
library(dplyr)
library(chron)
library(reshape2)
library(ggplot2)
options(stringsAsFactors = F)

shinyServer(function(input, output) {
    #### PREPARATION-------------
    #set directories, take source analyzer
    mainwd <- "/srv/shiny-server/files"
    sourcewd <- "/srv/shiny-server/ot2/PlateAnalysis/GrowthCurve/srcPlateAnalyzer.R"
    templatewd <- "/srv/shiny-server/ot2/PlateAnalysis/GrowthCurve/ControlMap.csv"
    
    #for troubleshooting
    #mainwd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator\\GrowthCurve"
    #sourcewd <- paste(mainwd, "/srcPlateAnalyzer.R", sep='')
    #templatewd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator\\GrowthCurve\\ControlMap.csv"
    
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
            
            ########
            #COPY FILES TO MAIN DIRECTORY
                #copying plate map
                    
            #copy and rename
            #plate map
            pmaps <- input$pmap$datapath
            file.copy(input$pMap$datapath, mainwd)
            if('csv' %in% input$pMap$name){
                newName <- paste(mainwd, "/plateMap.csv", sep='')
                oldName <- paste(mainwd, "/0.csv", sep='')
            }else{
                newName <- paste(mainwd, "/plateMap.xlsx", sep='')
                oldName <- paste(mainwd, "/0.xlsx", sep='')
                file.rename(oldName, newName)
            }
            
            #measurement files
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
                                control_selection=input$control_selection, control_map=input$control_map$datapath)
            
            return(grandRes)
        }
    })
    
    #OUTPUT TABLE----------
    output$tab <- renderTable({contents()})
    
    #CREATE PLOT------
    plotData <- reactiveValues()
    observeEvent(input$do,{
        req(input$do, input$plotOptions, contents())
        #create plot
        if(input$plotOptions=='log'){
            plotData$plot_m <- plt + scale_y_continuous(trans='log10')+
                ylab("log10(Absorbance) / (a.u.)")
        }else{
            plotData$plot_m <- plt + ylab("Absorbance / (a.u.)")
        }
    })
    
    output$plot <- renderPlot({plotData$plot_m})
    
    #show plot only after action button pushed
    output$plotting <- renderUI({
        req(input$do, contents())
        plotOutput("plot")
    })
    
    #CONTROL UPLOAD UI------------
    output$control_upload <- renderUI({
        req(input$control_selection)
        if(input$control_selection==5){
            fileInput("control_map", "Upload Control Map", accept=".csv")
        }
    })
    
    #download for control map template
    output$control_download <- renderUI({
        req(input$control_selection)
        if(input$control_selection==5){
            downloadButton('downloadControlMap', 'Download Control Map Template')
        }
        
    })
    
    output$downloadControlMap <- downloadHandler(
        filename = 'ControlMap.csv',
        content = function(file) {
            file.copy(templatewd, file)
        }
    )
    
    #RESULT DOWNLOAD BUTTONS------------
    #Raw Data; long-format
    output$download_raw_NM <- renderUI({
        req(input$do, contents())
        downloadButton('download_rawNM', 'Download Raw Data (long format)')
    })
    
    output$download_rawNM <- downloadHandler(
        filename = paste('RawData_', input$folderName, '_longFormat.csv', sep=''),
        content = function(file) {
            write.csv(rawData_NM, file, row.names=F)
        }
    )
    
    #Raw Data; Matrix format
    output$download_raw_matrix <- renderUI({
        req(input$do, contents())
        downloadButton('download_rawMat', 'Download Raw Data (matrix format)')
    })
    
    output$download_rawMat <- downloadHandler(
        filename = paste('RawData_', input$folderName, '_longFormat.csv', sep=''),
        content = function(file) {
            write.csv(rawData_matrix, file, row.names=F)
        }
    )
    
    #Pre-processed data
    output$download_prcNM <- renderUI({
        req(input$do, contents())
        downloadButton('download_preprocessed', 'Download Preprocessed Data')
    })
    
    output$download_preprocessed <- downloadHandler(
        filename = paste('Preprocessed_', input$folderName, '.csv', sep=''),
        content = function(file) {
            write.csv(proc_NM, file, row.names=F)
        }
    )
    
    #Pre-processed and averaged data
    output$download_prcNM_avg <- renderUI({
        req(input$do, contents())
        downloadButton('download_preprocessed_avg', 'Download Preprocessed Data (averaged)')
    })
    
    output$download_preprocessed_avg <- downloadHandler(
        filename = paste('Preprocessed_Averaged_', input$folderName, '.csv', sep=''),
        content = function(file) {
            write.csv(proc_NM_aeraged, file, row.names=F)
        }
    )
    
    #IMAGE DOWNLOAD BUTTON-----------
    output$plot_download <- renderUI({
        req(input$do, contents())
        downloadButton('downloadPlot', 'Download Plot')
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$folderName, '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = plotData$plot_m, device = "png")
        }
    )
    
    #PRE-PROCESSIR DOWNLOAD----------
    output$downloadScript <- downloadHandler(
        filename = "PlatePreProcessor_v2021-02-28.R",
        content = function(file){
            file.copy(sourcewd, file)
        }
    )
})
