library(shiny)
library(readxl)
library(writexl)
library(dplyr)
library(chron)
library(reshape2)
options(stringsAsFactors = F)

errMessage <- ""
shinyServer(function(input, output) {
    #### PREPARATION-------------
    #set directories, take source analyzer
    mainwd <- "/srv/shiny-server/files"
    sourcewd <- "/srv/shiny-server/ot2/PlateAnalysis/Checkerboard_DataReader/coreExec.R"
    #templatewd <- "/srv/shiny-server/ot2/PlateAnalysis/GrowthCurve/ControlMap.csv"
    
    #for troubleshooting
    #sourcewd <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\Checkerboard_DataReader\\coreExec.R"
    #mainwd <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\Checkerboard_DataReader"
    #templatewd <- paste0(mainwd, "/20220309_MK_E05_PMAPID.xlsx")
    
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
            #copy and rename
            #plate map - main measurement
            pmaps <- input$pmap$datapath
            file.copy(input$pMap$datapath, mainwd)
            
            #measurement files - main measurement
            for(i in c(1:length(fileNames))){
                file.copy(input$files$datapath[i], mainwd)
            }
            
            ## MAIN ##
            grandRes <- mainExec(mainwd, first_measurement=input$first_measurement,
                                 input$control_selection)
            prc_NM <<- grandRes
            return(grandRes)
        }
    })
    
    
    #OUTPUT TABLE----------
    output$tab <- renderTable({contents()})
    
    #CONTROL SELECTION UI------------
    output$ctrlSelectionUI <- renderUI({
        radioButtons("control_selection", "Control Selection",
                     c("No control" = 1,
                       "Medium control" = 2))
    })
    
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
    
    #ERROR MESSAGE----------
    err_report <- reactiveValues()
    observeEvent(input$do,{
        req(input$do, contents())
        err_report$message <- errMessage
    })
            
    output$err_message <- renderText({err_report$message})
})