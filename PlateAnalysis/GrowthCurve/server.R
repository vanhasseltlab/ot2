library(shiny)
.libPaths(c("/home/sebastian/R/x86_64-pc-linux-gnu-library/3.4"))
library(ggplot2)
library(chron)
library(reshape2)
library(xlsx)
options(stringsAsFactors = F)

errMessage <<- "SUCCESS"
shinyServer(function(input, output) {
    #### PREPARATION-------------
    #set directories, take source analyzer
    mainwd <- "/srv/shiny-server/files"
    sourcewd <- "/srv/shiny-server/ot2/PlateAnalysis/GrowthCurve/analyzer.R"
    #mainwd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\#OT2_Main\\PlateAnalysis\\GrowthCurve"
    #sourcewd <- paste(mainwd, "\\analyzer.R", sep='') 
    
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
        
        if(input$do==0){
            #perform if file is loaded; but action button is not yet pressed
            fileNames <- input$files$name
            
            return(fileNames)
        }else{
            #PROCESS
            fileNames <- input$files$name
            
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
            
            #copy files to main directory
            for(i in c(1:length(fileNames))){
                file.copy(input$files$datapath[input$files$name==fileNames[i]], mainwd)
                
                #renaming
                oldName <- paste(mainwd, "/", toString(i-1), ".csv", sep='')
                newName <- paste(mainwd, "/", fileNames[i], sep='')
                file.rename(oldName, newName)
            }
            
            ## MAIN ##
            grandRes <- tryCatch({
                main(mainwd, input$time, input$reader_type, as.numeric(input$controlOpt))
            },
            error=function(cond){
                return("NULL")
            })
            checkPoint3 <<- grandRes
            #data for plotting; switch blanks from grand res
            if(input$controlOpt == 3){
                plotRes <- grandRes[!grepl("blank", grandRes$Inoculum),]
                grandRes$Inoculum <- gsub("conc_blank","USED blank for each drug+medium+concentration", grandRes$Inoculum, ignore.case=T)
            }else if(input$controlOpt==1){
                plotRes <- grandRes[!grepl("absolute_blank", grandRes$Inoculum),]
                grandRes$Inoculum <- gsub("absolute_blank","USED blank for all", grandRes$Inoculum, ignore.case=T)
            }else if(input$controlOpt==2){
                plotRes <- grandRes[!(grepl("absolute_blank", grandRes$Inoculum) | grepl("drug_blank", grandRes$Inoculum)),]
                grandRes$Inoculum <- gsub("conc_blank","USED blank for each drug+medium", grandRes$Inoculum, ignore.case=T)
            }else{
                plotRes <- grandRes
                grandRes$Inoculum[grepl("blank", grandRes$Inoculum)] <- ""
            }
            
            variable <- c()
            for(i in c(1:length(plotRes[,1]))){
                variable <- c(variable,
                              paste(plotRes[i,c(2:5)], collapse='_'))
            }
            plotRes <<- cbind.data.frame(variable, plotRes)
            
            grandRes <<- grandRes
            return(grandRes)
        }
    })
    #creating the table
    output$tab <- renderTable({contents()})
    
    #CREATE PLOT------
    plotData <- reactiveValues()
    observeEvent(input$do,{
        req(input$do, contents())
        plotData$plot_m <- ggplot(data=plotRes, aes(x=time, y=Absorbance))+
            geom_point()+geom_line()+theme_bw()+
            facet_wrap(~variable)
        
        
        if("log" %in% input$plotOptions){
            plotData$plot_m <- plotData$plot_m + 
                scale_y_continuous(trans='log10')
        }
        })
    
    output$plot <- renderPlot({plotData$plot_m})
    
    #show plot only after action button pushed
    output$plotting <- renderUI({
        req(input$do, contents())
        plotOutput("plot")
    })
    
    #download button for the image
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
    
    #download for dataset
    output$dataset_download <- renderUI({
        req(input$do, contents())
        downloadButton('downloadDataset', 'Download Preprocessed Data')
    })
    
    output$downloadDataset <- downloadHandler(
        filename = paste('PreprocessedData_', input$folderName, '.csv', sep=''),
        content = function(file) {
            write.csv(grandRes, file, row.names = FALSE)
        }
    )
})
