library(shiny)
.libPaths(c("/home/sebastian/R/x86_64-pc-linux-gnu-library/3.4"))
library(ggplot2)
library(chron)
library(reshape2)
library(xlsx)
options(stringsAsFactors = F)

errMessage <<- "SUCCESS"
shinyServer(function(input, output) {
    #set directories, take source analyzer
    mainwd <- "/srv/shiny-server/files"
    sourcewd <- "/srv/shiny-server/ot2/PlateAnalysis/GrowthCurve/analyzer.R"
    #mainwd <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\#OT2_Main\\PlateAnalysis\\GrowthCurve"
    #sourcewd <- paste(mainwd, "\\analyzer.R", sep='') 
    
    if(!("Analysis" %in% list.files(mainwd))){
        setwd(mainwd)
        dir.create("Analysis", recursive=T)
    }
    #mainwd <- paste(mainwd, "\\Analysis", sep='')
    mainwd <- paste(mainwd, "/Analysis", sep='')
    
    source(sourcewd)
    
    #copy all files
    contents <- reactive({
        infile = input$files
        if(is.null(infile)){return(NULL)}
        
        if(input$do==0){
            #perform if file is loaded
            fileNames <- input$files$name
            
            return(fileNames)
        }else{
            fileNames <- input$files$name
            
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
            
            #perform main operation
            grandRes <<- tryCatch({
                main(mainwd, input$time, input$reader_type)
            },
            error=function(cond){
                return("NULL")
            })
            
            lower_bound_axis <<- round(min(grandRes$minVal), 2)
            upper_bound_axis <<- round(max(grandRes$maxVal), 1)
            
            #parse checkbox inputs
            plotOptions <<- as.vector(input$plotOptions)
            return(errMessage)
        }
    })
    #creating the table
    output$tab <- renderTable({contents()})
    
    #CREATE PLOT------
    plotData <- reactiveValues()
    observeEvent(input$do,{
        req(input$do, contents())
        plotData$plot_m <- ggplot(data=grandRes, aes(x=time, y=Absorbance))+
            geom_point()+geom_line()+theme_bw()+
            facet_wrap(~variable)
        
        if(!("auto" %in% plotOptions)){
            lower_bound_axis <<- input$lower_bound
            upper_bound_axis <<- input$upper_bound
        }
        
        if("log" %in% plotOptions){
            plotData$plot_m <- plotData$plot_m + 
                scale_y_continuous(trans='log10', limits=c(lower_bound_axis, upper_bound_axis))
        }
        if("errorBars" %in% plotOptions){
            plotData$plot_m <- plotData$plot_m + 
                geom_errorbar(aes(ymin=minVal, ymax=maxVal), width=0.5)
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
