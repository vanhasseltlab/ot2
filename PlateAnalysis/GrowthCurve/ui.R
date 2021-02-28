library(shiny)
shinyUI(
    pageWithSidebar(
        headerPanel("Growth Curve for 96-Well Plate"),
        
        sidebarPanel(
            #file inputs
            fileInput("files", "Upload Measurement Data", accept=".csv",
                      multiple=T),
            fileInput("pMap", "Upload Plate Map", accept=".xlsx"),
            
            #control selection
            radioButtons("control_selection", "",
                         c("No control" = 1, 
                           "Use control input" = 2)),
            uiOutput("control_upload"),
            uiOutput("control_download"),
            
            #names
            textInput("folderName", "Experiment Name", value='defaultFolder'),
            
            #plot options
            radioButtons("plotOptions", "Plotting Options",
                               c("Linear scale" = 'lin',
                                 "Logarithmic scale" = 'log')),
            
            #action buttons
            actionButton("do", "Confirm uploaded file and save"),
            
            downloadButton("downloadScript", "Download Processor Script"),
            
            #downloads
            uiOutput("plot_download"),
            uiOutput('download_raw_matrix'),
            uiOutput('download_raw_NM'),
            uiOutput('download_prcNM')
        ),
        
        mainPanel(
            uiOutput('plotting'),
            tableOutput('tab')
        )
))
