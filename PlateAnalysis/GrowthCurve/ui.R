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
            radioButtons("control_selection", "Control Selection",
                         c("No control" = 1, 
                           "Medium control" = 2,
                           "Drug-Medium control" = 3,
                           "Drug-Medium-Concentration control" = 4,
                           "User-defined control" = 5)),
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
            uiOutput('download_prcNM'),
            uiOutput('download_prcNM_avg')
        ),
        
        mainPanel(
            uiOutput('plotting'),
            tableOutput('tab')
        )
))
