library(shiny)
shinyUI(
    pageWithSidebar(
        headerPanel("Growth Curve Plot for 96-Well Plate (old version)"),
        
        sidebarPanel(
            selectInput("reader_type", "Select plate reader",
                        list("FluostarOmega + Robot Arm" = 2,
                             "FluostarOmega (no Robot Arm)" = 1)),
            fileInput("files", "Upload Measurement Data", accept=".csv",
                      multiple=T),
            fileInput("pMap", "Upload Plate Map", accept=".xlsx"),
            textInput("folderName", "Experiment Name", value='defaultFolder'),
            textInput("time", "Timepoint of first measurement", value="00:00:00"),
            selectInput("controlOpt", 'Control Options',
                        list("No Blank" = 0,
                             "Single Blank" = 1,
                             "One per-drug" = 2,
                             "One per-drug concentration" = 3)),
            checkboxGroupInput("plotOptions", "Plotting Options",
                               c("Logarithmic scale" = 'log')),
            actionButton("do", "Confirm uploaded file and save"),
            downloadButton("downloadScript", "Download Processor Script")
        ),
        
        mainPanel(
            uiOutput("plot_download"),
            uiOutput("dataset_download"),
            uiOutput("raw_dataset_download"),
            tableOutput('tab'),
            uiOutput('plotting')
        )
))
