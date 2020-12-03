library(shiny)
shinyUI(
    pageWithSidebar(
        headerPanel("Growth Curve Plot for 96-Well Plate"),
        
        sidebarPanel(
            selectInput("reader_type", "Select plate reader",
                        list("FluostarOmega without Robot Arm" = 1,
                             "FluostarOmega with Robot Arm" = 2)),
            fileInput("files", "Upload Measurement Data and Plate Map", accept=c(".csv", ".xlsx"), multiple=T),
            textInput("folderName", "Experiment Name", value='defaultFolder'),
            textInput("time", "Timepoint of first measurement", value="00:00:00"),
            selectInput("controlOpt", 'Control Options',
                        list("No Blank" = 0,
                             "Single Blank" = 1,
                             "One per-drug" = 2,
                             "One per-drug concentration" = 3)),
            checkboxGroupInput("plotOptions", "Plotting Options",
                               c("Logarithmic scale" = 'log')),
            actionButton("do", "Confirm uploaded file and save")
        ),
        
        mainPanel(
            tableOutput('tab'),
            uiOutput('plotting'),
            uiOutput("plot_download"),
            uiOutput("dataset_download")
        )
))
