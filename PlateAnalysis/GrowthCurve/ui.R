library(shiny)
shinyUI(
    pageWithSidebar(
        headerPanel("Growth Curve Plot for 96-Well Plate"),
        
        sidebarPanel(
            selectInput("reader_type", "Select plate reader",
                        list("FluostarOmega without Robot Arm" = 1,
                             "FluostarOmega with Robot Arm" = 2)),
            fileInput("files", "Upload Plate Map", accept=".csv", multiple=T),
            textInput("folderName", "Experiment Name", value='defaultFolder'),
            textInput("time", "Timepoint of first measurement", value="00:00:00"),
            checkboxGroupInput("plotOptions", "Plotting Options",
                               c("Logarithmic scale" = 'log',
                                 "Error bars" = 'errorBars',
                                 "Automatic y-axis" = 'auto')),
            numericInput("lower_bound", 'y-axis lower bound', value=0.05),
            numericInput("upper_bound", 'y-axis upper bound', value=2.5),
            actionButton("do", "Confirm uploaded file and save")
        ),
        
        mainPanel(
            tableOutput('tab'),
            uiOutput('plotting'),
            uiOutput("plot_download"),
            uiOutput("dataset_download")
        )
))
