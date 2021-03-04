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
                           "Drug-Medium-Concentration control" = 4)),
            checkboxInput("separate_control", "Separate control plate", value=F),
            uiOutput("control_meas_upload"),
            uiOutput("control_map_upload"),
            
            #names
            textInput("folderName", "Experiment Name", value='defaultFolder'),
            
            #action buttons
            actionButton("do", "Confirm uploaded file and save", width=300),
            
            downloadButton("downloadScript", "Download Processor Script", width=300),
            
            #downloads
            uiOutput('download_prcNM')
        ),
        
        mainPanel(
            textOutput('err_message'),
            tableOutput('tab')
        )
))
