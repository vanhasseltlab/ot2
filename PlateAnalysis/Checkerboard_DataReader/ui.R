library(shiny)
shinyUI(
    pageWithSidebar(
        headerPanel("Output Preprocessor for Checkerboard 96-Well Plate"),
        
        sidebarPanel(
            #link to home
            actionButton("Home", "Home", width='300px',
                         onclick ="window.open('https://ot2.lacdr.leidenuniv.nl/ot2/home')"),
            
            #names
            textInput("folderName", "Experiment Name", value='defaultFolder'),
            
            
            #first measurement timepoint
            numericInput("first_measurement", "First measurement timepoint (h)", value=0),
            
            #file inputs
            fileInput("files", "Upload Measurement Data", accept=".csv",
                      multiple=T),
            fileInput("pMap", "Upload Plate Map", accept=".xlsx"),
            
            #control selection
            uiOutput("ctrlSelectionUI"),
            uiOutput("control_map_upload"),
            
            
            #action buttons
            actionButton("do", "Confirm uploaded file and save", width=300),
            
            #downloads
            uiOutput('download_prcNM')
        ),
        
        mainPanel(
            textOutput('err_message'),
            tableOutput('tab')
        )
))
