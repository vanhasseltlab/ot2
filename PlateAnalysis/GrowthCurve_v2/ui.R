library(shiny)
library(shinyjs)
shinyUI(
  
  pageWithSidebar(
    headerPanel("Output Preprocessor for 96-Well Plate"),
    
    sidebarPanel(
      shinyjs::useShinyjs(),
      #link to home
      actionButton("Home", "Home", width='300px',
                   onclick ="window.open('https://ot2.lacdr.leidenuniv.nl/ot2/home')"),
      
      #file inputs
      fileInput("files", "Upload Measurement Data", accept=".csv",
                multiple=T),
      fileInput("pMap", "Upload Plate Map", accept=".xlsx"),
      
      #names
      textInput("folderName", "Experiment Name", value='defaultFolder'),
      
      #action buttons
      actionButton("do", "Confirm uploaded file and save", width=300),
      
      #downloads
      uiOutput('download_prcNM'),
      downloadButton("download_plot", "Download Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title='Data',
                 textOutput('err_message'),
                 tableOutput('tab')
        ),
        tabPanel(title='Plot', id="plot_tab",
                 plotOutput('timecourse_plot'))
      )
    )
))
