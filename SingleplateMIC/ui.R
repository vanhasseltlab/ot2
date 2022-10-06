library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Singleplate MIC - OT2 Commander"),
    
    sidebarPanel(
      #link to home
      actionButton("Home", "Home", width='300px',
                   onclick ="window.open('https://ot2.lacdr.leidenuniv.nl/ot2/home')"),
      
      #main
      fileInput("file", "Upload Plate Map", accept=".xlsx"),
      downloadButton("downloadTemplate", label = "Template Input"),
      textInput("pmid", 'Plate Map ID (PMID)'),
      textInput("f_name", 'First Name'),
      textInput("l_name", 'Last Name'),
      textInput("exp_name", 'Experiment Name'),
      textInput("exp_num", 'Experiment Number'),
      textOutput('tex'),
      actionButton("do", "Confirm uploaded file and save"),
      uiOutput('downloadData'),
      uiOutput('downloadData2')
    ),
    mainPanel(
      tableOutput('tab')
    )
))

#Last update: 2021-01-31