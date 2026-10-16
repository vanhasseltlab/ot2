library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("CQ Plate - MIC"),
    sidebarPanel(
      #link to home
      actionButton("Home", "Home", width='300px',
                   onclick ="window.open('https://ot2.lacdr.leidenuniv.nl/ot2/home')"),
      
      #main
      fileInput("file", "Upload Plate Map", accept=".xlsx"),
      downloadButton("downloadTemplate", label = "Template Input"),
      checkboxInput("randomize", "Randomize plate map (shuffle all non-FILL wells)", value = FALSE),
      textInput("pmid", 'Plate Map ID (PMID)'),
      textInput("f_name", 'First Name'),
      textInput("l_name", 'Last Name'),
      textInput("exp_name", 'Experiment Name'),
      textInput("exp_num", 'Experiment Number'),
      textOutput('tex'),
      actionButton("do", "Confirm uploaded file and save"),
      uiOutput('downloadData'),
      uiOutput('downloadData2'),
      uiOutput('downloadData3')
    ),
    mainPanel(
      tableOutput('tab')
    )
))
