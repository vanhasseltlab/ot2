library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Multiplate_SingleDrugMIC - OT2-Commander"),
    
    sidebarPanel(
      tags$a(href="http://132.229.100.197:2222/ot2/SingleplateMIC/",
             "Link to Single Plate MIC"),
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
      uiOutput('downloadData2'),
      uiOutput('downloadData3'),
      
      tags$a(href="http://132.229.100.197:2222/OldPatch/SingleplateMIC/",
             "Link to last working uploader")
    ),
    mainPanel(
      tableOutput('tab')
    )
))
