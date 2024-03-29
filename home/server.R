shinyServer(function(input, output) {
  #setup directories
  robotGuideDir <- "/home/shiny-ot2/ShinyApps/ot2/home/2021-03-05_OT2 General Guideline and Maintenance.pdf" 
  robotQuickGuideDir <- "/home/shiny-ot2/ShinyApps/ot2/home/2021-08-31_OT2QuickGuide.pdf"
  webserverGuideDir <- "/home/shiny-ot2/ShinyApps/ot2/home/2021-07-23_WebserverGuide.pdf" 
  
  output$downloadRobotGuide <- downloadHandler(
    filename="2021-03-05_OT2 General Guideline and Maintenance.pdf",
    content = function(file) {
      file.copy(robotGuideDir, file)
    }
  )
  
  output$downloadRobotQuickGuide <- downloadHandler(
    filename="2021-08-31_OT2QuickGuide.pdf",
    content = function(file) {
      file.copy(robotQuickGuideDir, file)
    }
  )
  
  output$downloadServerGuide <- downloadHandler(
    filename="2021-07-23_WebserverGuide.pdf",
    content = function(file) {
      file.copy(webserverGuideDir, file)
    }
  )
  
})
