shinyServer(function(input, output) {
  #setup directories
  robotGuideDir <- "/srv/shiny-server/ot2/home/2021-03-05_OT2 General Guideline and Maintenance.pdf" 
  
  output$downloadRobotGuide <- downloadHandler(
    filename="2021-03-05_OT2 General Guideline and Maintenance.pdf",
    content = function(file) {
      file.copy(robotGuideDir, file)
    }
  )
  
})
