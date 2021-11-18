#META #####
#S.T.Tandar_384-well plate operator -- 2021/11/17

library(shiny)
library(readxl)
library(writexl)
library(dplyr)

options(stringsAsFactors = F)

#SERVER MAIN------------
shinyServer(function(input, output) {
  #defining directory-------
  sourceDir <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\LauraPlate\\Plate384\\lauraPlate_processor.R"
  outputDir_cmdline <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\LauraPlate\\Plate384"
  outputDir_usrGuide <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\LauraPlate\\Plate384"
  inputTemplate <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\Incubator\\LauraPlate\\Plate384\\20211111_384TemplateInput.xlsx"
  
  #outputDir_cmdline <- "/srv/shiny-server/files/Output_CmdList"
  #outputDir_usrGuide <- "/srv/shiny-server/files/Output_UsrGuide"
  #inputTemplate <- "/srv/shiny-server/ot2/Plate384/20211111_384TemplateInput.xlsx" 
  #sourceDir <- "/srv/shiny-server/ot2/Plate384/lauraPlate_processor.R" 
  
  #loading functions--------
  source(sourceDir)
  
  
})