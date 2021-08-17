#INPUT-------------
#mainDir <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator\\Calendar\\EqBooking_v5"
mainDir <- "/srv/shiny-server/ot2/EqBooking_sample"

scheduleTable_dir <- "ScheduleHardCopy.xlsx"

#LIBRARIES-------
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
library(chron)
library(reshape2)
library(shinyjs)
library(scrypt)
source(paste0(mainDir, "\\CalendarSetup.R"))
scheduleTable <- read_excel(paste0(mainDir, "\\", scheduleTable_dir), sheet=1)

#MAIN----------
shinyUI(fluidPage(
    
))