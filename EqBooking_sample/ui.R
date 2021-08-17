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

#PRE - SETUP-----------------
eq_list <- c("Hood 3 (03.14)", "Hood 4 (03.14)", "Hood 1 (03.18)",
             "Hood 2 (03.18)", "Hood 5 (03.04)", "Autoclave",
             "Bench 3", "Bench 4", "KX-2 Fluostar3", "KX-2 Fluostar4",
             "OT2 L", "OT2 R", "Plate pourer")
names(eq_list) <- eq_list

timeSlots <- paste0(seq(8, 18, 1), ":00")
timeSlots <- sapply(timeSlots, function(x) if(nchar(x)<5){paste0("0", x)}else{x})
names(timeSlots) <- timeSlots
timeSlots <<- timeSlots

#SHINYJS EXTENSION-------------
css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;}"

#MAIN----------
shinyUI(fluidPage(
    
))