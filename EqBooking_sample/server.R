#INPUT-------------
#mainDir <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator\\Calendar\\EqBooking_v5"
mainDir <- "/srv/shiny-server/ot2/EqBooking_sample"
scheduleTable_dir <- "ScheduleHardCopy.xlsx"
userLog_dir <- "sneakyLogin.csv"

#LIBRARIES-------------
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
library(chron)
library(reshape2)
library(shinyjs)
#library(scrypt)
require("V8")
source(paste0(mainDir, "/CalendarSetup.R"))

#MAIN-------
shinyServer(function(input, output) {
  # GENERAL LOGIN--------------
  #   disabling tabs before login
  disable(selector = '.navbar-nav a[data-value="Overview"')
  disable(selector = '.navbar-nav a[data-value="New Booking"')
  disable(selector = '.navbar-nav a[data-value="Manage Bookings"')
  
})