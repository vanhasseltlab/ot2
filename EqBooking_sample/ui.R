#INPUT-------------
#mainDir <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator"
#mainDir2 <- "C:\\Users\\Sebastian\\Desktop\\MSc Leiden 2nd Year\\##LabAst Works\\Incubator\\EqBooking_sample"

#webserver inputs
mainDir <- "/srv/shiny-server/files/EqBooking"
mainDir2 <- "/srv/shiny-server/ot2/EqBooking_sample"

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
library(scrypt, lib.loc="/home/sebastian/R/x86_64-pc-linux-gnu-library/4.1/")
source(paste0(mainDir2, "/CalendarSetup.R"))
scheduleTable <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)

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
  #LOAD SHINYJS FUNCTIONS---------------
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(css),
  
  #MAIN UI--------------
  titlePanel("Lab Equipment Booking"),
  navbarPage("",id="navbarPage",
             #Login page-----------
             tabPanel("Account Manager", id = "first_tab",
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(type='tab',
                                      #Login
                                      tabPanel(title='Login',
                                               textInput("user_login", label="Username"),
                                               passwordInput("password_login", label="Password"),
                                               actionButton("login_book", label="Login")
                                      ),
                                      
                                      #Activate account
                                      tabPanel(title='Activate Account',
                                               textInput("activation_user", label="Username"),
                                               textInput("activation_code", label="Activation Code"),
                                               actionButton("activate_confirm", label="Activate Account"),
                                               passwordInput("password", label="Password"),
                                               passwordInput("password_retype", label="Re-type password"),
                                               actionButton("new_pass", label="Set Password"),
                                               textOutput("activation_error"),
                                               tags$head(tags$style("#activation_error{color:blue; font-style:italic; font-size:14px}"))
                                      ),
                                      tabPanel(title='Change Password',
                                               textInput("change_pass_user", label="Username"),
                                               passwordInput("old_password", label="Old password"),
                                               passwordInput("new_password", label="New password"),
                                               passwordInput("new_password_retype", label="Re-type new password"),
                                               actionButton("new_password_confirm", label="Change Password"),
                                               textOutput("change_pass_error"),
                                               tags$head(tags$style("#change_pass_error{color:blue; font-style:italic; font-size:14px}")),
                                      )
                          )
                        ),
                        mainPanel() #blank
                      )
             ),
             
             #New Booking----------
             tabPanel("New Booking", id = "tab_two",
                      verticalLayout(
                        sidebarLayout(
                          sidebarPanel(
                            #Select Equipment
                            selectInput("eqName", "Select Equipment", eq_list, selected=eq_list[1]),
                            
                            #from
                            uiOutput("start_date_ui"),
                            selectInput("start_time", "", timeSlots), #select start time
                            
                            #to
                            uiOutput("end_date_ui"),
                            uiOutput("end_time_ui"),
                            
                            #confirmation
                            actionButton("confirm_book", "Confirm Booking"),
                            textOutput("Error_message"),
                            textOutput("Conf_message"),
                            tags$head(tags$style("#Error_message{color:red; font-style:italic;}")),
                            tags$head(tags$style("#Conf_message{color:red; font-style:italic;}")),
                            
                            #reset button
                            actionLink("reset_booking", "Back")
                          ),
                          mainPanel(
                            actionButton("red_month", "<< Previous Month"),
                            actionButton("add_month", "Next Month>>"),
                            plotOutput("calendar", dblclick = "calendar_dblclick")
                          )
                        ),
                        tableOutput("availability"))
             ),
             #Manage Bookings----------
             tabPanel("Manage Bookings", id = "tab_three",
                      verticalLayout(
                        sidebarLayout(
                          sidebarPanel(
                            #Selections
                            uiOutput("book_to_modify_ui"),
                            radioButtons("modification", "", choices=c("Remove", "Modify"), inline=T, selected='Remove'),
                            
                            #modifications
                            #from
                            uiOutput("start_date_ui_modify"), #selecting start date; reactive to old booking
                            uiOutput("start_time_ui_modify"), #selecting start time; reactive to old booking
                            
                            #to
                            uiOutput("end_date_ui_modify"), #selecting end date; reactive to starting date
                            uiOutput("end_time_ui_modify"), #selecting end time; reactive to starting time
                            
                            #Confirm button
                            actionButton("confirm_manage", "Confirm"),
                            textOutput("confirm_message"),
                            textOutput("Conf_modify"),
                            tags$head(tags$style("#confirm_message{color:red; font-style:italic;}")),
                            tags$head(tags$style("#Conf_modify{color:red; font-style:italic;}")),
                            
                            #reset button
                            actionLink("reset_manage", "Back")
                          ),
                          mainPanel(
                            tableOutput("user_bookings"),
                            textOutput("error_message_no_bookings")
                          )
                        )
                      ),
                      tableOutput("time_availability")
             )
  )
))