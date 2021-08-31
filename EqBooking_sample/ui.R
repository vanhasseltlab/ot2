#INPUT-------------
#mainDir <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\EquipmentBook_CvH\\local_incubator"
#mainDir2 <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\EquipmentBook_CvH\\local_incubator\\main"

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
library(stringi)
#library(scrypt)
source(paste0(mainDir2, "/CalendarSetup.R"))
scheduleTable <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)

#PRE - SETUP-----------------
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
                            uiOutput("eqName_ui"),
                            
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
             ),
             #ADMIN------------
             tabPanel("Admin", id = "tab_four",
                      verticalLayout(
                        sidebarLayout(
                          sidebarPanel(
                            tabsetPanel(
                              tabPanel(title='Manage Accounts',
                                       radioButtons("acc_manage_menu", label="Selection", 
                                                    choices=c("Reset Password", "Make Admin", "Create New Account"), inline=T),
                                       textInput("account_name", label="Username"),
                                       uiOutput("activation_email_ui"),
                                       actionButton("confirm_acc_manage", "Confirm"),
                                       actionLink("refresh_account_admin", "Back")
                              ),
                              tabPanel(title="Manage Bookings",
                                       checkboxInput("show_removed", "Show Removed Bookings", value=F),
                                       checkboxInput("show_old", "Show Old Bookings", value=F),
                                       uiOutput("user_booking_ui"),
                                       uiOutput("eq_list_admin_ui"),
                                       uiOutput("booking_selection"),
                                       radioButtons("admin_modify_booking_options", label="", choices=c("Remove", "Modify"), inline=T, selected="Remove"),
                                       
                                       #modify time inputs
                                       #from
                                       uiOutput("admin_start_date_ui_modify"), #selecting start date; reactive to old booking
                                       uiOutput("admin_start_time_ui_modify"), #selecting start time; reactive to old booking
                                       
                                       #to
                                       uiOutput("admin_end_date_ui_modify"), #selecting end date; reactive to starting date
                                       uiOutput("admin_end_time_ui_modify"), #selecting end time; reactive to starting time
                                       
                                       #confirm button
                                       actionButton("admin_modify_confirm", "Confirm"),
                                       actionLink("admin_back", "Back"),
                                       textOutput("error_message_admin"),
                                       textOutput("conf_message_admin_2"),
                                       tags$head(tags$style("#error_message_admin{color:red; font-style:italic;}")),
                                       tags$head(tags$style("#conf_message_admin2{color:blue; font-style:italic;}"))
                              ),
                              tabPanel(title="Manage Equipments",
                                       radioButtons("eq_menu", label="", choices=c("Add", "Rename", "Remove"), inline=T),
                                       uiOutput("equipment_selection_menu"),
                                       textInput("new_eq_name", "Equipment Name"),
                                       actionButton("eq_modify_confirm", "Confirm")
                              )
                            )
                          ),
                          mainPanel(
                            verbatimTextOutput("confMessage_admin"),
                            tags$head(tags$style(HTML("#confMessage_admin {font-size: 20px;}"))),
                            
                            #booking list
                            dataTableOutput("user_booking_table_admin"),
                            tableOutput("check")
                          )
                        )
                      ),
                      tableOutput("time_avail_admin")
             )
  )
))