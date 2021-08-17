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
library(scrypt, lib.loc="/home/sebastian/R/x86_64-pc-linux-gnu-library/4.1/")
require("V8")
source(paste0(mainDir, "/CalendarSetup.R"))

#MAIN-------
shinyServer(function(input, output) {
  # GENERAL LOGIN--------------
  #   disabling tabs before login
  disable(selector = '.navbar-nav a[data-value="Overview"')
  disable(selector = '.navbar-nav a[data-value="New Booking"')
  disable(selector = '.navbar-nav a[data-value="Manage Bookings"')
  
  #  A | Login
  observeEvent(input$login_book, {
    userLog <- read.csv(paste0(mainDir, "\\", userLog_dir)) #re-read each time
    hashedAuth <- userLog$Password[userLog$Username==input$user_login]
    if(length(hashedAuth)>0){
      passAuth <- verifyPassword(hashedAuth, input$password_login)
    }else{
      passAuth <- F
    }
    
    if(passAuth){
      #show menus
      enable(selector = '.navbar-nav a[data-value="Overview"')
      enable(selector = '.navbar-nav a[data-value="New Booking"')
      enable(selector = '.navbar-nav a[data-value="Manage Bookings"')
      
      #disable login page
      disable("user_login")
      disable("password_login")
      hide("login_book")
    }
  })
  
  #   extract username
  currentUser <- reactive({input$user_login})
  
  #  B | Account Activation
  hide("password")
  hide("password_retype")
  hide("new_pass")
  observeEvent(input$activate_confirm, {
    userLog <<- read.csv(paste0(mainDir, "\\", userLog_dir)) %>% #re-read each time
      subset(Username==input$activation_user)
    
    if(nrow(userLog)==1 & !userLog$Activation){
      if(userLog$ActivationCode==input$activation_code){
        hide("activation_code")
        hide("activate_confirm")
        show("password")
        show("password_retype")
        show("new_pass")
        disable("activation_user")
        output$activation_error <- renderText({"Activation code approved! Set new password"})
      }else{
        output$activation_error <- renderText({"Invalid username/activation code!"})
      } 
    }else{
      output$activation_error <- renderText({"Invalid username/activation code!"})
    }
  })
  
  #setup new password
  observeEvent(input$new_pass, {
    userLog <- read.csv(paste0(mainDir, "\\", userLog_dir)) #re-read each time
    if(input$password==input$password_retype & nchar(input$password)>=5){
      #update user log
      userLog$Activation[userLog$Username==input$activation_user] <- T
      userLog$Password[userLog$Username==input$activation_user] <- hashPassword(input$password)
      userLog$ActivationCode[userLog$Username==input$activation_user] <- paste0("Activated on: ", toString(Sys.time()))
      
      #lock inputs
      disable("password")
      disable("password_retype")
      hide("new_pass")
      
      #message
      output$activation_error <- renderText({"Account activated. Password set successfully"})
      
      #update log hard copy
      write.csv(userLog, paste0(mainDir, "\\", userLog_dir), row.names=F)
    }else{
      if(nchar(input$password)<5 & nchar(input$password)>0){
        output$activation_error <- renderText({"Minimum password length is 5 characters!"})
      }else{
        output$activation_error <- renderText({"Re-typed password not identical!"})
      }
    }
  })
  
  #  C | Change Password
  observeEvent(input$new_password_confirm, {
    userLog <- read.csv(paste0(mainDir, "\\", userLog_dir)) #re-read each time
    hashedAuth <- userLog$Password[userLog$Username==input$change_pass_user]
    if(length(hashedAuth)>0){
      passAuth <- verifyPassword(hashedAuth, input$old_password)
    }else{
      passAuth <- F
    }
    
    if(passAuth){
      #setup password
      userLog$Password[userLog$Username==input$change_pass_user] <- hashPassword(input$new_password)
      
      #lock inputs
      disable("change_pass_user")
      disable("old_password")
      disable("new_password")
      disable("new_password_retype")
      hide("new_password_confirm")
      
      #message
      output$change_pass_error <- renderText("Password changed. Refresh page to make new bookings")
      
      #update log hard copy
      write.csv(userLog, paste0(mainDir, "\\", userLog_dir), row.names=F)
    }else{
      output$change_pass_error <- renderText("Invalid username/password!")
    }
  })
  
  observeEvent(c(input$new_password_retype, input$new_password), {
    if((input$new_password_retype == input$new_password) & nchar(input$new_password)>=5){
      enable("new_password_confirm")
    }else{
      disable("new_password_confirm")
      if((input$new_password_retype == input$new_password) & nchar(input$new_password)<5 & nchar(input$new_password)>0){
        output$change_pass_error <- renderText("Minimum password length is 5 characters")
      }else if((input$new_password_retype != input$new_password) & nchar(input$new_password_retype)>0){
        output$change_pass_error <- renderText("Re-typed password different!")
      }
    }
  })
})