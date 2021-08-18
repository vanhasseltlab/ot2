#INPUT-------------
#mainDir <- "C:/Users/Sebastian/Desktop/MSc Leiden 2nd Year/##LabAst Works/Incubator/Calendar/EqBooking_v5"
mainDir <- "/srv/shiny-server/files/EqBooking"
mainDir2 <- "/srv/shiny-server/ot2/EqBooking_sample"
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
source(paste0(mainDir2, "/CalendarSetup.R"))

#MAIN-------
shinyServer(function(input, output) {
  # GENERAL LOGIN--------------
  #   disabling tabs before login
  disable(selector = '.navbar-nav a[data-value="Overview"')
  disable(selector = '.navbar-nav a[data-value="New Booking"')
  disable(selector = '.navbar-nav a[data-value="Manage Bookings"')
  
  #  A | Login
  observeEvent(input$login_book, {
    userLog <- read.csv(paste0(mainDir, "/", userLog_dir)) #re-read each time
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
    userLog <<- read.csv(paste0(mainDir, "/", userLog_dir)) %>% #re-read each time
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
    userLog <- read.csv(paste0(mainDir, "/", userLog_dir)) #re-read each time
    if(input$password==input$password_retype & nchar(input$password)>=5){
      #update user log
      userLog$Activation[userLog$Username==input$activation_user] <- TRUE
      userLog$Password[userLog$Username==input$activation_user] <- scrypt::hashPassword(input$password)
      userLog$ActivationCode[userLog$Username==input$activation_user] <- paste0("Activated on: ", toString(Sys.time()))
      
      #lock inputs
      disable("password")
      disable("password_retype")
      hide("new_pass")
      
      #message
      output$activation_error <- renderText({"Account activated. Password set successfully"})
      
      #update log hard copy
      write.csv(userLog, paste0(mainDir, "/", userLog_dir), row.names=F)
      
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
    userLog <- read.csv(paste0(mainDir, "/", userLog_dir)) #re-read each time
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
      write.csv(userLog, paste0(mainDir, "/", userLog_dir), row.names=F)
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
  
  # OVERVIEW AND NEW BOOKING-----------
  calendar_month <- reactive({
    input$add_month - input$red_month
  })
  scheduleTable <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)
  calendarSc <- reactive({createCalendar(input$eqName, scheduleTable, calendar_month())})
  output$calendar <- renderPlot({calendarSc()[[1]]})
  
  #date handling
  bookPeriod <- reactive({
    if(input$confirm_book==0){
      current_click <- input$calendar_dblclick
    }else{
      current_click <- isolate(input$calendar_dblclick)
    }
    
    #starting period
    if(is.null(current_click)){
      starting_date <- Sys.Date()
    }else{
      starting_date <- calendarSc()[[2]] %>%
        filter(border_x>=(current_click$x-5) & border_y>=current_click$y) %>%
        arrange(border_x, border_y)
      starting_date <- paste0("20", toString(starting_date$actual_date[1])) %>% as.Date(format="%Y-%m-%d")
      if(starting_date < Sys.Date()){starting_date <- Sys.Date()}
    }
    
    #ending period
    if(is.null(current_click)){
      ending_date <- Sys.Date()
    }else{
      ending_date <- starting_date
    }
    
    return(c(starting_date, ending_date))
  })
  
  #  start/end date UI
  output$start_date_ui <- renderUI({
    dateInput("start_date", "From", min = Sys.Date(), value=bookPeriod()[1])
  })
  
  output$end_date_ui <- renderUI({
    dateInput("end_date", "To", min = bookPeriod()[1], value=bookPeriod()[2])
  })
  
  #  end time UI
  observeEvent(input$start_time, {
    min_index <- which(timeSlots == input$start_time)
    finish_time_slot <- if(min_index==length(timeSlots)){c()}else{timeSlots[(min_index+1):length(timeSlots)]}
    
    output$end_time_ui <- renderUI({
      selectInput("end_time", "", finish_time_slot)
    })
  })
  
  #  Availability Plot
  current_eqSch <- reactive({
    #check current input period
    book_period <- c(input$start_date, input$end_date) %>% sapply(function(x) toString(x)) %>%
      chron(format=c(dates='y-m-d'), out.format=c(dates='d/m/y'))
    
    book_time <- c(input$start_time, input$end_time) %>% sapply(function(x) toString(x))
    
    #read schedule table
    eqSch <- subset(scheduleTable, Equipment==input$eqName) %>% 
      mutate(Start.date = chron(as.numeric(Start.date), out.format=c(dates="d/m/y")), 
             End.date = chron(as.numeric(End.date), out.format=c(dates="d/m/y"))) %>%
      filter((Start.date  >= book_period[1] & Start.date <= book_period[2]) |
               (End.date >= book_period[1] & End.date <= book_period[2]))
    eqSch$Start.time <- sapply(eqSch$Start.time, function(x) paste0(x, ":00"))
    eqSch$End.time <- sapply(eqSch$End.time, function(x) paste0(x, ":00"))
    
    #creating schedule table
    time_slots <- seq(8, 18, 1) %>% sapply(function(x){
      time <- paste0(x, ":00")
      if(nchar(time) == 4){time <- paste0("0", time)}
      return(time)
    })
    
    date_period <- sapply(seq(0,as.numeric(book_period[2]-book_period[1]),1), 
                          function(x) toString(book_period[1]+x))
    date_period <- sapply(date_period, 
                          function(x) paste0(x, " - ", time_slots)) %>% as.vector()
    book_table <- replicate(2, replicate(length(date_period), "")) %>% t() %>% data.frame()
    rownames(book_table) <- c("Booked", "Current Selection")
    colnames(book_table) <- date_period
    
    #filling schedule table
    book_span <- sapply(c(1:2), 
                        function(x) paste0(book_period[x], " - ", book_time[x]))
    
    book_span <- which(colnames(book_table) %in% book_span)
    book_span <- c(book_span[1]:(book_span[2]-1))
    book_table[2, book_span] <- "Selected"
    
    #filling booked slots
    eqSch$Start <- apply(eqSch, 1, 
                         function(x) which(date_period==paste0(x["Start.date"], " - ", substring(toString(x["Start.time"]), 1, 5))))
    eqSch$End <- apply(eqSch, 1, 
                       function(x) which(date_period==paste0(x["End.date"], " - ", substring(toString(x["End.time"]), 1, 5))))
    
    date_period2 <- data.frame(num = seq(1, length(date_period), 1), slots = date_period)
    book_table[1,] <- sapply(date_period2$num, function(x){
      sub_eqsch <- subset(eqSch, Start <= x & End > x)
      if(nrow(sub_eqsch)==0){return(" ")}else{return(sub_eqsch$Username)}
    })
    return(book_table)
  })
  
  #  Create booking
  observeEvent(input$confirm_book, {
    #check approval
    selection <- data.frame(date_inp = c(input$start_date, input$end_date),
                            time_inp = c(input$start_time, input$end_time)) %>%
      mutate(time_inp = sapply(time_inp, function(x) paste0(x, ":00")))
    selection$chr <- apply(selection, 1, 
                           function(x) chron(x["date_inp"], x["time_inp"], format=c(date="y-m-d", times="h:m:s")))
    
    #equipment schedule subsetting
    eqSch <- subset(scheduleTable, Equipment==input$eqName) %>% 
      mutate(Start.date = chron(as.numeric(Start.date), out.format=c(dates="y-m-d")), 
             End.date = chron(as.numeric(End.date), out.format=c(dates="y-m-d")))
    eqSch$Start.time <- sapply(eqSch$Start.time, function(x) paste0(x, ":00"))
    eqSch$End.time <- sapply(eqSch$End.time, function(x) paste0(x, ":00"))
    
    if(nrow(eqSch) > 0){
      eqSch$Start <- apply(eqSch, 1, function(x)
        chron(toString(x["Start.date"]), toString(x["Start.time"]), 
              format=c(date='y-m-d', times='h:m:s')))
      eqSch$End <- apply(eqSch, 1, function(x)
        chron(toString(x["End.date"]), toString(x["End.time"]), 
              format=c(date='y-m-d', times='h:m:s')))
      
      #check approval
      eq_approval <- subset(eqSch, (!(Start >= selection$chr[2]) & !(End <= selection$chr[1])) |
                              (Start <= selection$chr[1] & End >= selection$chr[2])) %>% nrow()
      eq_approval <- eq_approval == 0
    }else{eq_approval <- T}
    
    # execute
    if(eq_approval){
      #execute if confirm button pushed and input is approved
      new_input <- c("No" = max(scheduleTable$No)+1, 
                     "Username"=currentUser(), "Equipment" = input$eqName,
                     "Start.date"=input$start_date, 
                     "Start.time"=input$start_time,
                     "End.date"=input$end_date, 
                     "End.time"=input$end_time,
                     "Note"="")
      
      new_schedule <- rbind.data.frame(scheduleTable, new_input)
      colnames(new_schedule) <- colnames(scheduleTable)
      
      #output:write excel
      write_xlsx(new_schedule, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
      
      #update calendar
      calendarSc_updated <- createCalendar(input$eqName, new_schedule, calendar_month())
      output$calendar <- renderPlot({calendarSc_updated[[1]]})
      
      #confirm; disable further inputs
      hide("confirm_book")
      output$Conf_message <- renderText({"Booking successful!"})
      show("Conf_message")
      toggleState("eqName")
      toggleState("start_date")
      toggleState("start_time")
      toggleState("end_date")
      toggleState("end_time")
    }else{
      show("Error_message")
    }
  })
  
  #  handling error message
  output$Error_message <- renderText({"Time slot not available!"})
  hide("Error_message")
  observeEvent(c(input$end_time, input$start_time, input$end_date, input$end_time),{
    hide("Error_message")
  })
  
  #  Render availability plot
  output$availability <- renderTable(current_eqSch(), rownames=T, bordered=T)
  
  # MANAGE BOOKINGS--------------
  hide("end_date_ui_modify")
  hide("end_time_ui_modify")
  hide("start_date_ui_modify")
  hide("start_time_ui_modify")
  hide("error_message_no_bookings")
  
  #  input for modify bookings
  time_slot_range_modify <- reactive({
    min_index <- which(timeSlots == input$start_time_modify)
    finish_time_slot <- if(min_index==length(timeSlots)){c()}else{timeSlots[(min_index+1):length(timeSlots)]}
    return(finish_time_slot)
  })
  output$end_date_ui_modify <- renderUI({
    dateInput("end_date_modify", "To", min=input$start_date_modify, value=original_dates()[2])
  })
  output$end_time_ui_modify <- renderUI({
    selectInput("end_time_modify", "", time_slot_range_modify())
  })
  output$start_date_ui_modify <- renderUI({
    dateInput("start_date_modify", "From", min = Sys.Date(), value=original_dates()[1]) #select start date
  })
  output$start_time_ui_modify <- renderUI({
    selectInput("start_time_modify", "", timeSlots, selected=original_times()[1]) #select start time
  })
  
  #read current user's bookings
  userBookings <- reactive({
    if(is.null(currentUser())){NULL}
    
    scheduleTable_r <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)
    userTable <- subset(scheduleTable_r, Username==currentUser()) %>%
      mutate(Start.date = sapply(Start.date, function(x) chron(as.numeric(x), out.format=c(dates='d-m-y'))),
             End.date = sapply(End.date, function(x) chron(as.numeric(x), out.format=c(dates='d-m-y')))) %>%
      filter(Start.date >= chron(toString(Sys.Date()), format=c(dates='y-m-d'))) %>%
      mutate(Start.date = sapply(Start.date, function(x){as.numeric(x) %>% chron(out.format=c(dates='d-m-y')) %>% toString()}),
             End.date = sapply(End.date, function(x){as.numeric(x) %>% chron(out.format=c(dates='d-m-y')) %>% toString()}))
    
    if(nrow(userTable)==0){
      show("error_message_no_bookings")
      return(NULL)
    }else{
      hide("error_message_no_bookings")
      return(userTable)
    }
  })
  
  #setup dropdown menu for booking selection
  output$book_to_modify_ui <- renderUI(selectInput("book_to_modify", "Select Booking", dropDown()))
  
  dropDown <- reactive({
    if(is.null(userBookings())){
      selections <- c("")
    }else{
      #selection for drop-down menu
      selections <- apply(userBookings(), 1, function(x) {
        if(x["Start.date"]==x["End.date"]){
          paste0(x["Equipment"], " : ", x["Start.date"], " - ", 
                 x["Start.time"]," ~ ", x["End.time"])
        }else{
          paste0(x["Equipment"], " : ", x["Start.date"], " - ", x["Start.time"],
                 " ~ ", x["End.date"], " - ", x["End.time"])
        }}) 
    }
    return(selections)
  })
  
  #get original settings
  original_dates <- reactive({
    if(is.null(input$book_to_modify)){NULL}
    
    #get relevant booking period
    book_index <- which(dropDown()==input$book_to_modify)
    book_dates <- c(userBookings()$Start.date[book_index], userBookings()$End.date[book_index]) %>%
      chron(format=c(dates='d-m-y'), out.format=c(dates="d-m-y"))
    
    return(book_dates)
  })
  
  original_times <- reactive({
    if(is.null(input$book_to_modify)){NULL}
    
    #get relevant booking period
    book_index <- which(dropDown()==input$book_to_modify)
    book_times <- c(userBookings()$Start.time[book_index], userBookings()$End.time[book_index]) 
    
    return(book_times)
  })
  
  #get availability table for selected
  availabilityDate <- reactive({
    if(input$modification=="Remove"){return(userBookings())}else{
      #get relevant booking period
      book_index <- which(dropDown()==input$book_to_modify)
      current_books <- userBookings()[book_index,]
      
      #input
      book_period <- c(input$start_date_modify, input$end_date_modify) %>%
        sapply(function(x) chron(toString(x), format=c(dates='y-m-d'), out.format = c(dates='d/m/y')))
      book_time <- c(input$start_time_modify, input$end_time_modify) %>% sapply(function(x) toString(x))
      
      #read booking schedule
      eq_name <- strsplit(input$book_to_modify, split=" : ")[[1]][1]
      scheduleTable_r <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)
      eqSch <- subset(scheduleTable_r, Equipment==eq_name & No != current_books$No) %>% 
        mutate(Start.date = chron(as.numeric(Start.date), out.format=c(dates="d/m/y")), 
               End.date = chron(as.numeric(End.date), out.format=c(dates="d/m/y"))) %>%
        filter((Start.date  >= book_period[1] & Start.date <= book_period[2]) |
                 (End.date >= book_period[1] & End.date <= book_period[2]))
      eqSch$Start.time <- sapply(eqSch$Start.time, function(x) paste0(x, ":00"))
      eqSch$End.time <- sapply(eqSch$End.time, function(x) paste0(x, ":00"))
      
      #creating schedule table
      time_slots <- seq(8, 18, 1) %>% sapply(function(x){
        time <- paste0(x, ":00")
        if(nchar(time) == 4){time <- paste0("0", time)}
        return(time)
      })
      date_period <- sapply(seq(0,as.numeric(book_period[2]-book_period[1]),1), 
                            function(x) toString(chron(book_period[1]+x, out.format=c(dates='d/m/y'))))
      date_period <- sapply(date_period, 
                            function(x) paste0(x, " - ", time_slots)) %>% as.vector()
      book_table <- replicate(3, replicate(length(date_period), "")) %>% t() %>% data.frame()
      rownames(book_table) <- c("Booked", "Old Booking", "Current Selection")
      colnames(book_table) <- date_period
      
      #filling schedule table
      book_span <- sapply(c(1:2), 
                          function(x) paste0(chron(as.numeric(book_period[x]), out.format=c(dates='d/m/y')), 
                                             " - ", book_time[x]))
      book_span <- which(colnames(book_table) %in% book_span)
      book_span <- c(book_span[1]:(book_span[2]-1))
      book_table[3, book_span] <- "Selected"
      
      #filling booked slots
      eqSch$Start <- apply(eqSch, 1, 
                           function(x) which(date_period==paste0(x["Start.date"], " - ", substring(toString(x["Start.time"]), 1, 5))))
      eqSch$End <- apply(eqSch, 1, 
                         function(x) which(date_period==paste0(x["End.date"], " - ", substring(toString(x["End.time"]), 1, 5))))
      
      date_period2 <- data.frame(num = seq(1, length(date_period), 1), slots = date_period)
      book_table[1,] <- sapply(date_period2$num, function(x){
        sub_eqsch <- subset(eqSch, Start <= x & End > x)
        if(nrow(sub_eqsch)==0){return(" ")}else{return(sub_eqsch$Username)}
      })
      
      #filling old slots
      current_books <- c(paste0(gsub("-", "/", current_books$Start.date), " - ", current_books$Start.time),
                         paste0(gsub("-", "/", current_books$End.date), " - ", current_books$End.time))
      
      current_books_index <- which(colnames(book_table) %in% current_books)
      
      current_books_index <- seq(current_books_index[1], current_books_index[2]-1, 1)
      book_table[2,current_books_index] <- "Old Booking"
      
      return(book_table)
    }
  })
  
  #show output table for current user's bookings
  output$user_bookings <- renderTable(availabilityDate(), bordered=T, rownames=T)
  output$error_message_no_bookings <- renderText("No bookings found")
  
  #hide/show based on radio input: modify OR remove booking
  observeEvent(input$modification, {
    if(input$modification=="Modify" & !is.null(userBookings())){
      show("start_date_ui_modify")
      show("start_time_ui_modify")
      show("end_date_ui_modify")
      show("end_time_ui_modify")
    }else{
      hide("start_date_modify")
      hide("start_time_modify")
      hide("end_date_ui_modify")
      hide("end_time_ui_modify")
    }
  })
  
  #execute modify/remove booking
  observeEvent(input$confirm_manage, {
    if(input$confirm_manage==0){NULL}
    
    scheduleTable_r <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)
    if(input$modification=="Modify"){
      #checking slot availability
      availability <- availabilityDate()[c(1,3),]
      availability <- apply(availability, 2, function(x) (x[1]!=" " & x[2]!="")) %>% sum()
      
      if(availability == 0){
        #get index of modified booking within current user's booking table
        book_index <- which(dropDown()==input$book_to_modify)
        
        #get user's booking
        current_books <- userBookings() %>% 
          mutate(Start.date = sapply(Start.date, function(x) chron(x, format=c(dates='d-m-y'))),
                 End.date = sapply(End.date, function(x) chron(x, format=c(dates='d-m-y'))))
        
        #modify user's booking
        current_books$Start.date[book_index] <- input$start_date_modify
        current_books$Start.time[book_index] <- input$start_time_modify
        current_books$End.date[book_index] <- input$end_date_modify
        current_books$End.time[book_index] <- input$end_time_modify
        
        #get the rest of bookings in the schedule table
        all_bookings <- subset(scheduleTable_r, Username!=currentUser()) %>%
          rbind.data.frame(current_books) %>%
          mutate(No = as.numeric(No)) %>% arrange(No)
        
        #write
        write_xlsx(all_bookings, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
        
        #confirm; disable further inputs
        hide("confirm_manage")
        output$Conf_modify <- renderText({"Booking modified"})
        show("Conf_modify")
        toggleState("book_to_modify")
        toggleState("modification")
        toggleState("start_date_modify")
        toggleState("start_time_modify")
        toggleState("end_date_modify")
        toggleState("end_time_modify")
      }
    }else{
      #get index of modified booking within current user's booking table
      book_index <- which(dropDown()==input$book_to_modify)
      
      #get user's booking; remove the indexed row
      current_books <- userBookings()[-book_index,]
      
      user_oldBookings <- subset(scheduleTable_r, Username==currentUser()) %>%
        mutate(Start.date = sapply(Start.date, function(x) chron(as.numeric(x), out.format=c(dates='d-m-y'))),
               End.date = sapply(End.date, function(x) chron(as.numeric(x), out.format=c(dates='d-m-y')))) %>%
        filter(Start.date < chron(toString(Sys.Date()), format=c(dates='y-m-d'))) %>%
        mutate(Start.date = sapply(Start.date, function(x){as.numeric(x) %>% chron(out.format=c(dates='d-m-y')) %>% toString()}),
               End.date = sapply(End.date, function(x){as.numeric(x) %>% chron(out.format=c(dates='d-m-y')) %>% toString()}))
      
      
      #get the rest of bookings in the schedule table
      all_bookings <- subset(scheduleTable_r, Username!=currentUser()) %>%
        rbind.data.frame(user_oldBookings)
      #  rbind.data.frame(current_books) %>%
      #  mutate(No = as.numeric(No)) %>%
      #  arrange(No) %>%
      #  mutate(Start.date = sapply(Start.date, function(x){toString(x) %>% chron(format=c(dates='d-m-y')) %>% as.numeric()}),
      #         End.date = sapply(End.date, function(x){toString(x) %>% chron(format=c(dates='d-m-y')) %>% as.numeric()}))
      
      output$test_table <- renderTable({user_oldBookings}) 
      
      #write
      #write_xlsx(all_bookings, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
      
      hide("confirm_manage")
      output$Conf_modify <- renderText({"Booking removed"})
      show("Conf_modify")
      toggleState("book_to_modify")
      toggleState("modification")
    }
  })
  
  # error message
  conf_message <- reactive({
    if(input$confirm_manage==0 | input$modification=="Remove"){return(NULL)}else{
      #checking slot availability
      availability <- availabilityDate()[c(1,3),]
      availability <- apply(availability, 2, function(x) (x[1]!=" " & x[2]!="")) %>% sum()
      
      if(availability == 0){
        return(NULL)
      }else{
        return("Time slot unavailable!")
      }
    }
  })
  output$confirm_message <- renderText({conf_message()})
})