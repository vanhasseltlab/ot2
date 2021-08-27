#INPUT-------------
#mainDir <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\EquipmentBook_CvH\\local_incubator"
#mainDir2 <- "C:\\Users\\sebas\\OneDrive\\Documents\\WebServer\\EquipmentBook_CvH\\local_incubator\\main"

#webserver inputs
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
library(stringi)
library(scrypt, lib.loc="/home/sebastian/R/x86_64-pc-linux-gnu-library/4.1/")
#library(scrypt)
source(paste0(mainDir2, "/CalendarSetup.R"))

#MAIN-------
shinyServer(function(input, output) {
  # GENERAL LOGIN--------------
  #   disabling tabs before login
  disable(selector = '.navbar-nav a[data-value="New Booking"')
  disable(selector = '.navbar-nav a[data-value="Manage Bookings"')
  disable(selector = '.navbar-nav a[data-value="Admin"')
  
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
      
      if(userLog$Admin[userLog$Username==input$user_login]){
        enable(selector = '.navbar-nav a[data-value="Admin"')
      }
      
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
    userLog <- read.csv(paste0(mainDir, "/", userLog_dir)) %>% #re-read each time
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
  hide("reset_booking")
  
  #  calendar and schedule udpate
  observeEvent((input$admin_modify_confirm | input$confirm_manage | input$confirm_book | input$login_book), {
    scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #initial read
    
    #calendar update
    calendar_month <- reactive({
      input$add_month - input$red_month
    })
    calendarSc <<- reactive({createCalendar(input$eqName, scheduleTable, calendar_month())})
    output$calendar <<- renderPlot({calendarSc()[[1]]})
    
    #availability plot update
    current_eqSch <<- reactive({
      #check current input period
      book_period <- c(input$start_date, input$end_date) %>% sapply(function(x) toString(x)) %>%
        chron(format=c(dates='y-m-d'), out.format=c(dates='d/m/y'))
      book_time <- c(input$start_time, input$end_time) %>% sapply(function(x) toString(x))
      
      #read schedule table
      eqSch <- subset(scheduleTable, Equipment==input$eqName) %>% 
        mutate(Start.date = chron(as.numeric(Start.date), out.format=c(dates="d/m/y")), 
               End.date = chron(as.numeric(End.date), out.format=c(dates="d/m/y")),
               Active = !grepl("Removed", Note)) %>%
        filter(((Start.date  >= book_period[1] & Start.date <= book_period[2]) |
                  (End.date >= book_period[1] & End.date <= book_period[2])) & Active)
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
    output$availability <<- renderTable(current_eqSch(), rownames=T, bordered=T)
  })
  
  #  date handling
  bookPeriod <- reactive({
    #starting period
    if(is.null(input$calendar_dblclick)){
      starting_date <- Sys.Date()
      ending_date <- Sys.Date()
    }else{
      current_click <- input$calendar_dblclick
      starting_date <- calendarSc()[[2]] %>%
        filter(border_x>=(current_click$x-5) & border_y>=current_click$y) %>%
        arrange(border_x, border_y)
      starting_date <- paste0("20", toString(starting_date$actual_date[1])) %>% as.Date(format="%Y-%m-%d")
      if(starting_date < Sys.Date()){starting_date <- Sys.Date()}
      
      ending_date <- starting_date
    }
    
    tryCatch({
      return_period <- isolated_period
      isolated_period <<- c()
      if(is.null(return_period)){return(c(starting_date, ending_date))}else{return(return_period)}
      }, error=function(cond){return(c(starting_date, ending_date))})
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
             End.date = chron(as.numeric(End.date), out.format=c(dates="y-m-d")),
             Active = !grepl("Removed", Note)) %>% filter(Active)
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
      booking_number <- if(nrow(scheduleTable)==0){1}else{max(as.numeric(scheduleTable$No))+1}
      new_input <- c("No" = booking_number, 
                     "Username"=currentUser(), "Equipment" = input$eqName,
                     "Start.date"=input$start_date, 
                     "Start.time"=input$start_time,
                     "End.date"=input$end_date, 
                     "End.time"=input$end_time,
                     "Note"="")
      
      new_schedule <- rbind.data.frame(scheduleTable, new_input)
      colnames(new_schedule) <- colnames(scheduleTable)
      
      #isolate booking period for subsequent bookings
      isolated_period <<- isolate(bookPeriod())
      
      #output:write excel
      write_xlsx(new_schedule, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
      scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #refresh read
      
      #confirm; disable further inputs
      hide("confirm_book")
      show("reset_booking")
      output$Conf_message <- renderText({"Booking successful!"})
      show("Conf_message")
      toggleState("eqName")
      toggleState("start_date")
      toggleState("start_time")
      toggleState("end_date")
      toggleState("end_time")
    }else{show("Error_message")}
  })
  
  #  handling error message
  output$Error_message <- renderText({"Time slot not available!"})
  hide("Error_message")
  observeEvent(c(input$end_time, input$start_time, input$end_date, input$end_time),{
    hide("Error_message")
  })
  
  #  reset
  observeEvent(input$reset_booking, {
    #hide/show action button
    hide("reset_booking")
    show("confirm_book")
    hide("Conf_message")
    
    #resetting inputs
    reset("confirm_booking")
    reset("reset_booking")
    reset("calendar_dblclick")
    enable("start_date")
    enable("start_time")
    enable("end_date")
    enable("end_time")
    enable("eqName")
  })
  
  # MANAGE BOOKINGS--------------
  hide("reset_manage")
  hide("end_date_ui_modify")
  hide("end_time_ui_modify")
  hide("start_date_ui_modify")
  hide("start_time_ui_modify")
  hide("error_message_no_bookings")
  
  #show output table for current user's bookings
  output$error_message_no_bookings <- renderText("No bookings found")
  
  #update availability table based on confirmed changes
  observeEvent((input$admin_modify_confirm | input$confirm_manage | input$confirm_book | input$login_book), {
    # User booking table
    userBookings <<- reactive({
      if(is.null(currentUser())){NULL}
      
      scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #update global
      userTable <- subset(scheduleTable, Username==currentUser()) %>%
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
    output$user_bookings <- renderTable(userBookings(), bordered=T, rownames=T)
    
    # Dropdown menu
    dropDown <<- reactive({
      available_selection <- userBookings() %>%
        mutate(Active = !grepl("Removed", Note)) %>% filter(Active)
      if(is.null(available_selection) | nrow(available_selection)==0){
        selections <- c("")
      }else{
        #selection for drop-down menu
        selections <- apply(available_selection, 1, function(x) {
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
    output$book_to_modify_ui <- renderUI(selectInput("book_to_modify", "Select Booking", dropDown()))
    
    # Availability date
    availabilityDate <<- reactive({
      if(input$modification=="Remove"){return(userBookings())}else{
        #get relevant booking period
        book_index <- which(dropDown()==input$book_to_modify)
        current_books <- userBookings() %>%
          mutate(Active = !grepl("Removed", Note)) %>% filter(Active)
        current_books <- current_books[book_index,]
        
        #input
        book_period <- c(input$start_date_modify, input$end_date_modify) %>%
          sapply(function(x) chron(toString(x), format=c(dates='y-m-d'), out.format = c(dates='d/m/y')))
        book_time <- c(input$start_time_modify, input$end_time_modify) %>% sapply(function(x) toString(x))
        
        #read booking schedule
        eq_name <- strsplit(input$book_to_modify, split=" : ")[[1]][1]
        
        eqSch <- subset(scheduleTable, Equipment==eq_name & No != current_books$No) %>% 
          mutate(Start.date = chron(as.numeric(Start.date), out.format=c(dates="d/m/y")), 
                 End.date = chron(as.numeric(End.date), out.format=c(dates="d/m/y")),
                 Active = !grepl("Removed", Note)) %>%
          filter(((Start.date  >= book_period[1] & Start.date <= book_period[2]) |
                    (End.date >= book_period[1] & End.date <= book_period[2])) & Active)
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
    output$time_availability <- renderTable({
      if(input$modification=="Remove"){NULL}else{availabilityDate()}
    }, bordered=T, rownames=T)
    
    # Modify: time inputs
    original_dates <- reactive({
      if(is.null(input$book_to_modify)){NULL}
      
      #get relevant booking period
      book_index <- which(dropDown()==input$book_to_modify)
      current_books <- userBookings() %>%
        mutate(Active = !grepl("Removed", Note)) %>% filter(Active)
      book_dates <- c(current_books$Start.date[book_index], current_books$End.date[book_index]) %>%
        chron(format=c(dates='d-m-y'), out.format=c(dates="d-m-y"))
    })
    original_times <- reactive({
      if(is.null(input$book_to_modify)){NULL}
      
      #get relevant booking period
      book_index <- which(dropDown()==input$book_to_modify)
      current_books <- userBookings() %>%
        mutate(Active = !grepl("Removed", Note)) %>% filter(Active)
      book_times <- c(current_books$Start.time[book_index], current_books$End.time[book_index]) 
      
      return(book_times)
    })
    output$start_date_ui_modify <- renderUI({
      dateInput("start_date_modify", "From", min = Sys.Date(), value=original_dates()[1]) #select start date
    })
    output$start_time_ui_modify <- renderUI({
      selectInput("start_time_modify", "", timeSlots, selected=original_times()[1]) #select start time
    })
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
  })
  
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
                 End.date = sapply(End.date, function(x) chron(x, format=c(dates='d-m-y'))),
                 Active = !grepl("Removed", Note)) %>% filter(Active) %>% dplyr::select(-Active)
        
        #modify user's booking
        current_books$Start.date[book_index] <- input$start_date_modify
        current_books$Start.time[book_index] <- input$start_time_modify
        current_books$End.date[book_index] <- input$end_date_modify
        current_books$End.time[book_index] <- input$end_time_modify
        
        #leave note
        new_note <- paste0("Modified from ", strsplit(input$book_to_modify, split=" : ")[[1]][2], "; by ", input$user_login, " at ", toString(Sys.time()))
        if(!is.na(current_books$Note[book_index])){
          current_books$Note[book_index] <- paste0(new_note, "\n", current_books$Note[book_index])
        }else{current_books$Note[book_index] <- new_note}
        
        removed_books <- userBookings() %>% 
          mutate(Start.date = sapply(Start.date, function(x) chron(x, format=c(dates='d-m-y'))),
                 End.date = sapply(End.date, function(x) chron(x, format=c(dates='d-m-y'))),
                 Active = !grepl("Removed", Note)) %>% filter(!Active) %>% dplyr::select(-Active)
        
        #get the rest of bookings in the schedule table
        all_bookings <- subset(scheduleTable, Username!=currentUser()) %>%
          rbind.data.frame(current_books) %>%
          rbind.data.frame(removed_books) %>%
          mutate(No = as.numeric(No)) %>% arrange(No) 
        
        #write
        write_xlsx(all_bookings, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
        scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #update global
        
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
        hide("time_availability")
        show("reset_manage")
      }
    }else{
      #get index of modified booking within current user's booking table
      book_index <- which(dropDown()==input$book_to_modify)
      
      #get user's booking; remove the indexed row
      current_books <- userBookings() %>%
        mutate(Start.date = sapply(Start.date, function(x){toString(x) %>% chron(format=c(dates='d-m-y')) %>% as.numeric()}),
               End.date = sapply(End.date, function(x){toString(x) %>% chron(format=c(dates='d-m-y')) %>% as.numeric()}),
               Active = !grepl("Removed", Note)) %>% filter(Active) %>% dplyr::select(-Active)
      
      current_books$Note[book_index] <- paste0("Removed by ", input$user_login, " at ", toString(Sys.time()))
      
      #other bookings to append
      removed_books <- userBookings() %>%
        mutate(Start.date = sapply(Start.date, function(x){toString(x) %>% chron(format=c(dates='d-m-y')) %>% as.numeric()}),
               End.date = sapply(End.date, function(x){toString(x) %>% chron(format=c(dates='d-m-y')) %>% as.numeric()}),
               Active = !grepl("Removed", Note)) %>% filter(!Active) %>% dplyr::select(-Active)
      
      user_oldBookings <- subset(scheduleTable, Username==currentUser()) %>%
        mutate(Start.date = sapply(Start.date, function(x) chron(as.numeric(x), out.format=c(dates='d-m-y'))),
               End.date = sapply(End.date, function(x) chron(as.numeric(x), out.format=c(dates='d-m-y')))) %>%
        filter(Start.date < chron(toString(Sys.Date()), format=c(dates='y-m-d')))
      
      #get the rest of bookings in the schedule table
      all_bookings <- subset(scheduleTable, Username!=currentUser()) %>%
        rbind.data.frame(user_oldBookings) %>%
        rbind.data.frame(current_books) %>%
        rbind.data.frame(removed_books) %>%
        mutate(No = as.numeric(No)) %>% arrange(No)
      
      #write
      write_xlsx(all_bookings, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
      scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #update global
      
      hide("confirm_manage")
      output$Conf_modify <- renderText({"Booking removed"})
      show("Conf_modify")
      toggleState("book_to_modify")
      toggleState("modification")
      show("reset_manage")
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
  
  # reset manage operations
  observeEvent(input$reset_manage, {
    #reactivating confirm buttons
    show("confirm_manage")
    hide("reset_manage")
    hide("confirm_message")
    
    #re-enabling inputs
    #hide("Conf_modify")
    enable("book_to_modify")
    enable("modification")
    enable("start_date_modify")
    enable("start_time_modify")
    enable("end_date_modify")
    enable("end_time_modify")
    show("time_availability")
    reset("confirm_manage")
  })
  
  # ADMIN OPERATIONS------------
  hide("refresh_account_admin")
  # A| Account Manager
  observeEvent(input$confirm_acc_manage, {
    userLog <- read.csv(paste0(mainDir, "/", userLog_dir)) #re-read each time
    
    if(input$acc_manage_menu=="Reset Password"){
      allowed <- userLog$Username[!userLog$Admin] #admin account can only be managed manually
      
      if(input$account_name %in% allowed){
        activation_code <- stri_rand_strings(1, 5)  #generate new activation code
        #reset account
        userLog$Activation[userLog$Username==input$account_name] <- FALSE
        userLog$Password[userLog$Username==input$account_name] <- NA
        userLog$ActivationCode[userLog$Username==input$account_name] <- activation_code
        
        #confirmation message
        conf_message <- paste0("User Password Reset\nUsername\t: ", input$account_name, "\nActivation Code\t: ", activation_code)
        output$confMessage_admin <- renderText(conf_message)
        write.csv(userLog, paste0(mainDir, "/", userLog_dir), row.names=F)
        
        #hide buttons
        hide("confirm_acc_manage")
        disable("account_name")
        disable("acc_manage_menu")
        show("refresh_account_admin")
      }else{
        if(input$account_name %in% userLog$Username){
          output$confMessage_admin <- renderText("Cannot reset admin!")
        }else{
          output$confMessage_admin <- renderText("User not found!")
        }
      }
      
    }else if(input$acc_manage_menu=="Make Admin"){
      allowed <- userLog$Username[!userLog$Admin] #admin account can only be managed manually
      
      if(input$account_name %in% allowed & !(paste0(input$account_name, "_admin") %in% userLog$Username)){
        userLog$Username[userLog$Username==input$account_name] <- paste0(userLog$Username[userLog$Username==input$account_name], "_admin")
        userLog$Admin[userLog$Username==input$account_name] <- TRUE
        
        #confirmation message
        conf_message <- paste0(input$account_name, " is now admin. Username changed to ", input$account_name, "_admin")
        output$confMessage_admin <- renderText(conf_message)
        write.csv(userLog, paste0(mainDir, "/", userLog_dir), row.names=F)
        
        #hide buttons
        hide("confirm_acc_manage")
        disable("account_name")
        disable("acc_manage_menu")
        show("refresh_account_admin")
      }else{
        if((input$account_name %in% userLog$Username) | (paste0(input$account_name, "_admin") %in% userLog$Username)){
          output$confMessage_admin <- renderText("User is already an admin!")
        }else{
          output$confMessage_admin <- renderText("User not found!")
        }
      }
      
    }else{
      try_address <- tryCatch({
        separated <- strsplit(input$activation_email, split="@")[[1]]
        return(length(separated)==2)
      }, error=function(cond){
        return(F)
      })
      continue_process <- (!(input$account_name %in% userLog$Username)) & try_address
      # selection = create new user
      if(continue_process){
        activation_code <- stri_rand_strings(1, 5)  #generate new activation code
        new_user <- data.frame(ID = max(as.numeric(userLog$ID))+1,
                               Username = input$account_name, 
                               Password = NA,
                               Activation = FALSE,
                               ActivationCode = activation_code,
                               Admin = FALSE,
                               EmailAddress = input$activation_email)
        userLog <- rbind.data.frame(userLog, new_user) %>% arrange(as.numeric(ID))
        
        #confirmation message
        conf_message <- paste0("Account Created\nUsername\t: ", input$account_name, "\nActivation Code\t: ", activation_code)
        output$confMessage_admin <- renderText(conf_message)
        write.csv(userLog, paste0(mainDir, "/", userLog_dir), row.names=F)
        
        #hide buttons
        hide("confirm_acc_manage")
        disable("account_name")
        disable("acc_manage_menu")
        disable("activation_email")
        show("refresh_account_admin")
      }else{
        if((input$account_name %in% userLog$Username)){
          output$confMessage_admin <- renderText("Username is already used!")
        }else if(!try_address){
          output$confMessage_admin <- renderText("Invalid email address!")
        }
      }
    }
  })
  
  #  show/hide activation email ui
  output$activation_email_ui <- renderUI({
    if(input$acc_manage_menu=="Create New Account"){
      textInput("activation_email", label="Email (for sending activation code)")
    }
  })
  
  #  refresh page
  observeEvent(input$refresh_account_admin, {
    #reset buttons
    enable("account_name")
    hide("refresh_account_admin")
    show("confirm_acc_manage")
    reset("account_name")
    enable("activation_email")
    enable("acc_manage_menu")
    
    #reset comment
    output$confMessage_admin <- renderText(NULL)
  })
  
  # B| Booking Manager
  hide("admin_start_date_ui_modify")
  hide("admin_start_time_ui_modify")
  hide("admin_end_date_ui_modify")
  hide("admin_end_time_ui_modify")
  hide("time_avail_admin")
  hide("admin_back")
  hide("error_message_admin")
  
  # subset booking per-user
  output$user_booking_ui <- renderUI({
    userLog <- read.csv(paste0(mainDir, "/", userLog_dir)) %>%
      filter(!Admin) %>% dplyr::select(Username) %>% as.vector()
    
    selectInput("user_booking", "Username", choices=c("All", userLog), selected="All")
  })
  
  # show/hide options when modify booking selected
  observeEvent(input$admin_modify_booking_options, {
    if(input$admin_modify_booking_options=="Modify"){
      show("admin_start_date_ui_modify")
      show("admin_start_time_ui_modify")
      show("admin_end_date_ui_modify")
      show("admin_end_time_ui_modify")
      show("time_avail_admin")
    }
  })
  
  # execute admin modify booking operations
  observeEvent(input$admin_modify_confirm, {
    #get index of booking to operate with
    book_index <- strsplit(input$selected_booking, split=".", fixed=T)[[1]][1] %>% as.numeric()
    
    #determine remove/modify
    if(input$admin_modify_booking_options=="Remove"){
      #if remove booking
      scheduleTable_new <- scheduleTable
      time_index <- Sys.time() %>% toString() %>% strsplit(split=" ")
      time_index <- paste(time_index[[1]][1:2], collapse=" ")
      scheduleTable_new$Note[book_index] <- paste0("Removed by ", input$user_login, " at ", time_index, "\n", scheduleTable_new$Note[book_index])
      
      #write
      write_xlsx(scheduleTable_new, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
      scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #refresh
      
      #disable options
      hide("admin_modify_confirm")
      disable("show_removed")
      disable("show_old")
      disable("eq_list_admin")
      disable("user_booking")
      disable("selected_booking")
      disable("admin_modify_booking_options")
      show("admin_back")
      
      #message
      output$conf_message_admin2 <- renderText({"Booking removed."})
    }else{
      #if modify booking
      # check slot availability
      avail_table <- availabilityTable_admin() %>% t()
      slot_available <- apply(avail_table, 1, function(x) !(x["Booked"]=="" | x["Current Selection"]=="")) %>% unlist() %>% sum()
      
      if(slot_available==0){
        #if book slot is available
        # note old booking
        scheduleTable_new <- scheduleTable
        old_booking_date <- c(scheduleTable_new$Start.date[book_index], scheduleTable_new$End.date[book_index]) %>%
          as.numeric() %>% chron(out.format=c(dates='d-m-y'))
        old_booking_time <- c(scheduleTable_new$Start.time[book_index], scheduleTable_new$End.time[book_index])
        old_book_marker <- if(old_booking_date[1]!=old_booking_date[2]){
          paste(old_booking_date[1], "-", old_booking_time[1], "~", old_booking_date[2], "-", old_booking_time[2], sep=" ")
        }else{
          paste(old_booking_date[1], "-", old_booking_time[1], "~", old_booking_time[2], sep=" ")
        }
        #get time stamp
        time_index <- Sys.time() %>% toString() %>% strsplit(split=" ")
        time_index <- paste(time_index[[1]][1:2], collapse=" ")
        
        #modify booking
        scheduleTable_new$Start.date[book_index] <- input$admin_start_date_modify
        scheduleTable_new$Start.time[book_index] <- input$admin_start_time_modify
        scheduleTable_new$End.date[book_index] <- input$admin_end_date_modify
        scheduleTable_new$End.time[book_index] <- input$admin_end_time_modify
        scheduleTable_new$Note[book_index] <- paste0("Modified from ", old_book_marker, "; by ", input$user_login, " at ", time_index,
                                                     "\n", scheduleTable_new$Note[book_index])
        
        #write
        write_xlsx(scheduleTable_new, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T)
        scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #refresh
        
        #disable options
        disable("admin_start_date_modify")
        disable("admin_start_time_modify")
        disable("admin_end_date_modify")
        disable("admin_end_time_modify")
        hide("admin_modify_confirm")
        disable("show_removed")
        disable("show_old")
        disable("eq_list_admin")
        disable("user_booking")
        disable("selected_booking")
        disable("admin_modify_booking_options")
        show("admin_back")
        
        #message
        output$conf_message_admin2 <- renderText({"Booking modified."})
      }else{
        output$error_message_admin <- renderText({"Slot not available!"})
        show("error_message_admin")
      }
    }
  })
  
  # updating output tables
  observeEvent((input$admin_modify_confirm | input$confirm_manage | input$confirm_book | input$login_book), {
    # Booking list
    userBooking_table <<- reactive({
      if(is.null(input$user_booking)){return(NULL)}else{
        #subset username's booking
        user_schedule <- if(input$user_booking!="All"){subset(scheduleTable, Username==input$user_booking)}else{scheduleTable}
        
        #subset equipment
        user_schedule <- if(input$eq_list_admin != "All"){subset(user_schedule, Equipment==input$eq_list_admin)}else{user_schedule}
        
        #show/no show removed bookings
        if(!input$show_removed){
          user_schedule <- user_schedule %>% mutate(Active = sapply(Note, function(x) !grepl("Removed", x))) %>%
            filter(Active) %>% dplyr::select(-Active)
        }
        
        #show/no show old bookings
        if(!input$show_old){
          current_date <- Sys.Date() %>% toString() %>% chron(format=c(dates='y-m-d')) %>% as.numeric()
          user_schedule <- user_schedule %>%
            mutate(endDate_filter = sapply(End.date, function(x) as.numeric(x) < current_date)) %>%
            filter(!endDate_filter)
        }
        
        #manage names
        user_schedule <- user_schedule %>% 
          mutate(Start.Period = apply(user_schedule, 1, function(x) chron(times=paste0(x["Start.time"], ":00"), format=c(dates="h:m:s"))+as.numeric(x["Start.date"])),
                 End.Period = apply(user_schedule, 1, function(x) chron(times=paste0(x["End.time"], ":00"), format=c(dates="h:m:s"))+as.numeric(x["End.date"]))) %>%
          mutate(Start.Period = sapply(Start.Period, function(x) toString(chron(x, out.format=c(dates='d/m/y', times="h:m:s")))),
                 End.Period = sapply(End.Period, function(x) toString(chron(x, out.format=c(dates='d/m/y', times="h:m:s")))))
        return(user_schedule)
      }
    })
    output$user_booking_table_admin <- renderDataTable({
      if(!is.null(userBooking_table())){
        if(nrow(userBooking_table())>0){
          dplyr::select(userBooking_table(), No, Username, Equipment, Start.Period, End.Period, Note)
        }else{NULL}
      }else{NULL}
    })
    
    # Availability table
    availabilityTable_admin <<- reactive({
      #re-read hard schedule
      scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)
      
      #get current period to show
      date_period_num <- c(input$admin_start_date_modify, input$admin_end_date_modify) %>% as.numeric()
      
      #create availability table
      date_list <- c(1:(date_period_num[2] - date_period_num[2] + 1))
      date_list <- sapply(date_list, function(x) toString(chron(date_period_num[1]+x-1, out.format='d-m-year')))
      column_names <- sapply(date_list, function(x) sapply(timeSlots, function(xi) paste(x, xi, sep=" - ")))
      
      #create availability table
      time_table <- cbind.data.frame(column_names, 
                                     unlist(sapply(date_list, function(x) replicate(length(timeSlots), x))),
                                     unlist(replicate(length(date_list), timeSlots)),
                                     replicate(length(column_names), ""),
                                     replicate(length(column_names), ""),
                                     replicate(length(column_names), ""))
      colnames(time_table) <- c("nameCol", "Date", "Time", "Booked", "OldBooking", "CurrentSelection")
      
      #read old bookings
      current_selection_no <- strsplit(input$selected_booking, split=".", fixed=T)[[1]][1] %>% as.numeric()
      current_bookings <- scheduleTable %>%
        mutate(Start.date = as.numeric(Start.date), End.date = as.numeric(End.date)) %>%
        filter((Start.date >= date_period_num[1] & Start.date <= date_period_num[2]) |
                 (End.date >= date_period_num[1] & End.date <= date_period_num[2])) %>%
        mutate(Active = sapply(Note, function(x) !grepl("Remove", x))) %>% filter(Active)
      current_bookings <- current_bookings %>%
        mutate(start_index = apply(current_bookings, 1, function(x){
          current_date <- chron(as.numeric(x["Start.date"]), out.format=c(dates='d-m-year'))
          current_name <- paste(current_date, x["Start.time"], sep=" - ")
          if(current_name %in% column_names){
            return(which(column_names==current_name))
          }else{
            return(1)
          }}),
          end_index = apply(current_bookings, 1, function(x){
            current_date <- chron(as.numeric(x["End.date"]), out.format=c(dates='d-m-year'))
            current_name <- paste(current_date, x["End.time"], sep=" - ")
            if(current_name %in% column_names){
              return(which(column_names==current_name))
            }else{
              return(length(column_names))
            }}))
      
      #index current selection
      selection_period <- data.frame(date = sapply(date_period_num, function(x){chron(x, out.format='d-m-year') %>% toString()}),
                                     time = c(input$admin_start_time_modify, input$admin_end_time_modify))
      selection_period$col_index <- apply(selection_period, 1, function(x) which(column_names==paste(x["date"], x["time"], sep=" - ")))
      
      #assign old booking slot
      old_booked_slots <- subset(current_bookings, No==current_selection_no)
      time_table$OldBooking[old_booked_slots$start_index:(old_booked_slots$end_index-1)] <- "Old Booking"
      
      #assign booked slots
      current_bookings <- subset(current_bookings, as.numeric(No) != current_selection_no) #remove old booking slots
      time_table$Booked <- sapply(c(1:nrow(time_table)), function(x){
        book_in_hour <- subset(current_bookings, start_index<=x & end_index>x)
        if(nrow(book_in_hour)==0){return("")}else{book_in_hour$Username}})
      
      #assign current selections
      time_table$CurrentSelection[selection_period$col_index[1]:(selection_period$col_index[2]-1)] <- "Selected"
      
      # select and reshape
      time_table <- time_table %>% dplyr::select(Booked, OldBooking, CurrentSelection) %>% t()
      colnames(time_table) <- column_names
      rownames(time_table) <- c("Booked", "Old Booking", "Current Selection")
      return(time_table)
    })
    output$time_avail_admin <- renderTable(availabilityTable_admin(), bordered=T, rownames=T)
    
    #  rendering selection input for booking number
    dropDown_admin <<- reactive({
      booking_list <- userBooking_table()
      booking_list <- booking_list %>%
        mutate(name = apply(booking_list, 1, function(x){
          if(x["Start.date"]==x["End.date"]){
            extension_time <- chron(as.numeric(x["Start.date"]), out.format='d/m/y') %>% toString()
            extension_time <- paste0(extension_time, " ", x["Start.time"], " ~ ", x["End.time"])
          }else{
            extension_time <- paste0(substring(x["Start.Period"], 2, 15), " ~ ", substring(x["End.Period"], 2, 15))
          }
          extension_time <- paste0(x["No"], ". ", x["Username"], "_", x["Equipment"], " - ", extension_time)
          return(extension_time)
        }))
      
      #exclude removed bookings from the dropdown menu
      booking_list <- booking_list %>%
        mutate(Active = sapply(Note, function(x) !grepl("Removed", x))) %>% filter(Active) %>% dplyr::select(-Active)
      return(booking_list)
    })
    output$booking_selection <- renderUI({
      selectInput("selected_booking", "Select booking to modify", choices=dropDown_admin()$name)
    })
    
    #     get original booking dates
    original_date_admin <- reactive({
      if(is.null(input$selected_booking)){NULL}
      
      current_item <- dropDown_admin() %>% filter(name==input$selected_booking)
      date_list <- c(current_item$Start.date, current_item$End.date) %>% 
        unlist() %>% as.numeric() %>% chron(out.format=c(dates="d-m-y"))
      time_list <- c(current_item$Start.time, current_item$End.time) %>% unlist()
      return(list(date_list, time_list))
    })
    admin_time_slot_range_modify <- reactive({
      min_index <- which(timeSlots == input$admin_start_time_modify)
      finish_time_slot <- if(min_index==length(timeSlots)){c()}else{timeSlots[(min_index+1):length(timeSlots)]}
      return(finish_time_slot)
    })
    output$admin_start_date_ui_modify <- renderUI({
      dateInput("admin_start_date_modify", "From", min = Sys.Date(), value=original_date_admin()[[1]][1]) #select start date
    })
    output$admin_start_time_ui_modify <- renderUI({
      selectInput("admin_start_time_modify", "", timeSlots, selected=original_date_admin()[[2]][1]) #select start time
    })
    output$admin_end_date_ui_modify <- renderUI({
      dateInput("admin_end_date_modify", "To", min=input$admin_start_date_modify, value=original_date_admin()[[1]][2])
    })
    output$admin_end_time_ui_modify <- renderUI({
      selectInput("admin_end_time_modify", "", admin_time_slot_range_modify(), selected=original_date_admin()[[2]][2])
    })
  })
  
  # Refresh input options
  observeEvent(input$admin_back, {
    enable("admin_start_date_modify")
    enable("admin_start_time_modify")
    enable("admin_end_date_modify")
    enable("admin_end_time_modify")
    show("admin_modify_confirm")
    enable("show_removed")
    enable("show_old")
    enable("eq_list_admin")
    enable("user_booking")
    enable("selected_booking")
    enable("admin_modify_booking_options")
    hide("admin_back")
  })
  
  # C | Equipment Manager
  
  # show/hide options
  hide("new_eq_name")
  observeEvent(input$eq_menu, {
    #re-read equipment list
    eq_table <- read.csv(paste0(mainDir, "/equipmentList.csv"), header=T, as.is=T) %>%
      mutate(Active = sapply(Comment, function(x) !grepl("Removed", x))) %>% filter(Active) %>% dplyr::select(-Active) #remove inactive books
    eq_list <- eq_table %>% dplyr::select(Equipment) %>% unlist()
    names(eq_list) <- eq_list
    
    if(input$eq_menu!="Add"){
      if(input$eq_menu=="Rename"){show("new_eq_name")}else{hide("new_eq_name")}
      output$equipment_selection_menu <- renderUI({selectInput("eq_to_modify", "Equipment", choices=eq_list)})
      show("eq_to_modify")
    }else{
      show("new_eq_name")
      hide("eq_to_modify")
    }
  })
  
  # execute changes
  observeEvent(input$eq_modify_confirm, {
    #re-read equipment list
    eq_table <- read.csv(paste0(mainDir, "/equipmentList.csv"), header=T, as.is=T)
    
    if(input$eq_menu == "Add"){
      #check equipment name
      pass_name <- !(input$new_eq_name %in% c(eq_table$Equipment, ""))
      
      if(pass_name){
        next_item <- data.frame(No=max(as.numeric(eq_table$No))+1,
                                Equipment=input$new_eq_name,
                                Comment = paste0("Created on ", toString(Sys.time())))
        
        eq_table <- rbind.data.frame(eq_table, next_item)
        write.csv(eq_table, paste0(mainDir, "/equipmentList.csv"), row.names = F)
        output$confMessage_admin <- renderText(paste("Equipment created :", input$new_eq_name, "\nRefresh page!"))
        
        #disable buttons
        hide("eq_modify_confirm")
        disable("eq_menu")
        disable("new_eq_name")
        disable(selector = '.navbar-nav a[data-value="New Booking"')
        disable(selector = '.navbar-nav a[data-value="Manage Bookings"')
      }else{
        output$confMessage_admin <- renderText("Invalid equipment name!")
      }
    }else if(input$eq_menu == "Rename"){
      #check equipment name
      pass_name <- !(input$new_eq_name %in% c(eq_table$Equipment, ""))
      
      if(pass_name){
        #change in equipment list
        adder <- if(!is.na(eq_table$Comment[eq_table$Equipment==input$eq_to_modify])){eq_table$Comment[eq_table$Equipment==input$eq_to_modify]}else{""}
        eq_table$Comment[eq_table$Equipment==input$eq_to_modify] <- paste0(adder, "\nRenamed from ",
                                                                           input$eq_to_modify, " by ", input$user_login, " on ", toString(Sys.time()))
        eq_table$Equipment[eq_table$Equipment==input$eq_to_modify] <- input$new_eq_name
        
        write.csv(eq_table, paste0(mainDir, "/equipmentList.csv"), row.names = F)
        output$confMessage_admin <- renderText(paste(input$eq_to_modify, "successfully renamed to", input$new_eq_name, "\nRefresh page!"))
        
        #rename in schedule table
        scheduleTable_current <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) 
        scheduleTable_current$Note <- apply(scheduleTable_current, 1, function(x){
          if(x["Equipment"] == input$eq_to_modify){
            if(is.na(x["Note"])){adder <- ""}else{adder <- x["Note"]}
            return(paste0(adder, "|| Equipment renamed from ", input$eq_to_modify, " by ", input$user_login, " on ", toString(Sys.time())))
          }else{x["Note"]}
        }) 
        scheduleTable_current$Equipment[scheduleTable$Equipment==input$eq_to_modify] <- input$new_eq_name
        write_xlsx(scheduleTable_current, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T) #write
        scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #refresh read
        
        #disable buttons
        hide("eq_modify_confirm")
        disable("eq_menu")
        disable("new_eq_name")
        disable(selector = '.navbar-nav a[data-value="New Booking"')
        disable(selector = '.navbar-nav a[data-value="Manage Bookings"')
      }else{
        output$confMessage_admin <- renderText("Invalid equipment name!")
      }
    }else{
      # if remove
      eq_table$Comment[eq_table$Equipment==input$eq_to_modify] <- paste0(eq_table$Comment[eq_table$Equipment==input$eq_to_modify], "|| Removed by ",
                                                                         input$user_login, " on ", toString(Sys.time()))
      write.csv(eq_table, paste0(mainDir, "/equipmentList.csv"), row.names = F)
      output$confMessage_admin <- renderText(paste(input$eq_to_modify, " removed!\nRefresh page!"))
      
      #remove in schedule table
      scheduleTable_current <- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1)
      scheduleTable_current$Note <- apply(scheduleTable_current, 1, function(x){
        if(x["Equipment"] == input$eq_to_modify){
          if(is.na(x["Note"])){adder <- ""}else{adder <- x["Note"]}
          return(paste0(adder, "\n Equipment Removed by ", input$user_login, " on ", toString(Sys.time())))
        }else{x["Note"]}
      }) 
      write_xlsx(scheduleTable_current, path=paste0(mainDir, "/", scheduleTable_dir), col_names=T) #write
      scheduleTable <<- read_excel(paste0(mainDir, "/", scheduleTable_dir), sheet=1) #refresh read
      
      #disable buttons
      hide("eq_modify_confirm")
      disable("eq_menu")
      disable(selector = '.navbar-nav a[data-value="New Booking"')
      disable(selector = '.navbar-nav a[data-value="Manage Bookings"')
    }
  })
})