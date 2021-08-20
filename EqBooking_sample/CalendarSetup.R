#LIBRARIES--------
library(ggplot2)
library(chron)
nextMonth <- function(current, n_month=0){
  current <- toString(current)
  target <- substring(current, 4, 5) %>% as.numeric()
  target <- (target + n_month) %>% toString()
  if(nchar(target)==1){paste0("0", target)}
  
  current <- paste0(substring(current, 1, 3), target, substring(current, 6, 8)) %>% chron(format=c(dates='y-m-d'))
  return(current)
}
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}
dateCorrection <- function(date_coordinate, day_list, add_month=0){
  #get today's date
  today <- Sys.Date() %>% toString() %>% chron(format=c(dates="y-m-d"))
  first_day <- paste0(substring(toString(Sys.Date()), 1, 8), "01") %>% 
    chron(format=c(dates="y-m-d")) %>% nextMonth(n_month=add_month)
 
  back_correction <- (which(unique(day_list)==toString(weekdays(first_day))) - 1) * -1  #calculate calendar back correction
  
  #apply correction
  date_coordinate$actual_date <- (seq(back_correction, 34 + back_correction, 1) + first_day)
  date_coordinate$date_num <- date_coordinate$actual_date %>% days()
  date_coordinate$month <- date_coordinate$actual_date %>% months() 
  date_coordinate <- date_coordinate %>% 
    mutate(date_day = sapply(actual_date, function(x) toString(weekdays(x))))
  date_coordinate$month_color <- apply(date_coordinate, 1, 
                                       function(x) if(x["month"]==months(first_day)){"black"}else{"gray"})
  date_coordinate$month_color[date_coordinate$date_day %in% c("Sat", "Sun")] <- "red"
  
  return(list(date_coordinate, as.Date(first_day)))
}
equipmentAvails <- function(eq_name, schedule_table, calendar_table){
  #get time period
  period <- c(min(calendar_table$actual_date), max(calendar_table$actual_date))
  
  #subset current equipment within relevant date
  eq_table <- subset(schedule_table, Equipment==eq_name) %>%
    mutate(Start.date = chron(as.numeric(Start.date), out.format='y-m-d'),
           End.date = chron(as.numeric(End.date), out.format='y-m-d'),
           Active = !grepl("Removed", Note)) %>%
    filter(((Start.date  >= period[1] & Start.date <= period[2]) |
              (End.date >= period[1] & End.date <= period[2])) & Active)
  
  if(nrow(eq_table)==0){
    calendar_table$Availability <- 0
    use_table <- NULL
  }else{
    #get used time in hours
    use_table <- apply(eq_table, 1, function(x){
      if(x["Start.date"]==x["End.date"]){
        use_period <- c(x["Start.date"], which(timeSlots==x["End.time"])-which(timeSlots==x["Start.time"]))
      }else{
        use_period <- rbind(c(x["Start.date"], length(timeSlots)-which(timeSlots==x["Start.time"])),
                            c(x["End.date"], which(timeSlots==x["End.time"])-1),
                            replicate((x["End.date"]-x["Start.date"]-1), length(timeSlots)-1))
      }}) %>% as.vector() %>% matrix(nrow=2) %>% t() %>% data.frame()
    colnames(use_table) <- c("Date", "Availability")
    
    use_table <- use_table %>% mutate(Date = sapply(Date, function(x) chron(x, format=c(dates='y-m-d'), out.format=c(dates="y-m-d"))),
                                      Availability = as.numeric(Availability)/length(timeSlots))
    
    #get sum of usage
    calendar_table$Availability <- vapply(calendar_table$actual_date, FUN.VALUE=1,
                                          function(x) sum(subset(use_table, Date==as.numeric(x))$Availability))
  }
  
  return(list(calendar_table, use_table))
}

#CORE FUNCTION------------
createCalendar <- function(equipment_name, all_schedule, add_month=0){
  #Setup Calendar
  days <- data.frame(day_name = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                     location_x = seq(2.5, 32.5, 5),
                     location_y = replicate(7, 26))
  
  #Date
  dateCoordinate <- data.frame(date_num = seq(1, 35, 1),
                               location_x = as.vector(replicate(5, seq(0.75, 30.75, 5))),
                               location_y = as.vector(sapply(seq(24, 4, -5), function(x) replicate(7, x))),
                               border_x = as.vector(replicate(5, seq(0, 30, 5))),
                               border_y = as.vector(sapply(seq(25, 5, -5), function(x) replicate(7, x))))
  
  #   Applying date correction based on today's date
  dateCoordinate <- dateCorrection(dateCoordinate, days, add_month)
  plot_name <- paste0(format(dateCoordinate[[2]], "%B"), "  ", format(dateCoordinate[[2]], "%Y"))
  dateCoordinate <- equipmentAvails(equipment_name, all_schedule, dateCoordinate[[1]])
  use_table <- dateCoordinate[[2]]
  dateCoordinate <- dateCoordinate[[1]]
  
  #   create ggplot
  output_plot <- ggplot()+xlim(0, 35)+ylim(0, 26)+theme_bw()+
    geom_hline(yintercept = seq(0, 25, 5))+
    geom_vline(xintercept = seq(0, 35, 5))+
    ggtitle(plot_name,
            subtitle = "Double-click to select date")+
    geom_text(data=days, aes(x=location_x, y=location_y, label=day_name), size=10)+
    geom_text(data=dateCoordinate, aes(x=location_x, y=location_y, label=date_num,
                                       color = month_color), fontface='bold', size=10)+
    geom_text(data=dateCoordinate, aes(x=location_x+2, y=location_y-1.7, 
                                       label=scales::percent(round(Availability, 2)),
                                       color = month_color), size=9)+
    scale_color_manual(values=c("black", "gray49", "red"))+
    theme(legend.position="none",
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size=35, face='bold', hjust = 0.5),
          plot.subtitle = element_text(hjust=0.5))
  
  #return
  return(list(output_plot, dateCoordinate, use_table))
}