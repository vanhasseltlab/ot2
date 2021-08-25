library(shiny)
library(emayili, lib.loc="/home/sebastian/R/x86_64-pc-linux-gnu-library/4.1/")
shinyServer(function(input, output) {
    observeEvent(input$Send, {
        email <- envelope() %>%
            from("cvh.lab.server@gmail.com") %>%
            to(input$email_address) %>%
            subject(input$subject) %>%
            text("Test send")
        
        smtp <- server(host='smtp.gmail.com',
                       port=465,
                       username='cvh.lab.server@gmail.com',
                       password='lacdr2021')
        
        smtp(email, verbose=T)
        
    })
})
