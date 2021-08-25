library(shiny)
library(emayili)
shinyServer(function(input, output) {
    observeEvent(input$Send, {
        email <- envelope() %>%
            from("cvh.lab.server@gmail.com") %>%
            to("sebastian.tandar@gmail.com") %>%
            subject("Testing_emayili") %>%
            text("Test_text_send")
        
        smtp <- server(host='smtp.gmail.com',
                       port=465,
                       username='cvh.lab.server@gmail.com',
                       password='lacdr2021')
        
        smtp(email, verbose=T)
        
    })
})
