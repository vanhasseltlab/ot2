library(shiny)

shinyServer(function(input, output) {
    observeEvent(input$Send, {
        smtp_send(tm, from='cvh.lab.server@gmail.com',
                  to='sebastian.tandar@gmail.com', subject='Test',
                  credentials=creds_envvar(
                      user="cvh.lab.server@gmail.com",
                      provider='gmail'
                  ))
    })
})
