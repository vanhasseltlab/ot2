
library(shiny)

shinyUI(fluidPage(
    textInput("email_address", "Send message to:"),
    textInput("subject", "Email Subject"),
    actionButton('send', "Send")
))
