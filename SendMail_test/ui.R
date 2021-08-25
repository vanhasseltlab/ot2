
library(shiny)

shinyUI(fluidPage(
    textInput("email_address", "Send message to:"),
    actionButton('send', "Send")
))
