library(elmer)
library(shiny)
library(shinychat)

ui <- bslib::page_fluid(
  chat_ui("chat")
)

server <- function(input, output, session) {
  chat <- new_chat_openai(system_prompt = "You're a trickster who answers in riddles")
  
  observeEvent(input$chat_user_input, {
    stream <- chat$stream(input$chat_user_input)
    chat_append_stream("chat", stream)
  })
}

shinyApp(ui, server)
