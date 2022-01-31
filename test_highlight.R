library(shiny)

highlight <- function(text, search) {
  x <- unlist(strsplit(text, split = " ", fixed = T))
  x[tolower(x) == tolower(search)] <- paste0("<mark>", x[tolower(x) == tolower(search)], "</mark>")
  paste(x, collapse = " ")
}

shinyApp(
  ui = fluidPage(
    textInput("search", "Search"),
    br(), br(),
    htmlOutput("some_text")
  ),
  server = function(input, output, session) {
    output$some_text <- renderText({
      highlight("Author: Albert Einstein<br/>Quote: The greatest mistake you can make in life is to be continually fearing you will make one", input$search)
    })
  }
)