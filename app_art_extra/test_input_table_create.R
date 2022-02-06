library(shiny)


ui <- fluidPage(
  titlePanel("TRY-1"),
  sidebarLayout(
    sidebarPanel(
      textInput("txtInput", "Input to Display"),
      actionButton("store", "Store value in dataframe")
    ),                                          
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- shinyServer(function(input, output) {
  
  
  
  rv <- reactiveValues(dfnew=data.frame(matrix(ncol = 2, nrow = 0)) ,count=1)
  
  storedvalues <- observeEvent(input$store, {
    if(nchar(input$txtInput) > 0)  {
      rv$dfnew <- rbind(rv$dfnew, df())
      rv$count = rv$count + 1
    } else {
    }
  })
  
  df <- reactive({
    data.frame(
      id = rv$count,
      value = input$txtInput
    )
  })
  
  output$table <- renderTable({
    rv$dfnew
  })
  
  
})

shinyApp(ui = ui, server = server)
