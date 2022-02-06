library(shiny)

shinyApp(
  ui = fluidPage(
    fluidRow(
      verbatimTextOutput("value1"),
      column(12,
             DT::dataTableOutput('table'),  tags$script(HTML('$(document).on("click", "input", function () {
                                                             var checkboxes = document.getElementsByName("selected");
                                                             var checkboxesChecked = [];
                                                             for (var i=0; i<checkboxes.length; i++) {
                                                             if (checkboxes[i].checked) {
                                                             checkboxesChecked.push(checkboxes[i].value);
                                                             }
                                                             }
                                                             Shiny.onInputChange("checked_rows",checkboxesChecked);  })'))
      ))),
  server = function(input, output) {
    library(DT)
    library(glue)
    output$value1 <- renderPrint({ input$checked_rows }) 
    
    output$table <- DT::renderDataTable({
      iris$checked<-''
      iris$checked[2:5]<-'checked=\"checked\"'
      iris[["Select"]]<-glue::glue('<input type="checkbox" name="selected" {iris$checked} value="{1:nrow(iris)}"><br>')
      datatable(iris,escape=F,rownames=F,  class = 'cell-border compact', 
                options=list(ordering=T,autowidth=F,scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
                ),
                selection="none"
      ) })})
