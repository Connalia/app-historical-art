dat <- data.frame(
  country = c('China', 'China'),                                             
  flag = c('<img src="https://www.arc.ritsumei.ac.jp/archive01/theater/image/PB/arc/Prints/arcUP/arcUP1662.jpg" width="100" height="80"></img>',
           '<img src="https://www.arc.ritsumei.ac.jp/archive01/theater/image/PB/arc/Prints/arcUP/arcUP1662.jpg" width="100" height="80"></img>'
           ),                                                    
  infolink = c('https://en.wikipedia.org/wiki/United_States', 'https://en.wikipedia.org/wiki/China'),
  stringsAsFactors = FALSE)

library(shiny)
# UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Image Hyperlinking Test"),
  
  mainPanel(
    DT::dataTableOutput("imagetest")#, href=dat$infolink, target="_blank")
  )
)

# Server for application
server <- function(input, output) {
  hlink <- apply(dat,1, function(x){
    as.character(a(HTML(x[["flag"]]), href=x[["infolink"]], target="_blank"))
  })      
  
  dat$link <- hlink
  output$imagetest <- DT::renderDataTable({
    DT::datatable(dat, escape = FALSE)
  }) 
}

shinyApp(ui = ui, server = server)