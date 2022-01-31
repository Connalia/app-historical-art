ui <- fluidPage(
  htmlOutput("picture")
)

server <- function(input, output) {
  
  src = "https://www.arc.ritsumei.ac.jp/archive01/theater/image/PB/arc/Prints/arcUP/arcUP1662.jpg"
  output$picture<-renderText({c('<img src="',src,'">')})
  
}

runApp(list(ui = ui, server = server))