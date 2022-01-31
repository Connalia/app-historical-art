library(leaflet)

highlight <- '
                function getSelectionText() {
var text = "";
if (window.getSelection) {
text = window.getSelection().toString();
} else if (document.selection) {
text = document.selection.createRange().text;
}
return text;
}

document.onmouseup = document.onkeyup = document.onselectionchange = function() {
var selection = getSelectionText();
Shiny.onInputChange("mydata", selection);
};
'

#######################################################

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Frequence score" = "Frequence"
)


navbarPage("Ukiyo-e Prints", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Location Explorer"),

        #selectInput("color", "Color", vars),
        
        numericInput("threshold", "Frequent Threshold (over n appears)", 5),
    
        #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
        #  # Only prompt for threshold when coloring or sizing by superzip
        #  numericInput("threshold", "Threshold (top n percentile)", 5)
        #),
        
        #plotOutput("scatterCollegeIncome", height = 250),

        plotOutput("hist", height = 200)
        
        
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  tabPanel("Locations explorer",
  
    fluidRow(
      column(3,
        numericInput("minScore", "Min Frequent", min=0, max=1000, value=0)
      ),
      column(3,
        numericInput("maxScore", "Max Frequent", min=0, max=4000, value=800)
      ),
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),
  
  tabPanel("Prints explorer",
           fluidPage(
             column(3,
                    selectInput("search", "Search Place (JAP)", 
                                c("All places"="", structure(df_loc$Location, names=df_loc$Location))),
                    br(), br(),
                    htmlOutput("some_text")
             ),
             column(3,
                    selectInput("search1", "Place Search (EN)", 
                                c("All places"="", structure(df_loc$Location, names=df_loc$Locations_Translate)))
             ),
             
           ),
           
           fluidPage(
             
             # Application title
             #titlePanel("Ukiyo-e Prints Informations"),
             
             
             #mainPanel(
             DT::dataTableOutput("imagetest")#, href=dat$infolink, target="_blank")
             #)
           )
           
           
  ),
  
  tabPanel("Prints tagging",
           
           
           fluidPage(
             
             # Application title
             #titlePanel("Tagging"),
             
             tags$script(highlight),
             tags$h1("Coding options"),
             actionButton("code1", "Assign selected text"),
             tags$h1("Code1 output"),
             verbatimTextOutput("selected_text"),
             
             
             #mainPanel(
             DT::dataTableOutput("imagetag")#, href=dat$infolink, target="_blank")
             #)
           )
           
           
  ),

  #conditionalPanel("false", icon("crosshair"))
)
