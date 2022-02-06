library(leaflet)
library(shinydashboard)

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

  tabPanel("Location explorer",
  
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
  
  tabPanel("Check Prints",
           
           
           fluidPage(
             # Application title
             #titlePanel("Ukiyo-e Prints Informations"),
             
             h4("Select title with the wrong annotation (check the box):"), 
             
             #verbatimTextOutput("checked_rows"),
             verbatimTextOutput("value1"),
             #textInput("inText", "Wrong print ids"),
             #textOutput("text"),
             
             h4("When you finish save your work (press the save button):"), 
             
             actionButton("save", "Save wrongs"),
             
             br(),
             br(),
             
             column(12,
                    DT::dataTableOutput("tablereann"),  tags$script(HTML('$(document).on("click", "input", 
                                                          function () {
                                                             var checkboxes = document.getElementsByName("selected");
                                                             var checkboxesChecked = [];
                                                             for (var i=0; i<checkboxes.length; i++) {
                                                                if (checkboxes[i].checked) {
                                                                    checkboxesChecked.push(checkboxes[i].value);
                                                                  }
                                                             }
                                                             Shiny.onInputChange("checked_rows",checkboxesChecked);  
                                                           })'))
             ),
             
             
             
             
           )
           
           
  ),
  
  #################################
  
  tabPanel("Reannotate Prints",
           
           dashboardPage(dashboardHeader(disable = T),
                         dashboardSidebar(disable = T),
                         dashboardBody(uiOutput("MainBody"))

           )
           
           
  ),
  
  tabPanel("Info",
           
           fluidPage(
             
             h2("Introduction"),
             
             h4("This paper investigates the application of Natural Language 
Processing as a means to study the relationship between
topography and its visual renderings in early modern Japanese ukiyo-e landscape prints. 
We introduce a new dataset with titles of landscape prints 
that have been annotated by an art historian for any included place-names. 
The prints are hosted by the digital database of the Art Research Center at 
the Ritsumeikan University, Kyoto, one of the hubs of Digital Humanities in
Japan. By applying, calibrating and assessing a Named Entity Recognition (NER) tool, 
we argue that 'distant viewing' or macroanalysis of visual datasets can 
be facilitated, which is needed to assist art historical studies of this rich, complex and
diverse research material. "), 
             
             h2("Publications"),
             
             h4("M. Chatzipanagiotou, E.Machotka and J.Pavlopoulos. 'Automated recognition of geographical named entities in titles of Ukiyo-e prints', Workshop (publiced)"),
             
             h4("K. Liagkou, J.Pavlopoulos and E.Machotka. 'A Study of Distant 	Viewing of ukiyo-e prints', 13th Edition of its Language 		Resources and Evaluation Conference. 	
             (submitted)"),
             

             
           )
           
           
  ),

  conditionalPanel("false", icon("crosshair"))
)
