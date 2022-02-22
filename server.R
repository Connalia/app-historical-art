library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(xlsx)
library(glue)
library(shinydashboard)
library(data.table)
library(DT)

coded_text <- character(0)

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

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- df_loc[sample.int(nrow(df_loc), 1000),]
# By ordering by Frequence, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$Frequence),]

temp = c('') 

function(input, output, session) {

  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 138.25, lat = 36.20, zoom = 5)
    
  })
  
  ##############
  
  output$hist<- renderPlot({
    
    n = 10
    
    # Basic Barplot
    my_bar <- barplot(head(df_loc$Frequence,n=n) , border=F , names.arg=head(df_loc$Locations_Translate,n=n), 
                      las=3 , 
                      ylim=c(0,5000), 
                      #col = '#00DD00',
                      main="" )
    
    # Add abline
    #abline(v=c(4.9 , 9.7) , col="grey")
    title(main="The 10 most frequent locations", ylab="Frequent")
    # Add the text 
    text(my_bar, 
         head(df_loc$Frequence,n=n), 
         head(df_loc$Frequence,n=n),
         #paste("n: ", head(df_loc$Frequence,n=5), sep="") ,
         cex=0.7, pos=3) 
    
    #Legende
    #legend("topleft", legend = c("Alone","with Himself","With other genotype" ) , 
    #       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
    #       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))
  })
  
  ##############
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    #colorBy <- input$color
    
    #if (colorBy == "superzip") {
    #  # Color and palette are treated specially in the "superzip" case, because
    #  # the values are categorical instead of continuous.
    colorData <- ifelse(zipdata$Frequence >= input$threshold, "yes", "no")
    pal <- colorFactor("viridis", colorData)
    #} else {
    #  colorData <- zipdata[[colorBy]]
    #  pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    #}
    
    # Radius is treated specially in the "superzip" case.
    radius <- ifelse(zipdata$Frequence >= input$threshold, sqrt(zipdata$Frequence) * 100, 0)
      
    
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~Longitude, ~Latitude, 
                 radius=radius, layerId=~Location,
                 stroke=FALSE, fillOpacity=0.4,
                 #fillColor=pal(colorData)
                 ) 
    #%>% addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,layerId="colorLegend")
  })
  
  ##############
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- df_loc[df_loc$Location == zipcode,]
    content <- as.character(tagList(
      tags$h4("Frequence of Location:", as.integer(selectedZip$Frequence)),
      #tags$strong(HTML(sprintf("%s, %s %s",
      #                         selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      #))), tags$br(),
      sprintf("Location (JAP): %s", selectedZip$Location), tags$br(),
      sprintf("Location (EN): %s", selectedZip$Locations_Translate)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  ##############
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  ##############
  
  ## Data Explorer ###########################################
  
  output$ziptable <-  renderDataTable({
    df <- cleantable %>%
      filter(
        Frequence >= input$minScore,
        Frequence <= input$maxScore
      )#%>%
    #  mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <-  dataTableAjax(session, df, outputId = "ziptable")
    
    # datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  ## Print Explorer ###########################################
  
  # autocomple
  observe({
    updateSelectInput(session, 'search1', selected = input$search)
  }) 
  observe({
    updateSelectInput(session, 'search', selected = input$search1)
  }) 
  
  #output$text1 <- renderText({ paste("hello input is","<font color=\"#FF0000\"><b>", 'input$n', "</b></font>") })
  
  #Print Table info
  output$imagetest <-  renderDataTable({
     datatable(dat, escape = FALSE, #rownames=FALSE,
                  options = list(searchHighlight = TRUE, search = list(search = input$search)))
  }) 
  
  ## Check Prints ###########################################
  
  # Initialize reactive values
  rv <- reactiveValues(prev_bins = NULL)
  
  # Append new value to previous values when checkbox changes 
  observeEvent(input$checked_rows, {
    temp <- c(rv$prev_bins, input$checked_rows)
    rv$prev_bins <- temp[!duplicated(temp)]
    rv$prev_bins <- sort(rv$prev_bins)
  })
  
  # Output
  output$value1 <- renderText({
    paste(rv$prev_bins, collapse = ",")
  })

  
  #output$value1 <- renderPrint({input$checked_rows})
  
  # Save the ids of wrong labels of print when press button save 
  observeEvent(input$save, {
    write.xlsx(rv$prev_bins, "wrong_ids_labels.xlsx")
  })
  
  
  #Print Table info
  output$tablereann <-  renderDataTable({
    
    dat[["Select False"]]<-glue::glue('<input type="checkbox" name="selected" value="{1:nrow(dat)}"><br>')
     datatable(dat,escape=FALSE,  #class = 'cell-border compact', #rownames=FALSE,
              options=list(ordering=T,autowidth=F,scrollX = TRUE,
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              selection="none"
    )

  }) 
  
  
  ## Reannotate Explorer ###########################################
  
  output$MainBody<-renderUI({
    fluidPage(
      box(width=12,
          h3(strong("Reannotate ukiyo-e titles"),align="center"),
          hr(),
          
          column(12,dataTableOutput("Main_table")),

          tags$script("$(document).on('click', '#Main_table button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())
  });")
          
      )
    )
  })
  
  
  output$Main_table<-renderDataTable({
    DT=dat
    
    DT[["New Annotations"]]<- "-"
    
    DT[["Actions"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(dat),'>Annotate</button>
             </div>
             
             ')
    datatable(DT,
              escape=F)}
  )
  
        ## A] Managing in row deletion <-------
          
          modal_modify<-modalDialog(
            fluidPage(
              
              h3(strong("Labeling modification"),align="center"),
              hr(),
              
              # dataTableOutput('row_modif'),
              uiOutput('row_modif', align="center"),
              
              br(), br(),
              
              sidebarLayout(
                sidebarPanel(
                  textInput("txtInput", "Input the place"),
                  actionButton("store", "Store")
                ),                                          
                uiOutput("valuePlace"),
                
              ),
              
              actionButton("save_changes","Save annotations"),
        
                
              #############################################################

              #      tags$script(HTML("$(document).on('click', '#save_changes', function () {
              #var list_value=[]
              #for (i = 0; i < $( '.new_input' ).length; i++)
              #                       {
              #                          list_value.push($( '.new_input' )[i].value)
              #                       }
              #Shiny.onInputChange('newValue', list_value)
              #  });"))
              #############################################################
        
        
            ),
            size="l", 
            footer = modalButton("Close")
          )
          
          
        ### Table auto complete from user Input with button Store  ###
          
          observeEvent(input$lastClick,
                       {
                         if (input$lastClickId%like%"modify"){
                           showModal(modal_modify)
                         }
                       }
          )
        
          
        ### Print Title with the tags from the row we press button  ###
          
          output$row_modif<-renderUI({  
            HTML(dat$Marked[as.numeric(gsub("modify_","",input$lastClickId))])
          })
          
          
        #########################################
          
          # Initialize reactive values
          rv2 <- reactiveValues(prev_bins = NULL)
          
          # table for save id title and tag
          rv <- reactiveValues(dfnew=data.frame(matrix(ncol = 2, nrow = 0)) ,count=1)
          
          # Press store
          observeEvent(input$store, {
            if(nchar(input$txtInput) > 0)  { # If user write input (anything), except nan
                temp <- c(rv2$prev_bins, input$txtInput)
                #rv2$prev_bins <- temp[!duplicated(temp)]
                rv2$prev_bins <- temp
                
                rv$dfnew <- rbind(rv$dfnew, df_tags()) # table for save id title and tag
            
                
                ###################### Table with New Annotations update ###################### 
                #print('rv$dfnew')
                #print(rv$dfnew)
                
                group_and_concat <- rv$dfnew %>%
                  select(value, id_table) %>% 
                  group_by(id_table) %>%
                  summarise(all_names = paste(value, collapse = " | "))
                group_and_concat <- group_and_concat[!duplicated(group_and_concat)]
                
                #print("group_and_concat")
                #print(group_and_concat)
                
                output$Main_table<-renderDataTable({
                          DT=dat
                          DT[["New Annotations"]]<- "-"

                          for(i in 1:nrow(group_and_concat)){
                            row_update<-strtoi(group_and_concat[i,'id_table'])
                            tag_update<-group_and_concat[i,'all_names']
                            
                            DT[["New Annotations"]][row_update]<- tag_update
                          }

                          DT[["Actions"]]<-
                            paste0('<div class="btn-group" role="group" aria-label="Basic example">
                                    <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(dat),'>Annotate</button>
                                    </div>')
                          datatable(DT,escape=F
                        )})
            } else {
            }
          })
          
        
          df_tags <- reactive({
            data.frame(
              value = input$txtInput,
              id_table = as.numeric(gsub("modify_","",input$lastClickId)) #input$lastClickId: Table id
            )
          })
          
        
          # Output
          output$valuePlace <- renderUI({ 
            paste(rv2$prev_bins, collapse = ", ")
          })
          
          #output$value1 <- renderPrint({input$checked_rows})
          
          # Save the ids of wrong labels of print when press button save 
          observeEvent(input$save_changes, {
            write.xlsx(rv$dfnew, "wrong_labels_per_row.xlsx")
            
            group_labes <- rv$dfnew %>%
              select(value, id_table) %>% 
              group_by(id_table) %>%
              summarise(all_names = paste(value, collapse = " | "))
            
            write.xlsx(group_labes, "wrong_labels.xlsx")
          })
  

  
}
