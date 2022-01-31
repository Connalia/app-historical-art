library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

coded_text <- character(0)


# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- df_loc[sample.int(nrow(df_loc), 1000),]
# By ordering by Frequence, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$Frequence),]

###########################################################
highlight <- function(text, search) {
  x <- unlist(strsplit(text, split = " ", fixed = T))
  x[tolower(x) == tolower(search)] <- paste0("<mark>", x[tolower(x) == tolower(search)], "</mark>")
  paste(x, collapse = " ")
}
###########################################################

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
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Frequence >= input$minScore,
        Frequence <= input$maxScore
      )#%>%
    #  mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    #DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
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
  output$imagetest <- DT::renderDataTable({
    DT::datatable(dat, escape = FALSE,
                  options = list(searchHighlight = TRUE, search = list(search = input$search)))
  }) 
  
  ## Prints Tagging ###########################################
  
  coded <- eventReactive(input$code1, {
    coded_text <<- c(coded_text, input$mydata)
    coded_text
  })
  
  output$selected_text <- renderPrint({
    coded()
  })

  #Print Table info
  output$imagetag <- DT::renderDataTable({
    DT::datatable(dat, escape = FALSE,
                  options = list(searchHighlight = TRUE, search = list(search = input$search)))
  }) 
  
  
}
