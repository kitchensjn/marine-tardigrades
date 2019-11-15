# This file acts as the backend of the shiny application, included filtering functions and map display

library(gsheet)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)


# Filter data based on ID
filterByIDFunction <- function(df, i){
  if(!is.null(i[["IDmin"]]) && !is.null(i[["IDmax"]])){
    return(df[which(df$ID>=i[["IDmin"]]&df$ID<=i[["IDmax"]]),])
  }else{
    return(df)
  }
}

# Filter data based on other filters
filterFunction <- function(df, i, columns){
  idFiltered.df <- filterByIDFunction(df=df, i=i)
  dataToShow.df <- data.frame() #setNames(data.frame(matrix(ncol = length(colnames(idFiltered.df)), nrow = 0)), colnames(idFiltered.df))
  for (columnName in columns) { # Loops over each filter column
    if (!is.null(i[[columnName]])) {
      dataToShow.df <- rbind(dataToShow.df, idFiltered.df[which(idFiltered.df[[columnName]] %in% i[[columnName]]),])
    }
  }
  if (nrow(dataToShow.df) > 0){
    finalDataToShow.df <- distinct(dataToShow.df, ID, .keep_all=TRUE)
    return(finalDataToShow.df)
  }else{
    return(idFiltered.df)
  }
}

# Setup vector of filters, OceanSea is separated to add '/'
inputCreatorFunction <- function(x, df){
  if (x == "OceanSea"){
    filterColumn.input <- selectInput(inputId = x,
                                      label = "Ocean/Sea",
                                      choices = c(sort(unique(df[[x]]))),
                                      multiple = TRUE) 
  }else{
    filterColumn.input <- selectInput(inputId = x,
                                      label = x,
                                      choices = c(sort(unique(df[[x]]))),
                                      multiple = TRUE) 
  }
  return(filterColumn.input)
}

# backend of the webpage
server <- function(input, output, session) {
  googleSheetsUrl <- reactive({
    'https://docs.google.com/spreadsheets/d/13vViHqxkp2wqjef5DRhahf2iA0-bABFmpfQimvWeks0'
  })
  
  updated.df <- reactive({ # Allows for data.frame to update with spreadsheet
    invalidateLater(600000,session=session) # Waits 10min before requesting sheet again
    tardigrade.df <- gsheet2tbl(googleSheetsUrl())
    tardigrade.df
  })
  
  output$fileLocation <- renderText({
    paste("Google Sheets File URL:", googleSheetsUrl())
  })
  
  output$citation <- renderText({
      "Data compiled by Dr. Paul Bartels. Website designed by James Kitchens."
  })
  
  
  filterColumns.vec <- reactive({
    c("Family", "Genus", "Species", "OceanSea", "Country", "Depth", "Authority") # Columns to be included in filters [colnames(updated.df())]
  })
  
  output$idTextMin <- renderUI({
    tagList(
      numericInput(inputId="IDmin", label="ID min", value=min(updated.df()$ID), min=min(updated.df()$ID), max=max(updated.df()$ID))
    )
  })
  
  output$idTextMax <- renderUI({
    tagList(
      numericInput(inputId="IDmax", label="ID max", value=max(updated.df()$ID), min=min(updated.df()$ID), max=max(updated.df()$ID))
      
    )
  })
  
  output$createUI <- renderUI({ # Renders the filter UI, updates the available filters with spreadsheet
    tagList(  # Can filter by various columns
      lapply(filterColumns.vec(), FUN=inputCreatorFunction, df=updated.df()) # Vector of filters
    )
  })
  
  observe({ # Somewhat cumbersome way of updating the filters dynamically in correspondence to other filters
    filteredData.df <- filterByIDFunction(df=updated.df(), i=input)
    for (columnName in filterColumns.vec()){
      updateSelectInput(session = session,
                        inputId = columnName,
                        choices = c(sort(unique(filteredData.df[[columnName]])))) 
    }
  })
  
  observeEvent(input$clearFilters, {
    updateNumericInput(session = session,
                       inputId = "IDmin",
                       value = min(updated.df()$ID))
    updateNumericInput(session = session,
                       inputId = "IDmax",
                       value = max(updated.df()$ID))
    for (columnName in filterColumns.vec()){
      updateSelectInput(session = session,
                        inputId = columnName,
                        choices = c(sort(unique(updated.df()[[columnName]]))),
                        selected = NULL)
    }
  })

  mapPoints.df <- reactive({
    mainMap.df <- filterFunction(df=updated.df(), i=input, columns=filterColumns.vec()) # Points in center of screen
    leftMap.df <- mainMap.df
    leftMap.df$Lon <- leftMap.df$Lon - 360 # Points for left of screen (appear when panning)
    rightMap.df <- mainMap.df
    rightMap.df$Lon <- rightMap.df$Lon + 360 # Points for right of screen (appear when panning)
    combined.df <- rbind(leftMap.df, mainMap.df, rightMap.df) # Combine all points to be displayed
    combined.df
  })
  
  output$mymap <- renderLeaflet({ # Renders leaflet map, updates the markers on map with spreadsheet
    leaflet(data = mapPoints.df(), options=leafletOptions(minZoom=2, worldCopyJump=TRUE)) %>% # Filters the markers based on filter UI inputs
      addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap = FALSE)) %>% # Background map that markers will be added on top of
      addMarkers(~Lon, ~Lat, label=~ID, group="Tardigrades", # Adds markers to map with slight jitter because many marks have exact same lon,lat
                 popup = ~paste("<div>", # Tells what to display for popups
                                  "<center><h4><b><i>", as.character(Species),"</i></b></h4></center>",
                                  "<b>", "ID:", "</b>", as.character(ID), "<br>",
                                  "<b>", "Family:", "</b>", as.character(Family), "<br>",
                                  "<b>", "Subfamily:", "</b>", as.character(Subfamily), "<br>",
                                  "<b>", "Longitude:", "</b>", as.character(Lon), "<br>",
                                  "<b>", "Latitude:", "</b>", as.character(Lat), "<br>",
                                  "<b>", "Ocean/Sea:", "</b>", as.character(OceanSea), "<br>",
                                  "<b>", "Country:", "</b>", as.character(Country), "<br>",
                                  "<b>", "Region:", "</b>", as.character(Region), "<br>",
                                  "<b>", "Sample:", "</b>", as.character(Sample), "<br>",
                                  "<b>", "Depth:", "</b>", as.character(Depth), "<br>",
                                  "<b>", "Authority:", "</b>", as.character(Authority), "<br><br>",
                                  "<center>", "<img src =", as.character(Image), "height='150', width='100'>", "</center>",
                                  "<center><font size='1'>", as.character(ImageCredit), "</font></center><br>",
                                "</div>"),
                 clusterOptions = markerClusterOptions(), # Clusters the markers when zoomed out so that not displaying all points at once
                 popupOptions = popupOptions(minWidth = 300, maxWidth = 300)) #%>% # Makes sure that the popups are consistent size
    })
  
  output$tardigrades <- DT::renderDataTable({
    filteredData.df <- filterFunction(df=updated.df(), i=input, columns=filterColumns.vec())
    toDisplay.df <- filteredData.df[,-which(names(filteredData.df) %in% c("Image", "Image Credit"))]
    DT::datatable(toDisplay.df, escape = FALSE, rownames=toDisplay.df$ID, 
                  options = list(autoWidth=FALSE, scrollX=TRUE, 
                                 scrollY=TRUE, columnDefs=list(list(width = '50px', targets = 1)), 
                                 pageLength=100))
  })
}