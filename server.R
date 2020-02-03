# This file acts as the backend of the shiny application, included filtering functions and map display

library(gsheet)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(rlist)


# Filter data based on ID
filterByIDFunction <- function(df, i, filterTab){
  nameIDmin <- paste("IDmin",filterTab,sep="")
  nameIDmax <- paste("IDmax",filterTab,sep="")
  if(!is.null(i[[nameIDmin]]) && !is.null(i[[nameIDmax]])){
    return(df[which(df$ID>=i[[nameIDmin]]&df$ID<=i[[nameIDmax]]),])
  }else{
    return(df)
  }
}

# Filter data based on other filters
filterFunction2 <- function(df, i, columns){
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

filterFunction <- function(filterTab, df, i, columns, deletedFilters, pivot=""){
  if (filterTab %in% deletedFilters){
    return()
  }else{
    dataToShow.df <- filterByIDFunction(df=df, i=i, filterTab=filterTab)
    if (!is.null(i[[paste(columns[[1]],filterTab,sep="")]]) && !is.na(i[[paste(columns[[1]],filterTab,sep="")]])){ # Checks that filters have loaded and exist
      for (columnName in columns) { # Loops over each filter column
        if (i[[paste(columnName,filterTab,sep="")]]!="-" && pivot!=columnName){
          if (i[[paste(columnName,filterTab,sep="")]]=="NA"){
            dataToShow.df <- dataToShow.df[which(is.na(dataToShow.df[[columnName]])),]
          }else{
            dataToShow.df <- dataToShow.df[which(dataToShow.df[[columnName]]==i[[paste(columnName,filterTab,sep="")]]),]
          }
        }
      }
    }
    return(dataToShow.df)
  }
}

inputCreatorFunction <- function(x, df, filterTab){
  nameId <- paste(x,filterTab,sep="")
  if (x == "OceanSea"){
    filterColumn.input <- selectInput(inputId = nameId,
                                      label = paste("Ocean/Sea"," (",length(unique(df[[x]])), ")", sep=""),
                                      choices = c("-", sort(unique(df[[x]]))))
  }else{
    filterColumn.input <- selectInput(inputId = nameId,
                                      label = paste(x," (",length(unique(df[[x]])), ")", sep=""),
                                      choices = c("-", sort(unique(df[[x]]))))
    
  }
  return(filterColumn.input)
}

removeFilter <- function(filterTab, i){
  removeTab(inputId = "filters", target = filterTab)
}


# backend of the webpage
server <- function(input, output, session) {
  googleSheetsUrl <- reactive({
    'https://docs.google.com/spreadsheets/d/1_IBXNwTZ5dtas54wakxMZGVwq3D7ppfnRfGj9Ms24mY'
  })
  
  updated.df <- reactive({ # Allows for data.frame to update with spreadsheet
    invalidateLater(600000,session=session) # Waits 10min before requesting sheet again
    tardigrade.df <- gsheet2tbl(googleSheetsUrl())
    tardigrade.df
  })
  
  valuesToUpdate <- reactiveValues(mapPoints.df=data.frame(), filtersInPanel=0, ignoreFilters=c(0))
  
  filterColumns.vec <- reactive({
    c("Family", "Genus", "Species", "OceanSea", "Country", "Depth", "Authority") # Columns to be included in filters [colnames(updated.df())]
  })
  
  observeEvent(input$applyFilters, {
    if (all(1:valuesToUpdate$filtersInPanel %in% valuesToUpdate$ignoreFilters)){
      valuesToUpdate$mapPoints.df <- updated.df()
    }else{
      valuesToUpdate$mapPoints.df <- list.rbind(lapply(1:valuesToUpdate$filtersInPanel, FUN=filterFunction, df=updated.df(), i=input, columns=filterColumns.vec(), deletedFilters=valuesToUpdate$ignoreFilters)) %>% distinct()
    }
  })
  
  output$createUI <- renderUI({ # Renders the filter UI, updates the available filters with spreadsheet
    tagList(  # Can filter by various columns
      lapply(filterColumns.vec(), FUN=inputCreatorFunction, df=updated.df()) # Vector of filters
    )
  })
  
  output$createUpdateDate <- renderText({
    paste("This dataset was last update on:", max(updated.df()$EntryDate))
  })
  
  observeEvent(input$addFilter, {
    newTabValue <- as.character(valuesToUpdate$filtersInPanel + 1)
    nameIDmin <- paste("IDmin",newTabValue,sep="")
    nameIDmax <- paste("IDmax",newTabValue,sep="")
    appendTab(inputId = "filters",
              tabPanel(newTabValue, style="direction:ltr; overflow-y:auto; overflow-x:hidden; max-height: 63vh",
                       fluidRow(h6("")),
                       fluidRow(
                         column(width=6, renderUI({
                           tagList(
                             numericInput(inputId=nameIDmin, label="ID min", value=min(updated.df()$ID), min=min(updated.df()$ID), max=max(updated.df()$ID))
                           )
                         })),
                         column(width=6, renderUI({
                           tagList(
                             numericInput(inputId=nameIDmax, label="ID max", value=max(updated.df()$ID), min=min(updated.df()$ID), max=max(updated.df()$ID))
                             
                           )
                         }))
                       ),
                       renderUI({
                         tagList(
                           lapply(filterColumns.vec(), FUN=inputCreatorFunction, df=updated.df(), filterTab=newTabValue) # Vector of filters
                         )
                       }),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1("")),
                       fluidRow(h1(""))
              )
    )
    updateTabsetPanel(session, inputId = "filters", selected = newTabValue)
    valuesToUpdate$filtersInPanel <- valuesToUpdate$filtersInPanel + 1  
  })
  
  observeEvent(input$removeFilter, {
    valuesToUpdate$ignoreFilters <- unique(c(valuesToUpdate$ignoreFilters, input$filters))
    removeFilter(i=input, filterTab=input$filters)
  })
  
  observe({ # Somewhat cumbersome way of updating the filters dynamically in correspondence to other filters
    for (columnName in filterColumns.vec()){
      if (!is.null(input[[paste(columnName,input$filters,sep="")]]) && !is.na(input[[paste(columnName,input$filters,sep="")]])){
        filteredData.df <- filterFunction(df=updated.df(), i=input, columns=filterColumns.vec(), filterTab=input$filters, pivot=columnName, deletedFilters=valuesToUpdate$ignoreFilters)
        if (columnName == "OceanSea"){
          name <- "Ocean/Sea"
        }else{
          name <- columnName
        }
        nameId <- paste(columnName,input$filters,sep="")
        if (input[[paste(columnName,input$filters,sep="")]] == "-"){
          updateSelectInput(session = session,
                            inputId = nameId,
                            label = paste(name," (",length(unique(filteredData.df[[columnName]])), ")", sep=""),
                            choices = c("-", sort(unique(filteredData.df[[columnName]]))))
        }else if (input[[paste(columnName,input$filters,sep="")]] != "NA"){ # NAs have to be treated differently
          updateSelectInput(session = session,
                            inputId = nameId,
                            label = paste(name," (",length(unique(filteredData.df[[columnName]])), ")", sep=""),
                            choices = c(input[[paste(columnName,input$filters,sep="")]], "-", sort(unique(filteredData.df[[columnName]])[-which(unique(filteredData.df[[columnName]]==input[[paste(columnName,input$filters,sep="")]]))])))
        }else{
          updateSelectInput(session = session,
                            inputId = nameId,
                            label = paste(name," (",length(unique(filteredData.df[[columnName]])), ")", sep=""),
                            choices = c(input[[paste(columnName,input$filters,sep="")]], "-", sort(unique(filteredData.df[[columnName]])[-which(is.na(unique(filteredData.df[[columnName]])))])))
        }
      }
    }
  })
  
  observeEvent(input$clearFilters, {
    session$reload()
  })
  
  finalPoints.df <- reactive({
    if (nrow(valuesToUpdate$mapPoints.df) > 0){
      fp.df <- valuesToUpdate$mapPoints.df
    }else{
      fp.df <- updated.df()
    }
    fp.df
  })
  
  output$selectedNumber <- renderText({
    paste("Selected:", nrow(finalPoints.df()))
  })
  
  output$mymap <- renderLeaflet({ # Renders leaflet map, updates the markers on map with spreadsheet
    leaflet(data = finalPoints.df(), options=leafletOptions(minZoom=2, worldCopyJump=FALSE)) %>% # Filters the markers based on filter UI inputs
      addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap=TRUE)) %>% # Background map that markers will be added on top of
      addMarkers(~Lon, ~Lat, group="Tardigrades", # Adds markers to map
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
    filteredData.df <- finalPoints.df()
    toDisplay.df <- filteredData.df[,-which(names(filteredData.df) %in% c("Image", "Image Credit"))]
    DT::datatable(toDisplay.df, escape = FALSE, rownames=toDisplay.df$ID, 
                  options = list(autoWidth=FALSE, scrollX=TRUE, 
                                 scrollY=TRUE, columnDefs=list(list(width = '50px', targets = 1)), 
                                 pageLength=100))
  })
}