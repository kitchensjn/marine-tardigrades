# This file sets up the UI for the shiny application

library(leaflet)

ui <- navbarPage("Marine Tardigrades of the World", id="nav",
                 tabPanel("Interactive Map",
                          div(class="outer",
                              tags$head(
                                includeCSS("styles.css"), # Include our custom CSS
                                includeScript("gomap.js")
                              ),
                              leafletOutput("mymap", height="100vh"), # Map fills screen
                              absolutePanel(id = "controls", class = "panel panel-default", # Filter UI panel displays on top of map
                                            draggable = TRUE,  fixed = TRUE,
                                            top = 66, left = 50, right = "auto", bottom = "auto",
                                            width = 325, height = "auto",
                                            #uiOutput("createUI")),
                                            fluidRow(
                                              column(width=6, h3("Filters")),
                                              column(width=6, align="right",
                                                     h6(),
                                                     actionLink("clearFilters", "Clear All Filters"),
                                                     uiOutput("selectedNumber")
                                                     )
                                            ),
                                            fluidRow(align="center", 
                                                     column(width=6, actionLink("addFilter", "Add Filter")),
                                                     column(width=6, actionLink("removeFilter", "Remove Filter"))),
                                            fluidRow(h6()),
                                            tabsetPanel(id="filters")),
                              tags$div(id="cite", textOutput("citation"), align="right") # Optional file location: textOutput("fileLocation")
                              )
                   
                 ),
                 tabPanel("Data Explorer",
                          DT::dataTableOutput("tardigrades"))
)