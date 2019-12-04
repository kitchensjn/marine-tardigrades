# This file sets up the UI for the shiny application

library(leaflet)

ui <- navbarPage("Marine Tardigrades of the World", id="nav", position="fixed-top",
                 tabPanel("Interactive Map",
                          div(class="outer",
                              tags$head(
                                includeCSS("styles.css"), # Include our custom CSS
                                includeScript("gomap.js")
                              ),
                              sidebarLayout(
                                sidebarPanel(id="sidebar", 
                                             width=3,
                                  fluidRow(
                                    column(width=6, h3("Filters")),
                                    column(width=6, align="right",
                                           h6(),
                                           actionLink("clearFilters", "Clear All Filters"),
                                           uiOutput("selectedNumber")
                                    )
                                  ),
                                  fluidRow(align="center",
                                           h6(),
                                           column(width=6, actionLink("addFilter", "Add Filter")),
                                           column(width=6, actionLink("removeFilter", "Remove Filter"))),
                                  fluidRow(h6()),
                                  tabsetPanel(id="filters"),
                                  fluidRow(align="center",
                                           h3(),
                                           actionLink("applyFilters", "Apply Filters")
                                  )
                                ),
                                mainPanel(width=9,
                                  leafletOutput("mymap", height="100vh")
                                )
                              ),
                              tags$div(id="cite", textOutput("citation"), align="right") # Optional file location: textOutput("fileLocation")
                              )
                   
                 ),
                 tabPanel("Data Explorer",
                          div(class="dataExplorer",
                              tags$head(
                                includeCSS("styles.css"), # Include our custom CSS
                                includeScript("gomap.js")
                              ),
                              DT::dataTableOutput("tardigrades"))
                 )
)