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
                                            splitLayout(cellWidths=c("75%","100%"), h3("Filters"), # Panel Title
                                                        actionLink("clearFilters", "Clear Filters"), # Clear Filter button 
                                                        cellArgs = list(style = "vertical-align: bottom")),
                                            splitLayout(cellWidths="60%", 
                                                        uiOutput("idTextMin"), uiOutput("idTextMax")), # ID Filter added to panel
                                            uiOutput("createUI")), # Other Filters added to panel
                              tags$div(id="cite", textOutput("citation"), align="right") # Optional file location: textOutput("fileLocation")
                              )
                   
                 ),
                 tabPanel("Data Explorer",
                          DT::dataTableOutput("tardigrades"))
)