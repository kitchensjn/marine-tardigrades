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
                                             style="direction:rtl; overflow-y:auto; overflow-x:hidden; max-height: 90vh",
                                  fluidRow(style="direction:ltr",
                                    column(width=6, h3("Filters")),
                                    column(width=6, align="right",
                                           h6(),
                                           actionLink("clearFilters", "Clear All Filters"),
                                           uiOutput("selectedNumber")
                                    )
                                  ),
                                  fluidRow(style="direction:ltr",
                                           align="center",
                                           h6(),
                                           column(width=6, actionLink("addFilter", "Add Filter")),
                                           column(width=6, actionLink("removeFilter", "Remove Filter"))),
                                  fluidRow(style="direction:ltr", h6()),
                                  tabsetPanel(id="filters"),
                                  fluidRow(style="direction:ltr",
                                           align="center",
                                           h3(),
                                           actionLink("applyFilters", "Apply Filters")
                                  )
                                ),
                                mainPanel(width=9,
                                  leafletOutput("mymap", height="100vh")
                                )
                              )
                            )
                   
                 ),
                 tabPanel("Data Explorer",
                          div(class="dataExplorer",
                              tags$head(
                                includeCSS("styles.css") # Include our custom CSS
                              ),
                              DT::dataTableOutput("tardigrades"))
                 ),
                 tabPanel("About/Contact",
                          div(class="contact",
                              tags$head(
                                includeCSS("styles.css")
                              ),
                              fluidRow(style='height:5vh'),
                              fluidRow(align="center", h3("About This Application")),
                              fluidRow(align="left", p("We created this application to display data collected through Dr. Bartels' study, 
                                                       The Zoogeography of Marine Tardigrada (2015), and subsequent additions since publication. These 
                                                       data were originally presented using Google Fusion Tables, but, with the end of Google's support 
                                                       for Fusion Tables in December 2019, we needed a new method.")),
                              fluidRow(align="left", p(paste("This application is built using Shiny, a package in R that allows users to create interactive web applications, and hosted through shinyapps.io. We stored the data within a Google Sheets spreadsheet, rather than a standard text file, and, by doing this, we can update the spreadsheet, and the application responds to these changes automatically without the need to redeploy. All code for the wesbite can be found at: "), tags$a(href="https://github.com/kitchensjn/marine-tardigrades","https://github.com/kitchensjn/marine-tardigrades"))),
                              fluidRow(align="left", p("If you have any questions about either the data or the application, please feel free to email Dr. Bartels or James Kitchens.")),
                              fluidRow(align="left", strong("To cite this application:")),
                              fluidRow(align="left", p("Bartels, P.J., Kaczmarek, Ł., Roszkowska, M. & Nelson, D.R. (2015) Interactive map of marine 
                                                       tardigrades of the world. https://paul-bartels.shinyapps.io/marine-tardigrades/")),
                              fluidRow(h1()),
                              fluidRow(align="center", h3("Contact Information")),
                              fluidRow(id="contactInfo", align="center",
                                column(6, h4("Dr. Paul Bartels"), h5(em("Data Collector")), p("Department of Biology"), p("Warren Wilson College"), p("Asheville, NC 28815, USA"), p("pbartels@warren-wilson.edu")),
                                column(6, h4("James Kitchens"), h5(em("Application Designer")), p("kitchensjn@gmail.com"))
                              )
                          )
                 )
)