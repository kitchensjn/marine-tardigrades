# This file ensures that dependencies (modules) have been downloaded

if (!require(gsheet)) install.packages("gsheet")
if (!require(shiny)) install.packages("shiny")
if (!require(leaflet)) install.packages("leaflet")
if (!require(leaflet.extras)) install.packages("leaflet.extras")
if (!require(dplyr)) install.packages("dplyr")
if (!require(DT)) install.packages("DT")

runApp() # Tests whether the shiny app work
