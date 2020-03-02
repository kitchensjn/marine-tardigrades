# Marine Tardigrades Web Map
These are the source files for the tardigrade web application that I created for Dr. Paul Bartels at Warren Wilson College. Dr. Bartels asked for a map that would display the location of tardigrades that have been discovered across the world. He and his collaborators have created a Google Sheets spreadsheet of tardigrade findings that have been published between 1911 to present. As the spreadsheet is constantly being updated, the map needed to reflect these changes.

I built the map as a Shiny application using Leaflet for the map creation. Tardigrade data were pulled from the spreadsheet using the "gsheets" module. The Shiny application is hosted using shinyapps.io. The map checks the spreadsheet for changes every 10 minutes while the website is active and everytime the page is refreshed. Both map markers and filter options are updated to reflect the current spreadsheet. styles.css and gomap.js are coopted from "SuperZip example" on on Shiny from RStudio website (https://shiny.rstudio.com/gallery/superzip-example.html).

This application is hosted at: https://paul-bartels.shinyapps.io/marine-tardigrades/.
