library(shiny)             # Load the Shiny package for building web applications
library(shinyjs)           # improve user experience with JavaScript
library(shinythemes)       # themes for shiny
library(bslib)             # Load the bslib package for customizing Bootstrap-based CSS stylesheets
library(dbplyr)            # Load the dbplyr package for using dplyr syntax to query databases directly
library(lubridate)         # Load the lubridate package for working with dates and times
library(plotly)            # Load the plotly package for creating interactive plots
library(bsicons)           # Load the bsicons package for adding Bootstrap icons to Shiny applications
library(reactable)         # Load the reactable package for creating interactive tables
library(echarts4r)         # Load the echarts4r package for creating interactive charts and maps
library(DBI)               # Load the DBI package for database interface
library(stringr)           # Load the stringr package for working with strings
library(scales)            # Load the scales package for formatting numbers and dates
library(readr)             # Load the readr package for reading and parsing data files
library(shinyWidgets)      # Load the shinyWidgets package for adding custom widgets to Shiny applications
library(shinymanager)      # Load the shinymanager package for adding user authentication to Shiny applications
library(htmltools)         # Load the htmltools package for working with HTML code in R
library(htmlwidgets)       # Load the htmlwidgets package for creating interactive widgets
library(shiny.fluent)      # Load the shiny.fluent package for creating Shiny applications with a Fluent Design style
# library(RMySQL)            # Load the RMySQL package for interfacing with MySQL databases from R
library(janitor)           # Load the janitor package for cleaning and formatting messy data
library(heatmaply)
library(mapboxer)
library(bcrypt)
library(dplyr)
library(shinyauthr)
library(DT)


port = Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)