#**************************************************
#*PACKAGES
#**************************************************
#*
#*
# 
# library(shiny)             # Load the Shiny package for building web applications
# library(shinyjs)           # improve user experience with JavaScript
# library(shinythemes)       # themes for shiny
# library(bslib)             # Load the bslib package for customizing Bootstrap-based CSS stylesheets
# library(dbplyr)            # Load the dbplyr package for using dplyr syntax to query databases directly
# library(lubridate)         # Load the lubridate package for working with dates and times
# library(plotly)            # Load the plotly package for creating interactive plots
# library(bsicons)           # Load the bsicons package for adding Bootstrap icons to Shiny applications
# library(reactable)         # Load the reactable package for creating interactive tables
# library(echarts4r)         # Load the echarts4r package for creating interactive charts and maps
# library(DBI)               # Load the DBI package for database interface
# library(stringr)           # Load the stringr package for working with strings
# library(scales)            # Load the scales package for formatting numbers and dates
# library(readr)             # Load the readr package for reading and parsing data files
# library(shinyWidgets)      # Load the shinyWidgets package for adding custom widgets to Shiny applications
# library(shinymanager)      # Load the shinymanager package for adding user authentication to Shiny applications
# library(htmltools)         # Load the htmltools package for working with HTML code in R
# library(htmlwidgets)       # Load the htmlwidgets package for creating interactive widgets
# library(shiny.fluent)      # Load the shiny.fluent package for creating Shiny applications with a Fluent Design style
# # library(RMySQL)            # Load the RMySQL package for interfacing with MySQL databases from R
# library(janitor)           # Load the janitor package for cleaning and formatting messy data
# library(heatmaply)
# library(mapboxer)
# library(bcrypt)
# library(dplyr)             # Load the dplyr package for data manipulation
# library(shinyauthr)
# library(DT)

#************************************************** 
#*GLOBAL PARAMETERS
#**************************************************
#*
# *******  parameters 
# Define parameters for the dashboard, such as color scheme, logo, and title

primary_colour <- "#008080" #"#0675DD"
name_dashboard <- "ANNUAL HOUSEHOLD INCOME & EXPENDITURE SURVEY, 2023"
logo_location  <- "images/logo.jpg"
colour_1 <- "#F66068"
colour_2 <- "#206095"

# Set color palette for male and female
sex <- c(colour_1, colour_2)
pal <- setNames(sex, c("Male", "Female"))


#************************************************** 
#*USER INTERFACE
#**************************************************
#*

#* User Guide
user_guide <- layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      uiOutput("userguide_boxes", fill = TRUE)
    )
)


#* Enumeration
#* 
enumeration <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    uiOutput("enumeration_boxes", fill = TRUE)
  )
)


#* Teams
teams <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    uiOutput("team_boxes", fill = TRUE)
  )
)

#* Enumerators
enumerators <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    uiOutput("enumerator_boxes", fill = TRUE)
  )
)

economic <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    uiOutput("economic_boxes", fill = TRUE)
  )
)

demography <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    uiOutput("demography_boxes", fill = TRUE)
  )
)

enum_summary_search <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    card_body(
      selectizeInput(
        'team_2',
        'Team',
        choices = seq(0,400, by=10),
        multiple = TRUE,
        selected = 10,
        options = list(maxOptions = 50, placeholder = "Team")
      ),
      uiOutput("enum_summary_search")
    )
  )
)


enum_summary_output <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  card(
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      card(
        uiOutput("summary_tables_output", fill = TRUE),
      ),
    )
  )
)

# 
plot_graph <- layout_column_wrap(
  width = 1,
  heights_equal = "row",
  
  #plots showing total enumerations by day and by enumerator
  card(
    uiOutput("plot_graph", fill = TRUE)
  ),
  
  #plots with reactive display of enumeration by team no
  card(
    selectizeInput(
      'team_3', 
      'Team', 
      choices = seq(0,400, by=10),
      multiple = FALSE,
      selected = 10,
      options = list(maxOptions = 50, placeholder = "Team")
    ),
    plotlyOutput("plot_enum_by_day_by_team", fill = TRUE)
  ),
  
  #plots with reactive display of enumeration by enumerator by team no
  card(
    selectizeInput(
      'team_4', 
      'Team', 
      choices = seq(0,400, by=10),
      multiple = FALSE,
      selected = 10,
      options = list(maxOptions = 50, placeholder = "Team")
    ),
    plotlyOutput("plot_enum_by_day_by_enumeration_by_team", fill = TRUE)
  )
)

# add login panel UI function
panels <- mainPanel(

  # add LOGIN PANEL UI function
  shinyauthr::loginUI(id = "login", title = "PIT Login"),
  
  tabsetPanel(
    type = "tabs",
    id = "maintab",
    tabPanel("User Guide", user_guide),
    tabPanel("Summary of Enumeration", enumeration),
    tabPanel("Teams", teams),
    tabPanel("Enumerators", enumerators),
    tabPanel("Demography", demography),
    tabPanel("Economic", economic),
    tabPanel("Work Output - View", enum_summary_output),
    tabPanel("Plots", plot_graph),
  ), width = 12
)

# Define UI for application
ui <- page_navbar(
       tags$head(
         tags$style(
           HTML(
                   '
          /* Align header text to the bottom */
          .header,
          .group-header {
            display: flex;
            flex-direction: column;
            justify-content: flex-end;
          }
          
          .header {
            border-bottom-color: #555;
            font-size: 13px;
            font-weight: 400;
            text-transform: uppercase;
          }
          
          /* Highlight headers when sorting */
          .header:hover,
          .header[aria-sort="ascending"],
          .header[aria-sort="descending"] {
            background-color: #eee;
          }
          
          .border-left {
            border-left: 2px solid #555;
          }
          
          .selectize-input {height: 30px;
          width: 300px;}
          ')
         )
       ),
       fluid = TRUE,
       # This styles the look and feel of the plot
       theme = bs_theme(
         version = 5,
         base_font = font_google("Lato"),
         "primary" = primary_colour,
         "navbar-bg" = primary_colour
       ),
       
       # This includes a logo to the app
       title = tags$span(
         tags$img(src = logo_location,
                  style = "width:40px;height:auto;margin-right:24px;"),
         name_dashboard,
         
         # add logout button UI
         span(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
        ),
       
       
      panels
       
     )
    

shinyUI(ui = ui)





# layout_column_wrap(
#   width = 1,
#   heights_equal = "row",
#   card(
#     layout_column_wrap(
#       width = 1/2,
#       heights_equal = "row",
#       card(
#         card_header("enumeration per day per team"),
#         card_body(
#           fill = FALSE,
#           selectizeInput(
#             'team_1', 
#             'Team', 
#             choices = seq(0,400, by=10),
#             multiple = TRUE,
#             selected = 10,
#             options = list(maxOptions = 50, placeholder = "Team")
#           ),
#           #uiOutput("enum_per_day_per_team")
#           #DT::dataTableOutput("enum_per_day_per_team"),
#         ),
#       ),
#       card(
#         card_header("enumeration per day per team per enumerator"),
#         card_body(
#           fill = TRUE,
#           selectizeInput(
#             'team_2', 
#             'Team', 
#             choices = seq(0,400, by=10),
#             multiple = TRUE,
#             selected = 10,
#             options = list(maxOptions = 50, placeholder = "Team")
#           ),
#           renderDataTable({ DT::datatable(df_enum_per_day_per_team %>% 
#                                             filter(team %in% input$team_1) %>% 
#                                             arrange(team),
#                                           caption = htmltools::tags$caption(
#                                             style = 'caption-side: bottom; text-align: center;',
#                                             'table: ', htmltools::em('enumeration per day per team')
#                                           ),
#                                           extensions = 'Buttons',
#                                           
#                                           options = list(
#                                             fixedColumns = TRUE,
#                                             autoWidth = TRUE,
#                                             ordering = TRUE,
#                                             dom = 'Bftsp',
#                                             buttons = c('copy', 'csv', 'excel')
#                                           )) 
#           })
#           #
#         )
#       ),
#     )
#     # full_screen = TRUE
#   ),
# )


#uiOutput("user_guide")
# #*Enumeration Summary
# page_1 <- layout_column_wrap(
#   tags$head(tags$style(HTML(".small-box {height: 50px}"))),
#   width = 1,
#   heights_equal = "row",
#   card(
#     card_header("Summary of Enumeration Data"),
#     layout_column_wrap(
#       width = "100px",
#       card(
#         full_screen = TRUE,
#         layout_column_wrap(
#           width = "50px",
#           value_box(
#             title = tags$p("Households Enumerated",  style = "font-size: 100%;"),
#             value = textOutput(outputId = "hhold_enum"),
#             showcase = bs_icon("house"),
#             height = 20,
#             style = "font-size: 80%;"
#           ),
#           value_box(
#             title = tags$p("Households Enumerated (3 days)", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(as.character(hhold_with_data_3), style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("house-add"),
#           ),
#           value_box(
#             title = tags$p("Individuals in Data", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(as.character(no_of_indiv), style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("people"),
#           ),
#           value_box(
#             title = tags$p("Partial Saves", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p( paste0(partial_save_total, " (", partial_save_percent, "%)"), 
#                             # style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("mask"),
#           ),
#           value_box(
#             title = tags$p("Deleted Cases", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p( paste0(deleted_total, " (", deleted_percent, "%)"), 
#                             # style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("x-circle"),
#           )
#         ),
#         
#         tags$p(),
#         
#         layout_column_wrap(
#           width = "50px",
#           value_box(
#             title = tags$p("Average Household Size", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(as.character(hhold_size), style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("file-break"),
#           ),
#           value_box(
#             title = tags$p("EAs with Data", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(as.character(eas_with_data), style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("pin-map")
#           ),
#           value_box(
#             title = tags$p("EAs with Data (3days)", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(eas_with_data_3, style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("pin-map-fill"),
#           ),
#           value_box(
#             title = tags$p("Enumerators with synced data (3days)", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(enumerators_with_data_3, style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("clock-history"),
#           ),
#           value_box(
#             title = tags$p("Duplicate Cases", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(duplicate_cases, style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("clipboard"),
#           ),
#         ),
#         
#         tags$p(),
#         
#         layout_column_wrap(
#           width = "50px",
#           value_box(
#             title = tags$p("Males", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(as.character(9999), style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("gender-male"),
#           ),
#           value_box(
#             title = tags$p("Females", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(as.character(9999), style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("gender-female")
#           ),
#           value_box(
#             title = tags$p("EAs with Data (3days)", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(eas_with_data_3, style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("pin-map-fill"),
#           ),
#           value_box(
#             title = tags$p("Enumerators with synced data (3days)", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(enumerators_with_data_3, style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("clock-history"),
#           ),
#           value_box(
#             title = tags$p("Duplicate Cases", style = "font-size: 100%;"),
#             value = 0,
#             # value = tags$p(duplicate_cases, style = "font-size: 150%; color: yellow;"),
#             showcase = bs_icon("clipboard"),
#           ),
#         )
#         
#       )
#     )
#   ))

# #* Team 01 - completed hholds
# team1_info_1 <- completed_hholds %>% 
#   filter(id00 == 10)


# card1 <- card(
#   card_header("Team 01", 
#               class = "bg-primary",),
#   card_body(
#     #dataTableOutput(team1_info_1),
#     # "Total Households: ", tags$span(as.character(t1_total_hholds), style = "font-size: 150%; color: yellow;"),
#     class = "bg-primary",
#   )
# )
# card2 <- card(
#   card_header("Nothing much here", class = "bg-primary"),
#   card_body("This is card 3", class = "bg-primary")
# )
# card3 <- card(
#   card_header("Filling content", class = "bg-primary"),
#   card_body("This is card 3", class = "bg-primary")
# )
# 
# #* Team Performance
# page_2 <- layout_column_wrap(
#   width = 1,
#   heights_equal = "row",
#   card(
#     # tags$style("background-color: teal !important;"),
#     card_header("Team Performance"),
#     layout_column_wrap(
#       width = "100px",
#       card(
#         full_screen = TRUE,
#         layout_column_wrap(
#           width = "50px",
#           card1, 
#           card2, 
#           card3, 
#           card3
#         )
#       )
#     )
#   )
# )



