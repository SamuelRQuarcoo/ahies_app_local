#**************************************************
#*PACKAGES
#**************************************************
#*
#* remove cache
#* flash cache
rm(list=ls())

#remove.packages("readr")
#install.packages("readr")
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
# library(janitor)           # Load the janitor package for cleaning and formatting messy data
# library(heatmaply)         # Load package for heat maps
# library(mapboxer)
# library(bcrypt)
# library(dplyr)
# library(shinyauthr)
# library(DT)
# library(openxlsx)         # load package for excel manipulations
# library(xlsx)

# library(RMySQL)            # Load the RMySQL package for interfacing with MySQL databases from R
#source("helper_functions/convert_to_rds.R")

#************************************************** 
#*SERVER FUNCTION
#**************************************************
shinyServer(function(input, output, session) {
  
  
  # dataframe that holds usernames, passwords and other user data
  user_base <- tibble::tibble(
    user = c("user1", "AHIES"),
    password = c("pass1", "Top@z"),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
  )
  
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth))
  
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  
  #******************************************************************************************************
  #*                                DATAFRAMES/SECTIONS OF QUESTIONNAIRE
  #******************************************************************************************************

  #* *************************************************
  #* ID 
  #* *************************************************
  #* 
  #* rds file path
  file_path <- "input_data/ahies_rds/"

  rds_identification <- readRDS(paste0(file_path, "identification.RDS")) %>% 
    filter(idq0 == 3)
  
  #* *************************************************
  #* CASES 
  #* *************************************************
  rds_cases <- readRDS(paste0(file_path, "cases.RDS")) %>% 
    inner_join(rds_identification, by = c("id" = "case_id")) %>% 
    filter(idq0 == 3)
  
  #* partial saves
  partial_saves <- rds_cases %>% 
    count(partial_save_mode)
  
  #* 1
  partial_save_1 <- partial_saves %>% filter(partial_save_mode == 1)
  partial_save_1 <- partial_save_1$n
  
  #* 2
  partial_save_2 <- partial_saves %>% filter(partial_save_mode == 2)
  partial_save_2 <- partial_save_2$n
  
  partial_save_total <- partial_save_1 + partial_save_2
  partial_save_percent <- as.character(round((partial_save_total)/nrow(rds_cases) * 100, digits = 1))
  partial_save_total <- as.character(partial_save_total)
  
  #* deleted cases
  deleted_cases <- rds_cases %>% 
    filter(deleted > 0)
  
  deleted_total <- nrow(deleted_cases)
  deleted_percent <- round(deleted_total/nrow(rds_cases) * 100, digits = 1)
  
  
  #* *************************************************
  #* Household 
  #* *************************************************
  rds_household <- readRDS(paste0(file_path,"household.RDS"))
  #inner_join(rds_identification, by = "level_1_id")
  
  #***************************************************
  #* Meta Data 
  #* *************************************************
  rds_metadata <- readRDS(paste0(file_path,"metadata.RDS"))
  #inner_join(rds_identification, by = "level_1_id")
  
  #***************************************************
  #* Individual Data 
  #* *************************************************
  rds_indiv <- readRDS(paste0(file_path,"indiv.RDS")) 
  #inner_join(rds_identification, by = "level_1_id")
  
  
  #* Household Statistics
  #* ********************
  #* total households enumerated
  hhold_enumerated <- formatC(nrow(rds_household), big.mark = ",")
  
  #* number of households enumerated in the last 3 days
  cur_date <- Sys.Date()
  df_hhold_with_data_3 <- rds_metadata %>% 
    select(hhrecord_id, metadata_id, v04, v05, v09, id00, idq0, id01, id02) %>% 
    filter(v05 > 0) %>% 
    mutate(no_days = difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')) %>% 
    filter(no_days <= 7) 
  
  hhold_with_data_3 <- formatC(nrow(df_hhold_with_data_3), big.mark = ",")
  
  #* no of individuals in data
  no_of_indiv <- formatC(nrow(rds_indiv), big.mark = ",")
  
  #* average houshold size
  avg_hhold_size <- round(nrow(rds_indiv)/nrow(rds_household), digits = 1)
  
  
  #* EA Statistics
  #* *************
  
  #* EAs with data
  df_eas_with_data <- rds_household %>% 
    select(id01) %>% 
    distinct(id01) %>% 
    group_by(id01) %>%
    summarize(count_nos = n()) %>% 
    filter(!is.na(id01))
  
  eas_with_data <- formatC(nrow(df_eas_with_data), big.mark = ",")
  
  #* EAs with data - past 24 hrs
  df_eas_with_data_3 <- rds_metadata %>% 
    select(hhrecord_id, metadata_id, v04, v05, v09, id00, idq0, id01, id02) %>% 
    mutate(no_days = as.character(round(difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')), digit = 1)) %>% 
    filter(no_days %in% c("3")) %>% 
    distinct(id01) %>% 
    filter(id01 > 0)
  
  eas_with_data_3 <- as.character(nrow(df_eas_with_data_3))
  
  #* Enumerators with synced data - past 24 hrs
  df_enumerators_with_data_3 <- rds_metadata %>% 
    select(hhrecord_id, metadata_id, v04, v05, v09, id00, idq0, id01, id02, v01) %>% 
    filter(v01 > 0) %>%  
    mutate(no_days = as.character(round(difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')), digit = 1)) %>%  
    filter(no_days %in% c("3"))
  
  enumerators_with_data_3 <- as.character(nrow(df_enumerators_with_data_3))
  
  #* Duplicate cases
  df_duplicate_cases <- rds_cases %>% 
    inner_join(rds_household, by=c("id" = "case_id")) %>% 
    select(id) %>% 
    group_by(id) %>% 
    summarize(count_id = n()) %>% 
    filter(count_id > 1)
  
  #* save and read dataframe as RDS
  # #saveRDS(df_duplicate_cases, file = "input_data/duplicate_cases.RDS")
  # rds_duplicate_cases <- readRDS("input_data/duplicate_cases.RDS")
  
  duplicate_cases <- as.character(nrow(df_duplicate_cases)) 
  
  #* percentage of male only households
  df_sex <- rds_indiv %>% 
    # inner_join(rds_indiv, by = c("id00", "idq0", "id01", "id02" )) %>%
    group_by(s1aq1) %>% 
    summarize(count_gender = n())
  
  # males
  male_count <- df_sex %>% filter(s1aq1 == 1) %>% pull
  male_count <- formatC(male_count, big.mark = ",")
  
  # females
  female_count <- df_sex %>% filter(s1aq1 == 2) %>% pull
  female_count <- formatC(female_count, big.mark = ",")
  
  # missing
  missing_sex_count <- df_sex %>% filter(is.na(s1aq1)) %>% pull
  
  #mean interview time - entire survey
  df_interview_time <- rds_metadata %>%
    select(id00, v04, v05, v03a, v03b, final_confirmation) %>%
    filter(final_confirmation == 1) %>% 
    na.omit() %>% 
    mutate(start_date = as.character(strptime(paste0(v04, v03a), format = "%Y%m%d %H%M")), 
           end_date = as.character(strptime(paste0(v05, v03b), format = "%Y%m%d %H%M"))) %>% 
    mutate(interview_time = round(difftime(end_date, start_date, units = 'days'), digits = 1))

  # mean interview time - all interviews
  df_mean_interview_time <- df_interview_time %>% na.omit()
  mean_interview_time <- as.integer(mean(df_mean_interview_time$interview_time))
 
  # mean interview time - by team
  df_interview_time_by_team <- df_interview_time %>%
    na.omit() %>% 
    mutate(interview_time = difftime(end_date, start_date, units = 'days')) %>% 
    group_by(id00) %>%
    summarize(mean_interview_time = round(mean(interview_time), digits = 1),
              min_interview_time = round(min(interview_time), digits = 1),
              max_interview_time = round(max(interview_time), digits = 1),
              median_interview_time = round(median(interview_time), digits = 1))
  
  #* mean
  mit_team_01_mean <- df_interview_time_by_team %>% filter(id00 == 10) %>% select(mean_interview_time) %>% pull
  mit_team_02_mean <- df_interview_time_by_team %>% filter(id00 == 20) %>% select(mean_interview_time) %>% pull
  mit_team_03_mean <- df_interview_time_by_team %>% filter(id00 == 30) %>% select(mean_interview_time) %>% pull
  mit_team_04_mean <- df_interview_time_by_team %>% filter(id00 == 40) %>% select(mean_interview_time) %>% pull
  mit_team_05_mean <- df_interview_time_by_team %>% filter(id00 == 50) %>% select(mean_interview_time) %>% pull
  mit_team_06_mean <- df_interview_time_by_team %>% filter(id00 == 60) %>% select(mean_interview_time) %>% pull
  mit_team_07_mean <- df_interview_time_by_team %>% filter(id00 == 70) %>% select(mean_interview_time) %>% pull
  mit_team_08_mean <- df_interview_time_by_team %>% filter(id00 == 80) %>% select(mean_interview_time) %>% pull
  mit_team_09_mean <- df_interview_time_by_team %>% filter(id00 == 90) %>% select(mean_interview_time) %>% pull
  mit_team_10_mean <- df_interview_time_by_team %>% filter(id00 == 100) %>% select(mean_interview_time) %>% pull
  mit_team_11_mean <- df_interview_time_by_team %>% filter(id00 == 110) %>% select(mean_interview_time) %>% pull
  mit_team_12_mean <- df_interview_time_by_team %>% filter(id00 == 120) %>% select(mean_interview_time) %>% pull
  mit_team_13_mean <- df_interview_time_by_team %>% filter(id00 == 130) %>% select(mean_interview_time) %>% pull
  mit_team_14_mean <- df_interview_time_by_team %>% filter(id00 == 140) %>% select(mean_interview_time) %>% pull
  mit_team_15_mean <- df_interview_time_by_team %>% filter(id00 == 150) %>% select(mean_interview_time) %>% pull
  mit_team_16_mean <- df_interview_time_by_team %>% filter(id00 == 160) %>% select(mean_interview_time) %>% pull
  mit_team_17_mean <- df_interview_time_by_team %>% filter(id00 == 170) %>% select(mean_interview_time) %>% pull
  mit_team_18_mean <- df_interview_time_by_team %>% filter(id00 == 180) %>% select(mean_interview_time) %>% pull
  mit_team_19_mean <- df_interview_time_by_team %>% filter(id00 == 190) %>% select(mean_interview_time) %>% pull
  mit_team_20_mean <- df_interview_time_by_team %>% filter(id00 == 200) %>% select(mean_interview_time) %>% pull
  mit_team_21_mean <- df_interview_time_by_team %>% filter(id00 == 210) %>% select(mean_interview_time) %>% pull
  mit_team_22_mean <- df_interview_time_by_team %>% filter(id00 == 220) %>% select(mean_interview_time) %>% pull
  mit_team_23_mean <- df_interview_time_by_team %>% filter(id00 == 230) %>% select(mean_interview_time) %>% pull
  mit_team_24_mean <- df_interview_time_by_team %>% filter(id00 == 240) %>% select(mean_interview_time) %>% pull
  mit_team_25_mean <- df_interview_time_by_team %>% filter(id00 == 250) %>% select(mean_interview_time) %>% pull
  mit_team_26_mean <- df_interview_time_by_team %>% filter(id00 == 260) %>% select(mean_interview_time) %>% pull
  mit_team_27_mean <- df_interview_time_by_team %>% filter(id00 == 270) %>% select(mean_interview_time) %>% pull
  mit_team_28_mean <- df_interview_time_by_team %>% filter(id00 == 280) %>% select(mean_interview_time) %>% pull
  mit_team_29_mean <- df_interview_time_by_team %>% filter(id00 == 290) %>% select(mean_interview_time) %>% pull
  mit_team_30_mean <- df_interview_time_by_team %>% filter(id00 == 300) %>% select(mean_interview_time) %>% pull
  mit_team_31_mean <- df_interview_time_by_team %>% filter(id00 == 310) %>% select(mean_interview_time) %>% pull
  mit_team_32_mean <- df_interview_time_by_team %>% filter(id00 == 320) %>% select(mean_interview_time) %>% pull
  mit_team_33_mean <- df_interview_time_by_team %>% filter(id00 == 330) %>% select(mean_interview_time) %>% pull
  mit_team_34_mean <- df_interview_time_by_team %>% filter(id00 == 340) %>% select(mean_interview_time) %>% pull
  mit_team_35_mean <- df_interview_time_by_team %>% filter(id00 == 350) %>% select(mean_interview_time) %>% pull
  mit_team_36_mean <- df_interview_time_by_team %>% filter(id00 == 360) %>% select(mean_interview_time) %>% pull
  mit_team_37_mean <- df_interview_time_by_team %>% filter(id00 == 370) %>% select(mean_interview_time) %>% pull
  mit_team_38_mean <- df_interview_time_by_team %>% filter(id00 == 380) %>% select(mean_interview_time) %>% pull
  mit_team_39_mean <- df_interview_time_by_team %>% filter(id00 == 390) %>% select(mean_interview_time) %>% pull
  mit_team_40_mean <- df_interview_time_by_team %>% filter(id00 == 400) %>% select(mean_interview_time) %>% pull
  
  #* min
  mit_team_01_min <- df_interview_time_by_team %>% filter(id00 == 10) %>% select(min_interview_time) %>% pull
  mit_team_02_min <- df_interview_time_by_team %>% filter(id00 == 20) %>% select(min_interview_time) %>% pull
  mit_team_03_min <- df_interview_time_by_team %>% filter(id00 == 30) %>% select(min_interview_time) %>% pull
  mit_team_04_min <- df_interview_time_by_team %>% filter(id00 == 40) %>% select(min_interview_time) %>% pull
  mit_team_05_min <- df_interview_time_by_team %>% filter(id00 == 50) %>% select(min_interview_time) %>% pull
  mit_team_06_min <- df_interview_time_by_team %>% filter(id00 == 60) %>% select(min_interview_time) %>% pull
  mit_team_07_min <- df_interview_time_by_team %>% filter(id00 == 70) %>% select(min_interview_time) %>% pull
  mit_team_08_min <- df_interview_time_by_team %>% filter(id00 == 80) %>% select(min_interview_time) %>% pull
  mit_team_09_min <- df_interview_time_by_team %>% filter(id00 == 90) %>% select(min_interview_time) %>% pull
  mit_team_10_min <- df_interview_time_by_team %>% filter(id00 == 100) %>% select(min_interview_time) %>% pull
  mit_team_11_min <- df_interview_time_by_team %>% filter(id00 == 110) %>% select(min_interview_time) %>% pull
  mit_team_12_min <- df_interview_time_by_team %>% filter(id00 == 120) %>% select(min_interview_time) %>% pull
  mit_team_13_min <- df_interview_time_by_team %>% filter(id00 == 130) %>% select(min_interview_time) %>% pull
  mit_team_14_min <- df_interview_time_by_team %>% filter(id00 == 140) %>% select(min_interview_time) %>% pull
  mit_team_15_min <- df_interview_time_by_team %>% filter(id00 == 150) %>% select(min_interview_time) %>% pull
  mit_team_16_min <- df_interview_time_by_team %>% filter(id00 == 160) %>% select(min_interview_time) %>% pull
  mit_team_17_min <- df_interview_time_by_team %>% filter(id00 == 170) %>% select(min_interview_time) %>% pull
  mit_team_18_min <- df_interview_time_by_team %>% filter(id00 == 180) %>% select(min_interview_time) %>% pull
  mit_team_19_min <- df_interview_time_by_team %>% filter(id00 == 190) %>% select(min_interview_time) %>% pull
  mit_team_20_min <- df_interview_time_by_team %>% filter(id00 == 200) %>% select(min_interview_time) %>% pull
  mit_team_21_min <- df_interview_time_by_team %>% filter(id00 == 210) %>% select(min_interview_time) %>% pull
  mit_team_22_min <- df_interview_time_by_team %>% filter(id00 == 220) %>% select(min_interview_time) %>% pull
  mit_team_23_min <- df_interview_time_by_team %>% filter(id00 == 230) %>% select(min_interview_time) %>% pull
  mit_team_24_min <- df_interview_time_by_team %>% filter(id00 == 240) %>% select(min_interview_time) %>% pull
  mit_team_25_min <- df_interview_time_by_team %>% filter(id00 == 250) %>% select(min_interview_time) %>% pull
  mit_team_26_min <- df_interview_time_by_team %>% filter(id00 == 260) %>% select(min_interview_time) %>% pull
  mit_team_27_min <- df_interview_time_by_team %>% filter(id00 == 270) %>% select(min_interview_time) %>% pull
  mit_team_28_min <- df_interview_time_by_team %>% filter(id00 == 280) %>% select(min_interview_time) %>% pull
  mit_team_29_min <- df_interview_time_by_team %>% filter(id00 == 290) %>% select(min_interview_time) %>% pull
  mit_team_30_min <- df_interview_time_by_team %>% filter(id00 == 300) %>% select(min_interview_time) %>% pull
  mit_team_31_min <- df_interview_time_by_team %>% filter(id00 == 310) %>% select(min_interview_time) %>% pull
  mit_team_32_min <- df_interview_time_by_team %>% filter(id00 == 320) %>% select(min_interview_time) %>% pull
  mit_team_33_min <- df_interview_time_by_team %>% filter(id00 == 330) %>% select(min_interview_time) %>% pull
  mit_team_34_min <- df_interview_time_by_team %>% filter(id00 == 340) %>% select(min_interview_time) %>% pull
  mit_team_35_min <- df_interview_time_by_team %>% filter(id00 == 350) %>% select(min_interview_time) %>% pull
  mit_team_36_min <- df_interview_time_by_team %>% filter(id00 == 360) %>% select(min_interview_time) %>% pull
  mit_team_37_min <- df_interview_time_by_team %>% filter(id00 == 370) %>% select(min_interview_time) %>% pull
  mit_team_38_min <- df_interview_time_by_team %>% filter(id00 == 380) %>% select(min_interview_time) %>% pull
  mit_team_39_min <- df_interview_time_by_team %>% filter(id00 == 390) %>% select(min_interview_time) %>% pull
  mit_team_40_min <- df_interview_time_by_team %>% filter(id00 == 400) %>% select(min_interview_time) %>% pull
  
  #* max
  mit_team_01_max <- df_interview_time_by_team %>% filter(id00 == 10) %>% select(max_interview_time) %>% pull 
  mit_team_02_max <- df_interview_time_by_team %>% filter(id00 == 20) %>% select(max_interview_time) %>% pull
  mit_team_03_max <- df_interview_time_by_team %>% filter(id00 == 30) %>% select(max_interview_time) %>% pull
  mit_team_04_max <- df_interview_time_by_team %>% filter(id00 == 40) %>% select(max_interview_time) %>% pull
  mit_team_05_max <- df_interview_time_by_team %>% filter(id00 == 50) %>% select(max_interview_time) %>% pull
  mit_team_06_max <- df_interview_time_by_team %>% filter(id00 == 60) %>% select(max_interview_time) %>% pull
  mit_team_07_max <- df_interview_time_by_team %>% filter(id00 == 70) %>% select(max_interview_time) %>% pull
  mit_team_08_max <- df_interview_time_by_team %>% filter(id00 == 80) %>% select(max_interview_time) %>% pull
  mit_team_09_max <- df_interview_time_by_team %>% filter(id00 == 90) %>% select(max_interview_time) %>% pull
  mit_team_10_max <- df_interview_time_by_team %>% filter(id00 == 100) %>% select(max_interview_time) %>% pull
  mit_team_11_max <- df_interview_time_by_team %>% filter(id00 == 110) %>% select(max_interview_time) %>% pull
  mit_team_12_max <- df_interview_time_by_team %>% filter(id00 == 120) %>% select(max_interview_time) %>% pull
  mit_team_13_max <- df_interview_time_by_team %>% filter(id00 == 130) %>% select(max_interview_time) %>% pull
  mit_team_14_max <- df_interview_time_by_team %>% filter(id00 == 140) %>% select(max_interview_time) %>% pull
  mit_team_15_max <- df_interview_time_by_team %>% filter(id00 == 150) %>% select(max_interview_time) %>% pull
  mit_team_16_max <- df_interview_time_by_team %>% filter(id00 == 160) %>% select(max_interview_time) %>% pull
  mit_team_17_max <- df_interview_time_by_team %>% filter(id00 == 170) %>% select(max_interview_time) %>% pull
  mit_team_18_max <- df_interview_time_by_team %>% filter(id00 == 180) %>% select(max_interview_time) %>% pull
  mit_team_19_max <- df_interview_time_by_team %>% filter(id00 == 190) %>% select(max_interview_time) %>% pull
  mit_team_20_max <- df_interview_time_by_team %>% filter(id00 == 200) %>% select(max_interview_time) %>% pull
  mit_team_21_max <- df_interview_time_by_team %>% filter(id00 == 210) %>% select(max_interview_time) %>% pull
  mit_team_22_max <- df_interview_time_by_team %>% filter(id00 == 220) %>% select(max_interview_time) %>% pull
  mit_team_23_max <- df_interview_time_by_team %>% filter(id00 == 230) %>% select(max_interview_time) %>% pull
  mit_team_24_max <- df_interview_time_by_team %>% filter(id00 == 240) %>% select(max_interview_time) %>% pull
  mit_team_25_max <- df_interview_time_by_team %>% filter(id00 == 250) %>% select(max_interview_time) %>% pull
  mit_team_26_max <- df_interview_time_by_team %>% filter(id00 == 260) %>% select(max_interview_time) %>% pull
  mit_team_27_max <- df_interview_time_by_team %>% filter(id00 == 270) %>% select(max_interview_time) %>% pull
  mit_team_28_max <- df_interview_time_by_team %>% filter(id00 == 280) %>% select(max_interview_time) %>% pull
  mit_team_29_max <- df_interview_time_by_team %>% filter(id00 == 290) %>% select(max_interview_time) %>% pull
  mit_team_30_max <- df_interview_time_by_team %>% filter(id00 == 300) %>% select(max_interview_time) %>% pull
  mit_team_31_max <- df_interview_time_by_team %>% filter(id00 == 310) %>% select(max_interview_time) %>% pull
  mit_team_32_max <- df_interview_time_by_team %>% filter(id00 == 320) %>% select(max_interview_time) %>% pull
  mit_team_33_max <- df_interview_time_by_team %>% filter(id00 == 330) %>% select(max_interview_time) %>% pull
  mit_team_34_max <- df_interview_time_by_team %>% filter(id00 == 340) %>% select(max_interview_time) %>% pull
  mit_team_35_max <- df_interview_time_by_team %>% filter(id00 == 350) %>% select(max_interview_time) %>% pull
  mit_team_36_max <- df_interview_time_by_team %>% filter(id00 == 360) %>% select(max_interview_time) %>% pull
  mit_team_37_max <- df_interview_time_by_team %>% filter(id00 == 370) %>% select(max_interview_time) %>% pull
  mit_team_38_max <- df_interview_time_by_team %>% filter(id00 == 380) %>% select(max_interview_time) %>% pull
  mit_team_39_max <- df_interview_time_by_team %>% filter(id00 == 390) %>% select(max_interview_time) %>% pull
  mit_team_40_max <- df_interview_time_by_team %>% filter(id00 == 400) %>% select(max_interview_time) %>% pull
  
  
  #* median
  mit_team_01_median <- df_interview_time_by_team %>% filter(id00 == 10) %>% select(median_interview_time) %>% pull
  mit_team_02_median <- df_interview_time_by_team %>% filter(id00 == 20) %>% select(median_interview_time) %>% pull
  mit_team_03_median <- df_interview_time_by_team %>% filter(id00 == 30) %>% select(median_interview_time) %>% pull
  mit_team_04_median <- df_interview_time_by_team %>% filter(id00 == 40) %>% select(median_interview_time) %>% pull
  mit_team_05_median <- df_interview_time_by_team %>% filter(id00 == 50) %>% select(median_interview_time) %>% pull
  mit_team_06_median <- df_interview_time_by_team %>% filter(id00 == 60) %>% select(median_interview_time) %>% pull
  mit_team_07_median <- df_interview_time_by_team %>% filter(id00 == 70) %>% select(median_interview_time) %>% pull
  mit_team_08_median <- df_interview_time_by_team %>% filter(id00 == 80) %>% select(median_interview_time) %>% pull
  mit_team_09_median <- df_interview_time_by_team %>% filter(id00 == 90) %>% select(median_interview_time) %>% pull
  mit_team_10_median <- df_interview_time_by_team %>% filter(id00 == 100) %>% select(median_interview_time) %>% pull
  mit_team_11_median <- df_interview_time_by_team %>% filter(id00 == 110) %>% select(median_interview_time) %>% pull
  mit_team_12_median <- df_interview_time_by_team %>% filter(id00 == 120) %>% select(median_interview_time) %>% pull
  mit_team_13_median <- df_interview_time_by_team %>% filter(id00 == 130) %>% select(median_interview_time) %>% pull
  mit_team_14_median <- df_interview_time_by_team %>% filter(id00 == 140) %>% select(median_interview_time) %>% pull
  mit_team_15_median <- df_interview_time_by_team %>% filter(id00 == 150) %>% select(median_interview_time) %>% pull
  mit_team_16_median <- df_interview_time_by_team %>% filter(id00 == 160) %>% select(median_interview_time) %>% pull
  mit_team_17_median <- df_interview_time_by_team %>% filter(id00 == 170) %>% select(median_interview_time) %>% pull
  mit_team_18_median <- df_interview_time_by_team %>% filter(id00 == 180) %>% select(median_interview_time) %>% pull
  mit_team_19_median <- df_interview_time_by_team %>% filter(id00 == 190) %>% select(median_interview_time) %>% pull
  mit_team_20_median <- df_interview_time_by_team %>% filter(id00 == 200) %>% select(median_interview_time) %>% pull
  mit_team_21_median <- df_interview_time_by_team %>% filter(id00 == 210) %>% select(median_interview_time) %>% pull
  mit_team_22_median <- df_interview_time_by_team %>% filter(id00 == 220) %>% select(median_interview_time) %>% pull
  mit_team_23_median <- df_interview_time_by_team %>% filter(id00 == 230) %>% select(median_interview_time) %>% pull
  mit_team_24_median <- df_interview_time_by_team %>% filter(id00 == 240) %>% select(median_interview_time) %>% pull
  mit_team_25_median <- df_interview_time_by_team %>% filter(id00 == 250) %>% select(median_interview_time) %>% pull
  mit_team_26_median <- df_interview_time_by_team %>% filter(id00 == 260) %>% select(median_interview_time) %>% pull
  mit_team_27_median <- df_interview_time_by_team %>% filter(id00 == 270) %>% select(median_interview_time) %>% pull
  mit_team_28_median <- df_interview_time_by_team %>% filter(id00 == 280) %>% select(median_interview_time) %>% pull
  mit_team_29_median <- df_interview_time_by_team %>% filter(id00 == 290) %>% select(median_interview_time) %>% pull
  mit_team_30_median <- df_interview_time_by_team %>% filter(id00 == 300) %>% select(median_interview_time) %>% pull
  mit_team_31_median <- df_interview_time_by_team %>% filter(id00 == 310) %>% select(median_interview_time) %>% pull
  mit_team_32_median <- df_interview_time_by_team %>% filter(id00 == 320) %>% select(median_interview_time) %>% pull
  mit_team_33_median <- df_interview_time_by_team %>% filter(id00 == 330) %>% select(median_interview_time) %>% pull
  mit_team_34_median <- df_interview_time_by_team %>% filter(id00 == 340) %>% select(median_interview_time) %>% pull
  mit_team_35_median <- df_interview_time_by_team %>% filter(id00 == 350) %>% select(median_interview_time) %>% pull
  mit_team_36_median <- df_interview_time_by_team %>% filter(id00 == 360) %>% select(median_interview_time) %>% pull
  mit_team_37_median <- df_interview_time_by_team %>% filter(id00 == 370) %>% select(median_interview_time) %>% pull
  mit_team_38_median <- df_interview_time_by_team %>% filter(id00 == 380) %>% select(median_interview_time) %>% pull
  mit_team_39_median <- df_interview_time_by_team %>% filter(id00 == 390) %>% select(median_interview_time) %>% pull
  mit_team_40_median <- df_interview_time_by_team %>% filter(id00 == 400) %>% select(median_interview_time) %>% pull

  #******************
  #* Team Performance
  #******************
   
  #* completed hholds by team
  compl_hholds_by_team <- rds_household %>% 
    inner_join(rds_metadata, by = c("level_1_id")) %>% 
    filter(final_confirmation == 1) %>% 
    group_by(id00.y, id01.y) %>% 
    summarize(total_hholds = n()) 
  
  team_01_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 10) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_02_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 20) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_03_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 30) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_04_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 40) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_05_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 50) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_06_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 60) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_07_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 70) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_08_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 80) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_09_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 90) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_10_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 100) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_11_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 110) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_12_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 120) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_13_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 130) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_14_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 140) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_15_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 150) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_16_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 160) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_17_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 170) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_18_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 180) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_19_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 190) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_20_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 200) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_21_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 210) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_22_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 220) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_23_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 230) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_24_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 240) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_25_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 250) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_26_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 260) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_27_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 270) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_28_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 280) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_29_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 290) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_30_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 300) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  
  team_31_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 310) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_32_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 320) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_33_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 330) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_34_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 340) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_35_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 350) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_36_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 360) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_37_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 370) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_38_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 380) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_39_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 390) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  team_40_hh <- compl_hholds_by_team %>% 
    filter(id00.y == 400) %>% 
    ungroup(`id00.y`) %>% 
    select(id01.y, total_hholds) %>% 
    rename("cluster" = id01.y, "households" = total_hholds)
  
  
  #* last sync update by team
  last_sync_by_team <- rds_metadata %>% 
    select(id00, id01, v05) %>% 
    mutate(last_sync = difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')) %>% 
    na.omit() %>% 
    group_by(id00) %>% 
    summarize(last_sync_days = as.numeric(round(min(last_sync), digits = 0)))
  
  team_01_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 10) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_02_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 20) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_03_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 30) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_04_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 40) %>% 
    select(last_sync_days) %>% 
    pull
  

  team_05_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 50) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_05_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 50) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_06_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 60) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_07_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 70) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_08_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 80) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_09_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 90) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_10_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 100) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_11_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 110) %>% 
    select(last_sync_days) %>% 
    pull
  
  
  team_12_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 120) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_13_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 130) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_14_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 140) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_15_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 150) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_16_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 160) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_17_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 170) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_18_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 180) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_19_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 190) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_20_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 200) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_21_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 210) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_22_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 220) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_23_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 230) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_24_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 240) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_25_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 250) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_26_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 260) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_27_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 270) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_28_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 280) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_29_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 290) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_30_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 300) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_31_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 310) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_32_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 320) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_33_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 330) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_34_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 340) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_35_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 350) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_36_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 360) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_37_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 370) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_38_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 380) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_39_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 390) %>% 
    select(last_sync_days) %>% 
    pull
  
  team_40_last_sync_days <- last_sync_by_team %>% 
    filter(id00 == 400) %>% 
    select(last_sync_days) %>% 
    pull
 
   
#* ENUMERATION SUMMARIES
#* **********************
#* 
#* Enumerations per day
#* 
#* 
  df_enum_summaries <- rds_indiv %>% 
    inner_join(rds_metadata, by = c("level_1_id"))
  
  teams = df_enum_summaries$id00.y %>% unique() %>% sort(decreasing = FALSE)
  
  #* get main enumeration data
  df_enum_per_day <- df_enum_summaries %>% 
    select(indrecord_id,  v04) %>% 
    na.omit() %>% 
    group_by(v04) %>% 
    summarize(count = n()) %>% 
    mutate(enumeration_date = substr(as.POSIXct(ymd(v04)), -1, 10)) %>% 
    select(enumeration_date, count) %>% 
    arrange(enumeration_date)
  
  
  #* Enumerations per day per team
  #* 
  df_enum_per_day_per_team <- df_enum_summaries %>% 
    select(id00.y, indrecord_id,  v04) %>% 
    na.omit() %>% 
    group_by(id00.y, v04) %>% 
    summarize(count = n()) %>% 
    mutate(enumeration_date = substr(as.POSIXct(ymd(v04)), -1, 10)) %>% 
    select(enumeration_date, count) %>% 
    arrange(enumeration_date) %>% 
    rename("team" = id00.y)
  
  create_enum_per_day_per_team <- function(xlist) {
    
    for (i in teams) {
      tx <- df_enum_per_day_per_team %>% filter(team == i)
      xlist[[length(xlist)+1]] <- tx
    }
    
    return(xlist)
  }
  
  enum_per_day_per_team <- list()
  enum_per_day_per_team <- create_enum_per_day_per_team(enum_per_day_per_team)

 
  #* Enumerations per enumerator
  #* 
  df_enum_per_enumerator <- df_enum_summaries %>% 
    select(indrecord_id,  v01c) %>% 
    na.omit() %>% 
    group_by(v01c) %>% 
    summarize(count = n()) %>% 
    rename("enumerator" = v01c) %>%
    arrange(enumerator)
  
  
  #* Enumerations per enumerator per team
  #* 
  df_enum_per_enumerator_per_team <- df_enum_summaries %>% 
    select(id00.y, indrecord_id,  v01c, v04) %>% 
    na.omit() %>% 
    group_by(id00.y, v01c, v04) %>% 
    summarize(count = n()) %>% 
    mutate(enumeration_date = substr(as.POSIXct(ymd(v04)), -1, 10)) %>%
    select(id00.y, v01c, enumeration_date, count) %>% 
    rename("enumerator" = v01c, "team" = id00.y) %>%
    arrange(team)
  

  create_enum_per_enumerator_per_team <- function(xlist) {
    
    for (i in teams) {
      tx <- df_enum_per_enumerator_per_team %>% filter(team == i)
      xlist[[length(xlist)+1]] <- tx
    }
    
    return(xlist)
  }
  
  enum_per_enumerator_per_team <- list()
  enum_per_enumerator_per_team <- create_enum_per_enumerator_per_team(enum_per_enumerator_per_team)
  
  #*******************************************************************************************************
  #*
  #* COLLATE AND SAVE AS XLXS
  #*
  #* households completed by cluster
  # team_vars = list(team_01_hh, team_02_hh, team_03_hh, team_04_hh, team_05_hh, team_06_hh, team_07_hh, team_08_hh, team_09_hh, team_10_hh,
  #                  team_11_hh, team_12_hh, team_13_hh, team_14_hh, team_15_hh, team_16_hh, team_17_hh, team_18_hh, team_19_hh, team_20_hh,
  #                  team_21_hh, team_22_hh, team_23_hh, team_24_hh, team_25_hh, team_26_hh, team_27_hh, team_28_hh, team_29_hh, team_30_hh,
  #                  team_31_hh, team_32_hh, team_33_hh, team_34_hh, team_35_hh, team_36_hh, team_37_hh, team_38_hh, team_39_hh, team_40_hh)
  # 
  # counter = 0
  # for (i in team_vars) {
  #   counter <- counter + 1
  #   write.xlsx(i, file = "ahies_households_completed_stats.xlsx",
  #              sheetName = paste0("Team_", as.character(counter)),
  #              col.names = TRUE,
  #              append = TRUE
  #   )
  # }
  # 
  # #* statistics
  # 
  # #* minimum
  # team_stats_min <- matrix(c(mit_team_01_min, mit_team_02_min, mit_team_03_min, mit_team_04_min, mit_team_05_min,
  #                            mit_team_06_min, mit_team_07_min, mit_team_08_min, mit_team_09_min, mit_team_10_min,
  #                            mit_team_11_min, mit_team_12_min, mit_team_13_min, mit_team_14_min, mit_team_15_min,
  #                            mit_team_16_min, mit_team_17_min, mit_team_18_min, mit_team_19_min, mit_team_20_min,
  #                            mit_team_21_min, mit_team_22_min, mit_team_23_min, mit_team_24_min, mit_team_25_min,
  #                            mit_team_26_min, mit_team_27_min, mit_team_28_min, mit_team_29_min, mit_team_30_min,
  #                            mit_team_31_min, mit_team_32_min, mit_team_33_min, mit_team_34_min, mit_team_35_min,
  #                            mit_team_36_min, mit_team_37_min, mit_team_38_min, mit_team_39_min, mit_team_40_min),
  #                          nrow = 40, ncol = 1)
  # #* maximum
  # team_stats_max <- matrix(c(mit_team_01_max, mit_team_02_max, mit_team_03_max, mit_team_04_max, mit_team_05_max,
  #                            mit_team_06_max, mit_team_07_max, mit_team_08_max, mit_team_09_max, mit_team_10_max,
  #                            mit_team_11_max, mit_team_12_max, mit_team_13_max, mit_team_14_max, mit_team_15_max,
  #                            mit_team_16_max, mit_team_17_max, mit_team_18_max, mit_team_19_max, mit_team_20_max,
  #                            mit_team_21_max, mit_team_22_max, mit_team_23_max, mit_team_24_max, mit_team_25_max,
  #                            mit_team_26_max, mit_team_27_max, mit_team_28_max, mit_team_29_max, mit_team_30_max,
  #                            mit_team_31_max, mit_team_32_max, mit_team_33_max, mit_team_34_max, mit_team_35_max,
  #                            mit_team_36_max, mit_team_37_max, mit_team_38_max, mit_team_39_max, mit_team_40_max),
  #                          nrow = 40, ncol = 1)
  # 
  # #* mean
  # team_stats_mean <- matrix(c(mit_team_01_mean, mit_team_02_mean, mit_team_03_mean, mit_team_04_mean, mit_team_05_mean,
  #                             mit_team_06_mean, mit_team_07_mean, mit_team_08_mean, mit_team_09_mean, mit_team_10_mean,
  #                             mit_team_11_mean, mit_team_12_mean, mit_team_13_mean, mit_team_14_mean, mit_team_15_mean,
  #                             mit_team_16_mean, mit_team_17_mean, mit_team_18_mean, mit_team_19_mean, mit_team_20_mean,
  #                             mit_team_21_mean, mit_team_22_mean, mit_team_23_mean, mit_team_24_mean, mit_team_25_mean,
  #                             mit_team_26_mean, mit_team_27_mean, mit_team_28_mean, mit_team_29_mean, mit_team_30_mean,
  #                             mit_team_31_mean, mit_team_32_mean, mit_team_33_mean, mit_team_34_mean, mit_team_35_mean,
  #                             mit_team_36_mean, mit_team_37_mean, mit_team_38_mean, mit_team_39_mean, mit_team_40_mean),
  #                           nrow = 40, ncol = 1)
  # 
  # #* median
  # team_stats_median <- matrix(c(mit_team_01_median, mit_team_02_median, mit_team_03_median, mit_team_04_median, mit_team_05_median,
  #                               mit_team_06_median, mit_team_07_median, mit_team_08_median, mit_team_09_median, mit_team_10_median,
  #                               mit_team_11_median, mit_team_12_median, mit_team_13_median, mit_team_14_median, mit_team_15_median,
  #                               mit_team_16_median, mit_team_17_median, mit_team_18_median, mit_team_19_median, mit_team_20_median,
  #                               mit_team_21_median, mit_team_22_median, mit_team_23_median, mit_team_24_median, mit_team_25_median,
  #                               mit_team_26_median, mit_team_27_median, mit_team_28_median, mit_team_29_median, mit_team_30_median,
  #                               mit_team_31_median, mit_team_32_median, mit_team_33_median, mit_team_34_median, mit_team_35_median,
  #                               mit_team_36_median, mit_team_37_median, mit_team_38_median, mit_team_39_median, mit_team_40_median),
  #                             nrow = 40, ncol = 1)
  # 
  # last_sync_data <- matrix(c(team_01_last_sync_days, team_02_last_sync_days, team_03_last_sync_days, team_04_last_sync_days, team_05_last_sync_days,
  #                            team_06_last_sync_days, team_07_last_sync_days, team_08_last_sync_days, team_09_last_sync_days, team_10_last_sync_days,
  #                            team_11_last_sync_days, team_12_last_sync_days, team_13_last_sync_days, team_14_last_sync_days, team_15_last_sync_days,
  #                            team_16_last_sync_days, team_17_last_sync_days, team_18_last_sync_days, team_19_last_sync_days, team_20_last_sync_days,
  #                            team_21_last_sync_days, team_22_last_sync_days, team_23_last_sync_days, team_24_last_sync_days, team_25_last_sync_days,
  #                            team_26_last_sync_days, team_27_last_sync_days, team_28_last_sync_days, team_29_last_sync_days, team_30_last_sync_days,
  #                            team_31_last_sync_days, team_32_last_sync_days, team_33_last_sync_days, team_34_last_sync_days, team_35_last_sync_days,
  #                            team_36_last_sync_days, team_37_last_sync_days, team_38_last_sync_days, team_39_last_sync_days, team_40_last_sync_days), 
  #                             nrow = 40, ncol = 1)
  # 
  # teams <- matrix(c(1:40))
  # team_stats_list <- list(teams, 
  #                         last_sync_data, 
  #                         team_stats_min, 
  #                         team_stats_max, team_stats_mean, team_stats_median)
  # 
  # summary_stats_by_team <- as.data.frame(team_stats_list)
  # colnames(summary_stats_by_team) = c("teams", "last_sync(days)", "minimum (hrs)", "maximum(hrs)", "mean(hrs)", "median(hrs)")
  # 
  # write.xlsx(summary_stats_by_team, file = "ahies_interview_time_stats.xlsx",
  #            sheetName = "interview_time_statistics",
  #            col.names = TRUE,
  #            append = TRUE
  # )
  # 
  # 
  # #**************************************************************************************************
  
  # mutate(last_sync = difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')) %>%
  
  #* Enumerators
  #* id00-team_code, id01.y-cluster, id02-hh_id, 
  df_enumerators_stats <- rds_metadata %>% 
    inner_join(rds_household, by = c("id00", "idq0", "id01", "id02")) %>% 
    select(id00, id01, id02, v01, v01a, v01b, v01c, v04, 
           v05, v03a, v03b, hhrecord_id.x, final_confirmation) %>% 
    filter(final_confirmation == 1)
  
  
  #* households completed/last syncing done in the last 24hrs by team by enumerators
  df_enumerators_hhold_compl <- df_enumerators_stats %>% 
    select(id00, v01c, v05) %>% 
    mutate(last_sync = difftime(cur_date, strptime(v05, format = "%Y%m%d"), units = 'days')) %>%
    na.omit() %>% 
    group_by(id00, v01c) %>% 
    summarize(completed_hholds = n())
  
  #* completed hholds by team by enumerator
  df_output_by_enums_10 <- df_enumerators_hhold_compl %>% 
    filter(id00 == "10") 

  
  df_output_by_enums_20 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 20)
  
  df_output_by_enums_30 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 30) 
  
  df_output_by_enums_40 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 40)
  
  df_output_by_enums_50 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 50) 
  
  df_output_by_enums_60 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 60) %>% 
    select(v01c, completed_hholds)
  
  df_output_by_enums_70 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 70) 
  
  df_output_by_enums_80 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 80) 
  
  df_output_by_enums_90 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 90) 
  
  df_output_by_enums_100 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 100) 
  
  df_output_by_enums_110 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 110) 
  
  df_output_by_enums_120 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 120) 
  
  df_output_by_enums_130 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 130)
  
  df_output_by_enums_140 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 140) 
  
  df_output_by_enums_150 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 150)
  
  df_output_by_enums_160 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 160) 
  
  df_output_by_enums_170 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 170) 
  
  df_output_by_enums_180 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 180) 
  
  df_output_by_enums_190 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 190) 
  
  df_output_by_enums_200 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 200) 
  
  df_output_by_enums_210 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 210) 
  
  df_output_by_enums_220 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 220) 
  
  df_output_by_enums_230 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 230) 
  
  df_output_by_enums_240 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 240) 
  
  df_output_by_enums_250 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 250) 
  
  df_output_by_enums_260 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 260) 
  
  df_output_by_enums_270 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 270) 
  
  df_output_by_enums_280 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 280) 
  
  df_output_by_enums_290 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 290) 
  
  df_output_by_enums_300 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 300) 
  
  df_output_by_enums_310 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 310)
  
  df_output_by_enums_320 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 320) 
  
  df_output_by_enums_330 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 330) 
  
  df_output_by_enums_340 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 340) 
  
  df_output_by_enums_350 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 350) 
  
  df_output_by_enums_360 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 360) 
  
  df_output_by_enums_370 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 370) 
  
  df_output_by_enums_350 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 350) 
  
  df_output_by_enums_360 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 360) 
  
  df_output_by_enums_370 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 370)
  
  df_output_by_enums_380 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 380) 
  
  df_output_by_enums_390 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 390) 
  
  df_output_by_enums_400 <- df_enumerators_hhold_compl %>% 
    filter(id00 == 400) 
  

  #* LAST SYNC BY ENUMERATOR
  #* ***********************
  #* last sync update by supervisor
  
  # supervisor ####
  last_sync_by_supervisor <- rds_metadata %>% 
    select(id00, v02, v03b, v05) %>% 
    mutate(interview_end_date = as.character(strptime(paste0(v05, v03b), format = "%Y%m%d %H%M"))) %>%
    mutate(last_sync = as.integer(difftime(cur_date, interview_end_date, units = 'days'))) %>%
    na.omit() %>% 
    group_by(id00, v02) %>% 
    summarize(last_sync = as.integer(min(last_sync))) %>%
    mutate(last_sync_24hrs = as.integer(ifelse(last_sync >= 0 & last_sync <= 1440, last_sync, -999))) %>% 
    mutate(team = as.integer(id00 %/% 10)) %>% 
    rename("supervisor"=v02) %>% 
    ungroup() %>% 
    select(team, supervisor, last_sync, last_sync_24hrs)
  
  last_sync_by_enumerator <- rds_metadata %>% 
    select(id00, v01c, v03b, v05) %>% 
    mutate(interview_end_date = as.character(strptime(paste0(v05, v03b), format = "%Y%m%d %H%M"))) %>%
    mutate(last_sync = as.integer(difftime(cur_date, interview_end_date, units = 'days'))) %>%
    na.omit() %>% 
    group_by(id00, v01c) %>% 
    summarize(last_sync = as.integer(min(last_sync))) %>%
    mutate(last_sync_24hrs = as.integer(ifelse(last_sync >= 0 & last_sync <= 24, last_sync, -999))) 
  
  # mutate(interview_time = round(difftime(end_date, start_date, units = 'hours'), digits = 1)) %>% 
  # mutate(last_sync = difftime(cur_date, strptime(interview_end_date, format = "%Y%m%d"), units = 'days')) %>%
  #  
  
  
  df_output_by_enums_10 <- last_sync_by_enumerator %>% 
    filter(id00 == 10) %>% 
    inner_join(df_output_by_enums_10, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_20 <- last_sync_by_enumerator %>% 
    filter(id00 == 20) %>% 
    inner_join(df_output_by_enums_20, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_30 <- last_sync_by_enumerator %>% 
    filter(id00 == 30) %>% 
    inner_join(df_output_by_enums_30, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_40 <- last_sync_by_enumerator %>% 
    filter(id00 == 40) %>% 
    inner_join(df_output_by_enums_40, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_50 <- last_sync_by_enumerator %>% 
    filter(id00 == 50) %>% 
    inner_join(df_output_by_enums_50, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  
  df_output_by_enums_60 <- last_sync_by_enumerator %>% 
    filter(id00 == 60) %>% 
    inner_join(df_output_by_enums_60, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_70 <- last_sync_by_enumerator %>% 
    filter(id00 == 70) %>% 
    inner_join(df_output_by_enums_70, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_80 <- last_sync_by_enumerator %>% 
    filter(id00 == 80) %>% 
    inner_join(df_output_by_enums_80, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_90 <- last_sync_by_enumerator %>% 
    filter(id00 == 90) %>% 
    inner_join(df_output_by_enums_90, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_100 <- last_sync_by_enumerator %>% 
    filter(id00 == 100) %>% 
    inner_join(df_output_by_enums_100, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_110 <- last_sync_by_enumerator %>% 
    filter(id00 == 110) %>% 
    inner_join(df_output_by_enums_110, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_120 <- last_sync_by_enumerator %>% 
    filter(id00 == 120) %>% 
    inner_join(df_output_by_enums_120, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_130 <- last_sync_by_enumerator %>% 
    filter(id00 == 130) %>% 
    inner_join(df_output_by_enums_130, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_140 <- last_sync_by_enumerator %>% 
    filter(id00 == 140) %>% 
    inner_join(df_output_by_enums_140, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_150 <- last_sync_by_enumerator %>% 
    filter(id00 == 150) %>% 
    inner_join(df_output_by_enums_150, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_160 <- last_sync_by_enumerator %>% 
    filter(id00 == 160) %>% 
    inner_join(df_output_by_enums_160, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_170 <- last_sync_by_enumerator %>% 
    filter(id00 == 170) %>% 
    inner_join(df_output_by_enums_170, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_180 <- last_sync_by_enumerator %>% 
    filter(id00 == 180) %>% 
    inner_join(df_output_by_enums_180, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_190 <- last_sync_by_enumerator %>% 
    filter(id00 == 190) %>% 
    inner_join(df_output_by_enums_190, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_200 <- last_sync_by_enumerator %>% 
    filter(id00 == 200) %>% 
    inner_join(df_output_by_enums_200, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_210 <- last_sync_by_enumerator %>% 
    filter(id00 == 210) %>% 
    inner_join(df_output_by_enums_210, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_220 <- last_sync_by_enumerator %>% 
    filter(id00 == 220) %>% 
    inner_join(df_output_by_enums_220, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_230 <- last_sync_by_enumerator %>% 
    filter(id00 == 230) %>% 
    inner_join(df_output_by_enums_230, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_240 <- last_sync_by_enumerator %>% 
    filter(id00 == 240) %>% 
    inner_join(df_output_by_enums_240, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_250 <- last_sync_by_enumerator %>% 
    filter(id00 == 250) %>% 
    inner_join(df_output_by_enums_250, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_260 <- last_sync_by_enumerator %>% 
    filter(id00 == 260) %>% 
    inner_join(df_output_by_enums_260, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_270 <- last_sync_by_enumerator %>% 
    filter(id00 == 270) %>% 
    inner_join(df_output_by_enums_270, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_280 <- last_sync_by_enumerator %>% 
    filter(id00 == 280) %>% 
    inner_join(df_output_by_enums_280, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_290 <- last_sync_by_enumerator %>% 
    filter(id00 == 290) %>% 
    inner_join(df_output_by_enums_290, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_300 <- last_sync_by_enumerator %>% 
    filter(id00 == 300) %>% 
    inner_join(df_output_by_enums_300, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  
  df_output_by_enums_310 <- last_sync_by_enumerator %>% 
    filter(id00 == 310) %>% 
    inner_join(df_output_by_enums_310, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_320 <- last_sync_by_enumerator %>% 
    filter(id00 == 320) %>% 
    inner_join(df_output_by_enums_320, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_330 <- last_sync_by_enumerator %>% 
    filter(id00 == 330) %>% 
    inner_join(df_output_by_enums_330, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_340 <- last_sync_by_enumerator %>% 
    filter(id00 == 340) %>% 
    inner_join(df_output_by_enums_340, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_350 <- last_sync_by_enumerator %>% 
    filter(id00 == 350) %>% 
    inner_join(df_output_by_enums_350, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_360 <- last_sync_by_enumerator %>% 
    filter(id00 == 360) %>% 
    inner_join(df_output_by_enums_360, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_370 <- last_sync_by_enumerator %>% 
    filter(id00 == 370) %>% 
    inner_join(df_output_by_enums_370, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_380 <- last_sync_by_enumerator %>% 
    filter(id00 == 380) %>% 
    inner_join(df_output_by_enums_380, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_390 <- last_sync_by_enumerator %>% 
    filter(id00 == 390) %>% 
    inner_join(df_output_by_enums_390, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  df_output_by_enums_400 <- last_sync_by_enumerator %>% 
    filter(id00 == 400) %>% 
    inner_join(df_output_by_enums_400, by = c("id00", "v01c")) %>% 
    rename("Team" = id00, 
           "Enumerator" = v01c, 
           "Completed_Households" = completed_hholds,
           "Last Sync" = last_sync,
           "Last_Sync_24hrs" = last_sync_24hrs)
  
  
  #**************************
  #* Cluster Completion Rate
  #* ************************
  # current_1####
  df_hh_completion <- rds_metadata %>% 
    select(id00, id01, idq0, final_confirmation) %>% 
    filter(final_confirmation == 1, idq0 == 3) %>% 
    group_by(id00, id01) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  df_cluster_completion <- df_hh_completion %>% 
    count(id00) %>% 
    mutate("team" = as.factor(as.numeric(id00 %/% 10)))
  
  # ggplot2::ggplot(data=df_cluster_completion, aes(x=id00, y=n)) +
  #   ggplot2::geom_col()
  
  
  
  
  
  #*************************************
  #* Economic
  #************************************
  #* 
  df_econ_act <- rds_indiv %>% 
    select(s1aq4y, s4aq1, s4aq4, s4aq7, s4aq11, s4aq15, s4aq15, s4aq19, s4aq22, s4aq25) %>% 
    mutate(age_group = case_when(s1aq4y >= 0 & s1aq4y <= 4    ~ "0-4",
                                 s1aq4y >= 5 & s1aq4y <= 14   ~ "5-14",
                                 s1aq4y >= 15 & s1aq4y <= 64  ~ "15-64",
                                 s1aq4y >= 65                 ~ "65+"
                                )
           )
  
  #* wage/salary
  df_wage_salary <- df_econ_act %>% 
    select(s4aq1, age_group) %>% 
    na.omit() %>% 
    count(s4aq1, age_group) %>% 
    rename("count" = n) %>% 
    mutate(wage_salary = case_when(`s4aq1` == 1 ~ "Yes",
                                `s4aq1` == 2 ~ "No")) %>% 
    select(wage_salary, age_group, count) %>% 
    arrange(desc(wage_salary), desc(age_group), desc(count))
  
  
  #* domestic
  df_domestic <- df_econ_act %>% 
    select(s4aq4, age_group) %>% 
    na.omit() %>% 
    count(s4aq4, age_group) %>% 
    rename("count" = n) %>% 
    mutate(domestic = case_when(`s4aq4` == 1 ~ "Yes",
                                   `s4aq4` == 2 ~ "No")) %>% 
    select(domestic, age_group, count) %>% 
    arrange(desc(domestic), desc(age_group), desc(count))
  
  #* farm work
  df_farm_work <- df_econ_act %>% 
    select(s4aq7, age_group) %>% 
    na.omit() %>% 
    count(s4aq7, age_group) %>% 
    rename("count" = n) %>% 
    mutate(farm_work = case_when(`s4aq7` == 1 ~ "Yes",
                                `s4aq7` == 2 ~ "No")) %>% 
    select(farm_work, age_group, count) %>% 
    arrange(desc(farm_work), desc(age_group), desc(count))
  
  #* non farm work
  df_non_farm_work <- df_econ_act %>% 
    select(s4aq11, age_group) %>% 
    na.omit() %>% 
    count(s4aq11, age_group) %>% 
    rename("count" = n) %>% 
    mutate(non_farm_work = case_when(`s4aq11` == 1 ~ "Yes", 
                                     `s4aq11` == 2 ~ "No")) %>% 
    select(non_farm_work, age_group, count) %>% 
    arrange(desc(non_farm_work), desc(age_group), desc(count))
  
  #* family help
  df_family_help <- df_econ_act %>% 
    select(s4aq15, age_group) %>% 
    na.omit() %>% 
    count(s4aq15, age_group) %>% 
    rename("count" = n) %>% 
    mutate(family_work = case_when(`s4aq15` == 1 ~ "Yes",
                                 `s4aq15` == 2 ~ "No")) %>% 
    select(family_work, age_group, count) %>% 
    arrange(desc(family_work), desc(age_group), desc(count))
  
  #* non-productive agric
  df_non_productive_agric <- df_econ_act %>% 
    select(s4aq19, age_group) %>% 
    na.omit() %>% 
    count(s4aq19, age_group) %>% 
    rename("count" = n) %>% 
    mutate(non_productive_agric = case_when(`s4aq19` == 1 ~ "Yes",
                                 `s4aq19` == 2 ~ "No")) %>% 
    select(non_productive_agric, age_group, count) %>% 
    arrange(desc(non_productive_agric), desc(age_group), desc(count))
  
  
  
  #*(s3bq3-wt, s3bq4-ht, s1aq1-sex, s1aq4y-age)
  #*
  
  # demography_distr ####
  
  # AGE DISTRIBUTION OF HOUSEHOLD HEADS
  df_hhold_head_age_distr <- rds_indiv %>% 
    select(s1aq2, s1aq4y) %>% 
    filter(s1aq2 == 1) %>% 
    mutate(age_group = case_when(s1aq4y >= 0 & s1aq4y <= 11    ~ "0-11",
                                 s1aq4y >= 12 & s1aq4y <= 18   ~ "12-18",
                                 s1aq4y > 18  ~ "18+",
                                 is.na(s1aq4y) ~ "Missing")
    ) %>% 
    mutate(rank = case_when(s1aq4y >= 0 & s1aq4y <= 11    ~ 1,
                            s1aq4y >= 12 & s1aq4y <= 18   ~ 2,
                            s1aq4y > 18                   ~ 18)
    ) %>% 
    group_by(s1aq2, rank, age_group) %>% 
    summarize(count = n()) %>% 
    arrange(rank)
  
  # AGE DISTRIBUTION OF HOUSEHOLD HEADS BY TEAM
  df_hhold_head_age_by_team_distr <- rds_indiv %>% 
    select(id00, s1aq2, s1aq4y) %>% 
    filter(s1aq2 == 1) %>% 
    mutate(age_group = case_when(s1aq4y >= 0 & s1aq4y <= 11    ~ "0-11",
                                 s1aq4y >= 12 & s1aq4y <= 18   ~ "12-18",
                                 s1aq4y > 18  ~ "18+",
                                 is.na(s1aq4y) ~ "Missing")
    ) %>% 
    mutate(rank = case_when(s1aq4y >= 0 & s1aq4y <= 11    ~ 1,
                            s1aq4y >= 12 & s1aq4y <= 18   ~ 2,
                            s1aq4y > 18                   ~ 18)
    ) %>% 
    group_by(id00, s1aq2, rank, age_group) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    arrange(id00, rank) %>% 
    rename("team" = id00)
  
  
  # AGE DISTRIBUTION
  df_age_distr <- rds_indiv %>% 
    select(s1aq4y) %>% 
    mutate(age_group = case_when(s1aq4y >= 0 & s1aq4y <= 4    ~ "0-4",
                                 s1aq4y >= 5 & s1aq4y <= 14   ~ "5-14",
                                 s1aq4y >= 15 & s1aq4y <= 64  ~ "15-64",
                                 s1aq4y >= 65                 ~ "65+",
                                 is.na(s1aq4y) ~ "Missing"
                                 )
    ) %>%
    mutate(rank = case_when(s1aq4y >= 0 & s1aq4y <= 4    ~ 1,
                                 s1aq4y >= 5 & s1aq4y <= 14   ~ 2,
                                 s1aq4y >= 15 & s1aq4y <= 64  ~ 3,
                                 s1aq4y >= 65                 ~ 4
                                 )
    ) %>% 
    group_by(rank, age_group) %>% 
    summarize(count = n()) %>% 
    arrange(rank)
  
  # MARITAL STATUS DISTRIBUTION
  df_marital_status <- rds_indiv %>% 
    select(s1aq5, s1aq4y) %>% 
    mutate(age_group = case_when(s1aq4y >= 0 & s1aq4y <= 4    ~ "0-4",
                                 s1aq4y >= 5 & s1aq4y <= 14   ~ "5-14",
                                 s1aq4y >= 15 & s1aq4y <= 64  ~ "15-64",
                                 s1aq4y >= 65                 ~ "65+",
                                 is.na(s1aq4y)                ~ "Missing")
    ) %>%
    mutate(rank = case_when(s1aq4y >= 0 & s1aq4y <= 4    ~ 1,
                            s1aq4y >= 5 & s1aq4y <= 14   ~ 2,
                            s1aq4y >= 15 & s1aq4y <= 64  ~ 3,
                            s1aq4y >= 65                 ~ 4)
    ) %>% 
    mutate(marital_status = case_when(s1aq5 == 1    ~ "Informal/Living Together",
                                      s1aq5 == 2    ~ "Married (Civil/Ordinance)",
                                      s1aq5 == 3    ~ "Married (Customary/Tradition)",
                                      s1aq5 == 4    ~ "Married (Islamic)",
                                      s1aq5 == 5    ~ "Married (Other Type)",
                                      s1aq5 == 6    ~ "Separated",
                                      s1aq5 == 7    ~ "Divorce",
                                      s1aq5 == 8    ~ "Widowed",
                                      s1aq5 == 9    ~ "Never Married",
                                      is.na(s1aq5)  ~ "Missing")
    ) %>%
    group_by(rank, s1aq5, age_group, marital_status) %>% 
    summarize(count = n()) %>% 
    arrange(rank)
  
  
  # REGLIGIOUS DENOMINATION
  df_religious_denom <- rds_indiv %>% 
    select(s1aq6) %>% 
    na.omit() %>% 
    mutate(denomination = case_when(s1aq6 == 1    ~ "Catholic",
                                      s1aq6 == 2    ~ "Protestant (Anglican, Lutheran, Presbyterian, Methodist etc)",
                                      s1aq6 == 3    ~ "Pentecostal/Charismatic",
                                      s1aq6 == 4    ~ "Other Christian",
                                      s1aq6 == 5    ~ "Islam",
                                      s1aq6 == 6    ~ "Traditionalist",
                                      s1aq6 == 7    ~ "No Religion",
                                      s1aq6 == 8    ~ "Other")) %>%
    group_by(s1aq6, denomination) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    mutate(total = sum(count), percentage = round(count/total * 100, digits = 2))
  
  # NATIONALITY
  df_nationality <- rds_indiv %>% 
    select(s1aq7) %>% 
    na.omit() %>% 
    mutate(nationality = case_when(s1aq7 == 1    ~ "Ghanaian by birth",
                                    s1aq7 == 2    ~ "Ghanaian by naturalisation",
                                    s1aq7 == 3    ~ "Dual nationality",
                                    s1aq7 == 4    ~ "Non-Ghanaian")) %>%
    group_by(s1aq7, nationality) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    mutate(total = sum(count), percentage = round(count/total * 100, digits = 2)) 
  
  
  # ETHNICITY
  df_ethnicity <- rds_indiv %>% 
    select(s1aq9a) %>% 
    group_by(s1aq9a) %>% 
    summarize(count = n()) %>% 
    mutate(s1aq9a = ifelse(is.na(s1aq9a), "Missing", s1aq9a)) %>%
    mutate(ethnicity = case_when(s1aq9a == 1    ~ "Akan",
                                 s1aq9a == 2    ~ "Ga-Dangme",
                                 s1aq9a == 3    ~ "Ewe",
                                 s1aq9a == 4    ~ "Guan",
                                 s1aq9a == 5    ~ "Gurma",
                                 s1aq9a == 6    ~ "Mole-Dagbani",
                                 s1aq9a == 7    ~ "Grusi",
                                 s1aq9a == 8    ~ "Mande",
                                 s1aq9a == 9    ~ "All Other Tribes (Hausa, Baribari, Zabrama)",
                                 is.na(s1aq9a)  ~ "Missing")) %>%
    mutate(total = sum(count), percentage = round(count/total * 100, digits = 2)) %>% 
    ungroup() %>% 
    select(ethnicity, count, percentage)
    
  

  #* WEIGHTS & HEIGHTS
  #* responds with weights taken 
  df_weights_heights <- rds_indiv %>% 
    select(s3bq3, s3bq4, s1aq1, s1aq4y) %>% 
    mutate(age_group = case_when(s1aq4y >= 0 & s1aq4y <= 4    ~ "0-4",
                                 s1aq4y >= 5 & s1aq4y <= 14   ~ "5-14",
                                 s1aq4y >= 15 & s1aq4y <= 64  ~ "15-64",
                                 s1aq4y >= 65                 ~ "65+"
                                )
    ) %>% 
    group_by(s1aq1, age_group) 
  
  #* responds with weights taken 
  df_weights <- df_weights_heights %>% 
    select(s3bq3, s1aq1, age_group) %>% 
    mutate(weight_taken = ifelse(s3bq3 > 0 & s3bq3 != 999.94, "Yes", "No")) %>% 
    group_by(weight_taken, s1aq1, age_group) %>% 
    summarize(count = n()) %>% 
    mutate(s1aq1 = case_when(
      s1aq1 == 1 ~ "Male",
      s1aq1 == 2 ~ "Female",
      s1aq1 == NA ~ "Missing")) %>% 
    rename("sex" = s1aq1) 
  
  #* males with heights taken
  df_weights_males <- df_weights %>% 
    filter(sex == "Male") %>% 
    arrange(age_group)
  
  #* females with heights taken
  df_weights_females <- df_weights %>% 
    filter(sex == "Female")
  
  #* responds with weights taken 
  df_heights <- df_weights_heights %>% 
    select(s3bq3, s1aq1, age_group) %>% 
    mutate(weight_taken = ifelse(s3bq3 > 0 & s3bq3 != 999.94, "Yes", "No")) %>% 
    group_by(weight_taken, s1aq1, age_group) %>% 
    summarize(count = n()) %>% 
    mutate(s1aq1 = case_when(
      s1aq1 == 1 ~ "Male",
      s1aq1 == 2 ~ "Female",
      s1aq1 == NA ~ "Missing")) %>% 
    rename("sex" = s1aq1) 
  
  #* males with heights taken
  df_heights_males <- df_heights %>% 
    filter(sex == "Male")
  
  #* females with heights taken
  df_heights_females <- df_heights %>% 
    filter(sex == "Female")
    
    
#*
#* 
#* 
  

  
  
  
  # age - s1aqay
  #s4aq1-  wage/salary; 
  #s4aq4-  domestics
  #s4aq7-  farm work
  #s4aq11- non-farm
  #s4aq15- family-help
  #s4aq19- non-productive agric
  #s4aq22- apprentice
  #s4aq25-  voluntary work
  
  
  #* RENDER USER INTERFACE
  #* *********************
  #* 
  #* USERGUIDE
  #* *********
  
  output$userguide_boxes <- renderUI({
    req(credentials()$user_auth)
    
    last_download <- Sys.time()
    
    ug_box1 <-  card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("How to Login", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        span("Last Download Date: "),
        span(as.character(last_download), style = "color: yellow;"),
        p("Use your assigned PIT username and login")
      )
    )
    
    ug_box2 <-  card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("How to Login", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("These are instructions for logging in to the dashboard", style = "color: white;")
      )
    )
    
    ug_box3 <- card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("Summary of Enumeration", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("This module summarizes the survey data...", style = "color: white;")
      )
    )
    
    ug_box4 <- card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("Team Activities", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("This module displays statistics on syncing, completed households and interview time of teams", style = "color: white;")
      )
    )
    
    ug_box5 <- card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("Enumerators", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("This module displays specific statistics on syncing activities and completed households", style = "color: white;")
      )
    )
    
    ug_box6 <- card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("Demography", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("This module displays summary statistics on respondent data at the individual", style = "color: white;")
      )
    )
    
    ug_box7 <- card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("Economic", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("This module displays summary statistics economic activities", style = "color: white;")
      )
    )
    
    ug_box8 <- card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("Work Output - Tables", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("This module displays the work output of enumerators and teams in tables", style = "color: white;")
      )
    )
    ug_box9 <- card(
      height = 350,
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_header("Work Output - Plots", style = "font-size: 150%; color: yellow;"),
      card_body(
        fill = FALSE,
        style = "color: white; background-color: teal;",
        p("This module displays the work output of enumerators and teams using plots", style = "color: white;")
      )
    )
    
    
    bslib::layout_column_wrap(width = 1, 
                  bslib::layout_column_wrap(width = 1/3, 
                                            ug_box1, 
                                            ug_box2, 
                                            ug_box3),
                  bslib::layout_column_wrap(width = 1/3, 
                                            ug_box4, 
                                            ug_box5, 
                                            ug_box6),
                  bslib::layout_column_wrap(width = 1/3, 
                                            ug_box7, 
                                            ug_box8, 
                                            ug_box9,
                  ug_box4),
                  heights_equal  = "all")
  })
  
  
  
  #* ENUMERATION
  #* ***********
  
    
  output$enumeration_boxes <- renderUI({
    req(credentials()$user_auth)
    
    #hhold_enumerated <- textOutput("hhold_enumerated")
    su_box1 <- value_box(
      title = "households enumerated",
      value = tags$span(hhold_enumerated, style = "font-size: 200%; color: yellow;"),
      #value = tags$p(as.character(hhold_enumerated), style = "font-size: 150%; color: yellow;"),
      showcase = bs_icon("house"),
      color = "yellow"
    )
    
    #hhold_with_data_3 <- textOutput("hhold_with_data_3")
    su_box2 <- value_box(
      title = "households enumerated (3d)", 
      value = tags$span(hhold_with_data_3, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("house-add")
    )
    
    #no_of_indiv <- textOutput("no_of_indiv")
    su_box3 <- value_box(
      title = tags$p("Individuals in Data", style = "font-size: 100%;"),
      value = tags$span(no_of_indiv, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("people")
    )
    
    #partial_save_total <- textOutput("partial_save_total")
    su_box4 <- value_box(
      title = tags$p("Partial Saves", style = "font-size: 100%;"),
      value = tags$span(partial_save_total, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("mask")
    )
    
    #deleted_total <- textOutput("deleted_total")
    su_box5 <-  value_box(
      title = "deleted cases", 
      value = tags$span(deleted_total, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("x-circle")
    )
    
    #avg_hhold_size <- textOutput("avg_hhold_size")
    su_box6 <- value_box(
      title = "average household size", 
      value = tags$span(avg_hhold_size, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("house-add")
    )
    
    #eas_with_data <- textOutput("eas_with_data")
    su_box7 <- value_box(
      title = tags$p("EAs with data", style = "font-size: 100%;"),
      value = tags$span(eas_with_data, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("pin-map-fill")
    )
    
    eas_with_data_3 <- textOutput("eas_with_data_3")
    su_box8 <- value_box(
      title = tags$p("EAs with data (3d)", style = "font-size: 100%;"),
      value = tags$span(eas_with_data_3, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("clock-history")
    )
    
    #male_count <- textOutput("male_count")
    su_box9 <-  value_box(
      title = "males", 
      value = tags$span(male_count, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("gender-male")
    )
    
    #female_count <- textOutput("female_count")
    su_box10 <- value_box(
      title = "females", 
      value = tags$span(female_count, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("gender-female")
    )
    
    #missing_sex_count <- textOutput("missing_sex_count")
    su_box11 <- value_box(
      title = tags$p("missing (sex)", style = "font-size: 100%;"),
      value = tags$span(missing_sex_count, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("gender-ambiguous")
    )
    
    #mean_interview_time <- textOutput("mean_interview_time")
    su_box12 <- value_box(
      title = tags$p("mean interview time (mins)", style = "font-size: 100%;"),
      value = tags$span(mean_interview_time, style = "font-size: 200%; color: yellow;"),
      showcase = bs_icon("yin-yang")
    )
    
    enum_boxes <- layout_column_wrap(width = 1, 
                     bslib::layout_column_wrap(width = 1/4, 
                                               su_box1, 
                                               su_box2,
                                               su_box3,
                                               su_box4
                     ),
                     bslib::layout_column_wrap(width = 1/4, 
                                               su_box5, 
                                               su_box6,
                                               su_box7,
                                               su_box8
                     ),
                     bslib::layout_column_wrap(width = 1/4, 
                                               su_box9, 
                                               su_box10,
                                               su_box11,
                                               su_box12
                     ),
                     
                     heights_equal  = "all")
    
    
  })
  

  # enum_vars <- renderUI({
  #  
  #   
  #   output$hhold_enumerated = "" 
  #   output$hhold_with_data_3 = renderText(hhold_with_data_3)
  #   output$no_of_indiv = renderText(no_of_indiv)
  #   output$partial_save_total = renderText(partial_save_total)
  #   
  #   output$deleted_total = renderText(deleted_total)
  #   output$avg_hhold_size = renderText(avg_hhold_size)
  #   output$eas_with_data = renderText(eas_with_data)
  #   output$eas_with_data_3 = renderText(eas_with_data_3)
  #   output$male_count = renderText(male_count)
  #   output$female_count = renderText(female_count)
  #   output$missing_sex_count = renderText(missing_sex_count)
  #   output$mean_interview_time = renderText(mean_interview_time)
  #   
  #   
  # 
  # })
  
  
  #* TEAMS
  output$team_boxes <- renderUI({
    req(credentials()$user_auth)
    
    tb_card_01 <- card(
      #full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 01",
          value = renderTable(team_01_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_01_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_01_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_01_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_01_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_01_max, style = "color: yellow;"))
      )
      
    )
    
    tb_card_02 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 02",
          value = renderTable(team_02_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_02_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_02_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_02_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_02_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_02_max, style = "color: yellow;"))
        )
    )
      
  
    tb_card_03 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 03",
          value = renderTable(team_03_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_03_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_03_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_03_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_03_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_03_max, style = "color: yellow;"))
      )
    )
    
    tb_card_04 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 04",
          value = renderTable(team_04_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_04_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_04_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_04_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_04_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_04_max, style = "color: yellow;"))
      )
    )
    
    tb_card_05 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 05",
          value = renderTable(team_05_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_05_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_05_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_05_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_05_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_05_max, style = "color: yellow;"))
      )
    )
    
    tb_card_06 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 06",
          value = renderTable(team_06_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_06_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_06_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_06_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_06_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_06_max, style = "color: yellow;"))
      )
    )
    
    tb_card_07 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 07",
          value = renderTable(team_07_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_07_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_07_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_07_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_07_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_07_max, style = "color: yellow;"))
      )
    )
    
    tb_card_08 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 08",
          value = renderTable(team_08_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_08_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_08_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_08_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_08_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_08_max, style = "color: yellow;"))
      )
    )
    
    tb_card_09 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 09",
          value = renderTable(team_09_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_09_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_09_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_09_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_09_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_09_max, style = "color: yellow;"))
      )
    )
    
    tb_card_10 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 10",
          value = renderTable(team_10_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_10_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_10_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_10_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_10_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_10_max, style = "color: yellow;"))
      )
    )
    
    tb_card_11 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 11",
          value = renderTable(team_11_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_11_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_11_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_11_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_11_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_11_max, style = "color: yellow;"))
      )
    )
    
    tb_card_12 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 12",
          value = renderTable(team_12_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_12_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_12_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_11_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_11_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_11_max, style = "color: yellow;"))
      )
    )
    
    tb_card_13 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 13",
          value = renderTable(team_13_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_13_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_13_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_13_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_13_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_13_max, style = "color: yellow;"))
      )
    )
    
    tb_card_14 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 14",
          value = renderTable(team_14_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_14_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_14_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_14_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_14_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_14_max, style = "color: yellow;"))
      )
    )
    
    tb_card_15 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 15",
          value = renderTable(team_15_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_15_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_15_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_15_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_15_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_15_max, style = "color: yellow;"))
      )
    )
    
    tb_card_16 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 16",
          value = renderTable(team_16_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_16_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_16_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_16_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_16_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_16_max, style = "color: yellow;"))
      )
    )
    
    tb_card_17 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 17",
          value = renderTable(team_18_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_17_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_17_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_17_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_17_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_17_max, style = "color: yellow;"))
      )
    )
    
    tb_card_18 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body_fill(
        fill = FALSE,
        value_box(
          title = "Team 18",
          value = renderTable(team_19_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_18_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_18_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_18_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_18_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_18_max, style = "color: yellow;"))
      )
    )
    
    tb_card_19 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 19",
          value = renderTable(team_19_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_19_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_19_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_19_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_19_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_19_max, style = "color: yellow;"))
      )
    )
    
    tb_card_20 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 20",
          value = renderTable(team_20_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_20_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_20_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_20_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_20_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_20_max, style = "color: yellow;"))
      )
    )
    
    tb_card_21 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 21",
          value = renderTable(team_21_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_21_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_21_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_21_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_21_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_21_max, style = "color: yellow;"))
      )
    )
    
    tb_card_22 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 22",
          value = renderTable(team_22_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_22_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_22_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_22_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_22_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_22_max, style = "color: yellow;"))
      )
    )
    
    tb_card_23 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 23",
          value = renderTable(team_23_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_23_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_23_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_23_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_23_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_23_max, style = "color: yellow;"))
      )
    )
    
    tb_card_24 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 24",
          value = renderTable(team_24_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_24_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_24_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_24_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_24_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_24_max, style = "color: yellow;"))
      )
    )
    
    tb_card_25 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 25",
          value = renderTable(team_25_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_25_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_25_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_25_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_25_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_25_max, style = "color: yellow;"))
      )
    )
    
    tb_card_26 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 26",
          value = renderTable(team_26_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_26_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_26_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_26_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_26_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_26_max, style = "color: yellow;"))
      )
    )
    
    tb_card_27 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 27",
          value = renderTable(team_27_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_27_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_27_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_27_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_27_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_27_max, style = "color: yellow;"))
      )
    )
    
    tb_card_28 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 28",
          value = renderTable(team_28_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_28_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_28_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_28_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_28_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_28_max, style = "color: yellow;"))
      )
    )
    
    tb_card_29 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 29",
          value = renderTable(team_30_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_29_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_29_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_29_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_29_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_29_max, style = "color: yellow;"))
      )
    )
    
    tb_card_30 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 30",
          value = renderTable(team_30_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_30_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_30_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_30_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_30_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_30_max, style = "color: yellow;"))
      )
    )
    
    tb_card_31 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 31",
          value = renderTable(team_31_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_31_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_31_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_31_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_31_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_31_max, style = "color: yellow;"))
        
      )
    )
    
    tb_card_32 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 32",
          value = renderTable(team_32_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_32_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_32_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_32_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_32_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_32_max, style = "color: yellow;"))
      )
    )
    
    tb_card_33 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 33",
          value = renderTable(team_33_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_33_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_33_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_33_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_33_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_33_max, style = "color: yellow;"))
      )
    )
    
    tb_card_34 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 34",
          value = renderTable(team_34_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_34_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_34_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_34_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_34_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_34_max, style = "color: yellow;"))
      )
    )
    
    tb_card_35 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 35",
          value = renderTable(team_36_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_35_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_35_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_35_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_35_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_35_max, style = "color: yellow;"))
      )
    )
    
    tb_card_36 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 36",
          value = renderTable(team_36_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_36_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_36_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_36_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_36_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_36_max, style = "color: yellow;"))
      )
    )
    
    tb_card_37 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 37",
          value = renderTable(team_37_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_37_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_37_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_37_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_37_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_37_max, style = "color: yellow;"))
      )
    )
    
    tb_card_38 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 38",
          value = renderTable(team_38_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_38_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_38_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_38_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_38_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_38_max, style = "color: yellow;"))
      )
    )
    
    tb_card_39 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 39",
          value = renderTable(team_39_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_39_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_39_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_39_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_39_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_39_max, style = "color: yellow;"))
      )
    )
    
    tb_card_40 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Team 40",
          value = renderTable(team_40_hh),
        )
      ),
      card_footer(
        h6("last sync (days): ", tags$span(team_40_last_sync_days, style = "color: yellow;")),
        h6("mean interview time (mins): ", tags$span(mit_team_40_mean, style = "color: yellow;")),
        h6("median interview time (mins): ", tags$span(mit_team_40_median, style = "color: yellow;")),
        h6("minimum interview time (mins): ", tags$span(mit_team_40_min, style = "color: yellow;")),
        h6("maximum interview time (mins): ", tags$span(mit_team_40_max, style = "color: yellow;"))
      )
    )
    
    tb_card_41 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Supervisor Last Sync (mins)",
          value = renderTable(last_sync_by_supervisor[1:20,]),
        )
      )
    )
    
    tb_card_42 <- card(
      full_screen = TRUE,
      style = "color: white; background-color: teal;",
      card_body(
        fill = FALSE,
        value_box(
          title = "Supervisor Last Sync (mins)",
          value = renderTable(last_sync_by_supervisor[21:40,]),
        )
      ),
    )
    
    
    
    
    
    
    layout_column_wrap(width = 1, 
      layout_column_wrap(width = 1/4, 
                          tb_card_01, 
                          tb_card_02,
                          tb_card_03, 
                          tb_card_04),
      layout_column_wrap(width = 1/4,
                          tb_card_05, 
                          tb_card_06,
                          tb_card_07, 
                          tb_card_08),
     layout_column_wrap(width = 1/4,
                          tb_card_09, 
                          tb_card_10,
                          tb_card_11, 
                          tb_card_12),
     layout_column_wrap(width = 1/4,
                          tb_card_13, 
                          tb_card_14,
                          tb_card_15, 
                          tb_card_16),
    layout_column_wrap(width = 1/4, 
                          tb_card_17, 
                          tb_card_18,
                          tb_card_19, 
                          tb_card_20),
    layout_column_wrap(width = 1/4,
                          tb_card_21, 
                          tb_card_22,
                          tb_card_23, 
                          tb_card_24),
    layout_column_wrap(width = 1/4, 
                          tb_card_25, 
                          tb_card_26,
                          tb_card_27, 
                          tb_card_28),
    layout_column_wrap(width = 1/4,
                          tb_card_29, 
                          tb_card_30,
                          tb_card_31, 
                          tb_card_32),
    layout_column_wrap(width = 1/4, 
                          tb_card_33, 
                          tb_card_34,
                          tb_card_35, 
                          tb_card_36),
    layout_column_wrap(width = 1/4,
                          tb_card_37,
                          tb_card_38, 
                          tb_card_39,
                          tb_card_40),
    layout_column_wrap(width = 1/2,
                       tb_card_41,
                       tb_card_42),
    
    heights_equal  = "all")
    
  })
  
  
  #* ENUMERATOR WORK OUTPUT
  output$enumerator_boxes <- renderUI({
    req(credentials()$user_auth)
    
    enut_card_01 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 1",
          value = renderTable(df_output_by_enums_10),
        )
      )
    )
    
    enut_card_02 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 2",
          value = renderTable(df_output_by_enums_20),
        )
      )
    )
    
    enut_card_03 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 3",
          value = renderTable(df_output_by_enums_30),
        )
      )
    )
    
    enut_card_04 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 4",
          value = renderTable(df_output_by_enums_40),
        )
      )
    )
    
    enut_card_05 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 5",
          value = renderTable(df_output_by_enums_50),
        )
      )
    )
    
    enut_card_06 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 6",
          value = renderTable(df_output_by_enums_60),
        )
      )
    )
    
    enut_card_07 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 7",
          value = renderTable(df_output_by_enums_70),
        )
      )
    )
    
    enut_card_08 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 8",
          value = renderTable(df_output_by_enums_80),
        )
      )
    )
    
      enut_card_09 <- card(
        style = "color: white; background-color: teal;",
        card_body_fill(
          value_box(
            title = "Team 9",
            value = renderTable(df_output_by_enums_90),
          )
        )
      )
    
    enut_card_10 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 10",
          value = renderTable(df_output_by_enums_100),
        )
      )
    )
    
    enut_card_11 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 11",
          value = renderTable(df_output_by_enums_110),
        )
      )
    )
    
    enut_card_12 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 12",
          value = renderTable(df_output_by_enums_120),
        )
      )
    )
    
    enut_card_13 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 13",
          value = renderTable(df_output_by_enums_130),
        )
      )
    )
    
    enut_card_14 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 14",
          value = renderTable(df_output_by_enums_140),
        )
      )
    )
    
    enut_card_15 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 15",
          value = renderTable(df_output_by_enums_150),
        )
      )
    )
    
    enut_card_16 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 16",
          value = renderTable(df_output_by_enums_160),
        )
      )
    )
    
    
    #*****
    enut_card_17 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 17",
          value = renderTable(df_output_by_enums_170),
        )
      )
    )
    
    enut_card_18 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 19",
          value = renderTable(df_output_by_enums_180),
        )
      )
    )
    
    enut_card_19 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 19",
          value = renderTable(df_output_by_enums_190),
        )
      )
    )
    
    enut_card_20 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 20",
          value = renderTable(df_output_by_enums_200),
        )
      )
    )
    
    enut_card_21 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 21",
          value = renderTable(df_output_by_enums_210),
        )
      )
    )
    
    enut_card_22 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 22",
          value = renderTable(df_output_by_enums_220),
        )
      )
    )
    
    enut_card_23 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 23",
          value = renderTable(df_output_by_enums_230),
        )
      )
    )
    
    enut_card_24 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 24",
          value = renderTable(df_output_by_enums_240),
        )
      )
    )
    
    #****
    enut_card_25 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 25",
          value = renderTable(df_output_by_enums_250),
        )
      )
    )
    
    enut_card_26 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 26",
          value = renderTable(df_output_by_enums_260),
        )
      )
    )
    
    enut_card_27 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 27",
          value = renderTable(df_output_by_enums_270),
        )
      )
    )
    
    enut_card_28 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 28",
          value = renderTable(df_output_by_enums_280),
        )
      )
    )
    
    enut_card_29 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 29",
          value = renderTable(df_output_by_enums_290),
        )
      )
    )
    
    enut_card_30 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 30",
          value = renderTable(df_output_by_enums_300),
        )
      )
    )
    
    enut_card_31 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 31",
          value = renderTable(df_output_by_enums_310),
        )
      )
    )
    
    enut_card_32 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 32",
          value = renderTable(df_output_by_enums_320),
        )
      )
    )
    
    #***
    enut_card_33 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 33",
          value = renderTable(df_output_by_enums_330),
        )
      )
    )
    
    enut_card_34 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 34",
          value = renderTable(df_output_by_enums_340),
        )
      )
    )
    
    enut_card_35 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 35",
          value = renderTable(df_output_by_enums_350),
        )
      )
    )
    
    enut_card_36 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 37",
          value = renderTable(df_output_by_enums_370),
        )
      )
    )
    
    enut_card_37 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 38",
          value = renderTable(df_output_by_enums_380),
        )
      )
    )
    
    enut_card_38 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 38",
          value = renderTable(df_output_by_enums_380),
        )
      )
    )
    
    enut_card_39 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 39",
          value = renderTable(df_output_by_enums_390),
        )
      )
    )
    
    enut_card_40 <- card(
      style = "color: white; background-color: teal;",
      card_body_fill(
        value_box(
          title = "Team 40",
          value = renderTable(df_output_by_enums_400),
        )
      )
    )
  
    
    bslib::layout_column_wrap(width = 1, 
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_01, 
                                                        enut_card_02
                                                        ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_03, 
                                                        enut_card_04,
                                                        ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_05,
                                                        enut_card_06
                                                        ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_07,
                                                        enut_card_08
                                                        ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_09, 
                                                        enut_card_10
                                                        ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_11, 
                                                        enut_card_12,
                                                        ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_13,
                                                        enut_card_14
                                                                    ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_15,
                                                        enut_card_16),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_17, 
                                                        enut_card_18),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_19, 
                                                        enut_card_20),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_21,
                                                        enut_card_22
                                                        ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_23,
                                                        enut_card_24
                                                        ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_25, 
                                                        enut_card_26),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_27, 
                                                        enut_card_28,
                                                        ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_29,
                                                        enut_card_30
                                                        ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_31,
                                                        enut_card_32),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_33, 
                                                        enut_card_34),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        enut_card_35, 
                                                        enut_card_36,
                              ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_37,
                                                        enut_card_38
                              ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        enut_card_39,
                                                        enut_card_40),
                              
                              heights_equal  = "all"
    )
    
  })
  
  # demography_ui ####
  
  
  #* DEMOGRAPHY
  output$demography_boxes <- renderUI({
    req(credentials()$user_auth)
    
    df_hhold_head_age_distr <- df_hhold_head_age_distr %>% ungroup() %>% select(age_group, count)
    de_hhold_head_age_distr <- card(
      class = "m-0 p-0",
      card_header("household heads by age"), 
      card_body(
        DT::renderDT(df_hhold_head_age_distr, options = list(dom = 't'))
      )
    )
    
    df_hhold_head_age_by_team_distr <- df_hhold_head_age_by_team_distr %>% select(team, age_group, count)
    de_hhold_head_age_by_team_distr <- card(
      class = "m-0 p-0",
      card_header("household heads by age by team"), 
      card_body(
        DT::renderDT(df_hhold_head_age_by_team_distr, options = list(dom = 'Bftsp'))
      )
    )
    
    df_age_distr <- df_age_distr %>% ungroup() %>% select(age_group, count)
    de_age_distr <- card(
      class = "m-0 p-0",
      card_header("age distribution of respondents"), 
      card_body(
        DT::renderDT(df_age_distr, options = list(dom = 't'))
      )
    )
    
    df_marital_status <- df_marital_status %>% ungroup() %>% select(age_group, marital_status, count)
    de_marital_status <- card(
      class = "m-0 p-0",
      card_header("marital status"), 
      card_body(
        DT::renderDT(df_marital_status, options = list(dom = 'tsp'))
      )
    )
    
    df_religious_denom <- df_religious_denom %>% ungroup() %>% select(denomination, count, total, percentage)
    de_religious_denom <- card(
      class = "m-0 p-0",
      card_header("religious denominations"), 
      card_body(
        DT::renderDT(df_religious_denom, options = list(dom = 't'))
      )
    )
    
    df_nationality <- df_nationality %>% ungroup() %>% select(nationality, count, total, percentage)
    de_nationality <- card(
      class = "m-0 p-0",
      card_header("nationality"), 
      card_body(
        DT::renderDT(df_nationality, options = list(dom = 't'))
      )
    )
    
    df_ethnicity <- df_ethnicity %>% ungroup() %>% select(ethnicity, count, percentage)
    de_ethnicity <- card(
      class = "m-0 p-0",
      card_header("ethnicity"), 
      card_body(
        DT::renderDT(df_ethnicity, options = list(dom = 't'))
      )
    )
    
    de_weights_box1 <- value_box(
      class = "m-0 p-0",
      title = "Weights - Males", 
      value = renderTable(df_weights_males),
      showcase = bs_icon("x-diamond")
    )
    
    de_weights_box2 <- value_box(
      class = "m-0 p-0",
      title = "Weights - Females", 
      value = renderTable(df_weights_females),
      showcase = bs_icon("yelp")
    )
    
    de_heights_box1 <- value_box(
      class = "m-0 p-0",
      title = "Heights - Males", 
      value = renderTable(df_heights_males),
      showcase = bs_icon("xbox")
    )
    
    de_heights_box2 <- value_box(
      class = "m-0 p-0",
      title = "Heights - Females", 
      value = renderTable(df_heights_females),
      showcase = bs_icon("vinyl")
    )
    
    
    bslib::layout_column_wrap(width = 1, 
                              bslib::layout_column_wrap(width = 1/3, 
                                                        de_hhold_head_age_distr, 
                                                        de_hhold_head_age_by_team_distr,
                                                        de_age_distr
                              ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        de_marital_status, 
                                                        de_religious_denom
                              ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        de_nationality, 
                                                        de_ethnicity
                              ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        de_weights_box1, 
                                                        de_weights_box2
                                                        ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        de_heights_box1, 
                                                        de_heights_box2
                              )
                              
    )
  })
  
  
  #* ECONOMIC
  output$economic_boxes <- renderUI({
    req(credentials()$user_auth)
    
    ec_box1 <- value_box(
      class = "m-0 p-0",
      title = "did you work for a wage, salary, etc.. in the last 7 days?", 
      value = renderTable(df_wage_salary),
      showcase = bs_icon("vector-pen")
    )
    
    ec_box2 <- value_box(
      class = "m-0 p-0",
      title = "did you work as a domestic worker in the last 7 days?", 
      value = renderTable(df_domestic),
      showcase = bs_icon("unity")
    )
    
    ec_box3 <- value_box(
      title = "did you work as a farm worker in the last 7 days?", 
      value = renderTable(df_farm_work),
      showcase = bs_icon("ubuntu")
    )
    
    ec_box4 <- value_box(
      title = "did you work as a non farm worker in the last 7 days?", 
      value = renderTable(df_non_farm_work),
      showcase = bs_icon("ui-radios")
    )
    
    ec_box5 <- value_box(
      title = "did you work as a family help in the last 7 days?", 
      value = renderTable(df_family_help),
      showcase = bs_icon("vimeo")
    )
    
    ec_box6 <- value_box(
      class = "m-0 p-0",
      title = "did you work in non-productive agric in the last 7 days?", 
      value = renderTable(df_non_productive_agric),
      showcase = bs_icon("x-diamond-fill")
    )
    
    bslib::layout_column_wrap(width = 1, 
                              bslib::layout_column_wrap(width = 1/2, 
                                                        ec_box1, 
                                                        ec_box2
                                                        ),
                              bslib::layout_column_wrap(width = 1/2, 
                                                        ec_box3,
                                                        ec_box4
                                                        ),
                              bslib::layout_column_wrap(width = 1/2,
                                                        ec_box5,
                                                        ec_box6
                                                        ),
                              
                              heights_equal  = "all"
    )
  })
  
  
  #* 
  #* TABLE SUMMARIES
  #* 
  #* 
  
  
    
  
  
  # output$enum_day_enumerator_team <- renderDataTable({df_enum_per_enumerator_per_team},
  #                                                    caption = htmltools::tags$caption(
  #                                                      style = 'caption-side: bottom; text-align: center;',
  #                                                      'table: ', htmltools::em('enumeration by day by enumerator by team.')
  #                                                    ),
  #                                                    extensions = 'Buttons',
  #                                                    
  #                                                    options = list(
  #                                                      fixedColumns = TRUE,
  #                                                      autoWidth = TRUE,
  #                                                      ordering = TRUE,
  #                                                      dom = 'Bftsp',
  #                                                      buttons = c('copy', 'csv', 'excel')
  #                                                    ))
  
  
  
  # output$enum_summary_search <- renderUI({
  #   req(credentials()$user_auth)
  #     
  #     card(
  #       layout_column_wrap(
  #         width = 1/2,
  #         heights_equal = "row",
  #         card(
  #           card_header("enumeration per day per team"),
  #           card_body(
  #             fill = FALSE,
  #             # renderDataTable({ sel_enums_1 },
  #             #                 caption = htmltools::tags$caption(
  #             #                   style = 'caption-side: bottom; text-align: center;',
  #             #                   'table: ',
  #             #                   htmltools::em('enumeration by day by team by enumerator')
  #             #                 ),
  #             #                 extensions = 'Buttons',
  #             #                 height = "100%",
  #             #                 options = list(
  #             #                   fixedColumns = TRUE,
  #             #                   autoWidth = TRUE,
  #             #                   columnDefs = list(list( targets = 2, width = '600px')),
  #             #                   #columnDefs = list(list(width = '200px', targets = c(3, 4))),
  #             #                   ordering = TRUE,
  #             #                   dom = 'Bftsp',
  #             #                   buttons = c('copy', 'csv', 'excel')
  #             #                 )
  #             # )
  #           ),
  #         ),
  #         card(
  #           card_header("enumeration per day per team"),
  #           card_body(
  #             fill = FALSE,
  #             # renderDataTable({ sel_enums_2 },
  #             #                 caption = htmltools::tags$caption(
  #             #                   style = 'caption-side: bottom; text-align: center;',
  #             #                   'table: ',
  #             #                   htmltools::em('enumeration by day by team by enumerator')
  #             #                 ),
  #             #                 extensions = 'Buttons',
  #             #                 height = "100%",
  #             #                 options = list(
  #             #                   fixedColumns = TRUE,
  #             #                   autoWidth = TRUE,
  #             #                   columnDefs = list(list( targets = 2, width = '600px')),
  #             #                   #columnDefs = list(list(width = '200px', targets = c(3, 4))),
  #             #                   ordering = TRUE,
  #             #                   dom = 'Bftsp',
  #             #                   buttons = c('copy', 'csv', 'excel')
  #             #                 )
  #             # )
  #           ),
  #         ),
  #       )
  #     )
  #     
  #     
  #   })
  #   
  #   
  # })
  # 
  
  #* reactive code
  
  
  # output$enum_day_enumerator_team <- renderDataTable({ sel_enums },
  #                                                    caption = htmltools::tags$caption(
  #                                                      style = 'caption-side: bottom; text-align: center;',
  #                                                      'table: ', 
  #                                                      htmltools::em('enumeration by day by team by enumerator')
  #                                                    ),
  #                                                    extensions = 'Buttons',
  #                                                    height = "100%",
  #                                                    options = list(
  #                                                      fixedColumns = TRUE,
  #                                                      autoWidth = TRUE,
  #                                                      columnDefs = list(list( targets = 2, width = '600px')),
  #                                                      #columnDefs = list(list(width = '200px', targets = c(3, 4))),
  #                                                      ordering = TRUE,
  #                                                      dom = 'Bftsp',
  #                                                      buttons = c('copy', 'csv', 'excel')
  #                                                    ))
  
    
      #* Enumeration by Date
      #* *******************
      
      output$summary_tables_output <- renderUI({
        req(credentials()$user_auth)
        
        st_card_01 <- card(
          full_screen = TRUE,
          card_title("total enumerations per day"),
          renderDataTable(df_enum_per_day[1:10,], options = list(dom = 'Bftsp'))
        )
        
        st_card_02 <- card(
          full_screen = TRUE,
          card_title("total enumerations per day"),
          renderDataTable(df_enum_per_day[11:20,], options = list(dom = 'Bftsp'))
        )
        
        st_card_03 <- card(
          full_screen = TRUE,
          card_title("total enumerations per day"),
          renderDataTable(df_enum_per_day[21:30,], options = list(dom = 'Bftsp'))
        )
        
        #* by enumerators
        st_card_04 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 1"),
          renderDataTable(enum_per_enumerator_per_team[[1]], options = list(dom = 'Bftsp'))
        )
        
        st_card_05 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 2"),
          renderDataTable(enum_per_enumerator_per_team[[2]], options = list(dom = 'Bftsp'))
        )
        
        st_card_06 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 3"),
          renderDataTable(enum_per_enumerator_per_team[[3]], options = list(dom = 'Bftsp'))
        )
        
        st_card_07 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 4"),
          renderDataTable(enum_per_enumerator_per_team[[4]], options = list(dom = 'Bftsp'))
        )
        
        st_card_08 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 5"),
          renderDataTable(enum_per_enumerator_per_team[[5]], options = list(dom = 'Bftsp'))
        )
        
        st_card_09 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 6"),
          renderDataTable(enum_per_enumerator_per_team[[6]], options = list(dom = 'Bftsp'))
        )
        
        st_card_10 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 7"),
          renderDataTable(enum_per_enumerator_per_team[[7]], options = list(dom = 'Bftsp'))
        )
        
        st_card_11 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 8"),
          renderDataTable(enum_per_enumerator_per_team[[8]], options = list(dom = 'Bftsp'))
        )
        
        st_card_12 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 9"),
          renderDataTable(enum_per_enumerator_per_team[[9]], options = list(dom = 'Bftsp'))
        )
        
        
        st_card_13 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 10"),
          renderDataTable( enum_per_enumerator_per_team[[10]], options = list(dom = 'Bftsp'))
        )
        
        st_card_14 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 11"),
          renderDataTable(enum_per_enumerator_per_team[[11]], options = list(dom = 'Bftsp'))
        )
        
        st_card_15 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 12"),
          renderDataTable(enum_per_enumerator_per_team[[12]], options = list(dom = 'Bftsp'))
        )
        
        st_card_16 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 13"),
          renderDataTable(enum_per_enumerator_per_team[[13]], options = list(dom = 'Bftsp'))
        )
        
        st_card_17 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 14"),
          renderDataTable(enum_per_enumerator_per_team[[14]], options = list(dom = 'Bftsp'))
        )
        
        st_card_18 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 15"),
          renderDataTable(enum_per_enumerator_per_team[[15]], options = list(dom = 'Bftsp'))
        )
        
        st_card_19 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 16"),
          renderDataTable(enum_per_enumerator_per_team[[16]], options = list(dom = 'Bftsp'))
        )
        
        st_card_20 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 17"),
          renderDataTable(enum_per_enumerator_per_team[[17]], options = list(dom = 'Bftsp'))
        )
        
        st_card_21 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 18"),
          renderDataTable(enum_per_enumerator_per_team[[18]], options = list(dom = 'Bftsp'))
        )
        
        
        st_card_22 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 19"),
          renderDataTable( enum_per_enumerator_per_team[[19]], options = list(dom = 'Bftsp'))
        )
        
        st_card_23 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 20"),
          renderDataTable(enum_per_enumerator_per_team[[20]], options = list(dom = 'Bftsp'))
        )
        
        st_card_24 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 21"),
          renderDataTable(enum_per_enumerator_per_team[[21]], options = list(dom = 'Bftsp'))
        )
        
        st_card_25 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 22"),
          renderDataTable(enum_per_enumerator_per_team[[22]], options = list(dom = 'Bftsp'))
        )
        
        st_card_26 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 23"),
          renderDataTable(enum_per_enumerator_per_team[[23]], options = list(dom = 'Bftsp'))
        )
        
        st_card_27 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 24"),
          renderDataTable(enum_per_enumerator_per_team[[24]], options = list(dom = 'Bftsp'))
        )
        
        st_card_28 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 25"),
          renderDataTable(enum_per_enumerator_per_team[[25]], options = list(dom = 'Bftsp'))
        )
        
        st_card_29 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 26"),
          renderDataTable(enum_per_enumerator_per_team[[26]], options = list(dom = 'Bftsp'))
        )
        
        st_card_30 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 27"),
          renderDataTable(enum_per_enumerator_per_team[[27]], options = list(dom = 'Bftsp'))
        )
        
        st_card_31 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 28"),
          renderDataTable(enum_per_enumerator_per_team[[28]], options = list(dom = 'Bftsp'))
        )
        
        st_card_32 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 29"),
          renderDataTable(enum_per_enumerator_per_team[[29]], options = list(dom = 'Bftsp'))
        )
        
        st_card_33 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 30"),
          renderDataTable(enum_per_enumerator_per_team[[30]], options = list(dom = 'Bftsp'))
        )
        
        st_card_34 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 31"),
          renderDataTable(enum_per_enumerator_per_team[[31]], options = list(dom = 'Bftsp'))
        )
        
        st_card_35 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 32"),
          renderDataTable(enum_per_enumerator_per_team[[32]], options = list(dom = 'Bftsp'))
        )
        
        
        st_card_36 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 33"),
          renderDataTable(enum_per_enumerator_per_team[[33]], options = list(dom = 'Bftsp'))
        )
        
        st_card_37 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 34"),
          renderDataTable(enum_per_enumerator_per_team[[34]], options = list(dom = 'Bftsp'))
        )
        
        st_card_38 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 35"),
          renderDataTable(enum_per_enumerator_per_team[[35]], options = list(dom = 'Bftsp'))
        )
        
        st_card_39 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 36"),
          renderDataTable(enum_per_enumerator_per_team[[36]], options = list(dom = 'Bftsp'))
        )
        
        st_card_40 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 37"),
          renderDataTable(enum_per_enumerator_per_team[[37]], options = list(dom = 'Bftsp'))
        )
        
        st_card_41 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 38"),
          renderDataTable(enum_per_enumerator_per_team[[38]], options = list(dom = 'Bftsp'))
        )
        
        st_card_42 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 39"),
          renderDataTable(enum_per_enumerator_per_team[39], options = list(dom = 'Bftsp'))
        )
        
        st_card_43 <- card(
          full_screen = TRUE,
          card_title("total enumerations per enumerator 40"),
          renderDataTable(enum_per_enumerator_per_team[[40]], options = list(dom = 'Bftsp'))
        )
        
        
        
        
        
        bslib::layout_column_wrap(width = 1,

                                  bslib::layout_column_wrap(width = 1/3,
                                                            st_card_01,
                                                            st_card_02,
                                                            st_card_03),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_04,
                                                            st_card_05),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_06,
                                                            st_card_07),
                                  bslib::layout_column_wrap(width = 1/2,                          
                                                            st_card_08,
                                                            st_card_09),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_10,
                                                            st_card_11),
                                  
                                  
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_12,
                                                            st_card_13),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_14,
                                                            st_card_15),
                                  bslib::layout_column_wrap(width = 1/2,                          
                                                            st_card_16,
                                                            st_card_17),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_18,
                                                            st_card_19),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_20,
                                                            st_card_21),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_22,
                                                            st_card_23),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_24,
                                                            st_card_25),
                                  bslib::layout_column_wrap(width = 1/2,                          
                                                            st_card_26,
                                                            st_card_27),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_28,
                                                            st_card_29),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_30,
                                                            st_card_31),
                                  bslib::layout_column_wrap(width = 1/2,                          
                                                            st_card_32,
                                                            st_card_33),
                                  bslib::layout_column_wrap(width = 1/2,
                                                            st_card_34,
                                                            st_card_35),
                                  

                                  heights_equal  = "all"
        )
      })

    
      output$plot_graph <- renderUI({
        req(credentials()$user_auth)
        
        plot_card_01 <- card(
          full_screen = TRUE,
          style = "color: white; background-color: teal;",
          card_body(
            fill = FALSE,
            value_box(
              title = "total enumerations per day",
              value = plot_ly(df_enum_per_day, x = ~enumeration_date, y = ~count)
            )
          ),
        )
        
        # plot_card_02 <- card(
        #   full_screen = TRUE,
        #   style = "color: white; background-color: teal;",
        #   card_body(
        #     fill = FALSE,
        #     value_box(
        #       title = "total enumerations per day",
        #       value = plot_ly(df_enum_per_day[16:30,], x = ~enumeration_date, y = ~count)
        #     )
        #   ),
        # )
        # 
        
        plot_card_03 <- card(
          full_screen = TRUE,
          style = "color: white; background-color: teal;",
          card_body(
            fill = FALSE,
            value_box(
              title = "total enumerations per enumerator",
              value = plot_ly(df_enum_per_enumerator[1:50,], x = ~enumerator, y = ~count)
            )
          ),
        )
        
        plot_card_04 <- card(
          full_screen = TRUE,
          style = "color: white; background-color: teal;",
          card_body(
            fill = FALSE,
            value_box(
              title = "total enumerations per enumerator",
              value = plot_ly(df_enum_per_enumerator[51:100,], x = ~enumerator, y = ~count)
            )
          ),
        )
        
        plot_card_05 <- card(
          full_screen = TRUE,
          style = "color: white; background-color: teal;",
          card_body(
            fill = FALSE,
            value_box(
              title = "total enumerations per enumerator",
              value = plot_ly(df_enum_per_enumerator[101:150,], x = ~enumerator, y = ~count)
            )
          ),
        )
        
        plot_card_06 <- card(
          full_screen = TRUE,
          style = "color: white; background-color: teal;",
          card_body(
            fill = FALSE,
            value_box(
              title = "total enumerations per enumerator",
              value = plot_ly(df_enum_per_enumerator[151:172,], x = ~enumerator, y = ~count)
            )
          ),
        )
        
        # current ####
        plot_card_07 <- card(
          full_screen = TRUE,
          style = "color: white; background-color: teal;",
          card_body(
            fill = FALSE,
            value_box(
              title = "total clusters completed per team",
              value = plot_ly(df_cluster_completion, x = ~team, y = ~n)
            )
          ),
        )
        
        
        bslib::layout_column_wrap(width = 1,
                                  bslib::layout_column_wrap(width = 1,
                                                            plot_card_01
                                  ),
                                  bslib::layout_column_wrap(width = 1,
                                                            plot_card_03                              ),
                                  bslib::layout_column_wrap(width = 1,
                                                            plot_card_04
                                  ),
                                  bslib::layout_column_wrap(width = 1,
                                                            plot_card_05
                                  ),
                                  bslib::layout_column_wrap(width = 1,
                                                            plot_card_06
                                  ),
                                  bslib::layout_column_wrap(width = 1,
                                                            plot_card_07
                                  ),
                                
                                  heights_equal  = "all"
        )
      })
      

      
      #*Count number of households enumerated by team
      #* reactive code
      observeEvent(input$team_3, {
        req(credentials()$user_auth)
        df <- df_enum_per_day_per_team %>% filter(team == input$team_3)
        
        output$plot_enum_by_day_by_team <- plotly::renderPlotly({
          
          #Create plot
          plot_ly(df,
                  x = ~enumeration_date,
                  y = ~count,
                  type = "bar",
                  textposition = "none") %>%
            layout(title = "enumeration by day per team",
                   xaxis = list(title = "enumeration date"),
                   yaxis = list(title = "count"))
          
          
        })
      })
      
      observeEvent(input$team_4, {
        req(credentials()$user_auth)
        
        df <- df_enum_per_enumerator_per_team %>% filter(team == input$team_4)
        
        output$plot_enum_by_day_by_enumeration_by_team <- plotly::renderPlotly({
          
          #Create plot
          plot_ly(df,
                  x = ~enumerator,
                  y = ~count,
                  type = "bar",
                  textposition = "none") %>%
            layout(title = "enumeration by enumerators per team",
                   xaxis = list(title = "enumerator"),
                   yaxis = list(title = "count"))
        })
      })
      
  })




# 
# output$summary_tables <- renderUI({
#   
#   #req(credentials()$user_auth)
#   
#   #* enumeration per day per team
#   #* default load
#   # df_enum_per_day_per_team <- df_enum_per_day_per_team %>% 
#   #   filter(team == 10)
#   
#   # output$enum_per_day_per_team <- renderDataTable({ df_enum_per_day_per_team },
#   #                                                 caption = htmltools::tags$caption(
#   #                                                   style = 'caption-side: bottom; text-align: center;',
#   #                                                   'table: ', htmltools::em('enumeration per day per team')
#   #                                                 ),
#   #                                                 extensions = 'Buttons',
#   #                                                 options = list(
#   #                                                   pageLength = 15,
#   #                                                   fixedColumns = TRUE,
#   #                                                   autoWidth = TRUE,
#   #                                                   ordering = TRUE,
#   #                                                   dom = 'Bftsp',
#   #                                                   buttons = c('copy', 'csv', 'excel'))
#   # )
#   
#   #* reactive load
#   observeEvent(input$team_1, {
#     
#     sel_team_1 <- df_enum_per_day_per_team %>% 
#       filter(team %in% input$team_1) %>% 
#       arrange(team) 
#       
#     output$enum_per_day_per_team <- renderDataTable({ sel_team_1 },
#                   caption = htmltools::tags$caption(
#                     style = 'caption-side: bottom; text-align: center;',
#                     'table: ', htmltools::em('enumeration per day per team')
#                   ),
#                   extensions = 'Buttons',
#                   
#                   options = list(
#                     fixedColumns = TRUE,
#                     autoWidth = TRUE,
#                     ordering = TRUE,
#                     dom = 'Bftsp',
#                     buttons = c('copy', 'csv', 'excel')
#                   )) 
#   })
#   

#* enumeration per day per team per enumerator 
# df_enum_per_enumerator_per_team <- df_enum_per_enumerator_per_team %>%
#   filter(team == 10)

#* default display
#enum_per_enumerator_per_team



# selectizeInput(
#   'e4', 
#   '4. Max number of options to show', 
#   choices = 1:10,
#   options = list(maxOptions = 5)
# ),
# card_body_fill(
#   fill = FALSE,
#   selectizeInput(
#     inputId = "input_1",
#     label = "Short description of control widget",
#     choices = c(
#       "Iowa" = "IA",
#       "Colorado" = "CO",
#       "Massachusetts" = "MA"
#     ),
#     options = list(placeholder = "Team")
#   ),
#   value_box(
#     title = "total enumerations per enumerator 8",
#     value = renderTable(df_enum_per_enumerator[141:160,])
#   )
# ),
  
  # st_card_14 <- selectizeInput(
  #   'e4', 
  #   '4. Max number of options to show', 
  #   choices = 1:10,
  #   options = list(maxOptions = 5)
  # )
  # 
  # output$stats_by_team <- renderUI(layout_column_wrap(width = 1/3,
  #                                  st_card_14)
  # )
  
  
  #* PLOTS
  #* 
  


  

# card(
#   card_header("Summary of Enumeration Data"),
#   layout_column_wrap(
#     width = "100px",
#     card(
#       full_screen = TRUE,
#       layout_column_wrap(
#         width = "50px",
#         value_box(
#           title = tags$p("Households Enumerated",  style = "font-size: 100%;"),
#           value = textOutput(outputId = "hhold_enum"),
#           showcase = bs_icon("house"),
#           height = 20,
#           style = "font-size: 80%;"
#         ),
#         value_box(
#           title = tags$p("Households Enumerated (3 days)", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(as.character(hhold_with_data_3), style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("house-add"),
#         ),
#         value_box(
#           title = tags$p("Individuals in Data", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(as.character(no_of_indiv), style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("people"),
#         ),
#         value_box(
#           title = tags$p("Partial Saves", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p( paste0(partial_save_total, " (", partial_save_percent, "%)"), 
#           # style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("mask"),
#         ),
#         value_box(
#           title = tags$p("Deleted Cases", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p( paste0(deleted_total, " (", deleted_percent, "%)"), 
#           # style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("x-circle"),
#         )
#       ),
#       
#       tags$p(),
#       
#       layout_column_wrap(
#         width = "50px",
#         value_box(
#           title = tags$p("Average Household Size", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(as.character(hhold_size), style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("file-break"),
#         ),
#         value_box(
#           title = tags$p("EAs with Data", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(as.character(eas_with_data), style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("pin-map")
#         ),
#         value_box(
#           title = tags$p("EAs with Data (3days)", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(eas_with_data_3, style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("pin-map-fill"),
#         ),
#         value_box(
#           title = tags$p("Enumerators with synced data (3days)", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(enumerators_with_data_3, style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("clock-history"),
#         ),
#         value_box(
#           title = tags$p("Duplicate Cases", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(duplicate_cases, style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("clipboard"),
#         ),
#       ),
#       
#       tags$p(),
#       
#       layout_column_wrap(
#         width = "50px",
#         value_box(
#           title = tags$p("Males", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(as.character(9999), style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("gender-male"),
#         ),
#         value_box(
#           title = tags$p("Females", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(as.character(9999), style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("gender-female")
#         ),
#         value_box(
#           title = tags$p("EAs with Data (3days)", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(eas_with_data_3, style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("pin-map-fill"),
#         ),
#         value_box(
#           title = tags$p("Enumerators with synced data (3days)", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(enumerators_with_data_3, style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("clock-history"),
#         ),
#         value_box(
#           title = tags$p("Duplicate Cases", style = "font-size: 100%;"),
#           value = 0,
#           # value = tags$p(duplicate_cases, style = "font-size: 150%; color: yellow;"),
#           showcase = bs_icon("clipboard"),
#         ),
#       )
#       
#     )
#   )

# output$mainTab <- renderUI(
#   mainPanel(
#     tabsetPanel(type = "tabs",
#                 tabPanel("User Guide", uiOutput("user_guide")),
#                 tabPanel("Overview", uiOutput("overview")),
#                 tabPanel("Overview", uiOutput("overview")),
#     ), width = 12
#   )
# )

# card_header("Summary of Enumeration Data"),
# layout_column_wrap(
#   width = "200px",
#   
#   value_box(
#     title = "total households enumerated", 
#     value = no_hh_enumerated,
#     showcase = bs_icon("house"),
#     #p("The 1st detail")

# output$overview <- renderUI({
#   # use req to only render results when credentials()$user_auth is TRUE
#   #req(credentials()$user_auth) # only run after a successful login
#   #page_1
# })
# 
# output$teams <- renderUI({
#   # use req to only render results when credentials()$user_auth is TRUE
#   #(credentials()$user_auth) # only run after a successful login
#   #page_2
# })

#* completed EAs
# completed_EAs <- rds_metadata %>%
#   group_by(v02a, completed_ea)
# summarize(completed_EAS = n()) %>% 
# select(v02a, completed_EAS) %>% 
# rename("Team" = v02a, "Completed_EAs" = completed_EAS)

#* *************************************************
#* LOAD SURVEY DATA
#* *************************************************

#* SET DATA FILE PATH
# data_path <- "input_data/"

# pff file 
# pff_file <- "ahies_download_app.pff"

#run pff file to download data
#openFile(pff_file)


#data_file <- paste0(data_path, "ahies_20230529.csdb")

#* LOAD CSDB FILE
#* **************
# dbConn <- dbConnect(RSQLite::SQLite(), data_file) 
# dbListTables(dbConn)

#******************************************************************************************************
#*                                CONNECT TO CSPRO DATABASE AND CONVERT TO RDS
#******************************************************************************************************

#* IDENTIFICATION
#* **************
# df_identification <- tbl(dbConn, 'level-1') %>%
#   #select(`id00`, `idq0`, `id01.y`, `id02`) %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   filter(idq0 == 3) #QUARTER 2
# 
# #* save and read dataframe as RDS
# saveRDS(df_identification, file = "input_data/identification.RDS")

#* CASES
#* *************
# df_cases <- tbl(dbConn, 'cases') %>%
#   #select(`id`, `key`, `deleted`, `partial_save_mode`) %>% 
#   collect() %>%
#   janitor::clean_names() 
# 
# #* save and read dataframe as RDS
# saveRDS(df_cases, file = "input_data/cases.RDS")

#* HOUSEHOLD
#* **************
# df_household <- tbl(dbConn, 'hhrecord') %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   inner_join(df_identification, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_household, file = "input_data/household.RDS")

#* METADATA
#* **************
# df_metadata <- tbl(dbConn, 'metadata') %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   inner_join(df_household, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_metadata, file = "input_data/metadata.RDS")

#* INDIVIDUAL
#* **************
# df_indiv <- tbl(dbConn, 'indrecord') %>%
#   collect() %>%
#   janitor::clean_names() %>% 
#   inner_join(df_identification, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_indiv, file = "input_data/indiv.RDS")

