#install.packages("PBSmodelling")

library(PBSmodelling)
library(DBI)               # Load the DBI package for database interface
library(stringr)           # Load the stringr package for working with strings
library(scales)            # Load the scales package for formatting numbers and dates
library(readr)             # Load the readr package for reading and parsing data files

# library(RMySQL)            # Load the RMySQL package for interfacing with MySQL databases from R
library(janitor)           # Load the janitor package for cleaning and formatting messy data
library(mapboxer)
library(bcrypt)
library(dplyr)


#* Convert .csdb file into RDS
downloadCSDB <- function() {
  
  #* *************************************************
  #* LOAD SURVEY DATA
  #* *************************************************
  
  #* SET DATA FILE PATH
  data_path <-  "input_data/ahies_csdb/"
  rds_data_path <- "input_data/ahies_rds/"
  
  pff_file <- paste0(data_path, "ahies_download_q2.pff")
  
  #run pff file to download data
  openFile(pff_file)

}

downloadCSDB()

#* LOAD CSDB FILE
#* **************
# dbConn <- dbConnect(RSQLite::SQLite(), data_file)
# dbListTables(dbConn)
# 
# #
# # #******************************************************************************************************
# # #*                                CONNECT TO CSPRO DATABASE AND CONVERT TO RDS
# # #******************************************************************************************************
# #
# # #* IDENTIFICATION
# # #* **************
# df_identification <- tbl(dbConn, 'level-1') %>%
#   #select(`id00`, `idq0`, `id01`, `id02`) %>%
#   collect() %>%
#   janitor::clean_names() %>%
#   filter(idq0 == 2) #QUARTER 2
# 
# # #* save and read dataframe as RDS
# saveRDS(df_identification, file = paste0(rds_data_path, "identification.RDS"))
# #
# # #* CASES
# # #* *************
# df_cases <- tbl(dbConn, 'cases') %>%
#   #select(`id`, `key`, `deleted`, `partial_save_mode`) %>%
#   collect() %>%
#   janitor::clean_names()
# 
# #* save and read dataframe as RDS
# saveRDS(df_cases, file = paste0(rds_data_path, "cases.RDS"))
# 
# #
# # #* HOUSEHOLD
# # #* **************
# df_household <- tbl(dbConn, 'hhrecord') %>%
#   collect() %>%
#   janitor::clean_names() %>%
#   inner_join(df_identification, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_household, file = paste0(rds_data_path, "household.RDS"))
# #
# # #* METADATA
# # #* **************
# df_metadata <- tbl(dbConn, 'metadata') %>%
#   collect() %>%
#   janitor::clean_names() %>%
#   inner_join(df_household, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_metadata, file = paste0(rds_data_path, "metadata.RDS"))
# 
# #
# # #* INDIVIDUAL
# # #* **************
# df_indiv <- tbl(dbConn, 'indrecord') %>%
#   collect() %>%
#   janitor::clean_names() %>%
#   inner_join(df_identification, by = ("level_1_id"))
# 
# #* save and read dataframe as RDS
# saveRDS(df_indiv, file = paste0(rds_data_path, "indiv.RDS"))