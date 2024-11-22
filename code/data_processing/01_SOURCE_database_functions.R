### LOAD RAW SOURCE DATA ###

# This script requires access to the server holding the raw data logs.
# We provide our preprocessing code to make all steps in our data handling transparent. 
# This script - 01_SOURCE_database_functions.R - is the starting point for the keyboard data preprocessing. 

## load R packages 
library(DBI)
library(dbplyr)
library(plyr)
library(dplyr)
library(lubridate)
library(RMariaDB)
library(tibble)
library(tidyr)
library(tidyverse)
library(anytime)
library(jsonlite)
library(rlang)
library(gsheet)
library(rio)
options(scipen = 999)

## load required scripts 

# Helper functions
source("Feature_Engineering/helper/aggregation.R")
source("Feature_Engineering/helper/connectivity_preprocessing.R")
source("Feature_Engineering/helper/ema_labels.R")
source("Feature_Engineering/helper/fill_information_up.R")
source("Feature_Engineering/helper/helper_JsonFormat.R")
source("Feature_Engineering/helper/helper_variables.R")
source("Feature_Engineering/helper/summary_es.R")
source("Feature_Engineering/helper/timestamp_correction.R")

# scripts for self-reports
source("Feature_Engineering/self_reports/ema/ema_coding_answers.R")
source("Feature_Engineering/self_reports/ema/ema_general_preprocessing.R")

# scripts for keyboard data
source("Feature_Engineering/keyboard_features/helper/keyboard_extract_liwc.R")
source("Feature_Engineering/keyboard_features/helper/keyboard_extract_sentiment.R")
source("Feature_Engineering/keyboard_features/helper/keyboard_extract_emoji.R")
source("Feature_Engineering/keyboard_features/helper/keyboard_extract_emoticons.R")
source("Feature_Engineering/keyboard_features/extract_keyboard_features.R")
source("Feature_Engineering/keyboard_features/keyboard_preprocessing.R")

## connect to databases in which the keyboard and experience sampling data are stored

# Sensing database
phonestudy = dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "rstudio",
  password = rstudioapi::askForPassword("Enter your password"),
  host = 'mc-ibt01.unisg.ch',
  port = 3306,
  dbname = "ssps")

# Keyboard database
keyboard = dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "rstudio",
  password = rstudioapi::askForPassword("Enter your password"),
  host = 'mc-ibt01.unisg.ch',
  port = 3306,
  dbname = "researchime")

# get overview of tables in data bases 
DBI::dbListTables(phonestudy)
DBI::dbListTables(keyboard)

## Load experience sampling data

# get complete experience sampling data for all users and waves and apply first preprocessing steps
ema_data = getEMAdata(audio.logging = FALSE)

## apply timestamp correction to ema data

# add new columns 
ema_data$notificationTimestamp_corrected = NA
ema_data$questionnaireStartedTimestamp_corrected = NA
ema_data$questionnaireEndedTimestamp_corrected = NA
ema_data$weekday = NA
ema_data$nr = NA
ema_data$date <- NA
ema_data$week <- NA

# loop over df

for (user in unique(ema_data$user_id)) {
  
  print(paste("Correcting EMA timestamps for user", user))
  
  # pull sensing data for that user for ema period (+/- 2 days)
  res = dbSendQuery(phonestudy, paste0("SELECT * FROM ps_activity WHERE user_id = ", user, 
                                       " AND (",
                                       "(timestamp >= '2020-07-25 00:00:00' AND timestamp <= '2020-08-11 23:59:59')", 
                                       " OR ",
                                       "(timestamp >= '2020-09-19 00:00:00' AND timestamp <= '2020-10-06 23:59:59')",
                                       ")"))
  sensing_data = dbFetch(res) # fetch data
  dbClearResult(res) # clear query
  
  if(nrow(sensing_data) > 0){ # if user has sensing data for timestamp correction
  
  # Apply time stamp correction to sensing data
  sensing_data = ps_activity_preproc_timestamps(sensing_data)
  
  # preprocess ema data for that user
  ema_data[ema_data$user_id == user,] = ema_preproc_timestamps(ema_data[ema_data$user_id == user,] , sensing_data) # preprocess ema data
  
  rm(sensing_data)
  
  }

}

# compute affect fluctuation from baseline
ema_data$valence_diff = ema_data$valence - ema_data$valence_avg
ema_data$arousal_diff = ema_data$arousal - ema_data$arousal_avg

# keep relevant columns for this project
ema_data = ema_data %>%
  dplyr::select(-c(
  "notificationTimestamp",
  "questionnaireStartedTimestamp",
  "questionnaireEndedTimestamp",  
  "Sleep_1_MCTQ",
  "Sleep_CSD1",
  "Sleep_CSD2",
  "Sleep_CSD3",
  "Sleep_CSD6",
  "Sleep_CSD7",
  "Sleep_CSD8",
  "Sleep_CSDM",
  "diamonds_adversity",
  "diamonds_deception",
  "diamonds_duty",
  "diamonds_intellect",
  "diamonds_mating",
  "diamonds_negativity",
  "diamonds_positivity",
  "diamonds_sociality",
  "stress" 
)) # keep relevant columns

## Create df for daily affect

ema_day <- ema_data %>%
  dplyr::group_by(user_id, date = lubridate::date(questionnaireStartedTimestamp_corrected)) %>%
  dplyr::summarise(
    es_count_day = n(),
    valence_day = median(valence, na.rm = TRUE),
    arousal_day = median(arousal, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(es_count_day >= 3) %>% # only keep days w at least three es instances per day
  dplyr::select(c(
    "user_id",
    "date",
    "es_count_day",
    "valence_day",
    "arousal_day"
  )) # keep relevant columns

## Create df for weekly affect

ema_week <- ema_data %>%
  dplyr::group_by(user_id, week = lubridate::week(questionnaireStartedTimestamp_corrected)) %>%
  dplyr::summarise(
    es_count_week = n(),
    es_days_week = length(unique(lubridate::day(questionnaireStartedTimestamp_corrected))),
    valence_week = median(valence, na.rm = TRUE),
    arousal_week = median(arousal, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(es_days_week == 7) %>% # only keep weeks w at least one es instance per unique day of the week 
  dplyr::select(c(
    "user_id",
    "week",
    "es_count_week",
    "es_days_week",
    "valence_week",
    "arousal_week"
  )) # keep relevant columns


# save ema data
saveRDS(ema_data, "data/results/ema/ema_data.rds")
saveRDS(ema_day, "data/results/ema/ema_day.rds")
saveRDS(ema_week, "data/results/ema/ema_week.rds")

## load required helper data

## App categorization (Schoedel et al.: https://doi.org/10.23668/psycharchives.5680)
cat_app = read.csv("data/helper/app_categorisation_2020_v2.csv")

## Keyboard action categorization (by F.Bemmann & T.Koch)
cat_action = readRDS("data/helper/input_prompt_cats_final_23-11-26.rds")

## Load Category names for LIWC
liwc.names = read_delim("data/helper/DE-LIWC2015.rimealiases", delim="\t", col_names=c("LIWC.cat","C.cat"))
unknown = c("unknown", "unknown")
liwc.names = rbind(liwc.names, unknown)
liwc.names$LIWC.name = paste0("LIWC_", liwc.names$LIWC.cat)

## load all emoticon names that had been used in the data set
emoticons_df <- readRDS("data/helper/emoticons_df.RData")

## Load emoji names and emoji sentiment
emoji_df <- readRDS("data/helper/emoji_df.RData")

## the data from the sql data base and runs some very basic preprocessing / enrichment. 

# pull all keyboard session data
res = dbSendQuery(keyboard, "select * from message_statistics")
keyboard_data_all = dbFetch(res) # fetch data
dbClearResult(res) # clear query

# get users who have keyboard session data
users <- unique(keyboard_data_all$user_uuid[!is.na(keyboard_data_all$user_uuid)])
length(users) # number of users in the keyboard data base 

for(user in users){ # iterate through users
  
  tryCatch({
    
  # print status 
  print(paste("Staring with user", user))
  
  t1 = Sys.time()
  
  # Step 1: filter keyboard data for that specific user
  keyboard_data = keyboard_data_all %>% 
    dplyr::filter(user_uuid %in% user) %>%    
    distinct(client_event_id, .keep_all = TRUE) # remove duplicates
  
  # Step 2: read sensing data for that specified user
  res = dbSendQuery(phonestudy, paste0("select * from ps_activity where user_id = ", user))
  sensing_data = dbFetch(res) # fetch data
  dbClearResult(res) # clear query
  
  # if there is no sensing data available from that user for timeoffset correction
  if(nrow(sensing_data) == 0){
    
    print(paste("user", user, "has no sensing data. Cannot compute timezone offset."))
    
    keyboard_data$timezone_offset = NA
    keyboard_data$timestamp_type_start_corrected = lubridate::ymd_hms(keyboard_data$timestamp_type_start)
    keyboard_data$timestamp_type_end_corrected = lubridate::ymd_hms(keyboard_data$timestamp_type_end)
    
    # Compute session duration, date, and week
    keyboard_data$session_duration <- keyboard_data$timestamp_type_end_corrected - keyboard_data$timestamp_type_start_corrected
    keyboard_data$date <- date(keyboard_data$timestamp_type_start_corrected)
    keyboard_data$week <- week(keyboard_data$timestamp_type_start_corrected)
    
    # recode user_uuid variable to character
    keyboard_data$user_uuid = as.character(keyboard_data$user_uuid)
    
  } else { # if there is sensing data to compute the timezone offset
    
  # Step 3: Apply time stamp correction to sensing data 
  sensing_data = ps_activity_preproc_timestamps(sensing_data)
  
  # Step 4: Only keep relevant columns from sensing data
  sensing_data = sensing_data %>% 
    #select(user_id, event, activityName, timestamp, timezoneOffset)   %>% # select relevant columns 
    dplyr::mutate(timestamp = as.character(timestamp), created_at = as.character(created_at), updated_at = as.character(updated_at)) %>% 
    data.frame()
  
  # Step 3: Recode user id variable as character to avoid confusion
  sensing_data$user_id = as.character(sensing_data$user_id)
  keyboard_data$user_uuid = as.character(keyboard_data$user_uuid)
  
  ## Step 4: apply timestamp correction to keyboard data
  keyboard_data = keyboard_preproc_timestamps(keyboard_data, sensing_data)
  
  }
  
  ## Step 5: enrich keyboard data
  keyboard_data = preprocessing_keyboard(keyboard_data)
  
  # save data 
  saveRDS(keyboard_data, paste0("/home/rstudio/data/ps_keyboard/", user, ".rds"))
  
  # remove data before next loop execution
  rm(keyboard_data, sensing_data)
  
  # compute execution time 
  t2 = Sys.time()
  processing_time <- round(as.numeric(difftime(t2, t1, units = "mins")),2)
  
  # return status message 
  print(paste("Completed processing for user", user, "taking", processing_time, "minutes. This is user number", which(users==user), "out of", length(users)))
  
  },
  
  # Tell tryCatch what do in case of an error
  error=function(e) {
  write(paste0(Sys.time(),": ERROR: ",conditionMessage(e)," --> Current user is: ", user), file = "data/errorlog_preprocessing.txt", append = TRUE)
  })
  
}

### continue with 02_SOURCE_feature_extraction.R ###