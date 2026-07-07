#' 1 Sensing Timestamp Preprocessing Steps per user
#' 
#' @family Preprocessing function
#' @import dplyr
#' @import lubridate
#' @import helper_variables.R
#' @details data is a subset from ps_activity 
#' @description this function corrects the timestamps in a dataframe extracted from ps_activity and generates some useful time variables (e.g. weekday). 
#' Please be aware that the timestamp.corrected variable has still a UTC format but actually has already been converted to the correct timezone off the user.
#' That means, you can use timestamp.corrected as it is without any need to converge to a different timezone (i.e., please ignore the UTC specification in timestamp.corrected)
#' @return dataset with new variable timestamp.corrected that should be used for further preprocessing steps
#' @export

ps_activity_preproc_timestamps = function(data) {
  
  # Step 1: order rows according to logging timestamps/ order timestamps chronologically 
  data = dplyr::arrange(data, lubridate::ymd_hms(data$timestamp))
  
  # Step 2: impute missing values with k-nearest neigbours in timezoneOffset
  data$timezoneOffset[which(data$timezoneOffset == 0)] = NA
  data$timezoneOffset = impute.knn(data$timezoneOffset, k = 10)
  
  # Step 3: consider timezoneOffset in timestamp
  data$timestamp.corrected = lubridate::ymd_hms(data$timestamp) + lubridate::hours(data$timezoneOffset/(60*60*1000))

  # Step 4: further useful time-related variables
  ## create weekday
  data$weekday = lubridate::wday(data$timestamp.corrected, label=TRUE, week_start=1,locale = "en_US.UTF-8")
  
  ## add Year, Date, Time based on timestamp.corrected
  data$date = lubridate::date(data$timestamp.corrected)
  data$time = format(data$timestamp.corrected, "%H:%M:%S")
  data$year = lubridate::year(data$timestamp.corrected)
  
  ## quality check: Delete timestamps with wrong year (study was conducted in 2020) as this is historic data that was still on the smartphone
  data = data %>% filter(year == 2020)
  
  ## quality check: delete ghost events (duplicate rows according to client_db_id which is a unique entry for each logging event per user)
  data = data %>% dplyr::distinct(client_db_id, .keep_all = TRUE)
  
  ## create continuous time variable in hours and seconds
  data$time_to_hours = lubridate::hour(data$timestamp.corrected) + lubridate::minute(data$timestamp.corrected)/60 + lubridate::second(data$timestamp.corrected)/3600
  data$time_to_sec = data$time_to_hours*60*60
  
  return(data)
}





#' 2 Experience Sampling Timestamp Preprocessing Steps per user
#' @author R. Schoedel
#' @family Preprocessing function
#' @import dplyr
#' @import lubridate
#' @import helper_variables.R
#' @details ema.data is the output of ema_general_preprocessing.R but filtered for one single user_id
#' @details sensing.data is the corresponding sensing data set that was recorded during the time of the ema wave.
#' @description this function corrects the timestamps in a dataframe extracted from ps_esanswers and generates some useful study variables (e.g. StudyDay, weekday). 
#' Please be aware that the timestamp.corrected variables have still a UTC format but actually have already been converted to the correct timezone off the user.
#' That means, you can use all timestamp.corrected variables as they are without any need to converge to a different timezone (i.e., please ignore the UTC specification in timestamp.corrected)
#' @return ema dataset with new variables timestamp.corrected that should be used for further preprocessing steps.
#' @export


ema_preproc_timestamps = function(ema_data, sensing_data){
  
  # Step 1: order rows according to logging timestamps/ order timestamps chronologically 
  ema_data = ema_data %>% arrange(lubridate::ymd_hms(ema_data$notificationTimestamp))
  
  # Step 2: look for the mode in the timezoneOffset variable in the timeperiod (+-30min) around the notification timestamp for ema in ps_activity 
  
  for(i in 1:nrow(ema_data)){
    df.timezone = sensing_data %>% filter(lubridate::ymd_hms(sensing_data$timestamp) > lubridate::ymd_hms(ema_data$notificationTimestamp)[i] - minutes(30) &
                                       lubridate::ymd_hms(sensing_data$timestamp) < lubridate::ymd_hms(ema_data$notificationTimestamp)[i] + minutes(30))
    timezoneoffset.ema = mode.knn(df.timezone$timezoneOffset)
    
    # if no timezone offset available for this specific timestamp, use the most frequent one in the es wave
    if(is.na(timezoneoffset.ema)){
      helper = sensing_data %>% group_by(timezoneOffset) %>% count()
      timezoneoffset.ema = helper$timezoneOffset[which(helper$n == max(helper$n, na.rm = TRUE))]
    }
    
    # Step 3: correct timestamps in ema data table
    ema_data$notificationTimestamp_corrected[i] = lubridate::ymd_hms(ema_data$notificationTimestamp)[i] + lubridate::hours(timezoneoffset.ema/(60*60*1000))
    ema_data$questionnaireStartedTimestamp_corrected[i] = lubridate::ymd_hms(ema_data$questionnaireStartedTimestamp)[i] + lubridate::hours(timezoneoffset.ema/(60*60*1000))
    ema_data$questionnaireEndedTimestamp_corrected[i] = lubridate::ymd_hms(ema_data$questionnaireEndedTimestamp)[i] + lubridate::hours(timezoneoffset.ema/(60*60*1000))
  }
  
  ema_data$notificationTimestamp_corrected = lubridate::as_datetime(ema_data$notificationTimestamp_corrected)
  ema_data$questionnaireStartedTimestamp_corrected = lubridate::as_datetime(ema_data$questionnaireStartedTimestamp_corrected)
  ema_data$questionnaireEndedTimestamp_corrected = lubridate::as_datetime(ema_data$questionnaireEndedTimestamp_corrected)

  
  # Step 4: extract further useful variables 
  ## create weekday
  ema_data$weekday = lubridate::wday(ema_data$questionnaireStartedTimestamp_corrected, label=TRUE, week_start=1,locale = "en_US.UTF-8")
  
  ##  create unique number for ema questionnaires 
  ema_data$nr = 1:nrow(ema_data) 
  
  # create date column
  ema_data$date <- lubridate::date(ema_data$questionnaireStartedTimestamp_corrected)
  
  # create week column 
  ema_data$week <- lubridate::week(ema_data$questionnaireStartedTimestamp)
  
  return(ema_data)
}


#' 3 Keyboard data Timestamp Preprocessing Steps per user
# same procedure as for ema_preproc_timestamps(), description see above

# keyboard_preproc_timestamps = function(keyboard_data, sensing_data){
#   # Step 1: order rows according to logging timestamps/ order timestamps chronologically
#   keyboard_data = keyboard_data %>% arrange(lubridate::ymd_hms(keyboard_data$timestamp_type_start))
# 
#   # Step 2: look for the mode in the timezoneOffset variable in the timeperiod (+-1min) around the respective timestamp in ps_activity
#   keyboard_data$timestamp_type_start_corrected = NA
#   keyboard_data$timestamp_type_end_corrected = NA
# 
#   for(i in 1:nrow(keyboard_data)){
#     df.timezone = sensing_data %>% filter(lubridate::ymd_hms(sensing_data$timestamp) > lubridate::ymd_hms(keyboard_data$timestamp_type_start)[i] - minutes(1) &
#                                             lubridate::ymd_hms(sensing_data$timestamp) < lubridate::ymd_hms(keyboard_data$timestamp_type_start)[i] + minutes(1))
#     timezoneoffset.keyboard = mode.knn(df.timezone$timezoneOffset)
# 
#     # Step 3: correct timestamps in keyboard data table
#     keyboard_data$timestamp_type_start_corrected[i] = lubridate::ymd_hms(keyboard_data$timestamp_type_start)[i] + lubridate::hours(timezoneoffset.keyboard/(60*60*1000))
#     keyboard_data$timestamp_type_end_corrected[i] = lubridate::ymd_hms(keyboard_data$timestamp_type_end)[i] + lubridate::hours(timezoneoffset.keyboard/(60*60*1000))
#   }
# 
#   keyboard_data$timestamp_type_start_corrected = lubridate::as_datetime(keyboard_data$timestamp_type_start_corrected)
#   keyboard_data$timestamp_type_end_corrected = lubridate::as_datetime(keyboard_data$timestamp_type_end_corrected)
# 
#   # add session duration, date and week info
#   keyboard_data$session_duration <- keyboard_data$timestamp_type_end_corrected - keyboard_data$timestamp_type_start_corrected
#   keyboard_data$date <- lubridate::date(keyboard_data$timestamp_type_start_corrected)
#   keyboard_data$week <- lubridate::week(keyboard_data$timestamp_type_start_corrected)
# 
#   return(keyboard_data)
# }

# 
# optimized version of the function (adapted from Max):
# max logic 

keyboard_preproc_timestamps = function(keyboard_data, sensing_data){

  keyboard_data$timestamp_type_start = as.integer(keyboard_data$timestamp_type_start/1000)
  keyboard_data$timestamp_type_start = lubridate::as_datetime(keyboard_data$timestamp_type_start)
  keyboard_data$timestamp_type_end = as.integer(keyboard_data$timestamp_type_end/1000)
  keyboard_data$timestamp_type_end = lubridate::as_datetime(keyboard_data$timestamp_type_end)

  # create new df for matching with sensing data containing new variables
  keyboard_data_match = keyboard_data %>%
    mutate(timezoneOffset = NA, # to be computed
           activityName = "KEYBOARD",
           timestamp = timestamp_type_start %>% as.character(),
           event = "KEYBOARD",
           user_id = user_uuid %>% as.character())

  keyboard_data_match = keyboard_data_match %>%
    dplyr::rename(packageName = input_target_app) # rename app column

  # apps_key <- keyboard_data_match$packageName %>% unique() # get unique apps used to produce any text
  # 
  # sensing_apps <- sensing_data %>% filter(packageName %in% apps_key) # get app events of those apps where text had been produced
  
  sensing_keyboard  <- bind_rows(sensing_data, keyboard_data_match) # bind sensing data of apps where text had been produced with keyboard data
  sensing_keyboard <- sensing_keyboard %>% arrange(timestamp) # arrange df by timestamp

  while (any(sensing_keyboard$activityName == "KEYBOARD" & is.na(sensing_keyboard$timezoneOffset))) {
    row_key <- which(sensing_keyboard$activityName == "KEYBOARD" & is.na(sensing_keyboard$timezoneOffset))
    
    # Handle the first row separately if it's the one with NA
    if (1 %in% row_key) {
      sensing_keyboard$timezoneOffset[1] <- 0  # Replace 0 with an appropriate default value
      row_key <- row_key[-1]  # Remove the first element from row_key
    }
    
    # Exclude the first row to avoid invalid indexing
    row_key <- row_key[row_key > 1]
    
    if(length(row_key) > 0) {
      # Ensure that the previous value is not NA
      previous_values <- sensing_keyboard$timezoneOffset[row_key - 1]
      valid_previous_values <- !is.na(previous_values)
      sensing_keyboard$timezoneOffset[row_key[valid_previous_values]] <- previous_values[valid_previous_values]
    }
  }

  # filter out keyboard events before joining
  keyboard_data_time <- sensing_keyboard %>% filter(activityName == "KEYBOARD" )

  # add timezone offset column to keyboard_data
  keyboard_data <- left_join(keyboard_data, keyboard_data_time[,c("client_event_id", "timezoneOffset")])

  # Correct timestamps in keyboard keyboard_data
  keyboard_data$timestamp_type_start_corrected <- keyboard_data$timestamp_type_start + hours(keyboard_data$timezoneOffset/(60*60*1000))
  keyboard_data$timestamp_type_end_corrected <- keyboard_data$timestamp_type_end + hours(keyboard_data$timezoneOffset/(60*60*1000))

  # Compute session duration, date, and week
  keyboard_data$session_duration <- keyboard_data$timestamp_type_end_corrected - keyboard_data$timestamp_type_start_corrected
  keyboard_data$date <- date(keyboard_data$timestamp_type_start_corrected)
  keyboard_data$week <- week(keyboard_data$timestamp_type_start_corrected)

  return(keyboard_data)
}

## helper script for time stamp correction for keyboard data

# get all keyboard data

# pull sensing data per user 

# 
# 
## iterate through users
# 
# keyboard_preproc_timestamps = function(keyboard_data, sensing_data){
# 
#   # change format of timestamps
#   keyboard_data$timestamp_type_start = as.integer(keyboard_data$timestamp_type_start/1000)
#   keyboard_data$timestamp_type_start = lubridate::as_datetime(keyboard_data$timestamp_type_start)
#   keyboard_data$timestamp_type_end = as.integer(keyboard_data$timestamp_type_end/1000)
#   keyboard_data$timestamp_type_end = lubridate::as_datetime(keyboard_data$timestamp_type_end)
# 
#   # order rows according to logging timestamps/ order timestamps chronologically
#   keyboard_data = keyboard_data %>% arrange(lubridate::ymd_hms(keyboard_data$timestamp_type_start))
# 
#   # Step 2: look for the mode in the timezoneOffset variable in the timeperiod (+-1min) around the respective timestamp in ps_activity
# 
#   keyboard_data$timezone_offset = NA
#   keyboard_data$timestamp_type_start_corrected = NA
#   keyboard_data$timestamp_type_end_corrected = NA
# 
#   for(i in 1:nrow(keyboard_data)){ # iterate through keyboard sessions
# 
#     # create df for that specific time window
#     df_timezone = sensing_data %>% filter(lubridate::ymd_hms(sensing_data$timestamp) > lubridate::ymd_hms(keyboard_data$timestamp_type_start)[i] - minutes(1) &
#                                             lubridate::ymd_hms(sensing_data$timestamp) < lubridate::ymd_hms(keyboard_data$timestamp_type_start)[i] + minutes(1))
# 
# 
#     # check unique timezone offsets
#     if (length(unique(df_timezone$timezoneOffset)) == 1) {
# 
#     timezoneoffset_keyboard = unique(df_timezone$timezoneOffset)
# 
#     }
# 
#     else  { # if there is more than one unique timezoneoffset use knn method
# 
#     # compute the timezone offset
#     timezoneoffset_keyboard = mode.knn(df_timezone$timezoneOffset)
# 
#     }
# 
#     # Step 3: correct timestamps in keyboard data table
# 
#     keyboard_data$timezone_offset[i] = timezoneoffset_keyboard
#     keyboard_data$timestamp_type_start_corrected[i] = lubridate::ymd_hms(keyboard_data$timestamp_type_start)[i] + lubridate::hours(timezoneoffset_keyboard/(60*60*1000))
#     keyboard_data$timestamp_type_end_corrected[i] = lubridate::ymd_hms(keyboard_data$timestamp_type_end)[i] + lubridate::hours(timezoneoffset_keyboard/(60*60*1000))
# 
#     print(i)
#     }
# 
#   # convert format
#   keyboard_data$timestamp_type_start_corrected = lubridate::as_datetime(keyboard_data$timestamp_type_start_corrected)
#   keyboard_data$timestamp_type_end_corrected = lubridate::as_datetime(keyboard_data$timestamp_type_end_corrected)
# 
#   # Step 4: further useful time-related variables
# 
#   # Compute session duration, date, and week
#   keyboard_data$session_duration <- keyboard_data$timestamp_type_end_corrected - keyboard_data$timestamp_type_start_corrected
#   keyboard_data$date <- date(keyboard_data$timestamp_type_start_corrected)
#   keyboard_data$week <- week(keyboard_data$timestamp_type_start_corrected)
# 
#   return(keyboard_data)
# }

# 
# 
# 
# 
