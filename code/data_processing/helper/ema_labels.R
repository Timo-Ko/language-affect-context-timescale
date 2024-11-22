#' Preprocess keyboard data by labeling them with specified time frames around the experience sampling timestamps
#' 
#' @author R. Schoedel
#' @family Preprocessing function for keyboard data 
#' @import dplyr
#' @import lubridate
#' @description this function indicates for all logging entries in the keyboard data set whether they are included in a certain time window around each experience sampling measurement.
#' A lower boundary (i.e., time before the experience sampling at which the sensing data labeling should start) and an upper boundary (i.e., time after the experience sapling at which the sensing data labeling should stop)  
#' can be specified in the function. 
#' @return keyboard data set with one new column for labeled data according to time windows around the experience sampling measurements. 
#' @export

label_ema_in_keyboard = function(keyboard_data, es_data, lower.boundary, upper.boundary){
  keyboard_data$es_questionnaire_id = NA  
  
  for (ema.id in es_data$es_questionnaire_id){
    lower.intervall = es_data$questionnaireStartedTimestamp_corrected[es_data$es_questionnaire_id == ema.id] - minutes(lower.boundary)
    upper.intervall = es_data$questionnaireStartedTimestamp_corrected[es_data$es_questionnaire_id == ema.id] + minutes(upper.boundary)
    keyboard_data$es_questionnaire_id[keyboard_data$timestamp_type_start_corrected >= lower.intervall & keyboard_data$timestamp_type_start_corrected <= upper.intervall] = ema.id
  }
  return(keyboard_data)
}


#' 
#' 
#' #' Add EXACT timestamps to sensing data and fill these new rows with surrounding information 
#' #' 
#' #' @author Ramona Schoedel
#' #' @family Preprocessing function 
#' #' @import dplyr
#' #' @import lubridate
#' #' @description this function adds specified timestamps to the sensing data and fills with information from rows around the timestamps
#' #' @return sensing data set with one new rows for timestamps of the experience sampling questionnaire 
#' #' @export 
#' 
#' ema_time_limits = function(sensing.data, es.data, low.limit, up.limit){
#'   
#'   sensing.data$es_questionnaire_id = NA
#'   
#'   for(i in unique(es.data$es_questionnaire_id)){
#'     es.start = es.data$questionnaireStartedTimestamp.corrected[es.data$es_questionnaire_id == i] - minutes(low.limit) 
#'     es.end = es.data$questionnaireStartedTimestamp.corrected[es.data$es_questionnaire_id == i] +  minutes(up.limit)
#'     
#'     if(!is.na(es.start) && !is.na(es.end)){
#'       sensing.data = add_time_limits(sensing.data, es.start, i)
#'       sensing.data = add_time_limits(sensing.data, es.end, i)
#'       
#'       fill = which(sensing.data$es_questionnaire_id == i)
#'       sensing.data$es_questionnaire_id[fill[1]:fill[2]] = i
#'     }
#'     
#'   }
#'   
#'   return(sensing.data)
#' }
#' 
#' #' # Corresponding helper function
#' 
#' add_time_limits = function(data, time.limit, es_id){
#'   firstrow = set_names(rep(NA, ncol(data)), colnames(data))
#'   data = rbind(firstrow, data)
#'   ema.start.time = time.limit 
#'   data$timestamp.corrected[1] = ema.start.time   
#'   data$es_questionnaire_id[1] = es_id
#'   
#'   data = data[order(data$timestamp.corrected),]
#'   
#'   before.ema.start = which(data$timestamp.corrected == ema.start.time)-1
#'   if(before.ema.start == 0){
#'     before.ema.start = which(data$timestamp.corrected == ema.start.time)
#'   }
#'   ema.start = which(data$timestamp.corrected == ema.start.time)
#'   after.ema.start = which(data$timestamp.corrected == ema.start.time)+1
#'   
#'   # use case 1: usage before and after ema start
#'   if(is.na(data$nonusage[before.ema.start]) && is.na(data$nonusage[after.ema.start])){
#'     data$nonusage[ema.start] = NA
#'     data$usage[ema.start] = ifelse(is.na(data$usage[before.ema.start]), data$usage[after.ema.start], data$usage[before.ema.start]) # if there is no previous entry, take after.ema usage label
#'   }
#'   
#'   ## use case 2: usage before ema start and no usage after ema start
#'   if(is.na(data$nonusage[before.ema.start]) && !is.na(data$nonusage[after.ema.start])){
#'     data$nonusage[ema.start] = data$nonusage[after.ema.start] 
#'     data$usage[ema.start] = data$usage[after.ema.start] 
#'   }
#'   
#'   ## use case 3: non usage before ema start and usage after ema start
#'   if(!is.na(data$nonusage[before.ema.start]) && is.na(data$nonusage[after.ema.start])){
#'     data$nonusage[ema.start] = data$nonusage[before.ema.start] 
#'     data$usage[ema.start] = data$usage[before.ema.start] 
#'   }
#'   
#'   ## use case 4: non usage before and after ema start
#'   if(!is.na(data$nonusage[before.ema.start]) && !is.na(data$nonusage[after.ema.start]) && data$nonusage[before.ema.start] == data$nonusage[after.ema.start]){
#'     data$nonusage[ema.start] = data$nonusage[before.ema.start] 
#'     data$usage[ema.start] = NA
#'   }
#'   return(data)
#' }
#' 
