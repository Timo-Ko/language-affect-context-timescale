#' Add exact timestamps to keyboard data and fill these new rows with surrounding information 
#' 
#' @author R. Schoedel
#' @family Preprocessing function 
#' @import dplyr
#' @import lubridate
#' @description this function adds specified timestamps to the keyboard data and fills with information from rows around the timestamps
#' @return keyboard data set with one new rows for timestamps of the experience sampling questionnaire 
#' @export 

# ema_time_limits_keyboard = function(keyboard_data, es_data, low.limit, up.limit){
#   
#   keyboard_data$es_questionnaire_id = NA # create new column indicating the corresponding es questionnaire id for given keyboard data 
#   
#   for(i in unique(es_data$es_questionnaire_id)){ # iterate through unique es questionnaire ids
#     
#     es.start = es_data$questionnaireStartedTimestamp_corrected[es_data$es_questionnaire_id == i] - minutes(low.limit) # compute start of time window around es 
#     es.end = es_data$questionnaireStartedTimestamp_corrected[es_data$es_questionnaire_id == i] +  minutes(up.limit) # compute end of time window around es
#     
#     # Create an interval object
#     es.interval = interval(es.start, es.end)
#     
#     # Apply the interval check to each keyboard data row
#     for(j in 1:nrow(keyboard_data)) {
#       if (keyboard_data$timestamp_type_start_corrected[j] %within% es.interval) {
#         keyboard_data$es_questionnaire_id[j] = i
#       }
#   
#     }
#  
#   } 
#   return(keyboard_data)
# }



#' #' # Corresponding helper function
#' 
#' add_time_limits = function(data, time.limit, es_id){
#'   
#'   firstrow = set_names(rep(NA, ncol(data)), colnames(data))
#'   data = rbind(firstrow, data)
#'   ema.start.time = time.limit 
#'   data$timestamp.corrected[1] = ema.start.time   
#'   data$es_questionnaire_id[1] = es_id
#'   
#'   data = data[order(data$timestamp.corrected),]
#'   
#'   before.ema.start = which(data$timestamp.corrected == ema.start.time)-1
#'   if(length(before.ema.start) == 0){
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

