#' Preprocess keyboard data by labeling them with specified time frames around the experience sampling timestamps
#' 
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

