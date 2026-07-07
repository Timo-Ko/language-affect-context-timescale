# General Experience Sampling Preprocessing Steps on all users
#' @family Preprocessing function
#' @import dplyr
#' @import ema_coding_answers.R
#' @details data from ps_esquestionnaire and ps_esanswer are joint and converted to a wide format
#' @details Should the audio logging be included in the dataset? default is FALSE
#' @details specify start.date, e.g., '2020-07-27 00:00:00' for wave 1 (chose one day before official start date to ensure all data is included -> see timestamp correction later)
# 'specify end.date, e.g., '2020-08-10 00:00:00' for wave 1 (chose one day after official start date to ensure all data is included -> see timestamp correction later)
# 'specify min.number.es, i.e., the number of answered experience sampling questionnaires to be counted as a valid user, e.g., 14 (at least 1 per day); indicate 0 to include all users
#' @description this function joins meta data about the experience sampling questionnaires (e.g. timestamps of notifications) and answering data of experience sampling questionnaires.
#' It excludes the audio logging data, filters ghost events with missing values, recodes participants' amswers in numeric values, filters cases where participants used the back button
#' and select the newer entries; finally it converts the dataset in an easiert-to-use wide format. 
#' @return experience sampling data set with multiple rows per user. 
#' @export

getEMAdata = function(audio.logging = T){

  # read data set1 for ema (read timestamps as character, see #7)
  es_q =  dplyr::tbl(phonestudy, "ps_esquestionnaire") %>% 
    dplyr::mutate(notificationTimestamp = as.character(notificationTimestamp), questionnaireStartedTimestamp = as.character(questionnaireStartedTimestamp), 
           questionnaireEndedTimestamp = as.character(questionnaireEndedTimestamp)) %>% data.frame()
  colnames(es_q)[which(colnames(es_q) == "id")] = "es_questionnaire_id" 
  
  # read data set2 for ema 
  es_a =  dplyr::tbl(phonestudy, "ps_esanswer") %>% dplyr::mutate(timestamp = as.character(timestamp)) %>% data.frame()
  colnames(es_a)[which(colnames(es_a) == "id")] = "running_es_id" 
  colnames(es_a)[which(colnames(es_a) == "e_s_questionnaire_id")] = "es_questionnaire_id"
  
  es_long =  dplyr::full_join(es_q, es_a, by = "es_questionnaire_id") 
  
  rm(es_a,es_q)
  
  # further first preprocessing steps
  
  ## 1. exclude audio logging entries if desired
  if(audio.logging == FALSE){
    es_long = es_long %>% dplyr::filter(!item_id %in% c(30, 31))
  }
  
  ## 2. filter ghost events (question is empty or NA)  
  es_long = es_long %>% dplyr::filter(question != "" & !is.na(question))
  
  ## 3. recode answer values stored as characters to numerics
  es_long = ema_to_numerics(es_long)
  
  ## 4. filter cases where participants used the back button (use entry with latest timestamp, see Issue #1)
  es_long = es_long %>% dplyr::group_by(questionnaireStartedTimestamp) %>% dplyr::arrange(desc(timestamp)) %>% dplyr::distinct(question, .keep_all = T) %>% dplyr::ungroup()
  
  ## 5. transform to wide format 
  es_wide = es_long %>% dplyr::select(user_id, es_questionnaire_id, notificationTimestamp, questionnaireStartedTimestamp, questionnaireEndedTimestamp, variable, value) %>% 
    dplyr::group_by(notificationTimestamp) %>% spread(variable, value) 
  
  return(es_wide)
}








