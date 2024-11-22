# helper function to summarize sensing data according to es and another specific grouping variable/label (e.g. home, hash)

summary_es = function(sensing.data, cols = c("es_questionnaire_id", "gps.atHOME")){
  summary_df = sensing.data %>% dplyr::group_by_at(.vars = cols) %>% count()
  summary_df$start.time = sensing.data %>% dplyr::group_by_at(.vars = cols) %>% slice(1) %>% pull(timestamp.corrected)
  summary_df = summary_df %>% arrange(start.time)
  summary_df$end.time = NA
  summary_df$end.time = lead(summary_df$start.time)
  summary_df = summary_df %>% dplyr::filter(!is.na(es_questionnaire_id))
  
  ## Handling of sessions that endure ema sessions (cut to 60 minutes)
  df_help = summary_df %>% group_by(es_questionnaire_id) %>% dplyr::count(es_questionnaire_id)
  for(k in df_help$es_questionnaire_id){
    frq = df_help$n[which(df_help$es_questionnaire_id == k)]
    summary_df$end.time[summary_df$es_questionnaire_id == k][frq] = summary_df$start.time[summary_df$es_questionnaire_id == k][1] + minutes(60)
  }
  
  summary_df$duration = NA
  summary_df$duration = difftime(summary_df$end.time, summary_df$start.time, units = "mins")
  summary_df = ungroup(summary_df)
  
  return(summary_df)
} 



