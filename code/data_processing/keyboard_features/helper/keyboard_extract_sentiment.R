## helper function to extract sentiment scores from the keyboard data

get_sentiment_data = function(word_data){

  # filter sentiment events (id == 3) 
  senti_data = word_data %>% dplyr::filter(logical_category_list_id == 3) %>% dplyr::select(-id) %>% distinct(user_uuid, client_event_id, .keep_all = TRUE)
  
  if(nrow(senti_data) > 0){
    # get sentiment columns
    senti_data_parsed = parseJsonColumnSensing(senti_data, "event_json")
    
    senti_data_parsed$client_event_id = NULL
    colnames(senti_data_parsed)[which(colnames(senti_data_parsed) == "message_statistics_id")] = "client_event_id" # rename column

    # extract sentiment weights and tags from sentiment columns for assigned category After typing the message
    with_sentiment = senti_data_parsed %>% mutate(df = helper_cell(categoryAfter)) %>% mutate(sentiment_weight = sub(" .*", "", df), sentiment_tag = sub(".* ","", df)) %>% dplyr::select(-c(df))
    
    ## group by typing session - calculate statistics per session (e.g. how many sentiment score does one session on avg have?)
    sentiment_by_session = with_sentiment %>% 
      dplyr::mutate(client_event_id = as.integer(client_event_id)) %>%  # convert client event id to character for grouping
      dplyr::filter(contentUnitEventType == "ADDED" | contentUnitEventType == "CHANGED") %>%
      dplyr::group_by(client_event_id) %>% 
      dplyr::mutate(
      count_sentiment_match = sum(!is.na(sentiment_weight)),
      session_sentiment_avg = median(as.numeric(sentiment_weight), na.rm = TRUE),
      session_sentiment_var = sd(as.numeric(sentiment_weight), na.rm = TRUE)
    ) %>%  dplyr::filter(row_number()==1) %>% ungroup()

    ## select relevant columns for further preprocessing
    sentiment_by_session = sentiment_by_session %>% dplyr::select(
      client_event_id,
      count_sentiment_match,
      session_sentiment_avg,
      session_sentiment_var
    ) #%>% dplyr::mutate_all(na_if,"NaN")  # replace NaN cells with NA
  }
  
  if(nrow(senti_data) == 0){
    sentiment_by_session = data.frame()
  }
  
  return(sentiment_by_session)
}
