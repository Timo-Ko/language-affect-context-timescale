## helper function to extract emoticon scores from the keyboard data
#' by Timo Koch
#' this function extracts emoticon features from keyboard data

get_emoticon_data = function(word_data) {
  
  # filter emoticon events (id == 2)
  emoticon_data = word_data %>% dplyr::filter(logical_category_list_id == 2) %>% dplyr::select(-id) %>% distinct(user_uuid, client_event_id, .keep_all = TRUE) # get all emoticon events, i.e. logical category 2

  if (nrow(emoticon_data) > 0) {
    # get emoticon columns
    emoticon_data_parsed = parseJsonColumnSensing(emoticon_data, "event_json")
    
    emoticon_data_parsed$client_event_id = NULL
    colnames(emoticon_data_parsed)[which(colnames(emoticon_data_parsed) == "message_statistics_id")] = "client_event_id"
    
    # statistics (emoticon counts) regarding only ADDED and CHANGED events per typing session -> Interpretation: actively produced text
    emoticon_per_action <- cbind(emoticon_data_parsed, qdapTools::mtabulate(as.data.frame(t(emoticon_data_parsed$categoryAfter))))
    
    # Get the names of the columns that are in emoticons_df$emoticon, i.e. the emoticons that user had used
    emoticon_columns = names(emoticon_per_action)[names(emoticon_per_action) %in% emoticons_df$emoticon_escaped]
    
    emoticon_by_session = emoticon_per_action %>% 
      dplyr::filter(contentUnitEventType == "ADDED" | contentUnitEventType == "CHANGED") %>%
      dplyr::mutate(emoticon_count = ifelse(categoryAfter == "unknown",0,1),
                    unique_emoticon_count = rowSums(select(., all_of(emoticon_columns)) > 0, na.rm = TRUE)) %>%
      dplyr::select(-c(
        user_uuid,
        date,
        logical_category_list_id,
        regex_matcher_id,
        unknown
      )) %>% group_by(client_event_id) %>% summarise_if(is.numeric, sum, na.rm = TRUE) 
    
    # remove double escapes from colnames
    colnames(emoticon_by_session) <-
      gsub("\\\\", "", colnames(emoticon_by_session))
    
    ## add missing emoticon columns (that have not been used by this user)
    cols = setNames(list(rep(0, nrow(emoticons_df)))[[1]], emoticons_df$emoticon)
    emoticon_all = emoticon_by_session %>% add_column(!!!cols[!names(cols) %in% names(.)])
    
    ### relativize emoticon scores for total number of emoticons per text input
    newby = list(apply(emoticon_all[, which(colnames(emoticon_all) %in% sub("^\\\\", "", emoticon_columns))], 2, function(t)
      t / emoticon_all$emoticon_count))
    
    emoticon_all[, which(colnames(emoticon_all) %in% sub("^\\\\", "", emoticon_columns))] = do.call(rbind.data.frame, newby)
    
    # Translate emoticon variables into readable labels
    colnames(emoticon_all) <-
      gsub("\\\\", "", colnames(emoticon_all)) # remove double escaping from colnames
    
    for (x in 1:ncol(emoticon_all)) {
      test.rename = which(emoticons_df$emoticon  == colnames(emoticon_all)[x])
      if (length(test.rename) == 1)
        colnames(emoticon_all)[x] = emoticons_df$emoticon_name[which(emoticons_df$emoticon == colnames(emoticon_all)[x])]
    }
  
    # keep relevant columns and reorder
    emoticon_all <- emoticon_all %>% 
     dplyr:: select(client_event_id, emoticon_count,  unique_emoticon_count, everything()) %>%
      mutate(across(everything(), ~replace(., is.nan(.), 0)))

    }
  
  # what to do if missing data
  if (nrow(emoticon_data) == 0) {
    emoticon_all = data.frame()
  }
  
  return(emoticon_all)
}
