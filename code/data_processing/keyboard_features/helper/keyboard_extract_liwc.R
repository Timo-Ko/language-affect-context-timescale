## helper function to extract LIWC scores from the keyboard data
#' by Florian Bemmann, Timo Koch
#' this function extracts LIWC features from keyboard data

get_liwc_data = function(word_data) {
  
  # filter liwc events (id == 1)
  liwc_data = word_data %>% dplyr::filter(logical_category_list_id == 1) %>% dplyr::select(-id) %>% distinct(user_uuid, client_event_id, .keep_all = TRUE)

  if (nrow(liwc_data) > 0) {
    # get liwc columns
    liwc_data_parsed = parseJsonColumnDictionary(liwc_data, "event_json")
    
    liwc_data_parsed$client_event_id = NULL
    colnames(liwc_data_parsed)[which(colnames(liwc_data_parsed) == "message_statistics_id")] = "client_event_id"
    
    # sometimes not all three event types (added, changed, removed) occur! "ADDED" always occurs when there are emoji present, "CHANGED" AND "REMOVED" not necessarily
    # define events col names 
    cols_events <- setNames(list(rep(0,3))[[1]], c("ADDED", "CHANGED", "REMOVED"))
    
    # statistics over all content change types (ADDED, CHANGED, REMOVED) per message
    liwc_event_type_counts_per_session = liwc_data_parsed %>% 
      cbind(qdapTools::mtabulate(liwc_data_parsed$contentUnitEventType)) %>%
      add_column(!!!cols_events[!names(cols_events) %in% names(.)]) %>% # add missing cols if required
      select(client_event_id, ADDED, CHANGED, REMOVED) %>% 
      group_by(client_event_id) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
      ungroup()
    
    # statistics (LIWC counts) regarding only ADDED and CHANGED events per message -> Interpretation: actively produced text
    liwc_per_action = cbind(liwc_data_parsed, qdapTools::mtabulate(strsplit(
      liwc_data_parsed$categoryAfter, ","
    )))
    
    liwc_by_session = liwc_per_action %>% 
      dplyr::filter(contentUnitEventType == "ADDED" | contentUnitEventType == "CHANGED") %>%
      mutate(words_liwc_match = ifelse(categoryAfter == "unknown",0,1)) %>% # count words that have been captured by liwc 
      dplyr::select(-c(
        user_uuid,
        date,
        logical_category_list_id,
        regex_matcher_id
      )) %>% group_by(client_event_id) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
      ungroup()
    
    ## add missing C columns (LIWC categories range from C0 to C76)
    cols = setNames(list(rep(0, 77))[[1]], sprintf("C%d", 0:76))
    liwc_by_session = liwc_by_session %>% add_column(!!!cols[!names(cols) %in% names(.)])
    
    # merge information on counts and liwc per typing session
    liwc_all = inner_join(liwc_event_type_counts_per_session, liwc_by_session, by = "client_event_id")
    
    # ### relativize LIWC scores for total number of added words per text input
    # cols.t = liwc_all %>% select(matches("^[C]"), -CHANGED, -client_event_id) %>% colnames()
    # newby = list(apply(liwc_all[, which(colnames(liwc_all) %in% cols.t)], 2, function(t)
    #   t / (liwc_all$ADDED + liwc_all$CHANGED)))
    # liwc_all[, which(colnames(liwc_all) %in% cols.t)] = do.call(rbind.data.frame, newby)
    
    # Translate LIWC variables into readable labels
    for (x in 1:ncol(liwc_all)) {
      test.rename = which(liwc.names$C.cat == colnames(liwc_all)[x])
      if (length(test.rename) == 1)
        colnames(liwc_all)[x] = liwc.names$LIWC.name[which(liwc.names$C.cat == colnames(liwc_all)[x])]
    }
    
    # rename columns and reorder
    
    liwc_all <- liwc_all %>% 
      dplyr::rename(words_added = ADDED,  words_changed = CHANGED, words_removed = REMOVED) %>%  # rename columns
      dplyr::mutate(words_typed = words_added + words_changed) %>%  # create new variable with all typed words
      dplyr::select(client_event_id, words_typed, words_added, words_changed, words_removed, words_liwc_match, everything())
    
    }

  # what to do if missing data
  if (nrow(liwc_data) == 0) {
    liwc_all = data.frame()
  }
  
  return(liwc_all)
}
