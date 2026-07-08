## helper function to extract emoji scores from the keyboard data
#' by Timo Koch
#' this function extracts emoji features from keyboard data
#' important: emoji are stored in utf16 decimal code!
#' emoji do not have their own dictionary but are detected using a regex matcher

get_emoji_data = function(word_data) {
  
  # filter emoji events (regex_matcher_id == 1)
  emoji_data = word_data %>% dplyr::filter(regex_matcher_id == 1) %>% dplyr::select(-id) %>% distinct(user_uuid, client_event_id, .keep_all = TRUE) # get all emoji events

  if (nrow(emoji_data) > 0 & !(all(emoji_data$event_json == '{"rawContentAfter":[45],"contentUnitEventType":"ADDED"}'))) {
    
    # get emoji information from json column
    emoji_data_parsed = parseJsonColumnEmoji(emoji_data, "event_json")
    
    emoji_data_parsed$client_event_id = NULL
    colnames(emoji_data_parsed)[which(colnames(emoji_data_parsed) == "message_statistics_id")] = "client_event_id"
    
    # create new column to store info on used emoji in decimal codepoint format
    emoji_data_parsed$emoji_code_point_dec <- NA
    
    # iterate through each row in RawContentAfter column, translate codes into decimal unicode
   
    # Iterate through rows and update emoji columns
    for (i in 1:nrow(emoji_data_parsed)) {
      # skip rows where no emoji had been used (i.e., rawContentAfter = NULL)
      if (is.null(unlist(emoji_data_parsed$rawContentAfter[i])) |
          45 %in% unlist(emoji_data_parsed$rawContentAfter[i])) {
        next # skip row
      }
      
      # apply function translating emoji to codepoint representation
      codepoints <-
        translate_to_codepoint(emoji_data_parsed$rawContentAfter[i])
    
      emoji_data_parsed$emoji_code_point_dec[i] <-
        list(codepoints) # store codepoints
      
    }
    
    # sometimes not all three event types (added, changed, removed) occur! "ADDED" always occurs when there are emoji present, "CHANGED" AND "REMOVED" not necessarily
    # define events col names 
    cols_events <- setNames(list(rep(0,3))[[1]], c("ADDED", "CHANGED", "REMOVED"))
    
    # statistics over all content change types (ADDED, CHANGED, REMOVED) per message
    event_type_counts_per_session = emoji_data_parsed %>% 
      cbind(qdapTools::mtabulate(emoji_data_parsed$contentUnitEventType)) %>%
      add_column(!!!cols_events[!names(cols_events) %in% names(.)]) %>% # add missing cols if required
      select(client_event_id, ADDED, CHANGED, REMOVED) %>% 
      group_by(client_event_id) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
      ungroup()
    
    # statistics (emoji counts) regarding only ADDED and CHANGED events per message -> Interpretation: actively produced text
    emoji_per_action = cbind(
      emoji_data_parsed,
      qdapTools::mtabulate(emoji_data_parsed$emoji_code_point_dec)
    )
    
    emoji_by_session = emoji_per_action %>% 
      dplyr::filter(contentUnitEventType == "ADDED" | contentUnitEventType == "CHANGED") %>%
      dplyr::select(-c(
        user_uuid,
        date,
        logical_category_list_id,
        regex_matcher_id
      )) %>% group_by(client_event_id) %>% 
      dplyr::summarise_if(is.integer, sum, na.rm = TRUE) %>%
      dplyr::mutate(
        emoji_count = rowSums(select(., matches("[[:digit:]]"))),
        unique_emoji_count = rowSums(select(., matches("[[:digit:]]")) > 0)
      ) %>% 
      ungroup()
    
    ## add missing emoji columns (those that have not been used by this particular user)
    cols = setNames(list(rep(0, nrow(emoji_df)))[[1]], emoji_df$unicode_code_point_dec)
    emoji_by_session = emoji_by_session %>% add_column(!!!cols[!names(cols) %in% names(.)])
    
    # merge information on counts and emoji per message
    emoji_all = inner_join(event_type_counts_per_session, emoji_by_session, by = "client_event_id")
    
    ## add emoji sentiment scores
    
    if (sum(emoji_all$emoji_count) >= 1) { # compute sentiment if emoji have been captured 
    
    # Apply sentiment function row-wise
      emoji_all_senti <- emoji_all %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          emoji_sentiment_count = as.numeric(calculate_sentiment_row(cur_data(), emoji_df)[[1]] %||% NA), # count emoji w corresponding sentiment score 
          emoji_sentiment_scores = list(as.numeric(calculate_sentiment_row(cur_data(), emoji_df)[[2]][!is.na(calculate_sentiment_row(cur_data(), emoji_df)[[2]])])), # Store sentiment scores as numeric, omitting NA
          emoji_sentiment_md = as.numeric(calculate_sentiment_row(cur_data(), emoji_df)[[3]] %||% NA) # compute md emoji sentiment per session
        ) %>%
        ungroup()     
      
      } else { # Initialize emoji_all_senti as an empty tibble with the expected columns if no emoji sentiment has been captured
          
          emoji_all_senti <- emoji_all %>%
            dplyr::mutate(
              emoji_sentiment_count = NA_real_,
              emoji_sentiment_scores = list(NA),
              emoji_sentiment_md = NA_real_
            )
          
        }

    # Add the prefix "emoji_" to column names containing only numbers (these are single emoji counts)
    colnames(emoji_all_senti)[grepl("^\\d+$", colnames(emoji_all_senti))] <-
      paste0("emoji_", colnames(emoji_all_senti)[grepl("^\\d+$", colnames(emoji_all_senti))])

    
    # ## relativize emoji counts for total number of typed emoji per text input
    # cols.t = emoji_all %>% select(starts_with("emoji_"),-emoji_count, -emoji_sentiment_count, -emoji_sentiment_avg) %>% colnames()
    # 
    # newby = list(apply(emoji_all[, which(colnames(emoji_all) %in% cols.t)], 2, function(t)
    #   t / (emoji_all$emoji_count)))
    # 
    # emoji_all[, which(colnames(emoji_all) %in% cols.t)] = do.call(rbind.data.frame, newby)
    
    # rename columns and reorder
    
    emoji_all_senti <- emoji_all_senti %>% 
      dplyr::select(-c(ADDED, CHANGED, REMOVED)) %>%  # Remove unneeded columns for emoji events
      dplyr::select(any_of(c("client_event_id", "emoji_count", "unique_emoji_count", "emoji_sentiment_count","emoji_sentiment_scores", "emoji_sentiment_md")), everything())
    
    return(emoji_all_senti)
    
  }
  
  # what to do if missing data
  if (nrow(emoji_data) == 0 | (all(emoji_data$event_json == '{"rawContentAfter":[45],"contentUnitEventType":"ADDED"}'))) {
    emoji_all = data.frame()
    return(emoji_all)
  }
  
}



#' helper function to translate a sequence of integers to code point decimal representation
translate_to_codepoint <- function(sequence) {
  
  # split sequences into pairs of two
  
  integers <- unlist(sequence) # unlist sequences
  
  # Remove "65039" from the integers (this is a zero-width joiner)
  integers <- integers[integers != 65039]
  
  # Remove "65532" from the integers (this is a object placeholder)
  integers <- integers[integers != 65532]
  
  # If there's an odd number of integers, remove the last one
  if (length(integers) %% 2 != 0) {
    integers <- integers[-length(integers)]
  }
  
  # Check if integers is empty (has no elements)
  if (length(integers) == 0) {
    return(NULL)
  }
  
  pairs <- matrix(integers, ncol = 2, byrow = TRUE) # Split into pairs
  
  codepoints <- sapply(1:dim(pairs)[1], function(i) {
    high_surrogate <- as.integer(pairs[i, 1])
    low_surrogate <- as.integer(pairs[i, 2])
    
    # Check if both surrogates are valid
    if (high_surrogate >= 0xD800 && high_surrogate <= 0xDBFF &&
        low_surrogate >= 0xDC00 && low_surrogate <= 0xDFFF) {
      # Calculate the Unicode code point from the surrogate pair
      unicode_code_point_dec <-
        (high_surrogate - 0xD800) * 0x400 + (low_surrogate - 0xDC00) + 0x10000
      
      return(unicode_code_point_dec)
    }
  })
  return(codepoints) #store codepoints
}


#' create function to compute rowwise emoji sentiment 
#' row is a row
#' emoji_df is a data frame containing emoji and corresponding sentiment scores (in helper data)

calculate_sentiment_row <- function(row, emoji_df) {
  
  # Extract the columns containing digits (i.e., are emoji counts) in the current row (these are the emoji counts)
  emoji_cols <- select(row, matches("[[:digit:]]")) %>%
    # Keep only emoji columns where the sum of values is at least 1
    dplyr::select(where(~ sum(. >= 1, na.rm = TRUE) > 0))  
  
  # Filter sentiment scores from emoji_df based on the used emoji from that row
  sentiment_df <- emoji_df %>%
    filter(unicode_code_point_dec %in% names(emoji_cols)) %>%
    select(unicode_code_point_dec, sentiment_score) %>%
    pivot_wider(names_from = unicode_code_point_dec, values_from = sentiment_score)
  
  # # If no sentiment scores are found for any used emoji, return NA for all variables
  # if (nrow(sentiment_df) == 0 | all(is.na(sentiment_df))) {
  #   return(c(NA, list(NA), NA))  # Return NA if no sentiment data is found
  # }
  
  # Use purrr::map2 to repeat sentiment scores based on emoji usage count
  emoji_sentiment_scores <- purrr::map2(names(emoji_cols), emoji_cols, 
                                        ~ rep(sentiment_df[[.x]], .y)) %>%
    # Flatten the list of repeated sentiment scores
    unlist()
  
  # Calculate the count of emojis with available sentiment scores
  emoji_sentiment_count <- sum(!is.na(emoji_sentiment_scores))
  
  # Calculate the median sentiment across emoji with sentiment scores
  emoji_sentiment_md <- median(emoji_sentiment_scores, na.rm = TRUE)
  
  # Return a vector containing the count, the list of sentiment scores, and the median sentiment
  return(list(emoji_sentiment_count, emoji_sentiment_scores, emoji_sentiment_md))
}

