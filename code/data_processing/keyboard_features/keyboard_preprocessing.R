#' Preprocessing of keyboard data
#' 
#' @family Preprocessing function
#' @import dplyr
#' @param keyboard_data keyboard data frame
#' @return keyboard_data - a data frame containing the preprocessed and enriched keyboard data
#' @details data is from sql database for keyboard data is preprocessed
#' @description this function prepares the datasets from the researchIME database for keyboard feature extraction
#' @export

preprocessing_keyboard = function(keyboard_data){
  
  if(nrow(keyboard_data) > 0){ # continue if user has keyboard data 
    
    ## Step 1: filter unique entries
    keyboard_data = keyboard_data %>% distinct(user_uuid, client_event_id, .keep_all = TRUE) %>% arrange(timestamp_type_start_corrected)
    
    ## Step 2: extract keyboard logging events from user
    words =  dplyr::tbl(keyboard, "abstracted_action_event") %>% dplyr::filter(user_uuid %in% user) %>% data.frame()
    
    ## Step 3: enrich keyboard data with information on assigned app categories
    keyboard_data$app_category = NA
    
    for (t in 1:nrow(keyboard_data)) {
      assign.category = cat_app$Final_Rating[which(cat_app$App_name == keyboard_data$input_target_app[t])]
      if (length(assign.category) > 0)
        keyboard_data$app_category[t] = assign.category
    }
    
    ## Step 4: enrich keyboard data with information on specific input actions
    keyboard_data$action_category = NA
    
    for (t in 1:nrow(keyboard_data)){
      assign.action.category = cat_action$final_category[which(cat_action$field_hint_text == keyboard_data$field_hint_text[t])]
      if(length(assign.action.category) > 0) keyboard_data$action_category [t] = assign.action.category
    }
    
    # Step 5: enrich keyboard session 
    if(nrow(words) > 0){ # if keyboard action events exists...
      
      senti = get_sentiment_data(words)
      if(nrow(senti) > 0) { keyboard_data = left_join(keyboard_data, senti, by = c("client_event_id"))} else { # if no sentiments have been captured
        
        senti = data.frame(
          client_event_id = keyboard_data$client_event_id,
          count_sentiment_match = 0,
          session_sentiment_count = 0,
          session_sentiment_avg = NA)
        
        keyboard_data = left_join(keyboard_data, senti, by = c("client_event_id")) 
        
        }
      
      liwc = get_liwc_data(words)
      if(nrow(liwc) > 0) { keyboard_data = left_join(keyboard_data, liwc, by = c("client_event_id"))} else { # if no liwc scores have been captured
        
        # Create a template for the liwc columns
        liwc_cols = setNames(lapply(liwc.names$LIWC.name, function(x) NA), liwc.names$LIWC.name)
        
        liwc = data.frame(
          client_event_id = keyboard_data$client_event_id,
          words_typed = 0,
          words_added = 0,
          words_changed = 0, 
          words_removed = 0,
          words_liwc_match = 0,
          liwc_cols
        )
        
        keyboard_data = left_join(keyboard_data, liwc, by = c("client_event_id")) 
        
      }
      
      emoticons = get_emoticon_data(words)
      if(nrow(emoticons) > 0) { keyboard_data = left_join(keyboard_data, emoticons, by = c("client_event_id"))} else { # if no emoticons have been captured
        
        emoticons = data.frame(
          client_event_id = keyboard_data$client_event_id,
          emoticon_count = 0,
          unique_emoticon_count = 0,
          emoticon_cols = setNames(lapply(emoticons_df$emoticon_name, function(x) NA), emoticons_df$emoticon_name)
        )
        
        keyboard_data = left_join(keyboard_data, emoticons, by = c("client_event_id"))         
      }

      emoji = get_emoji_data(words)
      if(sum(emoji$emoji_count) > 0) { keyboard_data = left_join(keyboard_data, emoji, by = c("client_event_id")) } else { # if no emoji had been used by that user
        
        # Create a template for the emoji columns
        emoji_cols = setNames(lapply(emoji_df$unicode_code_point_dec, function(x) NA), paste0("emoji_", emoji_df$unicode_code_point_dec))
        
        # Combine with the standard columns
        emoji = data.frame(
          client_event_id = keyboard_data$client_event_id,
          emoji_count = integer(nrow(keyboard_data)),
          unique_emoji_count = integer(nrow(keyboard_data)),
          emoji_sentiment_count = numeric(nrow(keyboard_data)),
          emoji_sentiment_avg = NA,
          emoji_cols)  
          
        keyboard_data = left_join(keyboard_data, emoji, by = c("client_event_id")) 
        
      }
      
      }  else { # if no word events exist
      keyboard_data$words_typed = 0
      print(paste("No word events from user", unique(keyboard_data$user_uuid)))
    }
    
  }
  
  ## convert NA in count columns to zero
  
  # List of columns to replace NA with zero if those columns exist in keyboard_data
  columns_to_replace <- c("count_sentiment_match", 
                          "words_typed", "words_added", "words_changed", "words_removed", 
                          "words_liwc_match", "emoticon_count", "unique_emoticon_count", 
                          "emoji_count", "unique_emoji_count", "emoji_sentiment_count")
  
  keyboard_data <- keyboard_data %>%
    mutate(across(.cols = intersect(columns_to_replace, names(.)), ~replace_na(., 0)))
  
  return(keyboard_data)

}
