#' Preprocessing of keyboard data
#' 
#' @author Timo Koch, adapted from Ramona Schoedel and Florian Bemmann 
#' @family Preprocessing function
#' @import dplyr
#' @param keyboard_data keyboard data frame (on row per session) from a user
#' @param keyboard the connection to a data base holding the logged word events
#' @return keyboard_data - a data frame containing the preprocessed and enriched keyboard data
#' @details data is from sql database for keyboard data is preprocessed
#' @description this function prepares the datasets from the researchIME database for keyboard feature extraction
#' @export

keyboard_enrichment = function(keyboard_data, keyboard){
  
    ## Step 1: filter unique entries
    keyboard_data = keyboard_data %>% distinct(user_uuid, client_event_id, .keep_all = TRUE) %>% arrange(timestamp_type_start_corrected)
    
    ## Step 2: enrich keyboard data with information on assigned app categories andinput action
    keyboard_data <- keyboard_data %>%
      left_join(cat_app[,c("App_name", "Final_Rating")], by = c("input_target_app" = "App_name")) %>%
      rename(app_category = Final_Rating) %>%
      left_join(cat_action[,c("field_hint_text", "final_category")], by = c("field_hint_text" = "field_hint_text")) %>%
      rename(action_category = final_category)
    
    # old less efficient code
    # keyboard_data$app_category = NA
    # 
    # for (t in 1:nrow(keyboard_data)) {
    #   assign.category = cat_app$Final_Rating[which(cat_app$App_name == keyboard_data$input_target_app[t])]
    #   if (length(assign.category) > 0)
    #     keyboard_data$app_category[t] = assign.category
    # }
    
    # ## Step 4: enrich keyboard data with information on specific input actions
    # keyboard_data$action_category = NA
    # 
    # for (t in 1:nrow(keyboard_data)){
    #   assign.action.category = cat_action$final_category[which(cat_action$field_hint_text == keyboard_data$field_hint_text[t])]
    #   if(length(assign.action.category) > 0) keyboard_data$action_category [t] = assign.action.category
    # }
    
    ## Step 3: pull keyboard logging word events from user
    user = unique(keyboard_data$user_uuid) # get user id 
    
    words =  dplyr::tbl(keyboard, "abstracted_action_event") %>% dplyr::filter(user_uuid == !!user) %>%       collect()

    # Step 4: enrich keyboard sessions w word events
    if(nrow(words) > 0){ # if keyboard action events exists...
      
      #print(paste("Processing word events for user", user))
      
      liwc = get_liwc_data(words)
      
      if(nrow(liwc) == 0) {  # if no liwc scores have been captured
        
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
        
      }
      
      senti = get_sentiment_data(words)
      
      if(nrow(senti) == 0) { # if no sentiments have been captured
        
        senti = data.frame(
          client_event_id = keyboard_data$client_event_id,
          count_sentiment_match = 0,
          sentiment_scores = NA,
          word_sentiment_md = NA)
        
      }
      
      emoticons = get_emoticon_data(words)
      
      if(nrow(emoticons) == 0) {  # if no emoticons have been captured
        
        emoticons = data.frame(
          client_event_id = keyboard_data$client_event_id,
          emoticon_count = 0,
          unique_emoticon_count = 0,
          emoticon_cols = setNames(lapply(emoticons_df$emoticon_name, function(x) NA), emoticons_df$emoticon_name)
        )
        
      }

      emoji = get_emoji_data(words)
      
      if(nrow(emoji) == 0) {  # if no emoji had been used by that user
        
        # Create a template for the emoji columns
        emoji_cols = setNames(lapply(emoji_df$unicode_code_point_dec, function(x) NA), paste0("emoji_", emoji_df$unicode_code_point_dec))
        
        # Combine with the standard columns
        emoji = data.frame(
          client_event_id = keyboard_data$client_event_id,
          emoji_count = 0,
          unique_emoji_count = 0,
          emoji_sentiment_count = 0,
          emoji_sentiment_scores = NA,
          emoji_sentiment_md = NA,
          emoji_cols)  
      }
        
      # merge all dfs
        keyboard_data_enriched <- keyboard_data %>%
        left_join(liwc, by = "client_event_id") %>%
        left_join(senti, by = "client_event_id") %>%
        left_join(emoticons, by = "client_event_id") %>%
        left_join(emoji, by = "client_event_id")
      
        ## convert NA in count columns to zero
        
        # Find the column index of "words_typed" (first feature column)
        start_col <- which(names(keyboard_data_enriched) == "words_typed")
        
        # Identify the columns to modify, excluding specific columns
        columns_to_modify <- setdiff(
          names(keyboard_data_enriched)[start_col:ncol(keyboard_data_enriched)], 
          c("emoji_sentiment_md", "word_sentiment_md", "sentiment_scores", "emoji_sentiment_scores")
        )
        
        # Replace NA with zero in the selected columns
        keyboard_data_enriched <- keyboard_data_enriched %>%
          mutate(across(.cols = all_of(columns_to_modify), ~replace_na(., 0)))
        
        # remove unneeded dfs
        rm(words, liwc, senti, emoticons, emoji)
      
      }  else { # if no word events exist
      keyboard_data$words_typed = 0
      keyboard_data_enriched = keyboard_data # enriched keyboard data is initial keyboard data bc there were no word events
      #print(paste("No word events from user", unique(keyboard_data$user_uuid)))
    }
  
  return(keyboard_data_enriched)

}
