#' Keyboard Features
#'
#' @author Timo Koch
#' @family Function for keyboard feature extraction for different time windows and text input categories
#' @details extract keyboard features according to specified app categories
#' @param keyboard_data the user's keyboard df
#' @param window_identifier a variable in the dfs that identifies the time window (user_uuid, week, date or es_questionnaire_id)
#' @param filter_var indicate if a app/ action filter should be applied ("all", "private", "public"), these can be easily adapted
#' @param min_words minimum number of words typer per given time window
#' @return dataset with summarized keyboard features
#' @example keyboard-features(255,keyboard_data, "user_uuid", "all")
#' @export

extract_keyboard_features = function(keyboard_data,
                             window_identifier,
                             filter_var,
                             min_words) {
  
    # apply app or action filter here (if desired)
    
    if (filter_var == "private") {
      keyboard_data <-
        keyboard_data %>% dplyr::filter(action_category == "Messaging")
    }
    
    if (filter_var == "public") {
      keyboard_data <-
        keyboard_data %>% dplyr::filter(action_category == "Posting" |
                                          action_category == "Commenting")
    }
  
    print(paste("Starting with user", user, "using", window_identifier, "as identifier for", filter_var, "text"))
    
    ## iterate through time windows per user
    ids <- as.list(unique(na.omit(keyboard_data[, window_identifier]))) # create list of unique ids
    
    # format ids
    ids <- sapply(ids, format)
    
    # skip if no unique identifier exist
    if (length(ids) == 0){ 
      print(paste("No identifier found for",window_identifier,"from user", unique(keyboard_data$user_uuid)))
      df_keyboard = data.frame() # return empty df
    }
    
    # Initialize df_keyboard data frame that will hold keyboard features per user across time windows

    df_keyboard <- data.frame()
    
    for (id in ids) {
      
      print(paste("Starting with user", user, "for", window_identifier, id))
      
      # inner loop start, iterate through each time window for each user
      
      keyboard_data_window <- keyboard_data[!is.na(keyboard_data[[window_identifier]]) & keyboard_data[[window_identifier]] == id, ] # filter out keyboard events (aka rows) for that time window id
      
      # filter out ghost events (i.e., session duration is 0 seconds)
      keyboard_data_window  <- keyboard_data_window  %>%
        dplyr::filter(session_duration > 0)
      
      # skip user if user has produced too little text
      if (sum(keyboard_data_window$words_typed) < min_words){  
        print(paste("Less than", min_words, "words in", window_identifier, id, "from user", user))
        next
      }
      
      ## create data frames for keyboard feature subsets (one df per user, one row per time window)
      
      # submitted characters and words per session
      df_meta = keyboard_data_window %>% 
        dplyr::group_by_at({{window_identifier}}) %>%
        dplyr::summarise(
          session_count = n(), #number of keyboard sessions
          
          # session duration
          duration_avg = mean(as.numeric(session_duration), na.rm = TRUE),
          duration_var = sd(as.numeric(session_duration), na.rm = TRUE),
          duration_min = min(as.numeric(session_duration), na.rm = TRUE),
          duration_max = max(as.numeric(session_duration), na.rm = TRUE),
          
          # typing duration per word
          duration_word_avg = mean(as.numeric(session_duration) / words_typed, na.rm = TRUE),
          duration_word_var = sd(as.numeric(session_duration) / words_typed, na.rm = TRUE),
          duration_word_min = min(as.numeric(session_duration) / words_typed, na.rm = TRUE),
          duration_word_max = max(as.numeric(session_duration) / words_typed, na.rm = TRUE),         
          
          # typed characters per session
          char_sum = sum(character_count_added, na.rm = TRUE), # total typed characters
          char_avg = mean(character_count_added, na.rm = TRUE), 
          char_var = sd(character_count_added, na.rm = TRUE),
          char_min = min(character_count_added, na.rm = TRUE),
          char_max = max(character_count_added, na.rm = TRUE),
          
          # typed words per session
          words_sum = sum(words_typed, na.rm = TRUE), # total typed words
          words_avg = mean(words_typed, na.rm = TRUE),
          words_var = sd(words_typed, na.rm = TRUE),
          words_min = min(words_typed, na.rm = TRUE),
          words_max = max(words_typed, na.rm = TRUE),
          
          # share of added / changed / removed words from total typed words per session
          words_added_avg = mean(words_added / words_typed, na.rm = TRUE),
          words_added_var = sd(words_added / words_typed, na.rm = TRUE),
          words_added_min = min(words_added / words_typed, na.rm = TRUE),
          words_added_max = max(words_added / words_typed, na.rm = TRUE),
          
          words_changed_avg = mean(words_changed / words_typed, na.rm = TRUE),
          words_changed_var = sd(words_changed / words_typed, na.rm = TRUE),
          words_changed_min = min(words_changed / words_typed, na.rm = TRUE),
          words_changed_max = max(words_changed / words_typed, na.rm = TRUE),
          
          words_removed_avg = mean(words_removed / words_typed, na.rm = TRUE),
          words_removed_var = sd(words_removed / words_typed, na.rm = TRUE),
          words_removed_min = min(words_removed / words_typed, na.rm = TRUE),
          words_removed_max = max(words_removed / words_typed, na.rm = TRUE),
          
          # share of typed words produced in different app categories
          words_share_app_communication = sum(ifelse(app_category == "Communication", words_typed, 0), na.rm = T) / sum(words_typed),
          words_share_app_socialmedia = sum(ifelse(app_category == "Social_Media", words_typed, 0), na.rm = T) / sum(words_typed),
          words_share_app_internet = sum(ifelse(app_category == "Internet", words_typed, 0), na.rm = T) / sum(words_typed),
          
          # share of typed words produced in different action categories
          words_share_action_search = sum(ifelse(action_category == "Search", words_typed, 0), na.rm = T) / sum(words_typed),
          words_share_action_messaging = sum(ifelse(action_category == "Messaging", words_typed, 0), na.rm = T) / sum(words_typed),
          words_share_action_posting = sum(ifelse(action_category == "Posting", words_typed, 0), na.rm = T) / sum(words_typed),
          words_share_action_commenting = sum(ifelse(action_category == "Commenting", words_typed, 0), na.rm = T) / sum(words_typed),
          words_share_action_datainput = sum(ifelse(action_category == "Data Input", words_typed, 0), na.rm = T) / sum(words_typed),
          
          .groups = "drop"  # Ungroup the result
          
        ) %>% distinct({{window_identifier}}, .keep_all = TRUE) %>%
        select(-last_col())
      
      ## word dictionaries (word sentiment and emoji)
      
      # word sentiment
      df_dic = keyboard_data_window %>% 
        dplyr::group_by_at({{window_identifier}}) %>%
        dplyr::summarise(
          wordsentiment_match_rate = sum(count_sentiment_match, na.rm = TRUE) / sum(words_typed, na.rm = TRUE),
         
          # sentiment per typed words
          wordsentiment_avg = mean(session_sentiment_avg, na.rm = TRUE),
          wordsentiment_var = sd(session_sentiment_avg, na.rm = TRUE),
          wordsentiment_min = min(session_sentiment_avg, na.rm = TRUE),
          wordsentiment_max = max(session_sentiment_avg, na.rm = TRUE),
          
          liwc_match_rate = sum(words_liwc_match, na.rm = TRUE) / sum(words_typed, na.rm = TRUE),
          .groups = "drop"  # Ungroup the result
        ) %>% distinct({{window_identifier}}, .keep_all = TRUE) %>%
        select(-last_col())
      
      # liwc categories
      
      liwc_vars = liwc.names$LIWC.name
      
      df_liwc <- keyboard_data_window %>%
        group_by_at({{window_identifier}}) %>%
        summarise(across(all_of(liwc_vars), list(
          avg = ~ mean(.x, na.rm = TRUE),
          var = ~ sd(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        )))
      
      df_dic = left_join(df_dic, df_liwc, by = window_identifier) # join to dic df
      
      ## emoticons
      
      # overall emoticon use
      df_emoticon = keyboard_data_window %>%
        dplyr::group_by_at({{window_identifier}}) %>%
        dplyr::summarise(
          emoticon_count_sum = sum(emoticon_count, na.rm = TRUE), # total number of emoticons used
          unique_emoticon_count_sum = sum(unique_emoticon_count, na.rm = TRUE), # total number of unique emoticons used
          
          # number of used emoticons per session
          emoticon_avg = mean(emoticon_count, na.rm = TRUE),
          emoticon_var = sd(emoticon_count, na.rm = TRUE),
          emoticon_min = min(emoticon_count, na.rm = TRUE),
          emoticon_max = max(emoticon_count, na.rm = TRUE),
          
          # number of unique emoticons per session
          unique_emoticon_avg = mean(unique_emoticon_count, na.rm = TRUE),
          unique_emoticon_var = sd(unique_emoticon_count, na.rm = TRUE),
          unique_emoticon_min = min(unique_emoticon_count, na.rm = TRUE),
          unique_emoticon_max = max(unique_emoticon_count, na.rm = TRUE),  
          
          # emoticon to word ratio
          emoticon_word_ratio_avg = mean(emoticon_count / words_typed, na.rm = TRUE),
          emoticon_word_ratio_var = sd(emoticon_count / words_typed, na.rm = TRUE),
          emoticon_word_ratio_min = min(emoticon_count/ words_typed, na.rm = TRUE),
          emoticon_word_ratio_max = max(emoticon_count / words_typed, na.rm = TRUE),  
          .groups = "drop"  # Ungroup the result
          ) %>% distinct({{window_identifier}}, .keep_all = TRUE) %>%
        select(-last_col())
      
      # single emoticons
      
      emoticon_vars = emoticons_df$emoticon_name
      
      df_singleemoticon <- keyboard_data_window %>%
        group_by_at({{window_identifier}}) %>%
        summarise(across(all_of(emoticon_vars), list(
          avg = ~ mean(.x, na.rm = TRUE),
          var = ~ sd(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        )))
      
      df_emoticon = left_join(df_emoticon, df_singleemoticon, by = window_identifier) # join to dic df
      
      
      ## emoji
      
      # overall emoji use + emoji sentiment
      df_emoji = keyboard_data_window %>%
        dplyr::group_by_at({{window_identifier}}) %>%
        dplyr::summarise(
          emoji_count_sum = sum(emoji_count, na.rm = TRUE), # total number of emoji used
          unique_emoji_count_sum = sum(unique_emoji_count, na.rm = TRUE), # total number of unique emoji used
          
          # number of used emojis per session
          emoji_avg = mean(emoji_count, na.rm = TRUE),
          emoji_var = sd(emoji_count, na.rm = TRUE),
          emoji_min = min(emoji_count, na.rm = TRUE),
          emoji_max = max(emoji_count, na.rm = TRUE),
          
          # number of unique emojis per session
          unique_emoji_avg = mean(unique_emoji_count, na.rm = TRUE),
          unique_emoji_var = sd(unique_emoji_count, na.rm = TRUE),
          unique_emoji_min = min(unique_emoji_count, na.rm = TRUE),
          unique_emoji_max = max(unique_emoji_count, na.rm = TRUE),  
          
          # emoji to word ratio
          emoji_word_ratio_avg = mean(emoji_count / words_typed, na.rm = TRUE),
          emoji_word_ratio_var = sd(emoji_count / words_typed, na.rm = TRUE),
          emoji_word_ratio_min = min(emoji_count / words_typed, na.rm = TRUE),
          emoji_word_ratio_max = max(emoji_count / words_typed, na.rm = TRUE),
          
          # emoji sentiment
          senti_emoji_match_rate = sum(emoji_sentiment_count, na.RM = TRUE) / sum(emoji_count, na.rm = TRUE), # share of emoji w sentiment score from all used emoji
          senti_emoji_avg = mean(emoji_sentiment_avg, na.rm = TRUE),
          senti_emoji_var = sd(emoji_sentiment_avg, na.rm = TRUE),
          senti_emoji_min = if (all(is.na(emoji_sentiment_avg))) NA_real_ else min(emoji_sentiment_avg, na.rm = TRUE),
          senti_emoji_max = if (all(is.na(emoji_sentiment_avg))) NA_real_ else max(emoji_sentiment_avg, na.rm = TRUE),
          .groups = "drop"  # Ungroup the result
        ) %>% distinct({{window_identifier}}, .keep_all = TRUE) %>%
        select(-last_col())
      
      # single emoji 
      
      emoji_vars = emoji_df$variable_name
      
      df_singleemoji <- keyboard_data_window %>%
        group_by_at({{window_identifier}}) %>%
        summarise(across(all_of(emoji_vars), list(
          avg = ~ mean(.x, na.rm = TRUE),
          var = ~ sd(.x, na.rm = TRUE),
          min = ~ if (all(is.na(.x))) NA_real_ else min(.x, na.rm = TRUE),
          max = ~ if (all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE)
        )))
      
      df_emoji = left_join(df_emoji, df_singleemoji, by = window_identifier) # join to dic df
    
      # merge all data frames of keyboard sub feature sets
      
      # we have one df per time window per feature group now
      
      # join all feature dfs (one row per time window) into one df per time window
      
      if (window_identifier == "user_uuid") {

        df_keyboard_window <- df_meta %>%
          dplyr::inner_join(df_dic,  by = "user_uuid") %>%
          dplyr::inner_join(df_emoticon,  by = "user_uuid") %>%
          dplyr::inner_join(df_emoji,  by = "user_uuid")
                
      } else {
        df_keyboard_window <- df_meta %>%
          dplyr::inner_join (df_dic,
                             by = window_identifier) %>%
          dplyr::inner_join (df_emoticon,
                             by = window_identifier) %>%
          dplyr::inner_join (df_emoji,
                             by = window_identifier)
      } # close else clause

      # now we have one joined df per user per single time window
      
      # Append df_keyboard_window to df_keyboard if it contains data
      if (exists("df_keyboard_window") && nrow(df_keyboard_window) > 0) {
        if (ncol(df_keyboard) == 0) {
          df_keyboard <- df_keyboard_window[FALSE,]  # Initialize structure if empty
        }
        df_keyboard <- rbind(df_keyboard, df_keyboard_window)  # Append data
      }
      
    } # close loop across time windows 
      
    # replace Inf values with NA (these occur when numbers are divided by zero)
    df_keyboard <- as.data.frame(lapply(df_keyboard, function(x) ifelse(is.infinite(x), NA, x)))
    
  return(df_keyboard) # return the final df containing language features per user for given time window (one row is one time window per user , i.e. there can be multiple rows per user)
  
} # close function

