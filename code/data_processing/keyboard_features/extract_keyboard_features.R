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
#' @example keyboard-features(255,keyboard_data, "user_uuid", "all", 100)
#' @export

extract_keyboard_features = function(keyboard_data,
                             window_identifier,
                             filter_var,
                             min_words) {
  
    # keep sessions where the dictionaries captured min one word 
    keyboard_data  <- keyboard_data %>%
    dplyr::filter(words_liwc_match > 0 | count_sentiment_match > 0 )
  
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
      
      # skip user if user has produced too little text
      if (sum(keyboard_data_window$words_typed) < min_words){  
        print(paste("Less than", min_words, "words in", window_identifier, id, "from user", user))
        next
      }
      
      ## create data frames for keyboard feature subsets (one df per user, one row per time window)
      
      ## word dictionaries
      
      # word sentiment
      df_dic = keyboard_data_window %>% 
        dplyr::group_by_at({{window_identifier}}) %>%
        dplyr::summarise(
          
          # overall sentiment match rate
          wordsentiment_match_rate = sum(count_sentiment_match, na.rm = TRUE) / sum(words_typed, na.rm = TRUE),
         
          # word sentiment of all text aggregated for time window
          wordsentiment_mean =
            mean(unlist(sentiment_scores), na.rm = TRUE),
          
          # word sentiment - on typing session basis
          wordsentiment_session_mean = mean(word_sentiment_md, na.rm = TRUE),
          wordsentiment_session_sd = sd(word_sentiment_md, na.rm = TRUE),
          wordsentiment_session_min = min(word_sentiment_md, na.rm = TRUE),
          wordsentiment_session_max = max(word_sentiment_md, na.rm = TRUE),
          
          .groups = "drop"  # Ungroup the result
        ) 
      
      # liwc categories
      
      liwc_vars = liwc.names$LIWC.name
      
      df_liwc <- keyboard_data_window %>% 
        group_by_at({{window_identifier}}) %>% 
        dplyr::summarise(

          # overall LIWC match rate for all aggregated text
          liwc_match_rate = sum(words_liwc_match, na.rm = TRUE) / 
            sum(words_typed,      na.rm = TRUE),
          
          # 1) share of each LIWC category in all typed words of the window
          across(all_of(liwc_vars),
                 ~ sum(.x, na.rm = TRUE) / sum(words_typed, na.rm = TRUE),
                 .names = "{.col}_share"),
          
          # overall LIWC match rate across sessions
          liwc_match_rate_sessions = mean(
            ifelse(words_typed > 0, words_liwc_match / words_typed, NA_real_),
            na.rm = TRUE
          ),
          
          # 2) session-level descriptors of the per-session share
          across(all_of(liwc_vars), 
                 list(
                   mean = ~ mean(.x / words_typed, na.rm = TRUE),
                   sd   = ~  sd (.x / words_typed, na.rm = TRUE),
                   min  = ~  min(.x / words_typed, na.rm = TRUE),
                   max  = ~  max(.x / words_typed, na.rm = TRUE)
                 ),
                 .names = "{.col}_{fn}"),
          
          .groups = "drop"
        )
      
      
      df_keyboard_window = left_join(df_dic, df_liwc, by = window_identifier) # join to dic df
      
      
      # Append df_keyboard_window to df_keyboard if it contains data
      if (exists("df_keyboard_window") && nrow(df_keyboard_window) > 0) {
        if (ncol(df_keyboard) == 0) {
          df_keyboard <- df_keyboard_window[FALSE,]  # Initialize structure if empty
        }
        df_keyboard <- rbind(df_keyboard, df_keyboard_window)  # Append data
      }
      
    } # close loop across time windows 
      
    df_keyboard <- df_keyboard %>% 
      mutate(
        across(everything(), ~ {
          if (is.numeric(.x)) {
            y <- .x
            y[is.na(y)]       <- 0      # 1) NA  -> 0
            y[is.nan(y)]      <- NA     # 2) NaN -> NA
            y[is.infinite(y)] <- 0      # 3) Inf/-Inf -> 0
            y
          } else {
            .x                 # leave non-numeric columns unchanged
          }
        })
      ) %>% 
      rename_with(tolower)       # lower-case all column names
    
    
  return(df_keyboard) # return the final df containing language features per user for given time window (one row is one time window per user , i.e. there can be multiple rows per user)
  
} # close function

