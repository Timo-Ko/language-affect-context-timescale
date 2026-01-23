#' Keyboard Features (emoji/emoticon names preloaded)
#'
#' @param keyboard_data data.frame of keyboard logs
#' @param window_identifier character scalar naming the window id column (e.g., "user_uuid" or "es_questionnaire_id")
#' @return data.frame with summarized keyboard features (one row per window id)
#' @export
extract_keyboard_features <- function(keyboard_data, window_identifier) {
  stopifnot(is.character(window_identifier), length(window_identifier) == 1)
  stopifnot(window_identifier %in% names(keyboard_data))
  
  # take names from preloaded lookup data frames
  emoticon_vars <- if (exists("emoticons_df")) {
    intersect(emoticons_df$emoticon_name, names(keyboard_data))
  } else character(0)
  
  emoji_vars <- if (exists("emoji_df")) {
    intersect(emoji_df$variable_name, names(keyboard_data))
  } else character(0)
  
  ids <- unique(na.omit(as.character(keyboard_data[[window_identifier]])))
  if (!length(ids)) return(data.frame())
  
  out <- list()
  
  for (id in ids) {
    kd <- keyboard_data[
      !is.na(keyboard_data[[window_identifier]]) &
        keyboard_data[[window_identifier]] == id, , drop = FALSE
    ]
    
    # keep sessions with text
    kd <- dplyr::filter(kd, .data$words_typed >= 1)
    
    # ---- emoticons (overall) ----
    df_emoticon <- kd %>%
      dplyr::group_by(.data[[window_identifier]]) %>%
      dplyr::summarise(
        
        # log typed words for later normalization
        words_typed_sum = sum(.data$words_typed, na.rm = TRUE),

        # total number of emoticons used (volume)
        emoticon_count_sum = if ("emoticon_count" %in% names(cur_data())) {
          sum(.data$emoticon_count, na.rm = TRUE)
        } else NA_real_,
        
        # number of DISTINCT emoticon TYPES used at least once in the window
        unique_emoticon_count_sum = if (length(emoticon_vars)) {
          sum(colSums(dplyr::pick(dplyr::all_of(emoticon_vars)), na.rm = TRUE) > 0)
        } else if ("unique_emoticon_count" %in% names(cur_data())) {
          # fallback if this column already stores per-window uniqueness
          max(.data$unique_emoticon_count, na.rm = TRUE)
        } else NA_real_,
        
        .groups = "drop"
      )
    
    # single emoticons (auto-summed, with "emoticon_" prefix)
    if (length(emoticon_vars)) {
      df_singleemoticon <- kd %>%
        dplyr::group_by(.data[[window_identifier]]) %>%
        dplyr::reframe(dplyr::across(
          dplyr::all_of(emoticon_vars),
          ~ sum(.x, na.rm = TRUE),
          .names = "emoticon_{col}_sum"
        ))
      df_emoticon <- dplyr::left_join(df_emoticon, df_singleemoticon, by = window_identifier)
    }
    
    # ---- emoji (overall + sentiment) ----
    df_emoji <- kd %>%
      dplyr::group_by(.data[[window_identifier]]) %>%
      dplyr::summarise(
        emoji_count_sum        = if ("emoji_count" %in% names(cur_data())) sum(.data$emoji_count, na.rm = TRUE) else NA_real_,
        unique_emoji_count_sum = if ("unique_emoji_count" %in% names(cur_data())) sum(.data$unique_emoji_count, na.rm = TRUE) else NA_real_,
        # share of emoji with sentiment scores
        senti_emoji_match_rate = {
          dnum <- if ("emoji_sentiment_count" %in% names(cur_data())) sum(.data$emoji_sentiment_count, na.rm = TRUE) else NA_real_
          ddat <- if ("emoji_count" %in% names(cur_data()))           sum(.data$emoji_count,           na.rm = TRUE) else NA_real_
          ifelse(!is.na(dnum) && !is.na(ddat) && ddat > 0, dnum / ddat, NA_real_)
        },
        # average sentiment across all numeric entries in list-column if present
        senti_emoji_avg = {
          d <- cur_data()
          if ("emoji_sentiment_scores" %in% names(d)) {
            vals <- unlist(d$emoji_sentiment_scores)
            if (length(vals) > 0) mean(vals, na.rm = TRUE) else NA_real_
          } else if ("emoji_sentiment_avg" %in% names(d)) {
            mean(d$emoji_sentiment_avg, na.rm = TRUE)
          } else {
            NA_real_
          }
        },
        .groups = "drop"
      )
    
    if (length(emoji_vars)) {
      df_singleemoji <- kd %>%
        dplyr::group_by(.data[[window_identifier]]) %>%
        dplyr::reframe(dplyr::across(
          dplyr::all_of(emoji_vars),
          ~ sum(.x, na.rm = TRUE),
          .names = "{col}_sum"
        ))
      df_emoji <- dplyr::left_join(df_emoji, df_singleemoji, by = window_identifier)
    }
    
    # ---- join per-window feature sets ----
    df_window <- dplyr::inner_join(df_emoticon, df_emoji, by = window_identifier)
    if (nrow(df_window)) out[[length(out) + 1L]] <- df_window
  }
  
  df <- if (length(out)) dplyr::bind_rows(out) else data.frame()
  if (nrow(df)) df[] <- lapply(df, function(x) { x[is.infinite(x)] <- NA; x })
  as.data.frame(df)
}
