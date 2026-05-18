#' Extract keyboard-derived features from session-level master table
#'
#' Aggregates raw session-level logging data to the requested analysis window
#' (e.g., user_uuid, es_questionnaire_id, date, week), optionally split by
#' communication scope (all / private / public).
#'
#' The function extracts:
#' - LIWC-based word-use features
#' - word sentiment features (e.g., SentiWS-derived)
#' - emoji and emoticon features
#' - typing meta features
#'
#' For the regression analyses, the function returns:
#' - LIWC category shares per time window
#' - mean word sentiment per time window
#' - mean emoji sentiment per time window
#'
#' For the ML analyses, the function additionally returns session-level
#' distribution descriptors (mean, sd, min, max across sessions) for LIWC,
#' emoji, emoticon, and typing features.
#'
#' No minimum-word threshold is imposed here.
#'
#' @param keyboard_data data.frame with one row per keyboard session
#' @param window_identifier character scalar naming the grouping column
#' @param filter_var character vector; any of c("all", "private", "public")
#' @param context_var character scalar naming the context column (default: "action_category")
#' @param private_values values in context_var that define private communication
#' @param public_values values in context_var that define public communication
#'
#' @return data.frame with one row per window x scope
#' @export

extract_keyboard_features <- function(
  keyboard_data,
  window_identifier,
  filter_var = c("all", "private", "public"),
  context_var = "action_category",
  private_values = c("Messaging"),
  public_values  = c("Posting", "Commenting")
) {
  
  stopifnot(is.character(window_identifier), length(window_identifier) == 1L)
  stopifnot(window_identifier %in% names(keyboard_data))
  
  valid_scopes <- c("all", "private", "public")
  if (any(!filter_var %in% valid_scopes)) {
    stop("filter_var must contain only: 'all', 'private', 'public'")
  }
  
  if (any(filter_var != "all") && !context_var %in% names(keyboard_data)) {
    stop("context_var not found in keyboard_data")
  }
  
  # -----------------------------
  # look up feature columns
  # -----------------------------
  emoticon_vars <- character(0)
  if (exists("emoticons_df", inherits = TRUE)) {
    emoticon_vars <- intersect(as.character(emoticons_df$emoticon_name), names(keyboard_data))
  }
  
  emoji_vars <- character(0)
  if (exists("emoji_df", inherits = TRUE)) {
    emoji_vars <- intersect(as.character(emoji_df$variable_name), names(keyboard_data))
  }
  
  liwc_vars <- character(0)
  if (exists("liwc.names", inherits = TRUE)) {
    liwc_vars <- intersect(as.character(liwc.names$LIWC.name), names(keyboard_data))
  }
  
  # Remove helper columns if they accidentally appear in liwc_vars
  liwc_vars <- setdiff(liwc_vars, c("words_liwc_match", "liwc_match_rate"))
  
  # -----------------------------
  # helper functions
  # -----------------------------
  safe_sum <- function(x) {
    if (length(x) == 0) return(NA_real_)
    if (inherits(x, "difftime")) x <- as.numeric(x, units = "secs")
    sum(x, na.rm = TRUE)
  }
  
  safe_mean <- function(x) {
    if (length(x) == 0 || !any(is.finite(x))) return(NA_real_)
    mean(x, na.rm = TRUE)
  }
  
  safe_sd <- function(x) {
    if (length(x) == 0 || sum(is.finite(x)) < 2) return(NA_real_)
    sd(x, na.rm = TRUE)
  }
  
  safe_min <- function(x) {
    if (length(x) == 0 || !any(is.finite(x))) return(NA_real_)
    min(x, na.rm = TRUE)
  }
  
  safe_max <- function(x) {
    if (length(x) == 0 || !any(is.finite(x))) return(NA_real_)
    max(x, na.rm = TRUE)
  }
  
  safe_ratio <- function(num, den) {
    if (!is.finite(num) || !is.finite(den) || den <= 0) return(NA_real_)
    num / den
  }
  
  safe_wmean <- function(x, w = NULL) {
    if (length(x) == 0) return(NA_real_)
    if (is.null(w)) return(safe_mean(x))
    
    keep <- is.finite(x) & is.finite(w) & w > 0
    if (!any(keep)) return(NA_real_)
    
    weighted.mean(x[keep], w[keep], na.rm = TRUE)
  }
  
  as_numeric_duration <- function(x) {
    if (inherits(x, "difftime")) {
      as.numeric(x, units = "secs")
    } else {
      as.numeric(x)
    }
  }
  
  safe_divide_vec <- function(num, den) {
    out <- rep(NA_real_, length(num))
    keep <- is.finite(num) & is.finite(den) & den > 0
    out[keep] <- num[keep] / den[keep]
    out
  }
  
  flatten_scores <- function(x) {
    if (is.list(x)) {
      unlist(x, recursive = TRUE, use.names = FALSE)
    } else {
      x
    }
  }
  
  make_liwc_name <- function(x) {
    x_low <- tolower(x)
    if (grepl("^liwc_", x_low)) x_low else paste0("liwc_", x_low)
  }
  
  add_dist_stats <- function(out, x, prefix) {
    out[[paste0(prefix, "_mean")]] <- safe_mean(x)
    out[[paste0(prefix, "_sd")]]   <- safe_sd(x)
    out[[paste0(prefix, "_min")]]  <- safe_min(x)
    out[[paste0(prefix, "_max")]]  <- safe_max(x)
    out
  }
  
  # -----------------------------
  # scope filtering helper
  # -----------------------------
  filter_scope_data <- function(df, scope) {
    if ("words_typed" %in% names(df)) {
      df <- dplyr::filter(df, .data$words_typed >= 1)
    }
    
    if (scope == "all") {
      # keep virtually all logged text with words_typed >= 1
      df <- df
    } else if (scope == "private") {
      df <- dplyr::filter(df, .data[[context_var]] %in% private_values)
    } else if (scope == "public") {
      df <- dplyr::filter(df, .data[[context_var]] %in% public_values)
    }
    
    df
  }
  
  # -----------------------------
  # per-window summarizer
  # -----------------------------
  summarise_window <- function(df) {
    
    words_sum    <- if ("words_typed" %in% names(df)) safe_sum(df$words_typed) else NA_real_
    chars_sum    <- if ("chars_typed" %in% names(df)) safe_sum(df$chars_typed) else NA_real_
    duration_vec <- if ("session_duration" %in% names(df)) {
      as_numeric_duration(df$session_duration)
    } else {
      rep(NA_real_, nrow(df))
    }
    
    duration_sum <- safe_sum(duration_vec)    
    weights_words <- if ("words_typed" %in% names(df)) df$words_typed else rep(1, nrow(df))
    
    out <- tibble::tibble(
      n_sessions = nrow(df),
      words_typed = words_sum,
      chars_typed = chars_sum,
      session_duration = duration_sum
    )
    
    # -------------------------
    # typing meta features
    # -------------------------
    # typing_rate = characters per minute (higher = faster)
    session_typing_rate <- if ("chars_typed" %in% names(df)) {
      safe_divide_vec(df$chars_typed, duration_vec / 60)
    } else {
      rep(NA_real_, nrow(df))
    }
    
    session_chars_per_word <- if (all(c("chars_typed", "words_typed") %in% names(df))) {
      safe_divide_vec(df$chars_typed, df$words_typed)
    } else {
      rep(NA_real_, nrow(df))
    }
    
    session_words_per_session <- if ("words_typed" %in% names(df)) {
      as.numeric(df$words_typed)
    } else {
      rep(NA_real_, nrow(df))
    }
    
    session_chars_per_session <- if ("chars_typed" %in% names(df)) {
      as.numeric(df$chars_typed)
    } else {
      rep(NA_real_, nrow(df))
    }
    
    session_duration_per_session <- duration_vec
    
    out <- add_dist_stats(out, session_typing_rate, "typing_rate")
    out <- add_dist_stats(out, session_chars_per_word, "chars_per_word")
    out <- add_dist_stats(out, session_words_per_session, "words_per_session")
    out <- add_dist_stats(out, session_chars_per_session, "chars_per_session")
    out <- add_dist_stats(out, session_duration_per_session, "duration_per_session")
    
    # -------------------------
    # word sentiment / SentiWS
    # -------------------------
    out$wordsentiment_match_rate <- if (
      "count_sentiment_match" %in% names(df) && "words_typed" %in% names(df)
    ) {
      safe_ratio(safe_sum(df$count_sentiment_match), words_sum)
    } else NA_real_
    
    out$wordsentiment_mean <- NA_real_
    
    if ("sentiment_scores" %in% names(df)) {
      vals <- flatten_scores(df$sentiment_scores)
      out$wordsentiment_mean <- if (length(vals) > 0) safe_mean(vals) else NA_real_
    } else if ("word_sentiment_md" %in% names(df)) {
      sent_w <- if ("count_sentiment_match" %in% names(df)) df$count_sentiment_match else weights_words
      out$wordsentiment_mean <- safe_wmean(df$word_sentiment_md, sent_w)
      
      out <- add_dist_stats(out, df$word_sentiment_md, "wordsentiment_session")
    }
    
    # -------------------------
    # LIWC features
    # -------------------------
    out$liwc_match_rate <- if (
      "words_liwc_match" %in% names(df) && "words_typed" %in% names(df)
    ) {
      safe_ratio(safe_sum(df$words_liwc_match), words_sum)
    } else NA_real_
    
    if (length(liwc_vars) > 0) {
      for (v in liwc_vars) {
        new_name <- make_liwc_name(v)
        
        # regression feature: share of LIWC category in all typed words of the window
        out[[new_name]] <- safe_ratio(safe_sum(df[[v]]), words_sum)
        
        # ML features: session-level LIWC shares, then mean/sd/min/max across sessions
        sess_share <- if ("words_typed" %in% names(df)) {
          safe_divide_vec(df[[v]], df$words_typed)
        } else {
          rep(NA_real_, nrow(df))
        }
        
        out <- add_dist_stats(out, sess_share, paste0(new_name, "_session"))
      }
    }
    
    # -------------------------
    # emoji features
    # -------------------------
    emoji_count_sum <- if ("emoji_count" %in% names(df)) {
      safe_sum(df$emoji_count)
    } else if (length(emoji_vars) > 0) {
      safe_sum(rowSums(df[, emoji_vars, drop = FALSE], na.rm = TRUE))
    } else NA_real_
    
    unique_emoji_count_sum <- if (length(emoji_vars) > 0) {
      sum(colSums(df[, emoji_vars, drop = FALSE], na.rm = TRUE) > 0)
    } else if ("unique_emoji_count" %in% names(df)) {
      safe_max(df$unique_emoji_count)
    } else NA_real_
    
    out$emoji_count <- emoji_count_sum
    out$unique_emoji_count <- unique_emoji_count_sum
    out$emoji_to_word_ratio <- safe_ratio(emoji_count_sum, words_sum)
    
    out$senti_emoji_match_rate <- if (
      "emoji_sentiment_count" %in% names(df) && is.finite(emoji_count_sum)
    ) {
      safe_ratio(safe_sum(df$emoji_sentiment_count), emoji_count_sum)
    } else NA_real_
    
    out$emoji_senti <- NA_real_
    
    if ("emoji_sentiment_scores" %in% names(df)) {
      vals <- flatten_scores(df$emoji_sentiment_scores)
      out$emoji_senti <- if (length(vals) > 0) safe_mean(vals) else NA_real_
    } else if ("emoji_sentiment_avg" %in% names(df)) {
      emoji_w <- if ("emoji_sentiment_count" %in% names(df)) {
        df$emoji_sentiment_count
      } else if ("emoji_count" %in% names(df)) {
        df$emoji_count
      } else {
        rep(1, nrow(df))
      }
      out$emoji_senti <- safe_wmean(df$emoji_sentiment_avg, emoji_w)
      
      out <- add_dist_stats(out, df$emoji_sentiment_avg, "emoji_senti_session")
    }
    
    # Per-emoji feature:
    # share of each emoji from all emoji used in this time window
    if (length(emoji_vars) > 0) {
      for (v in emoji_vars) {
        out[[paste0(v, "_share")]] <- safe_ratio(safe_sum(df[[v]]), emoji_count_sum)
      }
    }
    
    # -------------------------
    # emoticon features
    # -------------------------
    emoticon_count_sum <- if ("emoticon_count" %in% names(df)) {
      safe_sum(df$emoticon_count)
    } else if (length(emoticon_vars) > 0) {
      safe_sum(rowSums(df[, emoticon_vars, drop = FALSE], na.rm = TRUE))
    } else NA_real_
    
    unique_emoticon_count_sum <- if (length(emoticon_vars) > 0) {
      sum(colSums(df[, emoticon_vars, drop = FALSE], na.rm = TRUE) > 0)
    } else if ("unique_emoticon_count" %in% names(df)) {
      safe_max(df$unique_emoticon_count)
    } else NA_real_
    
    out$emoticon_count <- emoticon_count_sum
    out$unique_emoticon_count <- unique_emoticon_count_sum
    out$emoticon_to_word_ratio <- safe_ratio(emoticon_count_sum, words_sum)
    
    # Per-emoticon feature:
    # share of each emoticon from all emoticons used in this time window
    if (length(emoticon_vars) > 0) {
      for (v in emoticon_vars) {
        out[[paste0("emoticon_", v, "_share")]] <- safe_ratio(safe_sum(df[[v]]), emoticon_count_sum)
      }
    }
    
    out
  }
  
  # -----------------------------
  # run for each requested scope
  # -----------------------------
  out_list <- lapply(filter_var, function(scope) {
    
    kd <- filter_scope_data(keyboard_data, scope)
    kd <- kd[!is.na(kd[[window_identifier]]), , drop = FALSE]
    
    if (nrow(kd) == 0) return(NULL)
    
    kd %>%
      dplyr::group_by(.data[[window_identifier]]) %>%
      dplyr::group_modify(~ summarise_window(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(scope = scope, .after = dplyr::all_of(window_identifier))
  })
  
  df <- dplyr::bind_rows(out_list)
  
  if (nrow(df) == 0) return(data.frame())
  
  # clean numeric weirdness, but do NOT force NA -> 0 at this stage
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ {
          x <- .x
          x[is.nan(x) | is.infinite(x)] <- NA_real_
          x
        }
      )
    )
  
  as.data.frame(df)
}