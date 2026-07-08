#### FILTER AND NORMALIZE EMOJI AND EMOTICON FEATURES ####

library(dplyr)
library(tidyr)
library(psych)
library(stringr)

dir.create("results", recursive = TRUE, showWarnings = FALSE)

# load data sets
keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_changers.rds")   # trait
keyboard_data_day   <- readRDS("data/results/keyboard_data_day_changers.rds")     # daily
keyboard_data_ema   <- readRDS("data/results/keyboard_data_ema_changers.rds")     # momentary

############################################################
## SAMPLE DESCRIPTIVES BEFORE WORD THRESHOLD FILTERS
############################################################

summarise_unfiltered_sample <- function(
  data,
  dataset_name,
  word_threshold,
  additional_filter = rep(TRUE, nrow(data))
) {
  
  data_temp <- data %>%
    ungroup() %>%
    mutate(
      communication_context = scope,
      passes_word_filter = words_typed >= word_threshold,
      passes_additional_filter = replace_na(
        as.logical(additional_filter),
        FALSE
      ),
      retained = passes_word_filter & passes_additional_filter
    )
  
  summarise_group <- function(x, context_label) {
    
    day_counts <- if (dataset_name == "Trait") {
      
      x %>%
        distinct(user_id, n_language_days) %>%
        pull(n_language_days)
      
    } else if (dataset_name == "Daily") {
      
      x %>%
        distinct(user_id, date) %>%
        count(user_id, name = "n_days") %>%
        pull(n_days)
      
    } else {
      
      numeric(0)
    }
    
    tibble(
      dataset = dataset_name,
      communication_context = context_label,
      
      n_observations = nrow(x),
      n_participants = n_distinct(x$user_id, na.rm = TRUE),
      
      n_participant_days = if (length(day_counts) > 0) {
        sum(day_counts, na.rm = TRUE)
      } else {
        NA_real_
      },
      
      mean_days_per_participant = if (length(day_counts) > 0) {
        mean(day_counts, na.rm = TRUE)
      } else {
        NA_real_
      },
      
      sd_days_per_participant = if (length(day_counts) > 1) {
        sd(day_counts, na.rm = TRUE)
      } else {
        NA_real_
      },
      
      total_words = sum(x$words_typed, na.rm = TRUE),
      mean_words = mean(x$words_typed, na.rm = TRUE),
      sd_words = sd(x$words_typed, na.rm = TRUE),
      median_words = median(x$words_typed, na.rm = TRUE),
      min_words = min(x$words_typed, na.rm = TRUE),
      max_words = max(x$words_typed, na.rm = TRUE),
      
      n_below_word_threshold = sum(
        x$words_typed < word_threshold,
        na.rm = TRUE
      ),
      pct_below_word_threshold = 100 * mean(
        x$words_typed < word_threshold,
        na.rm = TRUE
      ),
      
      n_pass_word_filter = sum(
        x$passes_word_filter,
        na.rm = TRUE
      ),
      pct_pass_word_filter = 100 * mean(
        x$passes_word_filter,
        na.rm = TRUE
      ),
      
      n_retained = sum(
        x$retained,
        na.rm = TRUE
      ),
      pct_retained = 100 * mean(
        x$retained,
        na.rm = TRUE
      ),
      
      n_dropped = sum(
        !x$retained,
        na.rm = TRUE
      ),
      pct_dropped = 100 * mean(
        !x$retained,
        na.rm = TRUE
      )
    )
  }
  
  data_temp %>%
    group_split(communication_context) %>%
    lapply(function(x) {
      summarise_group(
        x,
        context_label = unique(x$communication_context)
      )
    }) %>%
    bind_rows()
}

############################################################
## TRAIT-LEVEL UNFILTERED SAMPLE
############################################################

trait_unfiltered_descriptives <- summarise_unfiltered_sample(
  data = keyboard_data_trait,
  dataset_name = "Trait",
  word_threshold = 100
)

############################################################
## DAILY-LEVEL UNFILTERED SAMPLE
############################################################

daily_unfiltered_descriptives <- summarise_unfiltered_sample(
  data = keyboard_data_day,
  dataset_name = "Daily",
  word_threshold = 10
)

############################################################
## MOMENTARY-LEVEL UNFILTERED SAMPLE
############################################################

momentary_unfiltered_descriptives <- summarise_unfiltered_sample(
  data = keyboard_data_ema,
  dataset_name = "Momentary",
  word_threshold = 10
)


############################################################
## COMBINE AND SAVE MAIN DESCRIPTIVES
############################################################

unfiltered_sample_descriptives <- bind_rows(
  trait_unfiltered_descriptives,
  daily_unfiltered_descriptives,
  momentary_unfiltered_descriptives
)

print(
  unfiltered_sample_descriptives,
  width = Inf,
  n = Inf
)

write.csv(
  unfiltered_sample_descriptives,
  file = "results/unfiltered_sample_descriptives.csv",
  row.names = FALSE
)


############################################################
## DAILY FILTER-FAILURE BREAKDOWN
############################################################

daily_filter_breakdown <- keyboard_data_day %>%
  ungroup() %>%
  mutate(
    filter_status = if_else(
      words_typed >= 10,
      "Retained",
      "Dropped: insufficient words"
    )
  ) %>%
  count(scope, filter_status, name = "n_observations") %>%
  group_by(scope) %>%
  mutate(
    percentage = 100 * n_observations / sum(n_observations)
  ) %>%
  ungroup()

print(daily_filter_breakdown)

write.csv(
  daily_filter_breakdown,
  file = "results/daily_filter_failure_breakdown.csv",
  row.names = FALSE
)

############################################################
## APPLY WORD-COUNT FILTERS
############################################################

# trait analyses: include participants/windows with at least 100 typed words
hist(
  keyboard_data_trait$words_typed,
  breaks = 1000,
  xlim = c(0, 1000)
)

describe(keyboard_data_trait$words_typed)

keyboard_data_trait_filter <- keyboard_data_trait %>%
  filter(words_typed >= 100)

# daily analyses: include participant-days with at least 10 typed words
hist(
  keyboard_data_day$words_typed,
  breaks = 100
)

describe(keyboard_data_day$words_typed)

keyboard_data_day_filter <- keyboard_data_day %>%
  filter(words_typed >= 10)

# momentary analyses: include windows with at least 10 typed words
hist(
  keyboard_data_ema$words_typed,
  breaks = 100
)

describe(keyboard_data_ema$words_typed)

keyboard_data_ema_filter <- keyboard_data_ema %>%
  filter(words_typed >= 10)

############################################################
## COMPARE AFFECT IN KEPT VS DROPPED OBSERVATIONS
############################################################

# momentary EMA valence: kept vs dropped
ema_comp <- keyboard_data_ema %>%
  mutate(kept = words_typed >= 10)

ema_comp %>%
  group_by(kept) %>%
  summarise(
    n = sum(!is.na(valence)),
    mean_valence   = mean(valence, na.rm = TRUE),
    sd_valence     = sd(valence, na.rm = TRUE),
    median_valence = median(valence, na.rm = TRUE),
    .groups = "drop"
  )

# daily valence: kept vs dropped
day_comp <- keyboard_data_day %>%
  mutate(kept = words_typed >= 10 )

day_comp %>%
  group_by(kept) %>%
  summarise(
    n = sum(!is.na(arousal_day_mean)),
    mean_daily_valence   = mean(arousal_day_mean, na.rm = TRUE),
    sd_daily_valence     = sd(arousal_day_mean, na.rm = TRUE),
    median_daily_valence = median(arousal_day_mean, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
## DROP RARE EMOJI AND EMOTICON FEATURES
############################################################

emoji_df <- readRDS("data/helper/emoji_df.rds")
emoticons_df <- readRDS("data/helper/emoticons_df.rds")

# Use one row per participant, aggregated across all communication
keyboard_data_trait_symbol_reference <- keyboard_data_trait_filter %>%
  filter(scope == "all")

############################
## Emoji features
############################

emoji_cols <- grep(
  "^emoji_\\d+_share$",
  names(keyboard_data_trait_symbol_reference),
  value = TRUE
)

proportion_emoji_used <- sapply(
  keyboard_data_trait_symbol_reference[emoji_cols],
  function(column) {
    mean(!is.na(column) & column != 0)
  }
)

proportion_emoji_df <- data.frame(
  variable_name = sub(
    "_share$",
    "",
    names(proportion_emoji_used)
  ),
  proportion_used = as.numeric(proportion_emoji_used)
)

emoji_df_extended <- emoji_df %>%
  left_join(
    proportion_emoji_df,
    by = "variable_name"
  )

rare_emoji <- emoji_df_extended %>%
  filter(
    !is.na(proportion_used),
    proportion_used < 0.10
  ) %>%
  pull(variable_name) %>%
  paste0("_share")

############################
## Emoticon features
############################

emoticon_cols <- grep(
  "^emoticon_.*_share$",
  names(keyboard_data_trait_symbol_reference),
  value = TRUE
)

proportion_emoticon_used <- sapply(
  keyboard_data_trait_symbol_reference[emoticon_cols],
  function(column) {
    mean(!is.na(column) & column != 0)
  }
)

proportion_emoticon_df <- data.frame(
  variable_name = sub(
    "_share$",
    "",
    names(proportion_emoticon_used)
  ),
  proportion_used = as.numeric(proportion_emoticon_used)
)

emoticons_df_extended <- emoticons_df %>%
  mutate(
    variable_name = paste0(
      "emoticon_",
      emoticon_name
    )
  ) %>%
  left_join(
    proportion_emoticon_df,
    by = "variable_name"
  )

rare_emoticons <- emoticons_df_extended %>%
  filter(
    !is.na(proportion_used),
    proportion_used < 0.10
  ) %>%
  pull(variable_name) %>%
  paste0("_share")

############################
## Drop rare symbols
############################

rare_symbols <- union(
  rare_emoji,
  rare_emoticons
)

keyboard_data_trait_cleaned <- keyboard_data_trait_filter %>%
  select(-any_of(rare_symbols))

keyboard_data_day_cleaned <- keyboard_data_day_filter %>%
  select(-any_of(rare_symbols))

keyboard_data_ema_cleaned <- keyboard_data_ema_filter %>%
  select(-any_of(rare_symbols))

############################
## Sanity checks
############################

stopifnot(
  all(rare_emoji %in% emoji_cols),
  all(rare_emoticons %in% emoticon_cols),
  !any(rare_symbols %in% names(keyboard_data_trait_cleaned)),
  !any(rare_symbols %in% names(keyboard_data_day_cleaned)),
  !any(rare_symbols %in% names(keyboard_data_ema_cleaned))
)

symbol_filter_summary <- tibble(
  symbol_type = c("Emoji", "Emoticon"),
  n_available = c(
    length(emoji_cols),
    length(emoticon_cols)
  ),
  n_rare_dropped = c(
    length(rare_emoji),
    length(rare_emoticons)
  ),
  n_retained = c(
    length(emoji_cols) - length(rare_emoji),
    length(emoticon_cols) - length(rare_emoticons)
  )
)

symbol_filter_summary

############################################################
## SAVE FILTERED + CLEANED DATA FILES
############################################################

saveRDS(keyboard_data_trait_cleaned, "data/results/keyboard_data_trait_final.rds")
saveRDS(keyboard_data_day_cleaned,   "data/results/keyboard_data_day_final.rds")
saveRDS(keyboard_data_ema_cleaned,   "data/results/keyboard_data_ema_final.rds")

### processing and feature extraction completed ###


############################
#### FEATURE COUNTS FOR INITIALLY EXTRACTED FEATURE SETS ####
############################

count_initial_feature_families <- function(data, no_feature_columns, dataset_name = "dataset") {
  
  # Keep only columns that are potential keyboard-derived predictors
  
  measurement_coverage_features <- c(
    "liwc_match_rate",
    "wordsentiment_match_rate",
    "senti_emoji_match_rate"
  )
  
  feature_cols <- setdiff(
    names(data),
    c(
      no_feature_columns,
      measurement_coverage_features
    )
  )
  
  # Word dictionary features
  dictionary_features <- feature_cols[
    str_detect(feature_cols, "^liwc_|^wordsentiment")
  ]
  
  # Emoji and emoticon features
  symbol_features <- feature_cols[
    str_detect(
      feature_cols,
      "^emoji_|^emoticon_|^unique_emoji_count$|^unique_emoticon_count$"
    )
  ]
  
  # Typing dynamics / behavioral production features
  typing_features <- setdiff(
    feature_cols,
    c(dictionary_features, symbol_features)
  )
  
  summary_out <- tibble(
    dataset = dataset_name,
    total_extracted_features = length(feature_cols),
    word_dictionary_features = length(dictionary_features),
    symbol_features = length(symbol_features),
    typing_dynamics_features = length(typing_features)
  )
  
  feature_list_out <- tibble(
    dataset = dataset_name,
    feature = sort(feature_cols),
    feature_family = case_when(
      feature %in% dictionary_features ~ "Word dictionaries",
      feature %in% symbol_features ~ "Emojis/emoticons",
      feature %in% typing_features ~ "Typing dynamics",
      TRUE ~ "Unclassified"
    )
  )
  
  return(list(
    summary = summary_out,
    feature_list = feature_list_out,
    dictionary_features = sort(dictionary_features),
    symbol_features = sort(symbol_features),
    typing_features = sort(typing_features)
  ))
}

# no feeature columns

no_feature_columns_trait_initial <- c(
  "user_id",
  "user_uuid",
  "scope",
  "n_language_days",
  "age",
  "gender",
  "pa_panas",
  "na_panas"
)

no_feature_columns_day_initial <- c(
  "user_id",
  "user_uuid",
  "scope",
  "date",
  "valence_day_mean",
  "arousal_day_mean",
  "es_count_day",
  "age",
  "gender"
)

no_feature_columns_moment_initial <- c(
  "user_id",
  "user_uuid",
  "scope",
  "date",
  "n_ema",
  "es_questionnaire_id",
  "arousal",
  "valence",
  "valence_median",
  "arousal_median",
  "valence_diff",
  "arousal_diff",
  "notificationTimestamp_corrected",
  "questionnaireStartedTimestamp_corrected",
  "questionnaireEndedTimestamp_corrected",
  "age",
  "gender"
)


############################
#### COUNT INITIAL EXTRACTED FEATURES ####
############################

trait_initial_counts <- count_initial_feature_families(
  data = keyboard_data_trait_cleaned,
  no_feature_columns = no_feature_columns_trait_initial,
  dataset_name = "Trait"
)

day_initial_counts <- count_initial_feature_families(
  data = keyboard_data_day_cleaned,
  no_feature_columns = no_feature_columns_day_initial,
  dataset_name = "Daily"
)

moment_initial_counts <- count_initial_feature_families(
  data = keyboard_data_ema_cleaned,
  no_feature_columns = no_feature_columns_moment_initial,
  dataset_name = "Momentary"
)

initial_feature_count_table <- bind_rows(
  trait_initial_counts$summary,
  day_initial_counts$summary,
  moment_initial_counts$summary
)

print(initial_feature_count_table)

write.csv(
  initial_feature_count_table,
  file = "results/initial_extracted_feature_counts.csv",
  row.names = FALSE
)

initial_feature_list <- bind_rows(
  trait_initial_counts$feature_list,
  day_initial_counts$feature_list,
  moment_initial_counts$feature_list
)

write.csv(
  initial_feature_list,
  file = "results/initial_extracted_feature_list_by_dataset.csv",
  row.names = FALSE
)

# finish