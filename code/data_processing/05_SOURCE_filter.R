#### FILTER AND NORMALIZE EMOJI AND EMOTICON FEATURES ####

library(dplyr)
library(psych)

# load data sets
keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_changers.rds")   # trait
keyboard_data_day   <- readRDS("data/results/keyboard_data_day_changers.rds")     # daily
keyboard_data_ema   <- readRDS("data/results/keyboard_data_ema_changers.rds")     # momentary

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
  filter(words_typed >= 10, n_ema_day >= 3)

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
  mutate(kept = words_typed >= 10 & n_ema_day >= 3)

day_comp %>%
  group_by(kept) %>%
  summarise(
    n = sum(!is.na(daily_valence)),
    mean_daily_valence   = mean(daily_valence, na.rm = TRUE),
    sd_daily_valence     = sd(daily_valence, na.rm = TRUE),
    median_daily_valence = median(daily_valence, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
## DROP RARE EMOJI AND EMOTICON FEATURES
############################################################

emoji_df     <- readRDS("data/helper/emoji_df.rds")
emoticons_df <- readRDS("data/helper/emoticons_df.rds")


## emoji features

emoji_cols <- grep("^emoji_\\d+_share$", names(keyboard_data_trait_filter), value = TRUE)

proportion_emoji_used <- sapply(keyboard_data_trait_filter[emoji_cols], function(column) {
  sum(!is.na(column) & column != 0) / nrow(keyboard_data_trait_filter)
})

proportion_emoji_df <- data.frame(
  variable_name = sub("_share", "", names(proportion_emoji_used)),
  proportion_used = proportion_emoji_used
)

emoji_df_extended <- merge(
  emoji_df,
  proportion_emoji_df,
  by = "variable_name",
  all.x = TRUE
)

rare_emoji <- emoji_df_extended[emoji_df_extended$proportion_used < 0.10, ]$variable_name
rare_emoji <- paste0(rare_emoji, "_share")

## emoticon features

emoticon_cols <- grep("^emoticon_.*_share$", names(keyboard_data_trait_filter), value = TRUE)

proportion_emoticon_used <- sapply(keyboard_data_trait_filter[emoticon_cols], function(column) {
  sum(!is.na(column) & column != 0) / nrow(keyboard_data_trait_filter)
})

rare_emoticons <- names(proportion_emoticon_used)[as.numeric(proportion_emoticon_used) < 0.10]

## drop rare symbols from all final datasets

rare_symbols <- union(rare_emoji, rare_emoticons)

keyboard_data_trait_cleaned <- keyboard_data_trait_filter %>%
  select(-any_of(rare_symbols))

keyboard_data_day_cleaned <- keyboard_data_day_filter %>%
  select(-any_of(rare_symbols))

keyboard_data_ema_cleaned <- keyboard_data_ema_filter %>%
  select(-any_of(rare_symbols))

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

library(dplyr)
library(stringr)

dir.create("results", recursive = TRUE, showWarnings = FALSE)

count_initial_feature_families <- function(data, no_feature_columns, dataset_name = "dataset") {
  
  # Keep only columns that are potential keyboard-derived predictors
  feature_cols <- setdiff(names(data), no_feature_columns)
  
  # Word dictionary features
  dictionary_features <- feature_cols[
    str_detect(feature_cols, "^liwc_|^wordsentiment")
  ]
  
  # Emoji and emoticon features
  emoji_features <- feature_cols[
    str_detect(feature_cols, "^emoji_|^emoticon_|^senti_emoji")
  ]
  
  # Typing dynamics / behavioral production features
  typing_features <- setdiff(
    feature_cols,
    c(dictionary_features, emoji_features)
  )
  
  summary_out <- tibble(
    dataset = dataset_name,
    total_extracted_features = length(feature_cols),
    word_dictionary_features = length(dictionary_features),
    emoji_emoticon_features = length(emoji_features),
    typing_dynamics_features = length(typing_features)
  )
  
  feature_list_out <- tibble(
    dataset = dataset_name,
    feature = sort(feature_cols),
    feature_family = case_when(
      feature %in% dictionary_features ~ "Word dictionaries",
      feature %in% emoji_features ~ "Emojis/emoticons",
      feature %in% typing_features ~ "Typing dynamics",
      TRUE ~ "Unclassified"
    )
  )
  
  return(list(
    summary = summary_out,
    feature_list = feature_list_out,
    dictionary_features = sort(dictionary_features),
    emoji_features = sort(emoji_features),
    typing_features = sort(typing_features)
  ))
}

# no feeature columns

no_feature_columns_trait_initial <- c(
  "user_id",
  "age",
  "gender",
  "pa_panas",
  "na_panas"
)

no_feature_columns_day_initial <- c(
  "user_id",
  "age",
  "gender",
  "date",
  "valence",
  "arousal",
  "n_ema",
  "mean_valence",
  "mean_arousal"
)

no_feature_columns_moment_initial <- c(
  "user_id",
  "age",
  "gender",
  "es_questionnaire_id",
  "timestamp",
  "valence",
  "arousal",
  "stress"
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