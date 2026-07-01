### PREPARATION ####

packages <- c("dplyr", "caret")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load function for target independet preprocessing
source("code/analyses/helper/target_independent_preproc.R")

## load data 
keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_final.rds")
keyboard_data_day <- readRDS("data/results/keyboard_data_day_final.rds")
keyboard_data_ema   <- readRDS("data/results/keyboard_data_ema_final.rds")

#### TARGET INDEPENDENT PREPROCESSING ####

# define the columns that are not features (here preprocessing is not applied)
no_feature_columns_trait <- c(
  "user_id",
  "scope",
  "age",
  "gender",
  "pa_panas",
  "na_panas",
  "n_language_days",
  "words_typed",
  "emoji_count",
  "emoticon_count",
  "n_sessions",
  "chars_typed",
  "session_duration"
)

no_feature_columns_day <- c(
  "user_id",
  "date",
  "daily_valence",
  "n_ema_day",
  "age",
  "gender",
  "scope",
  "n_sessions",
  "words_typed",
  "chars_typed",
  "session_duration"
)

no_feature_columns_moment <- c(
  "user_id",
  "es_questionnaire_id",
  "arousal",
  "valence",
  "valence_avg",
  "arousal_avg",
  "notificationTimestamp_corrected",
  "questionnaireStartedTimestamp_corrected",
  "questionnaireEndedTimestamp_corrected",
  "age",
  "gender",
  "scope",
  "n_sessions",
  "words_typed",
  "chars_typed",
  "session_duration"
)

# apply functions for target-independent preprocessing

keyboard_data_trait_ml <- target_independent_preproc(
  keyboard_data_trait,
  no_feature_columns_trait
)

keyboard_data_day_ml <- target_independent_preproc(
  keyboard_data_day,
  no_feature_columns_day
)

keyboard_data_moment_ema_ml <- target_independent_preproc(
  keyboard_data_ema,
  no_feature_columns_moment
)

# save data

saveRDS(keyboard_data_trait_ml, "data/results/keyboard_data_trait_ml.rds")
saveRDS(keyboard_data_day_ml, "data/results/keyboard_data_day_ml.rds")
saveRDS(keyboard_data_moment_ema_ml, "data/results/keyboard_data_ema_ml.rds")

############################
#### FEATURE COUNTS FOR FINAL FEATURE SETS ####
############################

count_feature_families <- function(data, no_feature_columns, dataset_name = "dataset") {
  
  feature_cols <- setdiff(names(data), no_feature_columns)
  
  dictionary_features <- feature_cols[
    grepl("^liwc_|^wordsentiment", feature_cols)
  ]
  
  emoji_features <- feature_cols[
    grepl("^emoji_|^emoticon_|^senti_emoji", feature_cols)
  ]
  
  typing_features <- setdiff(
    feature_cols,
    c(dictionary_features, emoji_features)
  )
  
  out <- data.frame(
    dataset = dataset_name,
    total_features = length(feature_cols),
    dictionary_features = length(dictionary_features),
    emoji_emoticon_features = length(emoji_features),
    typing_dynamics_features = length(typing_features)
  )
  
  return(list(
    summary = out,
    feature_cols = feature_cols,
    dictionary_features = sort(dictionary_features),
    emoji_features = sort(emoji_features),
    typing_features = sort(typing_features)
  ))
}

trait_counts <- count_feature_families(
  data = keyboard_data_trait_ml,
  no_feature_columns = no_feature_columns_trait,
  dataset_name = "Trait"
)

day_counts <- count_feature_families(
  data = keyboard_data_day_ml,
  no_feature_columns = no_feature_columns_day,
  dataset_name = "Daily"
)

moment_counts <- count_feature_families(
  data = keyboard_data_moment_ema_ml,
  no_feature_columns = no_feature_columns_moment,
  dataset_name = "Momentary"
)

feature_count_table <- bind_rows(
  trait_counts$summary,
  day_counts$summary,
  moment_counts$summary
)

print(feature_count_table)

write.csv(
  feature_count_table,
  file = "results/feature_set_counts.csv",
  row.names = FALSE
)

# FINISH