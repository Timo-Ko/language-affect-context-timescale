# Shared definitions for predictive models and their reported feature counts.
keyboard_metadata_columns <- c(
  "user_id", "user_uuid", "scope", "context", "date", "user_day_id",
  "occasion_id", "age", "gender", "pa_panas", "na_panas",
  "valence_day_mean", "arousal_day_mean", "n_ema_day", "es_count_day",
  "n_ema", "n_language_days", "valence", "arousal",
  "valence_avg", "arousal_avg", "valence_mean", "arousal_mean",
  "valence_median", "arousal_median", "valence_diff", "arousal_diff",
  "notificationTimestamp", "questionnaireStartedTimestamp",
  "questionnaireEndedTimestamp", "notificationTimestamp_corrected",
  "questionnaireStartedTimestamp_corrected", "questionnaireEndedTimestamp_corrected",
  "es_questionnaire_id", "weekday", "nr", "week", "..row_id"
)

keyboard_coverage_columns <- c(
  "liwc_match_rate", "wordsentiment_match_rate", "senti_emoji_match_rate"
)

keyboard_feature_names <- function(data, extra_drop = character(0)) {
  setdiff(
    names(data),
    c(keyboard_metadata_columns, keyboard_coverage_columns, extra_drop)
  )
}

keyboard_feature_families <- function(data) {
  features <- keyboard_feature_names(data)
  word <- features[grepl("^liwc_|^wordsentiment", features)]
  symbol <- features[grepl(
    "^emoji_|^emoticon_|^senti_emoji|^unique_emoji_count$|^unique_emoticon_count$",
    features
  )]
  list(
    word = sort(word),
    symbol = sort(symbol),
    typing = sort(setdiff(features, c(word, symbol)))
  )
}
