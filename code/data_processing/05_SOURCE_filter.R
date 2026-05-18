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

# IMPORTANT:
# derive rarity from the trait-filtered dataset, not the unfiltered one
# this aligns prevalence estimates with the actual analysis sample

## emoji features

emoji_cols <- grep("^emoji_\\d+_session_mean$", names(keyboard_data_trait_filter), value = TRUE)

proportion_emoji_used <- sapply(keyboard_data_trait_filter[emoji_cols], function(column) {
  sum(!is.na(column) & column != 0) / nrow(keyboard_data_trait_filter)
})

proportion_emoji_df <- data.frame(
  variable_name = sub("_session_mean", "", names(proportion_emoji_used)),
  proportion_used = proportion_emoji_used
)

emoji_df_extended <- merge(
  emoji_df,
  proportion_emoji_df,
  by = "variable_name",
  all.x = TRUE
)

rare_emoji <- emoji_df_extended[emoji_df_extended$proportion_used < 0.10, ]$variable_name
rare_emoji <- paste0(rare_emoji, "_session_mean")

## emoticon features

emoticon_cols <- grep("^emoticon_.*_session_mean$", names(keyboard_data_trait_filter), value = TRUE)

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