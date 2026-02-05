#### FILTER AND NORMALIZE EMOJI AND EMOTICON FEATURES ####

# load data sets
keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_changers.rds") # trait

#keyboard_data_ema_centered <- readRDS( "data/results/keyboard_data_ema_centered_changers.rds") # es moment 
keyboard_data_ema_pre180 <- readRDS( "data/results/keyboard_data_ema_pre180_changers.rds") # es moment 

keyboard_data_ema_pre60 <- readRDS( "data/results/keyboard_data_ema_pre60_changers.rds") # es moment 

# for trait analyses, only include participants with at least ten used symbols

hist(keyboard_data_trait$unique_emoji_count_sum,
     breaks = 1000,
     xlim = c(0, 1000))

describe(keyboard_data_trait$unique_emoji_count_sum)

keyboard_data_trait <- keyboard_data_trait %>%
  filter(emoji_count_sum >= 10)

# for state analyses, only include windows with at least one emoji used

hist(keyboard_data_ema_pre180$unique_emoji_count_sum,
     breaks = 100)

# sensitivity analysis 
hist(keyboard_data_ema_pre60$unique_emoji_count_sum,
     breaks = 100)

keyboard_data_ema_pre180_filter <- keyboard_data_ema_pre180 %>%
  filter(emoji_count_sum >= 1)

# compare valence scores with dropped emas
# kept vs dropped indicator
ema_comp <- keyboard_data_ema_pre180 %>%
  mutate(kept = emoji_count_sum >= 1)

# descriptive comparison
ema_comp %>%
  group_by(kept) %>%
  summarise(
    n = sum(!is.na(valence)),
    mean_valence = mean(valence, na.rm = TRUE),
    sd_valence   = sd(valence, na.rm = TRUE),
    median_valence = median(valence, na.rm = TRUE),
    .groups = "drop"
  )


### drop rare emoji and emoticons

emoji_df <- readRDS( "data/helper/emoji_df.rds") 
emoticons_df <- readRDS( "data/helper/emoticons_df.rds") 

# Calculate the proportion of participants with non-zero/non-NA usage for each emoji
emoji_cols <- paste0(emoji_df$variable_name, "_sum")

proportion_emoji_used <- sapply(keyboard_data_trait[emoji_cols], function(column) {
  sum(!is.na(column) & column != 0) / nrow(keyboard_data_trait)
})

# Convert proportion_emoji_used to a data frame
proportion_emoji_df <- data.frame(
  variable_name = sub("_sum", "", names(proportion_emoji_used)),
  proportion_used = proportion_emoji_used
)

# Join with emoji_df
emoji_df_extended <- merge(emoji_df, proportion_emoji_df, by = "variable_name", all.x = TRUE)

# Filter for emojis used by less than 10% of users
rare_emoji <- emoji_df_extended[emoji_df_extended$proportion_used < 0.10, ]$variable_name
rare_emoji <- paste0(rare_emoji, "_sum")

# emoticons

emoticon_cols <- grep("^emoticon_.*_sum$", names(keyboard_data_trait), value = TRUE)


proportion_emoticon_used <- sapply(keyboard_data_trait[emoticon_cols], function(column) {
  sum(!is.na(column) & column != 0) / nrow(keyboard_data_trait)
})

rare_emoticons <- names(proportion_emoticon_used)[as.numeric(proportion_emoticon_used) < 0.10]


## drop rare symbols 

rare_symbols <- union(rare_emoji, rare_emoticons)

keyboard_data_trait_cleaned <- keyboard_data_trait %>%
  select(-any_of(rare_symbols))

keyboard_data_ema_centered_cleaned <- keyboard_data_ema_centered %>%
  select(-any_of(rare_symbols))

keyboard_data_ema_pre60_cleaned <- keyboard_data_ema_pre60 %>%
  select(-any_of(rare_symbols))


### normalize symbol use metrics per typed words


# helper: add ratios + relocate after a given column
add_word_ratios <- function(df, after_col) {
  df %>%
    dplyr::mutate(
      emoticon_word_ratio        = dplyr::if_else(words_typed_sum > 0,
                                                  emoticon_count_sum / words_typed_sum, NA_real_),
      unique_emoticon_word_ratio = dplyr::if_else(words_typed_sum > 0,
                                                  unique_emoticon_count_sum / words_typed_sum, NA_real_),
      emoji_word_ratio           = dplyr::if_else(words_typed_sum > 0,
                                                  emoji_count_sum / words_typed_sum, NA_real_),
      unique_emoji_word_ratio    = dplyr::if_else(words_typed_sum > 0,
                                                  unique_emoji_count_sum / words_typed_sum, NA_real_)
    ) %>%
    dplyr::relocate(
      emoticon_word_ratio,
      unique_emoticon_word_ratio,
      emoji_word_ratio,
      unique_emoji_word_ratio,
      .after = dplyr::all_of(after_col)
    )
}

keyboard_data_trait_cleaned <- add_word_ratios(keyboard_data_trait_cleaned, after_col = "na_panas")

keyboard_data_ema_centered_cleaned <- add_word_ratios(keyboard_data_ema_centered_cleaned, after_col = "gender")
keyboard_data_ema_pre60_cleaned <- add_word_ratios(keyboard_data_ema_pre60_cleaned, after_col = "gender")


### normalize single emoji and emoticon use (trait + state)

# --- detect single-emoji and single-emoticon columns ---
emoji_single_cols <- intersect(
  paste0(emoji_df$variable_name, "_sum"),
  names(keyboard_data_trait_cleaned)
)

emoticon_single_cols <- intersect(
  paste0("emoticon_", emoticons_df$emoticon_name, "_sum"),
  names(keyboard_data_trait_cleaned)
)

# helper: convert *_sum columns to shares and drop original sums
add_symbol_shares <- function(df, emoji_cols, emoticon_cols) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(emoji_cols),
        ~ ifelse(.data$emoji_count_sum > 0, .x / .data$emoji_count_sum, NA_real_),
        .names = "{.col}_share"
      ),
      dplyr::across(
        dplyr::all_of(emoticon_cols),
        ~ ifelse(.data$emoticon_count_sum > 0, .x / .data$emoticon_count_sum, NA_real_),
        .names = "{.col}_share"
      )
    ) %>%
    dplyr::select(-dplyr::all_of(c(emoji_cols, emoticon_cols)))
}

keyboard_data_trait_norm <- add_symbol_shares(keyboard_data_trait_cleaned, emoji_single_cols, emoticon_single_cols)

keyboard_data_ema_centered_norm <- add_symbol_shares(keyboard_data_ema_centered_cleaned, emoji_single_cols, emoticon_single_cols)
keyboard_data_ema_pre60_norm <- add_symbol_shares(keyboard_data_ema_pre60_cleaned, emoji_single_cols, emoticon_single_cols)


## save cleaned data files 

saveRDS(keyboard_data_trait_norm, "data/results/keyboard_data_trait_final.rds") 

saveRDS(keyboard_data_ema_centered_norm, "data/results/keyboard_data_ema_centered_final.rds") 
saveRDS(keyboard_data_ema_pre60_norm, "data/results/keyboard_data_ema_pre60_final.rds") 

### pre60processing and feature extraction competed ###