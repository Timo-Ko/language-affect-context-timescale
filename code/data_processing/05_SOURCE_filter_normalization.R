#### FILTER AND NORMALIZE EMOJI AND EMOTICON FEATURES ####

# load data sets
keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_changers.rds") # trait
keyboard_data_state <- readRDS( "data/results/keyboard_data_state_changers.rds") # es moment 

# for trait analyses, only include participants with at least ten used symbols
keyboard_data_trait <- keyboard_data_trait %>%
  mutate(
    total_symbols = emoji_count_sum + emoticon_count_sum
  ) %>%
  filter(total_symbols >= 10)

# for state analyses, only include windows with at least one symbol used
keyboard_data_state <- keyboard_data_state %>%
  mutate(
    total_symbols = emoji_count_sum + emoticon_count_sum
  ) %>%
  filter(total_symbols >= 1)

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

keyboard_data_state_cleaned <- keyboard_data_state %>%
  select(-any_of(rare_symbols))

keyboard_data_trait_cleaned <- keyboard_data_trait %>%
  select(-any_of(rare_symbols))


### normalize symbol use metrics per typed words

# trait 

keyboard_data_trait_cleaned <- keyboard_data_trait_cleaned %>%
  mutate(
    emoticon_word_ratio = if_else(words_typed_sum > 0,
                                           (emoticon_count_sum / words_typed_sum),
                                           NA_real_),
    unique_emoticon_word_ratio = if_else(words_typed_sum > 0,
                                           (unique_emoticon_count_sum / words_typed_sum),
                                           NA_real_),
    emoji_word_ratio    = if_else(words_typed_sum > 0,
                                          (emoji_count_sum / words_typed_sum),
                                          NA_real_),
    unique_emoji_word_ratio    = if_else(words_typed_sum > 0,
                                           (unique_emoji_count_sum / words_typed_sum),
                                           NA_real_)
  ) %>%
  relocate(
    emoticon_word_ratio,
    unique_emoticon_word_ratio,
    emoji_word_ratio,
    unique_emoji_word_ratio,
    .after = na_panas
  )

# state

keyboard_data_state_cleaned <- keyboard_data_state_cleaned %>%
  mutate(
    emoticon_word_ratio = if_else(words_typed_sum > 0,
                                     (emoticon_count_sum / words_typed_sum),
                                     NA_real_),
    unique_emoticon_word_ratio = if_else(words_typed_sum > 0,
                                            (unique_emoticon_count_sum / words_typed_sum),
                                            NA_real_),
    emoji_word_ratio    = if_else(words_typed_sum > 0,
                                     (emoji_count_sum / words_typed_sum),
                                     NA_real_),
    unique_emoji_word_ratio    = if_else(words_typed_sum > 0,
                                            (unique_emoji_count_sum / words_typed_sum),
                                            NA_real_)
  ) %>%
  relocate(
    emoticon_word_ratio,
    unique_emoticon_word_ratio,
    emoji_word_ratio,
    unique_emoji_word_ratio,
    .after = gender
  )



### normalize single emoji and emoticon use

# --- detect single-emoji and single-emoticon columns ---
emoji_single_cols <- intersect(
  paste0(emoji_df$variable_name, "_sum"),
  names(keyboard_data_trait_cleaned)
)

emoticon_single_cols <- intersect(
  paste0("emoticon_", emoticons_df$emoticon_name, "_sum"),
  names(keyboard_data_trait_cleaned)
)


# traits

# --- normalize by total emoji / emoticon count per user ---
keyboard_data_trait_norm <- keyboard_data_trait_cleaned %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(emoji_single_cols),
      ~ ifelse(.data$emoji_count_sum > 0, .x / .data$emoji_count_sum, NA_real_),
      .names = "{.col}_share"
    ),
    dplyr::across(
      dplyr::all_of(emoticon_single_cols),
      ~ ifelse(.data$emoticon_count_sum > 0, .x / .data$emoticon_count_sum, NA_real_),
      .names = "{.col}_share"
    )
  ) %>%
  dplyr::select(
    -dplyr::all_of(c(emoji_single_cols, emoticon_single_cols))
  )

## state

# --- normalize by row-wise totals (ES window) ---
keyboard_data_state_norm <- keyboard_data_state_cleaned %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(emoji_single_cols),
      ~ ifelse(.data$emoji_count_sum > 0, .x / .data$emoji_count_sum, NA_real_),
      .names = "{.col}_share"
    ),
    dplyr::across(
      dplyr::all_of(emoticon_single_cols),
      ~ ifelse(.data$emoticon_count_sum > 0, .x / .data$emoticon_count_sum, NA_real_),
      .names = "{.col}_share"
    )
  ) %>%
  dplyr::select(
    -dplyr::all_of(c(emoji_single_cols, emoticon_single_cols))
  )


## save cleaned data files 

saveRDS(keyboard_data_trait_norm, "data/results/keyboard_data_trait_final.rds") # es moment 
saveRDS(keyboard_data_state_norm, "data/results/keyboard_data_state_final.rds") # trait

### preprocessing and feature extraction competed ###