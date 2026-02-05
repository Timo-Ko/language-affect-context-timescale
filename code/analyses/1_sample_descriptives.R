### PREPARATION ####

## Install and load required packages 

packages <- c("dplyr", "tidyr", "data.table", "psych","ggplot2", "ggcorrplot", "stringr", "ggrepel", "ragg", "systemfonts", "patchwork", "readr", "gridExtra")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

## load data 

# trait affect
keyboard_data_trait <- readRDS(file="data/results/keyboard_data_trait_final.rds")

# state affect
keyboard_data_state <- as.data.frame(readRDS(file="data/results/keyboard_data_ema_pre180_final.rds"))

#### SAMPLE DESCRIPTIVES ####

## trait

# sample demographics

table(keyboard_data_trait$gender)
summary(keyboard_data_trait$age)

# outcome distribution
describe(keyboard_data_trait$pa_panas)
describe(keyboard_data_trait$na_panas)

# overall emoji and emoticon use 
sum(keyboard_data_trait$emoji_count_sum)
sum(keyboard_data_trait$emoticon_count_sum)


describe(keyboard_data_trait$emoji_count_sum)
describe(keyboard_data_trait$emoticon_count_sum)

describe(keyboard_data_trait$emoji_word_ratio)
describe(keyboard_data_trait$emoticon_word_ratio)

describe(keyboard_data_trait$unique_emoji_count_sum)
describe(keyboard_data_trait$unique_emoticon_count_sum)

# demographics deep dive

# gender

# Outcome distributions by gender
keyboard_data_trait %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    pa_mean = mean(pa_panas, na.rm = TRUE),
    pa_sd   = sd(pa_panas, na.rm = TRUE),
    na_mean = mean(na_panas, na.rm = TRUE),
    na_sd   = sd(na_panas, na.rm = TRUE)
  )

# Emoji and emoticon use by gender
keyboard_data_trait %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(
    emoji_ratio_mean = mean(emoji_word_ratio, na.rm = TRUE),
    emoji_ratio_sd   = sd(emoji_word_ratio, na.rm = TRUE),
    emoticon_ratio_mean = mean(emoticon_word_ratio, na.rm = TRUE),
    emoticon_ratio_sd   = sd(emoticon_word_ratio, na.rm = TRUE)
  )

# age

cor(
  keyboard_data_trait$age,
  keyboard_data_trait$emoji_word_ratio,
  use = "complete.obs",
  method = "spearman"
)

cor(
  keyboard_data_trait$age,
  keyboard_data_trait$emoticon_word_ratio,
  use = "complete.obs",
  method = "spearman"
)


## state

# participants
nrow(keyboard_data_state)
length(unique(keyboard_data_state$user_id))

# sample demographics

# collapse to one row per participant
es_participants <- keyboard_data_state %>%
  distinct(user_id, gender, age)

# demographics (participant-level)
table(es_participants$gender, useNA = "ifany")

# how many participants have missing age?
sum(is.na(es_participants$age))

# age summary among those with valid data
describe(es_participants$age, na.rm = TRUE)

# outcome distribution
describe(keyboard_data_state$valence)

# emoji and emoticon use 
describe(keyboard_data_state$emoji_count_sum)
describe(keyboard_data_state$emoticon_count_sum)

# FINISH