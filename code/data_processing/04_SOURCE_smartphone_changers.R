#### APPLY EXCLUSION CRITERIA TO KEYBOARD AND AFFECT DATA ####

library(dplyr)
library(tidyr)

# load data sets
keyboard_data_trait <- readRDS("data/results/keyboard_data_trait.rds")
keyboard_data_day   <- readRDS("data/results/keyboard_data_day.rds")
keyboard_data_ema   <- readRDS("data/results/keyboard_data_ema.rds")

## fix smartphone changers

changers_map <- read.csv2("data/helper/Smartphonewechsel_20220608.csv") %>%
  rename(
    final_user_id = NewId,
    id_2 = p_0001_new,
    id_3 = p_0001_2,
    id_4 = p_0001_3
  ) %>%
  select(final_user_id, starts_with("id_")) %>%
  pivot_longer(
    cols = -final_user_id,
    names_to = "id_source",
    values_to = "user_id",
    values_drop_na = TRUE
  ) %>%
  filter(user_id != final_user_id) %>%
  select(user_id, final_user_id) %>%
  distinct() %>%
  mutate(
    user_id = as.character(user_id),
    final_user_id = as.character(final_user_id)
  )

## apply merging

# trait level
keyboard_data_trait_cleaned <- keyboard_data_trait %>%
  mutate(user_id = as.character(user_id)) %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)

# daily level
keyboard_data_day_cleaned <- keyboard_data_day %>%
  mutate(user_id = as.character(user_id)) %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)

# momentary level
keyboard_data_ema_cleaned <- keyboard_data_ema %>%
  mutate(user_id = as.character(user_id)) %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)

## run sanity checks

# Are there any mappings where multiple old IDs point to different finals? (should be no)
changers_map %>%
  count(user_id) %>%
  filter(n > 1)

# trait: should be at most one row per participant x scope
keyboard_data_trait_cleaned %>%
  count(user_id, scope) %>%
  filter(n > 1)

# daily: should be at most one row per participant x date x scope
keyboard_data_day_cleaned %>%
  count(user_id, date, scope) %>%
  filter(n > 1)

# momentary: row count should remain identical (IDs just reassigned)
stopifnot(nrow(keyboard_data_ema_cleaned) == nrow(keyboard_data_ema))

## optional deduplication if changer reassignment created exact duplicates

keyboard_data_trait_cleaned <- keyboard_data_trait_cleaned %>%
  distinct()

keyboard_data_day_cleaned <- keyboard_data_day_cleaned %>%
  distinct()

keyboard_data_ema_cleaned <- keyboard_data_ema_cleaned %>%
  distinct()

## save data files

saveRDS(keyboard_data_trait_cleaned, "data/results/keyboard_data_trait_changers.rds")
saveRDS(keyboard_data_day_cleaned,   "data/results/keyboard_data_day_changers.rds")
saveRDS(keyboard_data_ema_cleaned,   "data/results/keyboard_data_ema_changers.rds")

# finish