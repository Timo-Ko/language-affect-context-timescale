#### APPLY EXCLUSION CRITERIA TO KEYBOARD AND AFFECT DATA ####

# load data sets
keyboard_data_trait = readRDS("data/results/keyboard_data_trait.rds")


keyboard_data_ema_centered = readRDS("data/results/keyboard_data_ema_centered.rds")
keyboard_data_ema_pre60 = readRDS("data/results/keyboard_data_ema_pre60.rds")


## fix smartphone changers

changers_map <- read.csv2("data/helper/Smartphonewechsel_20220608.csv") %>%
  rename(
    final_user_id = NewId,
    id_2 = p_0001_new,
    id_3 = p_0001_2,
    id_4 = p_0001_3
  ) %>%
  # Keep final_user_id plus all alternative IDs that belong to that person
  select(final_user_id, starts_with("id_")) %>%
  pivot_longer(
    cols = -final_user_id,
    names_to = "id_source",
    values_to = "user_id",
    values_drop_na = TRUE
  ) %>%
  # Drop cases where user_id already equals final (no-op mappings)
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
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id) %>%
  # if trait data can contain multiple rows per person after merging, collapse to 1
  distinct(user_id, .keep_all = TRUE)

# state level - ema centered 

keyboard_data_ema_centered_cleaned <- keyboard_data_ema_centered %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)

# state level - pre60 ema 

keyboard_data_ema_pre60_cleaned <- keyboard_data_ema_pre60 %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)


## run some sanity checks

# Are there any mappings where multiple old IDs point to different finals? (should be no)
changers_map %>%
  count(user_id) %>%
  filter(n > 1)

# trait: exactly one row per participant
keyboard_data_trait_cleaned %>%
  count(user_id) %>%
  filter(n > 1)

# state: row count should remain identical (IDs just reassigned)
stopifnot(nrow(keyboard_data_moment_ema_cleaned) == nrow(keyboard_data_moment_ema))

## save data files 

saveRDS(keyboard_data_trait_cleaned, "data/results/keyboard_data_trait_changers.rds") # trait

saveRDS(keyboard_data_ema_centered_cleaned, "data/results/keyboard_data_ema_centered_changers.rds") # es moment 
saveRDS(keyboard_data_ema_pre60_cleaned, "data/results/keyboard_data_ema_pre60_changers.rds") # es moment 

# finish
