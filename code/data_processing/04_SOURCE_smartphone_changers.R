#### APPLY SMARTPHONE-CHANGER HANDLING TO KEYBOARD DATA ####

library(dplyr)
library(tidyr)

############################
#### LOAD DATA ####
############################

keyboard_data_trait <- readRDS("data/results/keyboard_data_trait.rds")
keyboard_data_day   <- readRDS("data/results/keyboard_data_day.rds")
keyboard_data_ema   <- readRDS("data/results/keyboard_data_ema.rds")

############################
#### CREATE SMARTPHONE-CHANGER MAP ####
############################

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

############################
#### SANITY CHECK CHANGER MAP ####
############################

# Each old user ID should map to only one final user ID.
duplicated_old_ids <- changers_map %>%
  count(user_id) %>%
  filter(n > 1)

duplicated_old_ids

stopifnot(nrow(duplicated_old_ids) == 0)

############################
#### APPLY ID REASSIGNMENT ####
############################

# Trait level
keyboard_data_trait_cleaned <- keyboard_data_trait %>%
  mutate(user_id = as.character(user_id)) %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)

# Daily level
keyboard_data_day_cleaned <- keyboard_data_day %>%
  mutate(user_id = as.character(user_id)) %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)

# Momentary / EMA level
keyboard_data_ema_cleaned <- keyboard_data_ema %>%
  mutate(user_id = as.character(user_id)) %>%
  left_join(changers_map, by = "user_id") %>%
  mutate(user_id = if_else(is.na(final_user_id), user_id, final_user_id)) %>%
  select(-final_user_id)

############################
#### CHECK DUPLICATES AFTER ID REASSIGNMENT ####
############################

# Trait: after ID reassignment, only user 770 should create duplicated rows.
trait_duplicates_after_mapping <- keyboard_data_trait_cleaned %>%
  count(user_id, scope) %>%
  filter(n > 1)

trait_duplicates_after_mapping

stopifnot(
  nrow(trait_duplicates_after_mapping) == 3,
  all(trait_duplicates_after_mapping$user_id == "770"),
  all(trait_duplicates_after_mapping$scope %in% c("all", "private", "public")),
  all(trait_duplicates_after_mapping$n == 2)
)

# Daily: should have no duplicated participant x date x scope rows.
day_duplicates_after_mapping <- keyboard_data_day_cleaned %>%
  count(user_id, date, scope) %>%
  filter(n > 1)

day_duplicates_after_mapping

stopifnot(nrow(day_duplicates_after_mapping) == 0)

# EMA: row count should remain identical because IDs are only reassigned.
stopifnot(nrow(keyboard_data_ema_cleaned) == nrow(keyboard_data_ema))

############################
#### COLLAPSE TRAIT DUPLICATES ####
############################

# Smartphone-change handling creates duplicated trait-level rows for user 770.
# We collapse duplicated user_id x scope rows.
# Count variables are summed.
# Stable person-level variables are filled from the non-missing row.
# Numeric feature variables that differ across duplicated rows are weighted by words_typed.

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}

weighted_mean_safe <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0
  
  if (sum(valid) == 0) {
    return(mean(x, na.rm = TRUE))
  }
  
  weighted.mean(x[valid], w[valid], na.rm = TRUE)
}

sum_vars <- c(
  "words_typed",
  "emoji_count",
  "emoticon_count",
  "n_sessions"
)

sum_vars_present <- intersect(sum_vars, names(keyboard_data_trait_cleaned))

keyboard_data_trait_cleaned <- keyboard_data_trait_cleaned %>%
  group_by(user_id, scope) %>%
  summarise(
    across(
      all_of(sum_vars_present),
      ~ sum(.x, na.rm = TRUE)
    ),
    across(
      setdiff(names(keyboard_data_trait_cleaned), c("user_id", "scope", sum_vars_present)),
      ~ {
        x <- .x
        
        if (!is.numeric(x)) {
          return(first_non_missing(x))
        }
        
        if (n_distinct(x[!is.na(x)]) <= 1) {
          return(first_non_missing(x))
        }
        
        weighted_mean_safe(x, .data$words_typed)
      }
    ),
    .groups = "drop"
  )

############################
#### FINAL SANITY CHECKS ####
############################

# Trait: should now have one row per participant x scope.
trait_duplicates_final <- keyboard_data_trait_cleaned %>%
  count(user_id, scope) %>%
  filter(n > 1)

trait_duplicates_final

stopifnot(nrow(trait_duplicates_final) == 0)

# Daily: should have one row per participant x date x scope.
day_duplicates_final <- keyboard_data_day_cleaned %>%
  count(user_id, date, scope) %>%
  filter(n > 1)

day_duplicates_final

stopifnot(nrow(day_duplicates_final) == 0)

# EMA: row count should remain identical.
stopifnot(nrow(keyboard_data_ema_cleaned) == nrow(keyboard_data_ema))

# Optional: inspect collapsed user 770.
keyboard_data_trait_cleaned %>%
  filter(user_id == "770") %>%
  select(user_id, scope, words_typed, emoji_count, emoticon_count, n_sessions, age, gender)

############################
#### SAVE DATA FILES ####
############################

saveRDS(
  keyboard_data_trait_cleaned,
  "data/results/keyboard_data_trait_changers.rds"
)

saveRDS(
  keyboard_data_day_cleaned,
  "data/results/keyboard_data_day_changers.rds"
)

saveRDS(
  keyboard_data_ema_cleaned,
  "data/results/keyboard_data_ema_changers.rds"
)

# finish