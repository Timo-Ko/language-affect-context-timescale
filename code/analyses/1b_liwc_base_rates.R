library(dplyr)
library(tidyr)
library(stringr)
library(readr)

############################
#### SELECT RELEVANT COLS
############################

keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_final.rds")
keyboard_data_state <- as.data.frame(readRDS("data/results/keyboard_data_ema_final.rds"))


# Inspect names first if needed
# names(keyboard_data_trait)

# Keep LIWC share variables only
liwc_share_cols <- names(keyboard_data_trait) %>%
  str_subset("^liwc_") %>%
  .[!str_detect(., "_(mean|sd|min|max)$")]


feature_cols <- liwc_share_cols


############################
#### PARTICIPANT x SCOPE MEANS
############################

feature_person_scope_means <- keyboard_data_trait %>%
  filter(scope %in% c("private", "public")) %>%
  group_by(user_id, scope) %>%
  summarise(
    across(all_of(feature_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

############################
#### SUMMARISE BY SCOPE
############################

base_rate_long <- feature_person_scope_means %>%
  group_by(scope) %>%
  summarise(
    across(
      all_of(feature_cols),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -scope,
    names_to = c("feature", "stat"),
    names_pattern = "(.+)_(mean|sd)$"
  ) %>%
  pivot_wider(
    names_from = c(scope, stat),
    values_from = value,
    names_glue = "{stat}_{scope}"
  )


############################
#### FINAL TABLE
############################

# Convert LIWC share variables from proportions to percentages.
# Sentiment variables are left unchanged.
base_rate_table <- base_rate_long %>%
  mutate(
    mean_private = ifelse(feature %in% liwc_share_cols, mean_private * 100, mean_private),
    sd_private   = ifelse(feature %in% liwc_share_cols, sd_private * 100, sd_private),
    mean_public  = ifelse(feature %in% liwc_share_cols, mean_public * 100, mean_public),
    sd_public    = ifelse(feature %in% liwc_share_cols, sd_public * 100, sd_public)
  ) %>%
  mutate(
    across(
      c(mean_private, sd_private, mean_public, sd_public),
      ~ round(.x, 2)
    )
  ) %>%
  select(feature, mean_private, sd_private, mean_public, sd_public) %>%
  arrange(feature)

print(base_rate_table, n = nrow(base_rate_table))

write.csv(
  base_rate_table,
  file = "results/liwc_sentiment_base_rates_private_public_wide.csv",
  row.names = FALSE
)
# finish