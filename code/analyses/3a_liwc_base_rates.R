############################
#### LIWC BASE RATES: ALL LIWC CATEGORIES + TABLE S6
############################

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

############################
#### LOAD DATA
############################

keyboard_data_trait  <- readRDS("data/results/keyboard_data_trait_final.rds")
keyboard_data_day    <- readRDS("data/results/keyboard_data_day_final.rds")
keyboard_data_moment <- as.data.frame(readRDS("data/results/keyboard_data_ema_final.rds"))

############################
#### SELECT RELEVANT LIWC COLS
############################

# Keep LIWC share variables only.
# Excludes session-level descriptive variants such as _mean, _sd, _min, _max.
liwc_share_cols <- names(keyboard_data_trait) %>%
  str_subset("^liwc_") %>%
  .[!str_detect(., "_(mean|sd|min|max)$")]

feature_cols <- liwc_share_cols

############################
#### ALL LIWC BASE RATES FOR REPOSITORY
############################

# Participant x scope means for trait-level data
feature_person_scope_means <- keyboard_data_trait %>%
  filter(scope %in% c("private", "public")) %>%
  group_by(user_id, scope) %>%
  summarise(
    across(all_of(feature_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Summarise by communication context
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

# Convert LIWC share variables from proportions to percentages.
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

############################
#### TABLE S6: THEORY-GUIDED LIWC BASE RATES
############################

# Adjust names here only if your actual LIWC variable names differ.
theory_liwc_vars <- c(
  pos_emotion = "liwc_posemo",
  neg_emotion = "liwc_negemo",
  i           = "liwc_i",
  we          = "liwc_we"
)

required_vars <- unname(theory_liwc_vars)

missing_trait  <- setdiff(required_vars, names(keyboard_data_trait))
missing_day    <- setdiff(required_vars, names(keyboard_data_day))
missing_moment <- setdiff(required_vars, names(keyboard_data_moment))

if (length(missing_trait) > 0) {
  stop("Missing LIWC variables in trait data: ", paste(missing_trait, collapse = ", "))
}

if (length(missing_day) > 0) {
  stop("Missing LIWC variables in daily data: ", paste(missing_day, collapse = ", "))
}

if (length(missing_moment) > 0) {
  stop("Missing LIWC variables in momentary data: ", paste(missing_moment, collapse = ", "))
}

############################
#### HELPER FUNCTIONS FOR TABLE S6
############################

fmt_mean_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  paste0(
    formatC(mean(x), format = "f", digits = digits),
    " (",
    formatC(sd(x), format = "f", digits = digits),
    ")"
  )
}

make_s6_dataset <- function(df, timescale_label) {
  df %>%
    filter(scope %in% c("private", "public")) %>%
    mutate(
      Timescale = timescale_label,
      Context = recode(scope, private = "Private", public = "Public")
    ) %>%
    group_by(Timescale, Context) %>%
    summarise(
      `Pos. emotion M (SD)` = fmt_mean_sd(.data[[theory_liwc_vars["pos_emotion"]]] * 100),
      `Neg. emotion M (SD)` = fmt_mean_sd(.data[[theory_liwc_vars["neg_emotion"]]] * 100),
      `1st. Person sing. M (SD)` = fmt_mean_sd(.data[[theory_liwc_vars["i"]]] * 100),
      `1st Person plur. M (SD)` = fmt_mean_sd(.data[[theory_liwc_vars["we"]]] * 100),
      .groups = "drop"
    )
}

############################
#### CREATE TABLE S6
############################

table_s5 <- bind_rows(
  make_s6_dataset(keyboard_data_trait,  "Trait"),
  make_s6_dataset(keyboard_data_day,    "Daily"),
  make_s6_dataset(keyboard_data_moment, "Momentary")
) %>%
  mutate(
    Timescale = factor(Timescale, levels = c("Trait", "Daily", "Momentary")),
    Context = factor(Context, levels = c("Private", "Public"))
  ) %>%
  arrange(Timescale, Context) %>%
  mutate(
    Timescale = as.character(Timescale),
    Context = as.character(Context)
  )

############################
#### SAVE TABLE S6
############################

write.csv(
  table_s5,
  file = "results/table_s5_theory_guided_liwc_base_rates.csv",
  row.names = FALSE,
  na = ""
)

table_s5

# finish