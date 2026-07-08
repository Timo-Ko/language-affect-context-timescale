############################
#### PREPARATION ####
############################

packages <- c("dplyr", "tidyr", "psych", "stringr", "ggplot2")
invisible(lapply(packages, library, character.only = TRUE))

keyboard_data_trait  <- readRDS("data/results/keyboard_data_trait_final.rds")
keyboard_data_day    <- readRDS("data/results/keyboard_data_day_final.rds")
keyboard_data_moment <- as.data.frame(readRDS("data/results/keyboard_data_ema_final.rds"))

############################
#### CORRELATIONS OF TRAIT AND STATE AFFECT OUTCOMES
############################

ema_data <- readRDS("data/ema/ema_data.rds") %>%
  as.data.frame() %>%
  mutate(
    user_id = as.character(user_id)
  )

analytic_users <- keyboard_data_trait %>%
  filter(
    scope %in% c(
      "private",
      "public"
    ),
    words_typed >= 100
  ) %>%
  distinct(
    user_id
  )


trait_affect_final <- keyboard_data_trait %>%
  semi_join(
    analytic_users,
    by = "user_id"
  ) %>%
  group_by(
    user_id
  ) %>%
  summarise(
    pa_panas = first(
      pa_panas[
        !is.na(pa_panas)
      ]
    ),
    na_panas = first(
      na_panas[
        !is.na(na_panas)
      ]
    ),
    .groups = "drop"
  )


ema_trait_affect_correlations_final <- ema_data %>%
  group_by(
    user_id
  ) %>%
  summarise(
    valence_median = median(
      valence,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(
    is.finite(
      valence_median
    )
  ) %>%
  inner_join(
    trait_affect_final,
    by = "user_id"
  ) %>%
  filter(
    !is.na(pa_panas),
    !is.na(na_panas)
  )


n_distinct(
  ema_trait_affect_correlations_final$user_id
)


cor.test(
  ema_trait_affect_correlations_final$valence_median,
  ema_trait_affect_correlations_final$pa_panas,
  method = "pearson"
)


cor.test(
  ema_trait_affect_correlations_final$valence_median,
  ema_trait_affect_correlations_final$na_panas,
  method = "pearson"
)

############################
#### SHARE OF TEXT PRODUCED IN EACH COMMUNICATION CONTEXT ####
############################

# Computes within-person shares of typed words occurring in private and public communication

# Trait-level share across the full study period
text_share_trait <- keyboard_data_trait %>%
  filter(scope %in% c("private", "public", "all")) %>%
  select(user_id, scope, words_typed) %>%
  pivot_wider(
    names_from = scope,
    values_from = words_typed
  ) %>%
  mutate(
    private_share_of_all = private / all,
    public_share_of_all  = public / all,
    private_public_share_sum = private_share_of_all + public_share_of_all
  )

# Descriptive summary
text_share_trait_summary <- text_share_trait %>%
  summarise(
    n_participants = sum(!is.na(private_share_of_all) | !is.na(public_share_of_all)),
    
    private_share_mean = mean(private_share_of_all, na.rm = TRUE),
    private_share_sd   = sd(private_share_of_all, na.rm = TRUE),
    private_share_median = median(private_share_of_all, na.rm = TRUE),
    
    public_share_mean = mean(public_share_of_all, na.rm = TRUE),
    public_share_sd   = sd(public_share_of_all, na.rm = TRUE),
    public_share_median = median(public_share_of_all, na.rm = TRUE),
    
    private_public_share_sum_mean = mean(private_public_share_sum, na.rm = TRUE),
    private_public_share_sum_sd   = sd(private_public_share_sum, na.rm = TRUE)
  ) %>%
  mutate(
    across(
      contains("share"),
      ~ .x * 100
    )
  )

# same calculation restricted to participants with both private and public data
text_share_trait_both_contexts <- text_share_trait %>%
  filter(!is.na(private), !is.na(public), !is.na(all))

text_share_trait_both_contexts_summary <- text_share_trait_both_contexts %>%
  summarise(
    n_participants = n(),
    
    private_share_mean = mean(private_share_of_all, na.rm = TRUE),
    private_share_sd   = sd(private_share_of_all, na.rm = TRUE),
    private_share_median = median(private_share_of_all, na.rm = TRUE),
    
    public_share_mean = mean(public_share_of_all, na.rm = TRUE),
    public_share_sd   = sd(public_share_of_all, na.rm = TRUE),
    public_share_median = median(public_share_of_all, na.rm = TRUE),
    
    private_public_share_sum_mean = mean(private_public_share_sum, na.rm = TRUE),
    private_public_share_sum_sd   = sd(private_public_share_sum, na.rm = TRUE)
  ) %>%
  mutate(
    across(
      contains("share"),
      ~ .x * 100
    )
  )

write.csv(
  text_share_trait_summary,
  file = "results/text_share_trait_summary.csv",
  row.names = FALSE
)

write.csv(
  text_share_trait_both_contexts_summary,
  file = "results/text_share_trait_both_contexts_summary.csv",
  row.names = FALSE
)

text_share_trait_summary
text_share_trait_both_contexts_summary

### FILTER FOR PRIVATE AND PUBLIC CONTEXT ONLY

keyboard_data_trait_main <- keyboard_data_trait %>%
  filter(scope %in% c("private", "public"))

keyboard_data_day_main <- keyboard_data_day %>%
  filter(scope %in% c("private", "public"))

keyboard_data_moment_main <- keyboard_data_moment %>%
  filter(scope %in% c("private", "public"))

# total number of participants in final sample
length(unique(keyboard_data_trait_main$user_id))

# total volume of words typed in final sample
sum(keyboard_data_trait_main$words_typed)

############################
#### TABLE S1: SAMPLE DESCRIPTIVES ####
############################

# This table summarizes the samples used for the main private/public analyses.
# Trait rows are participant-level rows.
# Daily rows are participant-day rows.
# Momentary rows are EMA-window rows.

# Helper: format mean (SD)
fmt_mean_sd <- function(x, digits = 2) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  
  if (is.nan(m) || is.nan(s)) {
    return(NA_character_)
  }
  
  paste0(
    formatC(m, format = "f", digits = digits),
    " (",
    formatC(s, format = "f", digits = digits),
    ")"
  )
}

# Helper: format percentage
fmt_pct <- function(x, digits = 2) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }
  
  pct <- mean(x, na.rm = TRUE) * 100
  
  formatC(pct, format = "f", digits = digits)
}

# Helper: participant-level descriptives
participant_desc <- function(df, id_var = "user_id") {
  
  df_person <- df %>%
    distinct(.data[[id_var]], .keep_all = TRUE)
  
  tibble(
    `N (participants)` = n_distinct(df[[id_var]]),
    `Age, mean (SD)` = fmt_mean_sd(df_person$age),
    `Female (%)` = fmt_pct(df_person$gender == 2)
  )
}

# Helper: analytic-unit-level descriptives
unit_desc <- function(df,
                      words_var = "words_typed",
                      emoji_var = "emoji_count",
                      sessions_var = "n_sessions") {
  
  out <- tibble(
    `N (instances)` = nrow(df),
    `Words typed, mean (SD)` = fmt_mean_sd(df[[words_var]]),
    `Emojis used, mean (SD)` = fmt_mean_sd(df[[emoji_var]])
  )
  
  if (sessions_var %in% names(df)) {
    out <- out %>%
      mutate(`Typing sessions, mean (SD)` = fmt_mean_sd(df[[sessions_var]]))
  }
  
  out
}

# Helper: create one Table S1 column for one dataset/context
make_s1_column <- function(df) {
  bind_cols(
    participant_desc(df),
    unit_desc(df)
  ) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "row",
      values_to = "value"
    )
}

############################
#### CREATE TABLE S1 COLUMNS ####
############################

s1_trait_private <- keyboard_data_trait_main %>%
  filter(scope == "private") %>%
  make_s1_column()

s1_trait_public <- keyboard_data_trait_main %>%
  filter(scope == "public") %>%
  make_s1_column()

s1_daily_private <- keyboard_data_day_main %>%
  filter(scope == "private") %>%
  make_s1_column()

s1_daily_public <- keyboard_data_day_main %>%
  filter(scope == "public") %>%
  make_s1_column()

s1_momentary_private <- keyboard_data_moment_main %>%
  filter(scope == "private") %>%
  make_s1_column()

s1_momentary_public <- keyboard_data_moment_main %>%
  filter(scope == "public") %>%
  make_s1_column()

############################
#### COMBINE TABLE S1 ####
############################

table_s1 <- s1_trait_private %>%
  rename(Trait_Private = value) %>%
  left_join(
    s1_trait_public %>% rename(Trait_Public = value),
    by = "row"
  ) %>%
  left_join(
    s1_daily_private %>% rename(Daily_Private = value),
    by = "row"
  ) %>%
  left_join(
    s1_daily_public %>% rename(Daily_Public = value),
    by = "row"
  ) %>%
  left_join(
    s1_momentary_private %>% rename(Momentary_Private = value),
    by = "row"
  ) %>%
  left_join(
    s1_momentary_public %>% rename(Momentary_Public = value),
    by = "row"
  )

# Enforce row order
table_s1 <- table_s1 %>%
  mutate(
    row = factor(
      row,
      levels = c(
        "N (participants)",
        "N (instances)",
        "Age, mean (SD)",
        "Female (%)",
        "Typing sessions, mean (SD)",
        "Words typed, mean (SD)",
        "Emojis used, mean (SD)"
      )
    )
  ) %>%
  arrange(row) %>%
  mutate(row = as.character(row))

############################
#### SAVE TABLE S1 ####
############################

write.csv(
  table_s1,
  file = "results/table_s1_sample_descriptives.csv",
  row.names = FALSE,
  na = ""
)

table_s1


############################
#### FIGURE: BEHAVIORAL VOLUME ####
############################

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

dir.create("figures", showWarnings = FALSE)

# Set colors
oi_private <- "#E69F00"
oi_public  <- "#56B4E9"

# Helper for one dataset
make_volume_plot_data <- function(df, timescale_label) {
  df %>%
    filter(scope %in% c("private", "public")) %>%
    mutate(
      context = recode(scope, private = "Private", public = "Public"),
      timescale = timescale_label
    ) %>%
    group_by(timescale, context) %>%
    summarise(
      words_mean  = mean(words_typed, na.rm = TRUE),
      words_sd    = sd(words_typed, na.rm = TRUE),
      emojis_mean = mean(emoji_count, na.rm = TRUE),
      emojis_sd   = sd(emoji_count, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

# Trait: person-level totals
volume_trait <- make_volume_plot_data(
  keyboard_data_trait_main,
  "Trait"
)

# Daily: participant-day-level counts
volume_daily <- make_volume_plot_data(
  keyboard_data_day_main,
  "Daily"
)

# Momentary: EMA-window-level counts
volume_momentary <- make_volume_plot_data(
  keyboard_data_moment_main,
  "Momentary"
)

# Combine and reshape
volume_plot_df <- bind_rows(
  volume_trait,
  volume_daily,
  volume_momentary
) %>%
  mutate(
    timescale = factor(timescale, levels = c("Trait", "Daily", "Momentary")),
    context = factor(context, levels = c("Private", "Public"))
  ) %>%
  pivot_longer(
    cols = c(
      words_mean, words_sd,
      emojis_mean, emojis_sd
    ),
    names_to = c("metric_raw", ".value"),
    names_pattern = "(words|emojis)_(mean|sd)"
  ) %>%
  mutate(
    metric = recode(
      metric_raw,
      words = "Words typed",
      emojis = "Emojis used"
    ),
    metric = factor(
      metric,
      levels = c("Words typed", "Emojis used")
    ),
    se = sd / sqrt(n)
  )

# Plot
fig_volume <- ggplot(
  volume_plot_df,
  aes(x = timescale, y = mean, fill = context)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(mean - se, 0.001),
      ymax = mean + se
    ),
    position = position_dodge(width = 0.7),
    width = 0.2,
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c("Private" = oi_private, "Public" = oi_public),
    name = "Context"
  ) +
  scale_y_log10(
    labels = scales::comma,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = "Mean count (log scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    strip.text         = element_text(size = 11, face = "bold"),
    axis.title.y       = element_text(size = 10),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 9),
    panel.spacing      = unit(1.2, "lines")
  )

fig_volume

ggsave(
  "figures/figure2_volume_private_public.png",
  plot  = fig_volume,
  width = 7,
  height = 4,
  dpi   = 300
)

# finish
