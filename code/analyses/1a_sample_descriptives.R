############################
#### PREPARATION ####
############################

packages <- c("dplyr", "tidyr", "psych", "stringr", "ggplot2")
invisible(lapply(packages, library, character.only = TRUE))

keyboard_data_trait  <- readRDS("data/results/keyboard_data_trait_final.rds")
keyboard_data_day    <- readRDS("data/results/keyboard_data_day_final.rds")
keyboard_data_moment <- as.data.frame(readRDS("data/results/keyboard_data_ema_final.rds"))

# Main paper focuses on private vs public only
keyboard_data_trait_main <- keyboard_data_trait %>%
  filter(scope %in% c("private", "public"))

keyboard_data_day_main <- keyboard_data_day %>%
  filter(scope %in% c("private", "public"))

keyboard_data_moment_main <- keyboard_data_moment %>%
  filter(scope %in% c("private", "public"))

# Supplementary benchmark: all typed text, regardless of communicative context
keyboard_data_trait_all <- keyboard_data_trait %>%
  filter(scope == "all")

keyboard_data_day_all <- keyboard_data_day %>%
  filter(scope == "all")

keyboard_data_moment_all <- keyboard_data_moment %>%
  filter(scope == "all")

############################
#### SANITY CHECKS ####
############################

table(keyboard_data_trait$scope, useNA = "ifany")
table(keyboard_data_day$scope, useNA = "ifany")
table(keyboard_data_moment$scope, useNA = "ifany")

# trait duplicates: should be one row per user x scope
trait_duplicates <- keyboard_data_trait_main %>%
  count(user_id, scope) %>%
  filter(n > 1)

if (nrow(trait_duplicates) > 0) {
  warning("Trait data contain duplicate user_id x scope rows.")
}

# daily duplicates: should be one row per user x date x scope
day_duplicates <- keyboard_data_day_main %>%
  count(user_id, date, scope) %>%
  filter(n > 1)

if (nrow(day_duplicates) > 0) {
  warning("Daily data contain duplicate user_id x date x scope rows.")
}

# momentary duplicates: should be one row per user x EMA x scope
moment_duplicates <- keyboard_data_moment_main %>%
  count(user_id, es_questionnaire_id, scope) %>%
  filter(n > 1)

if (nrow(moment_duplicates) > 0) {
  warning("Momentary data contain duplicate user_id x es_questionnaire_id x scope rows.")
}

############################
#### HELPER FUNCTIONS ####
############################

sample_desc <- function(df) {
  tibble(
    N = nrow(df),
    age_mean = mean(df$age, na.rm = TRUE),
    age_sd = sd(df$age, na.rm = TRUE),
    pct_women = mean(df$gender == 2, na.rm = TRUE) * 100
  )
}

volume_desc <- function(df, words_var = "words_typed", emoji_var = "emoji_count") {
  tibble(
    words_mean = mean(df[[words_var]], na.rm = TRUE),
    words_sd   = sd(df[[words_var]], na.rm = TRUE),
    emoji_mean = mean(df[[emoji_var]], na.rm = TRUE),
    emoji_sd   = sd(df[[emoji_var]], na.rm = TRUE)
  )
}

############################
#### TRAIT SAMPLE ####
############################

trait_scope <- keyboard_data_trait_main

private_trait_sample <- trait_scope %>%
  filter(scope == "private")

public_trait_sample <- trait_scope %>%
  filter(scope == "public")

all_trait_sample <- keyboard_data_trait_all

# direct N objects for convenience
N_private_trait <- nrow(private_trait_sample)
N_public_trait  <- nrow(public_trait_sample)
N_all_trait     <- nrow(all_trait_sample)

trait_private_desc <- sample_desc(private_trait_sample)
trait_public_desc  <- sample_desc(public_trait_sample)
trait_all_desc     <- sample_desc(all_trait_sample)

trait_private_volume <- volume_desc(private_trait_sample)
trait_public_volume  <- volume_desc(public_trait_sample)
trait_all_volume     <- volume_desc(all_trait_sample)

# Typing sessions - trait
trait_private_sessions_mean <- mean(
  private_trait_sample$n_sessions, na.rm = TRUE)
trait_private_sessions_sd   <- sd(
  private_trait_sample$n_sessions, na.rm = TRUE)

trait_public_sessions_mean  <- mean(
  public_trait_sample$n_sessions, na.rm = TRUE)
trait_public_sessions_sd    <- sd(
  public_trait_sample$n_sessions, na.rm = TRUE)

############################
#### DAILY SAMPLE ####
############################

# person x scope summary for daily data
day_scope <- keyboard_data_day_main %>%
  group_by(user_id, scope) %>%
  summarise(
    age = first(age),
    gender = first(gender),
    n_days = n(),
    words_typed = sum(words_typed, na.rm = TRUE),
    emoji_count = sum(emoji_count, na.rm = TRUE),
    mean_words_per_day = mean(words_typed, na.rm = TRUE),
    mean_emoji_per_day = mean(emoji_count, na.rm = TRUE),
    .groups = "drop"
  )

# person summary for all-text daily benchmark
day_scope_all <- keyboard_data_day_all %>%
  group_by(user_id) %>%
  summarise(
    age = first(age),
    gender = first(gender),
    n_days = n(),
    words_typed = sum(words_typed, na.rm = TRUE),
    emoji_count = sum(emoji_count, na.rm = TRUE),
    mean_words_per_day = mean(words_typed, na.rm = TRUE),
    mean_emoji_per_day = mean(emoji_count, na.rm = TRUE),
    .groups = "drop"
  )

private_day_sample <- day_scope %>%
  filter(scope == "private")

public_day_sample <- day_scope %>%
  filter(scope == "public")

all_day_sample <- day_scope_all

# direct N objects for convenience
N_private_day <- nrow(private_day_sample)
N_public_day  <- nrow(public_day_sample)
N_all_day     <- nrow(all_day_sample)

day_private_desc <- sample_desc(private_day_sample)
day_public_desc  <- sample_desc(public_day_sample)
day_all_desc     <- sample_desc(all_day_sample)

# number of retained days per participant
day_private_days_mean <- mean(private_day_sample$n_days, na.rm = TRUE)
day_private_days_sd   <- sd(private_day_sample$n_days, na.rm = TRUE)

day_public_days_mean <- mean(public_day_sample$n_days, na.rm = TRUE)
day_public_days_sd   <- sd(public_day_sample$n_days, na.rm = TRUE)

day_all_days_mean <- mean(all_day_sample$n_days, na.rm = TRUE)
day_all_days_sd   <- sd(all_day_sample$n_days, na.rm = TRUE)

# participant-level total text volumes across retained days
day_private_words_mean <- mean(private_day_sample$words_typed, na.rm = TRUE)
day_private_words_sd   <- sd(private_day_sample$words_typed, na.rm = TRUE)

day_private_emoji_mean <- mean(private_day_sample$emoji_count, na.rm = TRUE)
day_private_emoji_sd   <- sd(private_day_sample$emoji_count, na.rm = TRUE)

day_public_words_mean <- mean(public_day_sample$words_typed, na.rm = TRUE)
day_public_words_sd   <- sd(public_day_sample$words_typed, na.rm = TRUE)

day_public_emoji_mean <- mean(public_day_sample$emoji_count, na.rm = TRUE)
day_public_emoji_sd   <- sd(public_day_sample$emoji_count, na.rm = TRUE)

day_all_words_mean <- mean(all_day_sample$words_typed, na.rm = TRUE)
day_all_words_sd   <- sd(all_day_sample$words_typed, na.rm = TRUE)

day_all_emoji_mean <- mean(all_day_sample$emoji_count, na.rm = TRUE)
day_all_emoji_sd   <- sd(all_day_sample$emoji_count, na.rm = TRUE)

# participant-level mean words / emojis per retained day
day_private_words_per_day_mean <- mean(private_day_sample$mean_words_per_day, na.rm = TRUE)
day_private_words_per_day_sd   <- sd(private_day_sample$mean_words_per_day, na.rm = TRUE)

day_private_emoji_per_day_mean <- mean(private_day_sample$mean_emoji_per_day, na.rm = TRUE)
day_private_emoji_per_day_sd   <- sd(private_day_sample$mean_emoji_per_day, na.rm = TRUE)

day_public_words_per_day_mean <- mean(public_day_sample$mean_words_per_day, na.rm = TRUE)
day_public_words_per_day_sd   <- sd(public_day_sample$mean_words_per_day, na.rm = TRUE)

day_public_emoji_per_day_mean <- mean(public_day_sample$mean_emoji_per_day, na.rm = TRUE)
day_public_emoji_per_day_sd   <- sd(public_day_sample$mean_emoji_per_day, na.rm = TRUE)

day_all_words_per_day_mean <- mean(all_day_sample$mean_words_per_day, na.rm = TRUE)
day_all_words_per_day_sd   <- sd(all_day_sample$mean_words_per_day, na.rm = TRUE)

day_all_emoji_per_day_mean <- mean(all_day_sample$mean_emoji_per_day, na.rm = TRUE)
day_all_emoji_per_day_sd   <- sd(all_day_sample$mean_emoji_per_day, na.rm = TRUE)

# Typing sessions per day - daily
day_private_sessions_per_day_mean <- keyboard_data_day_main %>%
  filter(scope == "private") %>%
  summarise(m = mean(n_sessions, na.rm = TRUE)) %>%
  pull(m)

day_private_sessions_per_day_sd <- keyboard_data_day_main %>%
  filter(scope == "private") %>%
  summarise(s = sd(n_sessions, na.rm = TRUE)) %>%
  pull(s)

day_public_sessions_per_day_mean <- keyboard_data_day_main %>%
  filter(scope == "public") %>%
  summarise(m = mean(n_sessions, na.rm = TRUE)) %>%
  pull(m)

day_public_sessions_per_day_sd <- keyboard_data_day_main %>%
  filter(scope == "public") %>%
  summarise(s = sd(n_sessions, na.rm = TRUE)) %>%
  pull(s)


# day-level row counts
N_day_pairs_total   <- nrow(keyboard_data_day_main)
N_private_day_total <- sum(keyboard_data_day_main$scope == "private", na.rm = TRUE)
N_public_day_total  <- sum(keyboard_data_day_main$scope == "public", na.rm = TRUE)
N_all_day_total     <- nrow(keyboard_data_day_all)

# optional: number of EMAs per retained day
day_private_n_ema_mean <- keyboard_data_day_main %>%
  filter(scope == "private") %>%
  summarise(m = mean(n_ema_day, na.rm = TRUE)) %>%
  pull(m)

day_private_n_ema_sd <- keyboard_data_day_main %>%
  filter(scope == "private") %>%
  summarise(s = sd(n_ema_day, na.rm = TRUE)) %>%
  pull(s)

day_public_n_ema_mean <- keyboard_data_day_main %>%
  filter(scope == "public") %>%
  summarise(m = mean(n_ema_day, na.rm = TRUE)) %>%
  pull(m)

day_public_n_ema_sd <- keyboard_data_day_main %>%
  filter(scope == "public") %>%
  summarise(s = sd(n_ema_day, na.rm = TRUE)) %>%
  pull(s)

day_all_n_ema_mean <- keyboard_data_day_all %>%
  summarise(m = mean(n_ema_day, na.rm = TRUE)) %>%
  pull(m)

day_all_n_ema_sd <- keyboard_data_day_all %>%
  summarise(s = sd(n_ema_day, na.rm = TRUE)) %>%
  pull(s)

############################
#### MOMENTARY / EMA SAMPLE ####
############################

moment_scope <- keyboard_data_moment_main %>%
  group_by(user_id, scope) %>%
  summarise(
    age = first(age),
    gender = first(gender),
    n_ema_windows = n(),
    words_typed = sum(words_typed, na.rm = TRUE),
    emoji_count = sum(emoji_count, na.rm = TRUE),
    mean_words_per_window = mean(words_typed, na.rm = TRUE),
    mean_emoji_per_window = mean(emoji_count, na.rm = TRUE),
    .groups = "drop"
  )

# person summary for all-text momentary benchmark
moment_scope_all <- keyboard_data_moment_all %>%
  group_by(user_id) %>%
  summarise(
    age = first(age),
    gender = first(gender),
    n_ema_windows = n(),
    words_typed = sum(words_typed, na.rm = TRUE),
    emoji_count = sum(emoji_count, na.rm = TRUE),
    mean_words_per_window = mean(words_typed, na.rm = TRUE),
    mean_emoji_per_window = mean(emoji_count, na.rm = TRUE),
    .groups = "drop"
  )

private_moment_sample <- moment_scope %>%
  filter(scope == "private")

public_moment_sample <- moment_scope %>%
  filter(scope == "public")

all_moment_sample <- moment_scope_all

# direct N objects for convenience
N_private_moment <- nrow(private_moment_sample)
N_public_moment  <- nrow(public_moment_sample)
N_all_moment     <- nrow(all_moment_sample)

moment_private_desc <- sample_desc(private_moment_sample)
moment_public_desc  <- sample_desc(public_moment_sample)
moment_all_desc     <- sample_desc(all_moment_sample)

# total number of momentary rows/windows
N_moment_pairs_total   <- nrow(keyboard_data_moment_main)
N_private_moment_total <- sum(keyboard_data_moment_main$scope == "private", na.rm = TRUE)
N_public_moment_total  <- sum(keyboard_data_moment_main$scope == "public", na.rm = TRUE)
N_all_moment_total     <- nrow(keyboard_data_moment_all)

# participant-level averages of retained EMA windows
moment_private_windows_mean <- mean(private_moment_sample$n_ema_windows, na.rm = TRUE)
moment_private_windows_sd   <- sd(private_moment_sample$n_ema_windows, na.rm = TRUE)

moment_public_windows_mean <- mean(public_moment_sample$n_ema_windows, na.rm = TRUE)
moment_public_windows_sd   <- sd(public_moment_sample$n_ema_windows, na.rm = TRUE)

moment_all_windows_mean <- mean(all_moment_sample$n_ema_windows, na.rm = TRUE)
moment_all_windows_sd   <- sd(all_moment_sample$n_ema_windows, na.rm = TRUE)

# participant-level total text volumes across all retained EMA windows
moment_private_words_mean <- mean(private_moment_sample$words_typed, na.rm = TRUE)
moment_private_words_sd   <- sd(private_moment_sample$words_typed, na.rm = TRUE)

moment_private_emoji_mean <- mean(private_moment_sample$emoji_count, na.rm = TRUE)
moment_private_emoji_sd   <- sd(private_moment_sample$emoji_count, na.rm = TRUE)

moment_public_words_mean <- mean(public_moment_sample$words_typed, na.rm = TRUE)
moment_public_words_sd   <- sd(public_moment_sample$words_typed, na.rm = TRUE)

moment_public_emoji_mean <- mean(public_moment_sample$emoji_count, na.rm = TRUE)
moment_public_emoji_sd   <- sd(public_moment_sample$emoji_count, na.rm = TRUE)

moment_all_words_mean <- mean(all_moment_sample$words_typed, na.rm = TRUE)
moment_all_words_sd   <- sd(all_moment_sample$words_typed, na.rm = TRUE)

moment_all_emoji_mean <- mean(all_moment_sample$emoji_count, na.rm = TRUE)
moment_all_emoji_sd   <- sd(all_moment_sample$emoji_count, na.rm = TRUE)

# average per EMA window (participant-level means)
moment_private_words_per_window_mean <- mean(private_moment_sample$mean_words_per_window, na.rm = TRUE)
moment_private_words_per_window_sd   <- sd(private_moment_sample$mean_words_per_window, na.rm = TRUE)

moment_private_emoji_per_window_mean <- mean(private_moment_sample$mean_emoji_per_window, na.rm = TRUE)
moment_private_emoji_per_window_sd   <- sd(private_moment_sample$mean_emoji_per_window, na.rm = TRUE)

moment_public_words_per_window_mean <- mean(public_moment_sample$mean_words_per_window, na.rm = TRUE)
moment_public_words_per_window_sd   <- sd(public_moment_sample$mean_words_per_window, na.rm = TRUE)

moment_public_emoji_per_window_mean <- mean(public_moment_sample$mean_emoji_per_window, na.rm = TRUE)
moment_public_emoji_per_window_sd   <- sd(public_moment_sample$mean_emoji_per_window, na.rm = TRUE)

moment_all_words_per_window_mean <- mean(all_moment_sample$mean_words_per_window, na.rm = TRUE)
moment_all_words_per_window_sd   <- sd(all_moment_sample$mean_words_per_window, na.rm = TRUE)

moment_all_emoji_per_window_mean <- mean(all_moment_sample$mean_emoji_per_window, na.rm = TRUE)
moment_all_emoji_per_window_sd   <- sd(all_moment_sample$mean_emoji_per_window, na.rm = TRUE)

# Typing sessions per window - momentary
moment_private_sessions_per_window_mean <- keyboard_data_moment_main %>%
  filter(scope == "private") %>%
  summarise(m = mean(n_sessions, na.rm = TRUE)) %>%
  pull(m)

moment_private_sessions_per_window_sd <- keyboard_data_moment_main %>%
  filter(scope == "private") %>%
  summarise(s = sd(n_sessions, na.rm = TRUE)) %>%
  pull(s)

moment_public_sessions_per_window_mean <- keyboard_data_moment_main %>%
  filter(scope == "public") %>%
  summarise(m = mean(n_sessions, na.rm = TRUE)) %>%
  pull(m)

moment_public_sessions_per_window_sd <- keyboard_data_moment_main %>%
  filter(scope == "public") %>%
  summarise(s = sd(n_sessions, na.rm = TRUE)) %>%
  pull(s)

############################
#### FIGURE: BEHAVIORAL VOLUME ####
############################

library(ggplot2)
library(dplyr)

# set colors
oi_private <- "#E69F00"
oi_public  <- "#56B4E9"

# Build long-format dataframe covering all three metrics
volume_plot_df <- bind_rows(
  
  # Words
  data.frame(
    metric    = "Words typed",
    timescale = factor(c("Trait", "Trait", "Daily", "Daily", "Momentary", "Momentary"),
                       levels = c("Trait", "Daily", "Momentary")),
    context   = factor(rep(c("Private", "Public"), 3),
                       levels = c("Private", "Public")),
    mean_val  = c(
      trait_private_volume$words_mean,
      trait_public_volume$words_mean,
      day_private_words_per_day_mean,
      day_public_words_per_day_mean,
      moment_private_words_per_window_mean,
      moment_public_words_per_window_mean
    ),
    sd_val = c(
      trait_private_volume$words_sd,
      trait_public_volume$words_sd,
      day_private_words_per_day_sd,
      day_public_words_per_day_sd,
      moment_private_words_per_window_sd,
      moment_public_words_per_window_sd
    ),
    n = c(
      N_private_trait, N_public_trait,
      N_private_day,   N_public_day,
      N_private_moment, N_public_moment
    )
  ),
  
  # Emojis
  data.frame(
    metric    = "Emojis used",
    timescale = factor(c("Trait", "Trait", "Daily", "Daily", "Momentary", "Momentary"),
                       levels = c("Trait", "Daily", "Momentary")),
    context   = factor(rep(c("Private", "Public"), 3),
                       levels = c("Private", "Public")),
    mean_val  = c(
      trait_private_volume$emoji_mean,
      trait_public_volume$emoji_mean,
      day_private_emoji_per_day_mean,
      day_public_emoji_per_day_mean,
      moment_private_emoji_per_window_mean,
      moment_public_emoji_per_window_mean
    ),
    sd_val = c(
      trait_private_volume$emoji_sd,
      trait_public_volume$emoji_sd,
      day_private_emoji_per_day_sd,
      day_public_emoji_per_day_sd,
      moment_private_emoji_per_window_sd,
      moment_public_emoji_per_window_sd
    ),
    n = c(
      N_private_trait, N_public_trait,
      N_private_day,   N_public_day,
      N_private_moment, N_public_moment
    )
  ),
  
  # Typing sessions
  data.frame(
    metric    = "Typing sessions",
    timescale = factor(c("Trait", "Trait", "Daily", "Daily", "Momentary", "Momentary"),
                       levels = c("Trait", "Daily", "Momentary")),
    context   = factor(rep(c("Private", "Public"), 3),
                       levels = c("Private", "Public")),
    mean_val  = c(
      trait_private_sessions_mean,
      trait_public_sessions_mean,
      day_private_sessions_per_day_mean,
      day_public_sessions_per_day_mean,
      moment_private_sessions_per_window_mean,
      moment_public_sessions_per_window_mean
    ),
    sd_val = c(
      trait_private_sessions_sd,
      trait_public_sessions_sd,
      day_private_sessions_per_day_sd,
      day_public_sessions_per_day_sd,
      moment_private_sessions_per_window_sd,
      moment_public_sessions_per_window_sd
    ),
    n = c(
      N_private_trait, N_public_trait,
      N_private_day,   N_public_day,
      N_private_moment, N_public_moment
    )
  )
  
) %>%
  mutate(
    metric = factor(metric, levels = c("Words typed", "Emojis used", "Typing sessions")),
    se_val = sd_val / sqrt(n)
  )

# Plot
fig_volume <- ggplot(
  volume_plot_df,
  aes(x = timescale, y = mean_val, fill = context)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_errorbar(
    aes(
      ymin = mean_val - se_val,
      ymax = mean_val + se_val
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
  "figures/fig2_volume_private_public.png",
  plot  = fig_volume,
  width = 9,
  height = 4,
  dpi   = 300
)

# finish