### PREPARATION ####

## Install and load required packages 

packages <- c("dplyr", "tidyr", "data.table", "psych","ggplot2", "ggcorrplot", "stringr", "ggrepel", "ragg", "systemfonts", "patchwork", "readr", "gridExtra")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

## load function
source("code/analyses/helper/compute_corrs.R") # function to filter correlations

## load data 

# trait emotion
keyboard_data_trait <- readRDS(file="data/results/keyboard_data_trait_final.rds")

# state emotion
#keyboard_data_ema_centered <- as.data.frame(readRDS(file="data/results/keyboard_data_ema_centered_final.rds"))
keyboard_data_ema_pre180 <- as.data.frame(readRDS(file="data/results/keyboard_data_ema_pre180_final.rds"))
keyboard_data_ema_pre60 <- as.data.frame(readRDS(file="data/results/keyboard_data_ema_pre60_final.rds"))

# add new diversity measure

keyboard_data_trait <- keyboard_data_trait %>%
  mutate(emoji_div = if_else(emoji_count_sum > 0,
                             unique_emoji_count_sum / emoji_count_sum,
                             NA_real_))

keyboard_data_ema_pre180 <- keyboard_data_ema_pre180 %>%
  mutate(emoji_div = if_else(emoji_count_sum > 0,
                             unique_emoji_count_sum / emoji_count_sum,
                             NA_real_))

# read in helper dfs
emoji_df <- readRDS("data/helper/emoji_df.rds")
emoticons_df <- readRDS("data/helper/emoticons_df.rds")

#### COMPUTE CORRELATIONS OF EMOJI + EMOTICON USE AND AFFECT SELF-REPORTS ####

# get vector with feature names 
features <- names(keyboard_data_trait)[grepl("emoji|emoticon", names(keyboard_data_trait), ignore.case = TRUE)]

# drop unneeded features
features <- features[!features %in% c(
  "emoticon_count_sum",
  "unique_emoticon_count_sum",
  "emoji_count_sum",
  "unique_emoji_count_sum",
  "senti_emoji_match_rate"
)]

metrics <- c("emoji_word_ratio", "emoji_div", "senti_emoji_avg")
symbols <- features[6:112]

# compute corrs

# trait (overall)
cor_trait <- cor_table(
  data_frame = keyboard_data_trait,
  targets    = c("pa_panas", "na_panas"),
  features   = metrics
) %>%
  mutate(group = "all", level = "trait")

# state (overall; participant-blocked bootstrap)

cor_state <- cor_table_cluster(
  data_frame = keyboard_data_ema_pre180,
  targets    = c("valence"),
  features   = metrics,
  id_var     = "user_id",
  R          = 1000,
  seed       = 1
) %>%
  mutate(group = "all", level = "state")

# cor_state_pre60 <- cor_table_cluster(
#   data_frame = keyboard_data_ema_pre60,
#   targets    = c("valence"),
#   features   = metrics,
#   id_var     = "user_id",
#   R          = 1000,
#   seed       = 1
# ) %>%
#   mutate(group = "all", level = "state")


## Gender-sensitivity analyses

# trait by gender
cor_trait_by_gender <- cor_table_by_gender(
  data_frame = keyboard_data_trait,
  targets    = c("pa_panas", "na_panas"),
  features   = metrics,
  gender_var = "gender",
  R          = 1000,
  seed       = 1
) %>%
  filter(gender %in% c("1", "2")) %>%
  mutate(
    group = case_when(
      gender == "1" ~ "men",
      gender == "2" ~ "women"
    ),
    level = "trait"
  )

# state by gender (keep participant-blocked bootstrapping within gender)
cor_state_by_gender <- bind_rows(
  cor_table_cluster(
    data_frame = keyboard_data_ema_pre180 %>% filter(gender == "1"),
    targets    = c("valence"),
    features   = metrics,
    id_var     = "user_id",
    R          = 1000,
    seed       = 1
  ) %>% mutate(group = "men", level = "state"),
  
  cor_table_cluster(
    data_frame = keyboard_data_ema_pre180 %>% filter(gender == "2"),
    targets    = c("valence"),
    features   = metrics,
    id_var     = "user_id",
    R          = 1000,
    seed       = 1
  ) %>% mutate(group = "women", level = "state")
)


cor_overview <- dplyr::bind_rows(
  cor_trait,
  cor_trait_by_gender %>%
    dplyr::select(-c(gender, n_gender_total)) %>%
    dplyr::select(dplyr::all_of(names(cor_trait))),
  cor_state,
  cor_state_by_gender %>%
    dplyr::select(dplyr::all_of(names(cor_trait)))
) %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(.x, 3))
  )

# save as table
write.csv(cor_overview, "results/emoji_metrics_cor_table.csv")


### state within person modeling

library(dplyr)
library(lme4)
library(tibble)

# 1) within-person centered predictors
keyboard_data_state_within <- keyboard_data_ema_pre180 %>%
  group_by(user_id) %>%
  mutate(
    emoji_vol_mean  = mean(emoji_word_ratio, na.rm = TRUE),
    emoji_vol_wp    = emoji_word_ratio - emoji_vol_mean,
    emoji_div_mean  = mean(emoji_div, na.rm = TRUE),
    emoji_div_wp    = emoji_div - emoji_div_mean,
    emoji_sent_mean = mean(senti_emoji_avg, na.rm = TRUE),
    emoji_sent_wp   = senti_emoji_avg - emoji_sent_mean
  ) %>%
  ungroup()

# 2) standardize outcome + predictors (makes coefficients comparable)
keyboard_data_state_within <- keyboard_data_state_within %>%
  mutate(
    valence_z        = as.numeric(scale(valence)),
    emoji_vol_wp_z   = as.numeric(scale(emoji_vol_wp)),
    emoji_vol_mean_z = as.numeric(scale(emoji_vol_mean)),
    emoji_div_wp_z   = as.numeric(scale(emoji_div_wp)),
    emoji_div_mean_z = as.numeric(scale(emoji_div_mean)),
    emoji_sent_wp_z  = as.numeric(scale(emoji_sent_wp)),
    emoji_sent_mean_z= as.numeric(scale(emoji_sent_mean))
  )

# 3) fit standardized multilevel model
m_z <- lmer(
  valence_z ~ emoji_vol_wp_z + emoji_vol_mean_z +
    emoji_div_wp_z + emoji_div_mean_z +
    emoji_sent_wp_z + emoji_sent_mean_z +
    (1 | user_id),
  data = keyboard_data_state_within
)

# 4) bootstrap CIs for fixed effects (keep only the WP terms for reporting)
wp_terms <- c("emoji_vol_wp_z", "emoji_div_wp_z", "emoji_sent_wp_z")

ci_boot <- confint(
  m_z,
  method = "boot",
  nsim = 1000,
  parm = wp_terms
)

fe <- fixef(m_z)

within_person_overview <- tibble(
  term     = wp_terms,
  estimate = unname(fe[wp_terms]),
  ci_lower = ci_boot[wp_terms, 1],
  ci_upper = ci_boot[wp_terms, 2]
) %>%
  mutate(
    metric = case_when(
      term == "emoji_vol_wp_z"  ~ "Emoji volume",
      term == "emoji_div_wp_z"  ~ "Emoji diversity",
      term == "emoji_sent_wp_z" ~ "Emoji sentiment",
      TRUE ~ NA_character_
    )
  )

# 5) save
write.csv(within_person_overview,
          "results/emoji_metrics_within_person.csv",
          row.names = FALSE)

## ---------------------------
## Figure 1 (CHB): Global emoji metrics × affect
## Panel A: Between-person Spearman corrs (trait PA/NA + state valence) – ALL participants
## Panel B: Within-person standardized coefficients (WP terms) – ALL participants
## ---------------------------

library(dplyr)
library(ggplot2)
library(patchwork)

metric_labels <- c(
  "emoji_word_ratio"        = "Emoji volume",
  "emoji_div" = "Emoji diversity",
  "senti_emoji_avg"         = "Emoji sentiment"
)
metrics_keep  <- names(metric_labels)
metric_levels <- unname(metric_labels)

assoc_map <- c(
  "pa_panas" = "Trait PA",
  "na_panas" = "Trait NA",
  "valence"  = "State valence"
)

dodge <- position_dodge(width = 0.55)

## ---------------------------
## PANEL A: Between-person correlations from cor_overview
## ---------------------------
corA <- cor_overview %>%
  filter(group == "all", level %in% c("trait", "state"), feature %in% metrics_keep) %>%
  mutate(
    metric = recode(feature, !!!metric_labels),
    metric = factor(metric, levels = metric_levels),
    metric = forcats::fct_rev(metric),
    
    assoc_type = recode(target, !!!assoc_map),
    
    # IMPORTANT: dodge order bottom -> top should be Valence, NA, PA
    assoc_type = factor(assoc_type, levels = c("State valence", "Trait NA", "Trait PA"))
  ) %>%
  arrange(metric, assoc_type)

pA <- ggplot(
  corA,
  aes(
    x = r,
    y = metric,
    xmin = ci_lower,
    xmax = ci_upper,
    color = assoc_type,
    group = assoc_type
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_errorbarh(height = 0, linewidth = 0.5, position = dodge) +
  geom_point(size = 2.2, position = dodge) +
  # Use coord_cartesian so CIs slightly beyond ±0.2 are clipped, not dropped
  scale_x_continuous(breaks = seq(-0.2, 0.2, by = 0.1)) +
  coord_cartesian(xlim = c(-0.2, 0.2)) +
  scale_color_manual(
    values = c(
      "Trait PA"      = "#0072B2",
      "Trait NA"      = "#D55E00",
      "State valence" = "#009E73"
    ),
    # Legend order (independent of dodge order)
    breaks = c("Trait PA", "Trait NA", "State valence")
  ) +
  labs(
    title = "A) Trait and state associations",
    x = "Spearman correlation (\u03C1)",
    y = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

## ---------------------------
## PANEL B: Within-person coefficients from within_person_results
## ---------------------------
dfB <- within_person_overview %>%
  mutate(
    metric = factor(metric, levels = metric_levels),
    metric = forcats::fct_rev(metric)
  ) %>%
  arrange(metric)

pB <- ggplot(dfB, aes(x = estimate, y = metric, xmin = ci_lower, xmax = ci_upper)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_errorbarh(height = 0, linewidth = 0.5, color = "#009E73") +
  geom_point(size = 2.2, color = "#009E73") +
  scale_x_continuous(breaks = seq(-0.2, 0.2, by = 0.1)) +
  coord_cartesian(xlim = c(-0.2, 0.2)) +
  labs(
    title = "B) Within-person state associations",
    x = "Standardized within-person coefficient (\u03B2)",
    y = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_blank()
  )

## ---------------------------
## Combine + save
## ---------------------------
fig1 <- pA | pB
fig1

ggsave(
  filename = "figures/figure1_emoji_affect_global.png",
  plot = fig1,
  width = 10, height = 4, dpi = 300
)

### Symbol level affect correlations

# compute corrs

# trait 
cor_trait_symbol <- cor_table(
  data_frame = keyboard_data_trait,
  targets    = c("pa_panas", "na_panas"),
  features   = symbols
) %>%
  mutate(group = "all", level = "trait")

# state (overall; participant-blocked bootstrap)
cor_state_symbol <- cor_table_cluster(
  data_frame = keyboard_data_ema_pre180,
  targets    = c("valence"),
  features   = symbols,
  id_var     = "user_id",
  R          = 1000,
  seed       = 1
) %>%
  mutate(group = "all", level = "state")


# create overview table 
cor_symbol_table_overview <- dplyr::bind_rows(cor_trait_symbol, cor_state_symbol) %>%
  dplyr::mutate(
    # derive printable symbol from feature name
    symbol = dplyr::case_when(
      stringr::str_detect(feature, "^emoji_") ~ {
        cp <- suppressWarnings(as.integer(stringr::str_extract(feature, "(?<=emoji_)\\d+")))
        ifelse(!is.na(cp), intToUtf8(cp, multiple = TRUE), NA_character_)
      },
      stringr::str_detect(feature, "^emoticon_") ~ {
        x <- stringr::str_remove(feature, "^emoticon_")
        stringr::str_remove(x, "(_sum)?(_share)?$")
      },
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::relocate(symbol, .after = feature) %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(.x, 3))
  )


# --- BH-FDR correction (per target / outcome) ---
cor_symbol_table_overview <- cor_symbol_table_overview %>%
  dplyr::group_by(target) %>%  # change "target" to your outcome column name if different
  dplyr::mutate(q_bh = p.adjust(p_value, method = "BH")) %>%  # change "p_value" if your p column is named differently
  dplyr::ungroup()


# save as table
write.csv(cor_symbol_table_overview, "results/emoji_symbol_cor_table.csv")


## ============================================================
## FIGURE 2 (final): include emojis significant for ANY target
## Selection rule: keep emoji if CI excludes 0 for Trait PA OR Trait NA OR State valence
## - Emoji only (no emoticons)
## - Correct facial/non-facial classification (uses codepoint from `symbol`)
## - Ensures no stale `long_data` is reused
## - Dodge order matches Fig 1: PA (top) -> NA -> State valence (bottom)
## - Facet strip text 25% smaller (12 -> 9)
## ============================================================

library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

ci_excludes_zero <- function(l, u) {
  !is.na(l) & !is.na(u) & ((l > 0 & u > 0) | (l < 0 & u < 0))
}

## ---------- 0) start from overview table (emoji only) ----------
paper_all <- cor_symbol_table_overview %>%
  filter(group == "all") %>%
  filter(str_detect(feature, "^emoji_")) %>%              # emoji only
  filter(!is.na(symbol) & symbol != "" & symbol != "NA") %>%
  mutate(across(c(r, ci_lower, ci_upper), as.numeric))

## ---------- 1) select emoji set: significant for ANY target (PA/NA/Valence) ----------
selected_set <- paper_all %>%
  filter(
    (level == "trait" & target %in% c("pa_panas", "na_panas")) |
      (level == "state" & target == "valence")
  ) %>%
  group_by(feature) %>%
  summarise(sig_any_target = any(ci_excludes_zero(ci_lower, ci_upper)), .groups = "drop") %>%
  filter(sig_any_target) %>%
  select(feature)

## ---------- 2) keep ALL targets for selected emojis ----------
paper_plot <- paper_all %>%
  semi_join(selected_set, by = "feature") %>%
  filter(
    (level == "trait" & target %in% c("pa_panas", "na_panas")) |
      (level == "state" & target == "valence")
  )

## ---------- 3) compute ranking + emoji type per feature (codepoint from symbol; robust) ----------
emoji_meta <- paper_plot %>%
  group_by(feature) %>%
  summarise(
    symbol = dplyr::first(symbol),
    total_abs_corr = sum(abs(r), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    codepoint = vapply(symbol, function(s) {
      if (is.na(s) || s == "") return(NA_integer_)
      utf8ToInt(s)[1]
    }, integer(1)),
    emoji_type = case_when(
      !is.na(codepoint) & (
        dplyr::between(codepoint, 0x1F600, 0x1F64F) |  # emoticons block (faces)
          dplyr::between(codepoint, 0x1F910, 0x1F9FF)    # supplemental symbols & pictographs (faces incl. 🧐)
      ) ~ "Facial emojis",
      TRUE ~ "Non-facial emojis"
    )
  ) %>%
  select(feature, symbol, total_abs_corr, emoji_type)

feat_to_sym <- setNames(emoji_meta$symbol, emoji_meta$feature)

## ---------- 4) long plot data (REBUILT; do not reuse old long_data) ----------
long_data <- paper_plot %>%
  # drop any stale cols if they exist (safe no-op otherwise)
  dplyr::select(-dplyr::any_of(c("emoji_type", "total_abs_corr", "codepoint", "feature_faceted", "target_label", "alpha_val"))) %>%
  left_join(emoji_meta, by = "feature") %>%
  mutate(
    emoji_type = factor(emoji_type, levels = c("Facial emojis", "Non-facial emojis")),
    target_label = case_when(
      target == "pa_panas" ~ "Trait PA",
      target == "na_panas" ~ "Trait NA",
      target == "valence"  ~ "State valence",
      TRUE ~ as.character(target)
    ),
    # Dodge order (bottom -> top): Valence, NA, PA  => PA appears on top
    target_label = factor(target_label, levels = c("State valence", "Trait NA", "Trait PA")),
    sig = ci_excludes_zero(ci_lower, ci_upper),
    alpha_val = ifelse(sig, 1.0, 0.25),
    feature_faceted = reorder_within(feature, total_abs_corr, within = emoji_type)
  )

## ---------- 5) plot ----------
dodge <- position_dodge(width = 0.6)

symbol_plot <- ggplot(
  long_data,
  aes(
    x = r,
    y = feature_faceted,
    xmin = ci_lower,
    xmax = ci_upper,
    color = target_label,
    group = target_label
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_errorbarh(
    aes(alpha = alpha_val),
    height = 0, linewidth = 0.8,
    position = dodge,
    na.rm = TRUE
  ) +
  geom_point(
    aes(alpha = alpha_val),
    size = 3,
    position = dodge,
    na.rm = TRUE
  ) +
  facet_grid(emoji_type ~ ., scales = "free_y", space = "free_y") +
  scale_y_discrete(
    labels = function(x) {
      base <- gsub("___.*$", "", x)
      out  <- unname(feat_to_sym[base])
      out[is.na(out)] <- base[is.na(out)]
      out
    }
  ) +
  scale_color_manual(
    values = c(
      "Trait PA"      = "#0072B2",
      "Trait NA"      = "#D55E00",
      "State valence" = "#009E73"
    ),
    # legend order independent of dodge order
    breaks = c("Trait PA", "Trait NA", "State valence"),
    labels = c("Trait PA", "Trait NA", "State Valence")
  ) +
  scale_alpha_identity(guide = "none") +
  scale_x_continuous(breaks = seq(-0.25, 0.25, by = 0.1)) +
  coord_cartesian(xlim = c(-0.25, 0.25)) +
  labs(
    x = "Spearman correlation (\u03C1)",
    y = NULL,
    color = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 11),
    strip.text.y = element_text(size = 7, face = "bold"),  
    strip.placement = "outside"
  )

print(symbol_plot)

ggsave(
  filename = "figures/figure2_symbol_corr_plot.png",
  plot     = symbol_plot,
  width    = 6,
  height   = 10,
  units    = "in",
  dpi      = 300
)



# FINISH