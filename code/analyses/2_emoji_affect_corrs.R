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
keyboard_data_state <- as.data.frame(readRDS(file="data/results/keyboard_data_state_final.rds"))

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

## ============================================================
## CORRELATIONS: overall + by gender in ONE final table
## Men coded "1", Women coded "2" -> column suffixes _men / _women
## Trait: cor_table
## State: cor_table_cluster (overall + within gender)
## ============================================================

## ----------------------------
## 1) Overall correlations
## ----------------------------

# trait (overall)
cor_trait <- cor_table(
  data_frame = keyboard_data_trait,
  targets    = c("pa_panas", "na_panas"),
  features   = features
) %>%
  mutate(group = "all", level = "trait")

# state (overall; participant-blocked bootstrap)
cor_state <- cor_table_cluster(
  data_frame = keyboard_data_state,
  targets    = c("valence"),
  features   = features,
  id_var     = "user_id",
  R          = 1000,
  seed       = 1
) %>%
  mutate(group = "all", level = "state")

## ----------------------------
## 2) By-gender correlations
## ----------------------------

# trait by gender
cor_trait_by_gender <- cor_table_by_gender(
  data_frame = keyboard_data_trait,
  targets    = c("pa_panas", "na_panas"),
  features   = features,
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

# state by gender (IMPORTANT: keep participant-blocked bootstrapping within gender)
cor_state_by_gender <- bind_rows(
  cor_table_cluster(
    data_frame = keyboard_data_state %>% filter(gender == "1"),
    targets    = c("valence"),
    features   = features,
    id_var     = "user_id",
    R          = 1000,
    seed       = 1
  ) %>% mutate(group = "men", level = "state"),
  
  cor_table_cluster(
    data_frame = keyboard_data_state %>% filter(gender == "2"),
    targets    = c("valence"),
    features   = features,
    id_var     = "user_id",
    R          = 1000,
    seed       = 1
  ) %>% mutate(group = "women", level = "state")
)

## ----------------------------
## 3) Combine all results (long) + add symbols
## ----------------------------

cor_all_long <- bind_rows(
  cor_trait,
  cor_state,
  cor_trait_by_gender,
  cor_state_by_gender
) %>%
  mutate(
    # clean feature names for symbol join
    feature = str_remove(feature, "(_sum_share|_sum)$"),
    emoticon_name_key = str_remove(feature, "^emoticon_")
  ) %>%
  # join emoji symbols
  left_join(emoji_df %>% select(variable_name, emoji),
            by = c("feature" = "variable_name")) %>%
  # join emoticon symbols
  left_join(emoticons_df %>% select(emoticon_name, emoticon),
            by = c("emoticon_name_key" = "emoticon_name")) %>%
  mutate(symbol = coalesce(emoji, emoticon)) %>%
  # harmonize target labels
  mutate(target = case_when(
    target == "pa_panas" ~ "pa",
    target == "na_panas" ~ "na",
    TRUE ~ target
  )) %>%
  select(level, group, target, feature, symbol, r, ci_lower, ci_upper, p_value, n)

## ----------------------------
## 4) Final ONE table (wide): all / men / women side-by-side
## ----------------------------

# optional: enforce column order for groups
cor_all_long_filtered <- cor_all_long %>%
  mutate(group = factor(group, levels = c("all", "men", "women"))) %>%
  dplyr::filter(
    (ci_lower > 0 & ci_upper > 0) |
      (ci_lower < 0 & ci_upper < 0)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))



#paper <- cor_all_long_filtered %>% filter (group == "all")


# #### transform to wide format
# 
# cor_final_table <- cor_all_long %>%
#   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
#   pivot_wider(
#     id_cols     = c(level, target, feature, symbol),
#     names_from  = group,
#     values_from = c(r, ci_lower, ci_upper, p_value, n),
#     names_glue  = "{.value}_{group}"
#   ) %>%
#   arrange(level, target, feature)

## ----------------------------
## 5) Save
## ----------------------------

# write.csv(
#   cor_final_table,
#   "data/results/cor_table.csv",
#   row.names = FALSE
# )


## ============================================================
## FIGURE 1: Show ALL targets for the selected emoji set
## - Select emoji set: significant for >=1 TRAIT target (PA/NA) in pooled sample
## - For those emoji, plot PA/NA/Valence correlations
## - If a target-specific CI includes 0, draw that point/CI in a lighter shade
## ============================================================

library(tidyverse)
library(stringr)

## ---------- helpers: reorder within facet ----------
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_y_reordered <- function(sep = "___", ...) {
  scale_y_discrete(labels = function(x) gsub(paste0(sep, ".*$"), "", x), ...)
}

## ---------- helper: CI excludes 0 ----------
ci_excludes_zero <- function(l, u) {
  !is.na(l) & !is.na(u) & ((l > 0 & u > 0) | (l < 0 & u < 0))
}

## ============================================================
## IMPORTANT:
## To show non-sig targets (CI contains 0), you must start from an
## UNFILTERED correlations table. If `cor_all_long_filtered` already
## removed those rows, use your pre-filter object here instead.
## ============================================================
paper_all <- cor_all_long %>%                      # <-- use UNFILTERED object here
  dplyr::filter(group == "all") %>%
  dplyr::filter(!is.na(symbol) & symbol != "" & symbol != "NA") %>%
  mutate(across(c(r, ci_lower, ci_upper), as.numeric))

## For selecting the emoji set, apply the SAME significance rule as before
paper_sig <- paper_all %>%
  dplyr::filter(ci_excludes_zero(ci_lower, ci_upper))

## ---------- 1A) selection uses SIGNIFICANT TRAIT results only ----------
trait_wide_sig <- paper_sig %>%
  dplyr::filter(level == "trait", target %in% c("pa", "na")) %>%
  dplyr::select(feature, symbol, target, r, ci_lower, ci_upper) %>%
  tidyr::pivot_wider(
    names_from  = target,
    values_from = c(r, ci_lower, ci_upper),
    names_glue  = "{.value}_{target}"
  )

selected_set <- trait_wide_sig %>%
  mutate(
    sig_trait_pa = ci_excludes_zero(ci_lower_pa, ci_upper_pa),
    sig_trait_na = ci_excludes_zero(ci_lower_na, ci_upper_na)
  ) %>%
  dplyr::filter(sig_trait_pa | sig_trait_na) %>%
  dplyr::select(feature, symbol) %>%
  distinct()

## ---------- 1B) plotting uses ALL rows (sig + non-sig), but only for selected emoji ----------
paper_plot <- paper_all %>%
  dplyr::semi_join(selected_set, by = c("feature", "symbol")) %>%
  dplyr::filter(
    (level == "trait" & target %in% c("pa", "na")) |
      (level == "state" & target %in% c("valence"))
  )

## ---------- 2) wide by target for plotting (ALL CIs kept) ----------
trait_wide_all <- paper_plot %>%
  dplyr::filter(level == "trait", target %in% c("pa", "na")) %>%
  dplyr::select(feature, symbol, target, r, ci_lower, ci_upper) %>%
  tidyr::pivot_wider(
    names_from  = target,
    values_from = c(r, ci_lower, ci_upper),
    names_glue  = "{.value}_{target}"
  )

state_wide_all <- paper_plot %>%
  dplyr::filter(level == "state", target %in% c("valence")) %>%
  dplyr::select(feature, symbol, r, ci_lower, ci_upper) %>%
  dplyr::rename(
    r_valence        = r,
    ci_lower_valence = ci_lower,
    ci_upper_valence = ci_upper
  )

plot_data <- trait_wide_all %>%
  dplyr::full_join(state_wide_all, by = c("feature", "symbol"))

## ---------- 3) metadata + ordering computed once per emoji ----------
selected_symbols <- plot_data %>%
  mutate(
    # if state valence missing, treat as 0 for ranking only
    r_valence_for_rank = dplyr::coalesce(r_valence, 0),
    total_abs_corr     = abs(r_pa) + abs(r_na) + abs(r_valence_for_rank),
    
    codepoint = suppressWarnings(as.integer(str_extract(feature, "(?<=emoji_)\\d+"))),
    
    emoji_type = case_when(
      symbol == "^^" ~ "Facial symbols",
      !is.na(codepoint) & (
        dplyr::between(codepoint, 0x1F600, 0x1F64F) |
          dplyr::between(codepoint, 0x1F910, 0x1F92F) |
          dplyr::between(codepoint, 0x1F970, 0x1F97A)
      ) ~ "Facial symbols",
      TRUE ~ "Non-facial symbols"
    )
  ) %>%
  select(-r_valence_for_rank)

## ---------- 4) long format for plotting (PA, NA, Valence) ----------
long_data <- selected_symbols %>%
  select(symbol, emoji_type, total_abs_corr,
         r_pa, ci_lower_pa, ci_upper_pa,
         r_na, ci_lower_na, ci_upper_na,
         r_valence, ci_lower_valence, ci_upper_valence) %>%
  pivot_longer(cols = c(r_pa, r_na, r_valence),
               names_to = "target", values_to = "rho") %>%
  mutate(
    ci_lower = case_when(
      target == "r_pa" ~ ci_lower_pa,
      target == "r_na" ~ ci_lower_na,
      TRUE             ~ ci_lower_valence
    ),
    ci_upper = case_when(
      target == "r_pa" ~ ci_upper_pa,
      target == "r_na" ~ ci_upper_na,
      TRUE             ~ ci_upper_valence
    ),
    target_label = case_when(
      target == "r_pa" ~ "Trait Positive Affect",
      target == "r_na" ~ "Trait Negative Affect",
      TRUE             ~ "State Affective Valence"
    ),
    # significance for the CURRENT target (drives lighter shade)
    sig = ci_excludes_zero(ci_lower, ci_upper),
    alpha_val = ifelse(sig, 1.0, 0.25),
    
    # keep EXACT same y-axis emoji set; order within facet by total_abs_corr
    symbol_faceted = reorder_within(symbol, total_abs_corr, within = emoji_type),
    
    rho      = round(rho, 2),
    ci_lower = round(ci_lower, 2),
    ci_upper = round(ci_upper, 2)
  )

## ---------- 5) plot ----------
corr_plot <- ggplot(long_data, aes(x = rho, y = symbol_faceted, color = target_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper, alpha = alpha_val),
    height = 0, linewidth = 0.8,
    position = position_dodge(width = 0.6),
    na.rm = TRUE
  ) +
  geom_point(
    aes(alpha = alpha_val),
    size = 3,
    position = position_dodge(width = 0.6),
    na.rm = TRUE
  ) +
  facet_grid(emoji_type ~ ., scales = "free_y", space = "free_y") +
  scale_y_reordered() +
  scale_color_manual(
    values = c("Trait Positive Affect"   = "#0072B2",
               "Trait Negative Affect"   = "#D55E00",
               "State Affective Valence" = "#009E73"),
    labels = c("Trait Positive Affect"   = "Trait PA",
               "Trait Negative Affect"   = "Trait NA",
               "State Affective Valence" = "State Valence")
  ) +
  scale_alpha_identity(guide = "none") +
  labs(
    x = expression(paste("Spearman Correlation (", rho, ")")),
    y = NULL,
    color = "Affective Target"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 11),
    strip.text.y = element_text(size = 12, face = "bold"),
    strip.placement = "outside"
  )

print(corr_plot)

## ---------- 6) save ----------
ggsave(
  filename = "figures/fig1_corr_plot_facial_vs_nonfacial.png",
  plot     = corr_plot,
  width    = 6,
  height   = 9,
  units    = "in",
  dpi      = 300
)



# FINISH