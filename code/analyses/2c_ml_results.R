############################
#### PREDICTION RESULTS:
#### FIGURE 3 + TABLES S2, S3, S5
############################

############################
#### 0) PREPARATION
############################

required_packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "scales",
  "mlr3",
  "mlr3measures"
)

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

source("code/analyses/helper/plot_theme.R")
source("code/analyses/helper/msr_pearson.R")

############################
#### 1) LOAD BENCHMARK RESULTS
############################

bmr_keyboardlanguage_trait    <- readRDS("results/bmr_keyboardlanguage_trait.rds")
bmr_keyboardlanguage_day      <- readRDS("results/bmr_keyboardlanguage_day.rds")
bmr_keyboardlanguage_moment   <- readRDS("results/bmr_keyboardlanguage_moment.rds")
bmr_keyboardlanguage_moment_diff   <- readRDS("results/bmr_keyboardlanguage_moment_diff.rds")
bmr_keyboardlanguage_families <- readRDS("results/bmr_keyboardlanguage_feature_families.rds")
bmr_keyboardlanguage_age      <- readRDS("results/bmr_keyboardlanguage_age.rds")

mes <- c(
  list(msr_pearson),
  msrs(c("regr.rsq", "regr.mae", "regr.rmse"))
)

score_trait <- as.data.frame(bmr_keyboardlanguage_trait$score(mes)) %>%
  mutate(score_source = "trait")

score_age <- as.data.frame(bmr_keyboardlanguage_age$score(mes)) %>%
  mutate(score_source = "age")

score_day <- as.data.frame(bmr_keyboardlanguage_day$score(mes)) %>%
  mutate(score_source = "day")

score_moment_diff <- as.data.frame(bmr_keyboardlanguage_moment_diff$score(mes)) %>%
  mutate(score_source = "moment_diff")

score_moment <- as.data.frame(bmr_keyboardlanguage_moment$score(mes)) %>%
  mutate(score_source = "moment")

score_families <- as.data.frame(bmr_keyboardlanguage_families$score(mes)) %>%
  mutate(score_source = "families")

score_all <- bind_rows(
  score_trait,
  score_age,
  score_day,
  score_moment,
  score_moment_diff
)

############################
#### 2) HELPER FUNCTIONS
############################

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}

safe_quantile <- function(x, p) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(quantile(x, p, na.rm = TRUE))
}

fmt_median_iqr <- function(md, q25, q75, digits = 2) {
  ifelse(
    is.na(md) | is.na(q25) | is.na(q75),
    NA_character_,
    paste0(
      formatC(md, format = "f", digits = digits),
      " [",
      formatC(q25, format = "f", digits = digits),
      ", ",
      formatC(q75, format = "f", digits = digits),
      "]"
    )
  )
}

nice_algo <- function(x) {
  case_when(
    str_detect(x, "featureless") ~ "FL",
    str_detect(x, "ranger") ~ "RF",
    str_detect(x, "glmnet") ~ "EN",
    TRUE ~ x
  )
}

nice_context <- function(task_id) {
  case_when(
    str_detect(task_id, regex("private", ignore_case = TRUE)) ~ "Private",
    str_detect(task_id, regex("public", ignore_case = TRUE)) ~ "Public",
    TRUE ~ NA_character_
  )
}

nice_outcome <- function(task_id, score_source) {
  case_when(
    score_source == "trait" &
      str_detect(task_id, regex("(^|_)pa($|_)", ignore_case = TRUE)) ~
      "Trait positive affect",
    
    score_source == "trait" &
      str_detect(task_id, regex("(^|_)na($|_)", ignore_case = TRUE)) ~
      "Trait negative affect",
    
    score_source == "age" ~
      "Age",
    
    score_source == "day" ~
      "Daily affective valence",
    
    score_source == "moment" &
      str_detect(task_id, regex("arousal", ignore_case = TRUE)) ~
      "Momentary arousal",
    
    score_source == "moment" &
      str_detect(task_id, regex("valence", ignore_case = TRUE)) ~
      "Momentary affective valence",
    
    score_source == "moment_diff" &
      str_detect(task_id, regex("valence", ignore_case = TRUE)) ~
      "Fluctuation in momentary affective valence",
    
    score_source == "moment_diff" &
      str_detect(task_id, regex("arousal", ignore_case = TRUE)) ~
      "Fluctuation in momentary arousal",
    
    TRUE ~ task_id
  )
}

outcome_order <- c(
  "Trait positive affect",
  "Trait negative affect",
  "Age",
  "Daily affective valence",
  "Momentary affective valence",
  "Momentary arousal",
  "Fluctuation in momentary affective valence",
  "Fluctuation in momentary arousal"
)

context_order <- c("Private", "Public")
algo_order <- c("FL", "RF", "EN")

summarise_prediction_performance <- function(
  df,
  grouping_vars,
  min_pearson_coverage = 0.50
) {
  
  df %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n_folds_total = n(),
      n_folds_valid_pearson = sum(!is.na(pearson)),
      pearson_coverage = n_folds_valid_pearson / n_folds_total,
      
      r_md_raw  = safe_median(pearson),
      r_sd_raw  = safe_sd(pearson),
      r_q25_raw = safe_quantile(pearson, .25),
      r_q75_raw = safe_quantile(pearson, .75),
      
      rsq_md  = safe_median(regr.rsq),
      rsq_sd  = safe_sd(regr.rsq),
      rsq_q25 = safe_quantile(regr.rsq, .25),
      rsq_q75 = safe_quantile(regr.rsq, .75),
      
      mae_md  = safe_median(regr.mae),
      mae_sd  = safe_sd(regr.mae),
      mae_q25 = safe_quantile(regr.mae, .25),
      mae_q75 = safe_quantile(regr.mae, .75),
      
      rmse_md  = safe_median(regr.rmse),
      rmse_sd  = safe_sd(regr.rmse),
      rmse_q25 = safe_quantile(regr.rmse, .25),
      rmse_q75 = safe_quantile(regr.rmse, .75),
      
      .groups = "drop"
    ) %>%
    mutate(
      r_md = if_else(
        pearson_coverage >= min_pearson_coverage,
        r_md_raw,
        NA_real_
      ),
      r_sd = if_else(
        pearson_coverage >= min_pearson_coverage,
        r_sd_raw,
        NA_real_
      ),
      r_q25 = if_else(
        pearson_coverage >= min_pearson_coverage,
        r_q25_raw,
        NA_real_
      ),
      r_q75 = if_else(
        pearson_coverage >= min_pearson_coverage,
        r_q75_raw,
        NA_real_
      )
    ) %>%
    select(
      -r_md_raw,
      -r_sd_raw,
      -r_q25_raw,
      -r_q75_raw
    ) %>%
    mutate(
      across(
        c(
          pearson_coverage,
          r_md, r_sd, r_q25, r_q75,
          rsq_md, rsq_sd, rsq_q25, rsq_q75,
          mae_md, mae_sd, mae_q25, mae_q75,
          rmse_md, rmse_sd, rmse_q25, rmse_q75
        ),
        ~ round(.x, 3)
      )
    )
}
############################
#### 3) PARSE FULL MODEL RESULTS
############################

results_long <- score_all %>%
  mutate(
    algo = nice_algo(learner_id),
    outcome = nice_outcome(task_id, score_source),
    context = nice_context(task_id)
  ) %>%
  filter(
    context %in% context_order,
    outcome %in% outcome_order
  )

# Diagnostic export: check task parsing if needed
task_mapping_check <- score_all %>%
  distinct(score_source, task_id, learner_id) %>%
  mutate(
    algo = nice_algo(learner_id),
    outcome = nice_outcome(task_id, score_source),
    context = nice_context(task_id)
  ) %>%
  arrange(score_source, outcome, context, algo, task_id)

write.csv(
  task_mapping_check,
  "results/task_mapping_prediction_results_check.csv",
  row.names = FALSE
)

############################
#### 4) TABLE S2:
#### FULL PERFORMANCE RESULTS OF PREDICTION MODELS
############################

results_sum <- results_long %>%
  summarise_prediction_performance(
    grouping_vars = c("outcome", "context", "algo")
  )

table_s2_skeleton <- tidyr::expand_grid(
  outcome = factor(outcome_order, levels = outcome_order),
  context = factor(context_order, levels = context_order),
  algo = factor(algo_order, levels = algo_order)
)

table_s2 <- table_s2_skeleton %>%
  left_join(
    results_sum %>%
      mutate(
        outcome = factor(outcome, levels = outcome_order),
        context = factor(context, levels = context_order),
        algo = factor(algo, levels = algo_order)
      ),
    by = c("outcome", "context", "algo")
  ) %>%
  mutate(
    outcome_label = case_when(
      outcome == "Trait positive affect" ~ "Trait Positive Affect",
      outcome == "Trait negative affect" ~ "Trait Negative Affect",
      outcome == "Age" ~ "Age",
      outcome == "Daily affective valence" ~ "Daily Affective Valence",
      outcome == "Momentary affective valence" ~ "Momentary Affective Valence",
      outcome == "Momentary arousal" ~ "Momentary Arousal",
      outcome == "Fluctuation in momentary affective valence" ~
        "Fluctuation in Momentary Affective Valence",
      outcome == "Fluctuation in momentary arousal" ~
        "Fluctuation in Momentary Arousal",
      TRUE ~ as.character(outcome)
    ),
    `Median r [IQR]` = fmt_median_iqr(r_md, r_q25, r_q75, digits = 2),
    `Median R2 [IQR]` = fmt_median_iqr(rsq_md, rsq_q25, rsq_q75, digits = 2),
    `Median MAE [IQR]` = fmt_median_iqr(mae_md, mae_q25, mae_q75, digits = 2)
  ) %>%
  arrange(outcome, context, algo) %>%
  transmute(
    `Outcome Variable` = outcome_label,
    Context = as.character(context),
    `Algo.` = as.character(algo),
    `Median r [IQR]`,
    `Median R2 [IQR]`,
    `Median MAE [IQR]`
  )

write.csv(
  table_s2,
  file = "results/table_s2_full_prediction_performance_results.csv",
  row.names = FALSE,
  na = ""
)

############################
#### 5) FIGURE 3:
#### MAIN PAPER PREDICTION PERFORMANCE
############################

fig3_outcome_order <- c(
  "Trait negative affect",
  "Trait positive affect",
  "Daily affective valence",
  "Momentary affective valence"
)

fig3_folds <- results_long %>%
  filter(
    algo == "RF",
    outcome %in% fig3_outcome_order
  ) %>%
  mutate(
    outcome = factor(outcome, levels = fig3_outcome_order),
    context = factor(context, levels = context_order)
  )

fig3_summary <- fig3_folds %>%
  group_by(outcome, context) %>%
  summarise(
    n_folds_total = n(),
    n_folds_valid_pearson = sum(!is.na(pearson)),
    pearson_coverage = n_folds_valid_pearson / n_folds_total,
    r_md = safe_median(pearson),
    r_q25 = safe_quantile(pearson, 0.25),
    r_q75 = safe_quantile(pearson, 0.75),
    .groups = "drop"
  )

write.csv(
  fig3_summary,
  file = "results/figure_3_prediction_performance_plot_data.csv",
  row.names = FALSE,
  na = "NA"
)

context_cols <- c(
  "Private" = "#E69F00",
  "Public"  = "#56B4E9"
)

pd <- position_dodge(width = 0.55)

fig3_pred <- ggplot(
  fig3_summary,
  aes(
    x = outcome,
    y = r_md,
    color = context,
    shape = context,
    group = context
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray70",
    linewidth = 0.5
  ) +
  geom_linerange(
    aes(
      ymin = r_q25,
      ymax = r_q75
    ),
    position = pd,
    linewidth = 0.85,
    alpha = 0.75
  ) +
  geom_point(
    position = pd,
    size = 3.2,
    alpha = 0.95
  ) +
  scale_color_manual(
    values = context_cols,
    name = "Context"
  ) +
  scale_shape_manual(
    values = c("Private" = 16, "Public" = 17),
    name = "Context"
  ) +
  scale_x_discrete(
    labels = c(
      "Trait negative affect" = "Trait\nnegative\naffect",
      "Trait positive affect" = "Trait\npositive\naffect",
      "Daily affective valence" = "Daily\naffective\nvalence",
      "Momentary affective valence" = "Momentary\naffective\nvalence"
    )
  ) +
  scale_y_continuous(
    limits = c(-0.40, 0.40),
    breaks = seq(-0.40, 0.40, by = 0.10),
    labels = scales::label_number(accuracy = 0.01, trim = TRUE),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  labs(
    x = NULL,
    y = "Pearson correlation"
  ) +
  theme_custom(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text = element_text(size = 10),
    legend.margin = margin(b = -2),
    plot.margin = margin(8, 10, 6, 6),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.95
    )
  )

fig3_pred

ggsave(
  filename = "figures/figure3_prediction_performance_dot_iqr.png",
  plot = fig3_pred,
  width = 8,
  height = 5.4,
  dpi = 300
)


############################
#### 7) TABLE S4:
#### FEATURE-FAMILY PREDICTION ANALYSES FOR TRAIT AFFECT
############################

family_results_long <- score_families %>%
  mutate(
    algo = nice_algo(learner_id),
    outcome = case_when(
      str_detect(task_id, regex("_pa_", ignore_case = TRUE)) ~ "Trait positive affect",
      str_detect(task_id, regex("_na_", ignore_case = TRUE)) ~ "Trait negative affect",
      TRUE ~ task_id
    ),
    context = nice_context(task_id),
    feature_family = case_when(
      str_detect(task_id, regex("_word$", ignore_case = TRUE)) ~ "Word dictionaries",
      str_detect(task_id, regex("_emoji$", ignore_case = TRUE)) ~ "Emojis and emoticons",
      str_detect(task_id, regex("_typing$", ignore_case = TRUE)) ~ "Typing dynamics",
      TRUE ~ "Other"
    )
  ) %>%
  filter(
    feature_family != "Other",
    context %in% context_order,
    outcome %in% c("Trait positive affect", "Trait negative affect")
  )

family_results_sum <- family_results_long %>%
  summarise_prediction_performance(
    grouping_vars = c("outcome", "context", "feature_family", "algo")
  )

table_s4 <- family_results_sum %>%
  filter(algo == "RF") %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("Trait positive affect", "Trait negative affect")
    ),
    context = factor(context, levels = context_order),
    feature_family = factor(
      feature_family,
      levels = c(
        "Word dictionaries",
        "Emojis and emoticons",
        "Typing dynamics"
      )
    ),
    `Median r [IQR]` = fmt_median_iqr(r_md, r_q25, r_q75, digits = 2)
  ) %>%
  arrange(outcome, context, feature_family) %>%
  transmute(
    `Outcome variable` = as.character(outcome),
    Context = as.character(context),
    `Feature family` = as.character(feature_family),
    `Median r [IQR]`
  )

write.csv(
  table_s4,
  file = "results/table_s4_feature_family_prediction_trait_affect.csv",
  row.names = FALSE,
  na = ""
)

############################
#### 8) PRINT OUTPUTS
############################

table_s2
table_s4
fig3_pred

# finish