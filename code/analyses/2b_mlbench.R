##################################################
#### MACHINE LEARNING: TRAIT / DAILY / MOMENTARY
##################################################

############################
#### 0) PREPARATION ####
############################

packages <- c(
  "dplyr",
  "parallel",
  "data.table",
  "ggplot2",
  "mlr3",
  "mlr3learners",
  "mlr3pipelines",
  "ranger",
  "glmnet",
  "future",
  "stringr",
  "progressr",
  "lgr",
  "scales"
)
invisible(lapply(packages, library, character.only = TRUE))

source("code/analyses/helper/plot_theme.R")
source("code/analyses/helper/msr_pearson.R")


set.seed(123, kind = "L'Ecuyer")

dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

############################
#### 1) READ IN DATA ####
############################

keyboard_data_trait_ml <- readRDS(file = "data/keyboard_data_trait_ml.rds")
keyboard_data_day_ml   <- readRDS(file = "data/keyboard_data_day_ml.rds")
keyboard_data_ema_ml   <- readRDS(file = "data/keyboard_data_ema_ml.rds")

############################
#### 2) PREPARE DATA ####
############################

## Trait
keyboard_data_trait_private_ml <- keyboard_data_trait_ml %>% 
  filter(scope == "private")

keyboard_data_trait_public_ml <- keyboard_data_trait_ml %>% 
  filter(scope == "public")

## Daily
keyboard_data_day_private_ml <- keyboard_data_day_ml %>% 
  filter(scope == "private")

keyboard_data_day_public_ml <- keyboard_data_day_ml %>% 
  filter(scope == "public")

## Momentary
keyboard_data_ema_private_ml <- keyboard_data_ema_ml %>% 
  filter(scope == "private")

keyboard_data_ema_public_ml <- keyboard_data_ema_ml %>% 
  filter(scope == "public")

############################
#### 3) HELPER FUNCTIONS ####
############################

make_task_backend <- function(data, target, extra_drop = character(0)) {
  stopifnot(target %in% names(data))
  
  non_feature_cols <- c(
    "scope",
    "date",
    "user_day_id",
    "age",
    "gender",
    "pa_panas",
    "na_panas",
    "daily_valence",
    "daily_arousal",
    "n_ema_day",
    "valence",
    "arousal",
    "valence_avg",
    "arousal_avg",
    "notificationTimestamp",
    "questionnaireStartedTimestamp",
    "questionnaireEndedTimestamp",
    "notificationTimestamp_corrected",
    "questionnaireStartedTimestamp_corrected",
    "questionnaireEndedTimestamp_corrected",
    "es_questionnaire_id",
    "weekday",
    "nr",
    "week",
    "valence_diff",
    "arousal_diff"
  )
  
  predictor_cols <- setdiff(names(data), union(non_feature_cols, extra_drop))
  predictor_cols <- setdiff(predictor_cols, c("user_id", target))
  
  backend <- data[, c("user_id", target, predictor_cols), drop = FALSE]
  backend
}

make_regr_task <- function(data, id, target, extra_drop = character(0)) {
  backend <- make_task_backend(
    data = data,
    target = target,
    extra_drop = extra_drop
  )
  
  task <- TaskRegr$new(
    id = id,
    backend = backend,
    target = target
  )
  
  task$col_roles$group <- "user_id"
  task$col_roles$feature <- setdiff(task$col_roles$feature, "user_id")
  
  task
}

############################
#### 3b) FEATURE-FAMILY HELPERS ####
############################

get_feature_families <- function(data) {
  
  metadata_cols <- c(
    "user_id",
    "scope",
    "date",
    "user_day_id",
    "age",
    "gender",
    "pa_panas",
    "na_panas",
    "daily_valence",
    "daily_arousal",
    "n_ema_day",
    "valence",
    "arousal",
    "valence_avg",
    "arousal_avg",
    "notificationTimestamp",
    "questionnaireStartedTimestamp",
    "questionnaireEndedTimestamp",
    "notificationTimestamp_corrected",
    "questionnaireStartedTimestamp_corrected",
    "questionnaireEndedTimestamp_corrected",
    "es_questionnaire_id",
    "weekday",
    "nr",
    "week",
    "valence_diff",
    "arousal_diff"
  )
  
  candidate_features <- setdiff(names(data), metadata_cols)
  
  word_features <- candidate_features[
    str_detect(candidate_features, "^liwc_|^wordsentiment")
  ]
  
  emoji_features <- candidate_features[
    str_detect(candidate_features, "^emoji_|^emoticon_|^senti_emoji")
  ]
  
  typing_features <- setdiff(candidate_features, c(word_features, emoji_features))
  
  list(
    word   = sort(unique(word_features)),
    emoji  = sort(unique(emoji_features)),
    typing = sort(unique(typing_features))
  )
}

feature_families_trait  <- get_feature_families(keyboard_data_trait_ml)
feature_families_day    <- get_feature_families(keyboard_data_day_ml)
feature_families_moment <- get_feature_families(keyboard_data_ema_ml)

message("Trait families: word=", length(feature_families_trait$word),
        ", emoji=", length(feature_families_trait$emoji),
        ", typing=", length(feature_families_trait$typing))

message("Day families: word=", length(feature_families_day$word),
        ", emoji=", length(feature_families_day$emoji),
        ", typing=", length(feature_families_day$typing))

message("Moment families: word=", length(feature_families_moment$word),
        ", emoji=", length(feature_families_moment$emoji),
        ", typing=", length(feature_families_moment$typing))

make_family_regr_task <- function(data, id, target, family_features) {
  stopifnot(target %in% names(data))
  
  family_features <- intersect(family_features, names(data))
  
  if (length(family_features) == 0) {
    stop(paste0("No features found for task ", id))
  }
  
  backend <- data[, c("user_id", target, family_features), drop = FALSE]
  
  task <- TaskRegr$new(
    id = id,
    backend = backend,
    target = target
  )
  
  task$col_roles$group <- "user_id"
  task$col_roles$feature <- setdiff(task$col_roles$feature, "user_id")
  
  task
}

############################
#### 4) CREATE TASKS ####
############################

#### Age benchmark ####

keyboard_data_trait_private_ml_age <- keyboard_data_trait_private_ml %>%
  filter(!is.na(age))

keyboard_data_trait_public_ml_age <- keyboard_data_trait_public_ml %>%
  filter(!is.na(age))

keyboardlanguage_private_age <- make_regr_task(
  data = keyboard_data_trait_private_ml_age,
  id = "keyboardlanguage_private_age",
  target = "age"
)

keyboardlanguage_public_age <- make_regr_task(
  data = keyboard_data_trait_public_ml_age,
  id = "keyboardlanguage_public_age",
  target = "age"
)

#### Trait affect ####

keyboardlanguage_private_pa <- make_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_pa",
  target = "pa_panas"
)

keyboardlanguage_private_na <- make_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_na",
  target = "na_panas"
)

keyboardlanguage_public_pa <- make_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_pa",
  target = "pa_panas"
)

keyboardlanguage_public_na <- make_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_na",
  target = "na_panas"
)

#### Daily affect ####

keyboardlanguage_private_day_valence <- make_regr_task(
  data = keyboard_data_day_private_ml,
  id = "keyboardlanguage_private_day_valence",
  target = "daily_valence"
)

keyboardlanguage_public_day_valence <- make_regr_task(
  data = keyboard_data_day_public_ml,
  id = "keyboardlanguage_public_day_valence",
  target = "daily_valence"
)

#### Momentary affect ####

keyboardlanguage_private_moment_valence <- make_regr_task(
  data = keyboard_data_ema_private_ml,
  id = "keyboardlanguage_private_moment_valence",
  target = "valence"
)

keyboardlanguage_public_moment_valence <- make_regr_task(
  data = keyboard_data_ema_public_ml,
  id = "keyboardlanguage_public_moment_valence",
  target = "valence"
)

#### Supplementary momentary arousal ####

keyboard_data_ema_private_ml_arousal <- keyboard_data_ema_private_ml %>%
  filter(!is.na(arousal))

keyboard_data_ema_public_ml_arousal <- keyboard_data_ema_public_ml %>%
  filter(!is.na(arousal))

keyboardlanguage_private_moment_arousal <- make_regr_task(
  data = keyboard_data_ema_private_ml_arousal,
  id = "keyboardlanguage_private_moment_arousal",
  target = "arousal"
)

keyboardlanguage_public_moment_arousal <- make_regr_task(
  data = keyboard_data_ema_public_ml_arousal,
  id = "keyboardlanguage_public_moment_arousal",
  target = "arousal"
)

############################
#### 4b) FEATURE-FAMILY TASKS
#### TRAIT ONLY; SUPPLEMENT
############################

#### Private trait PA ####

keyboardlanguage_private_pa_word <- make_family_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_pa_word",
  target = "pa_panas",
  family_features = feature_families_trait$word
)

keyboardlanguage_private_pa_emoji <- make_family_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_pa_emoji",
  target = "pa_panas",
  family_features = feature_families_trait$emoji
)

keyboardlanguage_private_pa_typing <- make_family_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_pa_typing",
  target = "pa_panas",
  family_features = feature_families_trait$typing
)

#### Private trait NA ####

keyboardlanguage_private_na_word <- make_family_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_na_word",
  target = "na_panas",
  family_features = feature_families_trait$word
)

keyboardlanguage_private_na_emoji <- make_family_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_na_emoji",
  target = "na_panas",
  family_features = feature_families_trait$emoji
)

keyboardlanguage_private_na_typing <- make_family_regr_task(
  data = keyboard_data_trait_private_ml,
  id = "keyboardlanguage_private_na_typing",
  target = "na_panas",
  family_features = feature_families_trait$typing
)

#### Public trait PA ####

keyboardlanguage_public_pa_word <- make_family_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_pa_word",
  target = "pa_panas",
  family_features = feature_families_trait$word
)

keyboardlanguage_public_pa_emoji <- make_family_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_pa_emoji",
  target = "pa_panas",
  family_features = feature_families_trait$emoji
)

keyboardlanguage_public_pa_typing <- make_family_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_pa_typing",
  target = "pa_panas",
  family_features = feature_families_trait$typing
)

#### Public trait NA ####

keyboardlanguage_public_na_word <- make_family_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_na_word",
  target = "na_panas",
  family_features = feature_families_trait$word
)

keyboardlanguage_public_na_emoji <- make_family_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_na_emoji",
  target = "na_panas",
  family_features = feature_families_trait$emoji
)

keyboardlanguage_public_na_typing <- make_family_regr_task(
  data = keyboard_data_trait_public_ml,
  id = "keyboardlanguage_public_na_typing",
  target = "na_panas",
  family_features = feature_families_trait$typing
)

############################
#### 5) CREATE LEARNERS ####
############################

lrn_fl <- lrn("regr.featureless")
lrn_rf <- lrn("regr.ranger", num.trees = 1000)
lrn_rr <- lrn("regr.cv_glmnet", alpha = 0.5)

############################
#### 6) RESAMPLING ####
############################

resampling <- rsmp("repeated_cv", folds = 10L, repeats = 5L)

############################
#### 7) PREPROCESSING IN CV ####
############################

lrn_rf_oor  <- po("imputeoor") %>>% lrn_rf
lrn_rr_hist <- po("imputehist") %>>% lrn_rr

############################
#### 8) RUN BENCHMARKS ####
############################

logger <- lgr::get_logger("bbotk")
logger$set_threshold("warn")

progressr::handlers(global = TRUE)
progressr::handlers("progress")

future::plan("multisession", workers = 10)

#### Age benchmark ####

bmgrid_keyboardlanguage_age <- benchmark_grid(
  task = list(
    keyboardlanguage_private_age,
    keyboardlanguage_public_age
  ),
  learner = list(lrn_fl, lrn_rf_oor, lrn_rr_hist),
  resampling = resampling
)

bmr_keyboardlanguage_age <- benchmark(
  bmgrid_keyboardlanguage_age,
  store_models = FALSE,
  store_backends = FALSE
)

saveRDS(bmr_keyboardlanguage_age, "results/bmr_keyboardlanguage_age.rds")

#### Trait affect ####

bmgrid_keyboardlanguage_trait <- benchmark_grid(
  task = list(
    keyboardlanguage_private_pa,
    keyboardlanguage_private_na,
    keyboardlanguage_public_pa,
    keyboardlanguage_public_na
  ),
  learner = list(lrn_fl, lrn_rf_oor, lrn_rr_hist),
  resampling = resampling
)

bmr_keyboardlanguage_trait <- benchmark(
  bmgrid_keyboardlanguage_trait,
  store_models = FALSE,
  store_backends = FALSE
)

saveRDS(bmr_keyboardlanguage_trait, "results/bmr_keyboardlanguage_trait.rds")

#### Daily valence ####

bmgrid_keyboardlanguage_day <- benchmark_grid(
  task = list(
    keyboardlanguage_private_day_valence,
    keyboardlanguage_public_day_valence
  ),
  learner = list(lrn_fl, lrn_rf_oor, lrn_rr_hist),
  resampling = resampling
)

bmr_keyboardlanguage_day <- benchmark(
  bmgrid_keyboardlanguage_day,
  store_models = FALSE,
  store_backends = FALSE
)

saveRDS(bmr_keyboardlanguage_day, "results/bmr_keyboardlanguage_day.rds")

#### Momentary valence and supplementary arousal ####

bmgrid_keyboardlanguage_moment <- benchmark_grid(
  task = list(
    keyboardlanguage_private_moment_valence,
    keyboardlanguage_public_moment_valence,
    keyboardlanguage_private_moment_arousal,
    keyboardlanguage_public_moment_arousal
  ),
  learner = list(lrn_fl, lrn_rf_oor, lrn_rr_hist),
  resampling = resampling
)

bmr_keyboardlanguage_moment <- benchmark(
  bmgrid_keyboardlanguage_moment,
  store_models = FALSE,
  store_backends = FALSE
)

saveRDS(bmr_keyboardlanguage_moment, "results/bmr_keyboardlanguage_moment.rds")

############################
#### 8b) FEATURE-FAMILY BENCHMARKS
#### TRAIT ONLY; SUPPLEMENT
############################

bmgrid_keyboardlanguage_families <- benchmark_grid(
  task = list(
    keyboardlanguage_private_pa_word,
    keyboardlanguage_private_pa_emoji,
    keyboardlanguage_private_pa_typing,
    
    keyboardlanguage_private_na_word,
    keyboardlanguage_private_na_emoji,
    keyboardlanguage_private_na_typing,
    
    keyboardlanguage_public_pa_word,
    keyboardlanguage_public_pa_emoji,
    keyboardlanguage_public_pa_typing,
    
    keyboardlanguage_public_na_word,
    keyboardlanguage_public_na_emoji,
    keyboardlanguage_public_na_typing
  ),
  learner = list(lrn_rf_oor),
  resampling = resampling
)

bmr_keyboardlanguage_families <- benchmark(
  bmgrid_keyboardlanguage_families,
  store_models = FALSE,
  store_backends = FALSE
)

saveRDS(
  bmr_keyboardlanguage_families,
  "results/bmr_keyboardlanguage_feature_families.rds"
)

############################
#### 9) BENCHMARK RESULTS ####
############################

bmr_keyboardlanguage_trait    <- readRDS("results/bmr_keyboardlanguage_trait.rds")
bmr_keyboardlanguage_day      <- readRDS("results/bmr_keyboardlanguage_day.rds")
bmr_keyboardlanguage_moment   <- readRDS("results/bmr_keyboardlanguage_moment.rds")
bmr_keyboardlanguage_families <- readRDS("results/bmr_keyboardlanguage_feature_families.rds")
bmr_keyboardlanguage_age      <- readRDS("results/bmr_keyboardlanguage_age.rds")

mes <- c(
  list(msr_pearson),
  msrs(c("regr.rsq", "regr.mae", "regr.rmse"))
)

score_age      <- as.data.frame(bmr_keyboardlanguage_age$score(mes))
score_trait    <- as.data.frame(bmr_keyboardlanguage_trait$score(mes))
score_day      <- as.data.frame(bmr_keyboardlanguage_day$score(mes))
score_moment   <- as.data.frame(bmr_keyboardlanguage_moment$score(mes))
score_families <- as.data.frame(bmr_keyboardlanguage_families$score(mes))

score_all <- bind_rows(score_age, score_trait, score_day, score_moment)

############################
#### 10) PARSE TASK / LEARNER LABELS
#### MAIN / SUPPLEMENTAL RESULTS
############################

results_long <- score_all %>%
  mutate(
    algo = case_when(
      str_detect(learner_id, "featureless") ~ "FL",
      str_detect(learner_id, "ranger") ~ "RF",
      str_detect(learner_id, "glmnet") ~ "EN",
      TRUE ~ learner_id
    ),
    outcome = case_when(
      str_detect(task_id, regex("(^|_)pa($|_)", ignore_case = TRUE)) ~ "Trait PA",
      str_detect(task_id, regex("(^|_)na($|_)", ignore_case = TRUE)) ~ "Trait NA",
      str_detect(task_id, regex("day_valence", ignore_case = TRUE)) ~ "Daily valence",
      str_detect(task_id, regex("moment_valence", ignore_case = TRUE)) ~ "Momentary valence",
      str_detect(task_id, regex("moment_arousal", ignore_case = TRUE)) ~ "Momentary arousal",
      str_detect(task_id, regex("age", ignore_case = TRUE)) ~ "Age",
      TRUE ~ task_id
    ),
    context = case_when(
      str_detect(task_id, regex("private", ignore_case = TRUE)) ~ "Private",
      str_detect(task_id, regex("public", ignore_case = TRUE)) ~ "Public",
      TRUE ~ NA_character_
    ),
    section = case_when(
      outcome %in% c("Trait PA", "Trait NA", "Daily valence", "Momentary valence") ~ "Main Analyses",
      outcome %in% c("Age", "Momentary arousal") ~ "Supplemental Analyses",
      TRUE ~ "Other"
    )
  )

############################
#### 11) SUMMARISE ACROSS RESAMPLES
#### WITH PEARSON COVERAGE
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

summarise_prediction_performance <- function(df) {
  df %>%
    group_by(section, outcome, context, algo) %>%
    summarise(
      n_folds_total = n(),
      n_folds_valid_pearson = sum(!is.na(pearson)),
      pearson_coverage = n_folds_valid_pearson / n_folds_total,
      
      r_md  = safe_median(pearson),
      r_sd  = safe_sd(pearson),
      r_q25 = safe_quantile(pearson, .25),
      r_q75 = safe_quantile(pearson, .75),
      
      rsq_md = safe_median(regr.rsq),
      rsq_sd = safe_sd(regr.rsq),
      
      mae_md = safe_median(regr.mae),
      mae_sd = safe_sd(regr.mae),
      
      .groups = "drop"
    ) %>%
    mutate(
      across(
        c(
          pearson_coverage,
          r_md, r_sd, r_q25, r_q75,
          rsq_md, rsq_sd,
          mae_md, mae_sd
        ),
        ~ round(.x, 3)
      )
    )
}

results_sum <- results_long %>%
  filter(context %in% c("Private", "Public")) %>%
  summarise_prediction_performance() %>%
  filter(section %in% c("Main Analyses", "Supplemental Analyses"))

############################
#### 12) ORDER ROWS
#### MAIN / SUPPLEMENTAL RESULTS
############################

table_df <- results_sum %>%
  mutate(
    section = factor(section, levels = c("Main Analyses", "Supplemental Analyses")),
    outcome = factor(
      outcome,
      levels = c(
        "Trait PA",
        "Trait NA",
        "Daily valence",
        "Momentary valence",
        "Age",
        "Momentary arousal"
      )
    ),
    context = factor(context, levels = c("Private", "Public")),
    algo = factor(algo, levels = c("FL", "RF", "EN"))
  ) %>%
  arrange(section, outcome, context, algo) %>%
  mutate(
    `Outcome Variable` = as.character(outcome),
    `Context` = as.character(context),
    `Algo.` = as.character(algo),
    `Valid folds` = paste0(n_folds_valid_pearson, "/", n_folds_total),
    `Pearson coverage` = pearson_coverage,
    `r (Md)` = r_md,
    `r (SD)` = r_sd,
    `r (Q1)` = r_q25,
    `r (Q3)` = r_q75,
    `R² (Md)` = rsq_md,
    `R² (SD)` = rsq_sd,
    `MAE (Md)` = mae_md,
    `MAE (SD)` = mae_sd
  ) %>%
  select(
    `Outcome Variable`, `Context`, `Algo.`,
    `Valid folds`, `Pearson coverage`,
    `r (Md)`, `r (SD)`, `r (Q1)`, `r (Q3)`,
    `R² (Md)`, `R² (SD)`, `MAE (Md)`, `MAE (SD)`
  )

############################
#### 13) CSV EXPORT
#### MAIN / SUPPLEMENTAL RESULTS
############################

write.csv(
  table_df,
  file = "results/prediction_performance_results_table.csv",
  row.names = FALSE,
  na = "NA"
)


############################
#### 14) PARSE FEATURE-FAMILY RESULTS
############################

family_results_long <- score_families %>%
  mutate(
    algo = case_when(
      str_detect(learner_id, "featureless") ~ "FL",
      str_detect(learner_id, "ranger") ~ "RF",
      str_detect(learner_id, "glmnet") ~ "EN",
      TRUE ~ learner_id
    ),
    outcome = case_when(
      str_detect(task_id, regex("_pa_", ignore_case = TRUE)) ~ "Trait PA",
      str_detect(task_id, regex("_na_", ignore_case = TRUE)) ~ "Trait NA",
      TRUE ~ task_id
    ),
    context = case_when(
      str_detect(task_id, regex("private", ignore_case = TRUE)) ~ "Private",
      str_detect(task_id, regex("public", ignore_case = TRUE)) ~ "Public",
      TRUE ~ NA_character_
    ),
    feature_family = case_when(
      str_detect(task_id, regex("_word$", ignore_case = TRUE)) ~ "Word features",
      str_detect(task_id, regex("_emoji$", ignore_case = TRUE)) ~ "Emoji features",
      str_detect(task_id, regex("_typing$", ignore_case = TRUE)) ~ "Typing-dynamics features",
      TRUE ~ "Other"
    ),
    section = "Supplementary Feature-Family Analyses"
  ) %>%
  filter(feature_family != "Other", !is.na(context))

############################
#### 15) SUMMARISE FEATURE-FAMILY RESULTS
############################

family_results_sum <- family_results_long %>%
  group_by(section, outcome, context, feature_family, algo) %>%
  summarise(
    n_folds_total = n(),
    n_folds_valid_pearson = sum(!is.na(pearson)),
    pearson_coverage = n_folds_valid_pearson / n_folds_total,
    
    r_md  = safe_median(pearson),
    r_sd  = safe_sd(pearson),
    r_q25 = safe_quantile(pearson, .25),
    r_q75 = safe_quantile(pearson, .75),
    
    rsq_md = safe_median(regr.rsq),
    rsq_sd = safe_sd(regr.rsq),
    
    mae_md = safe_median(regr.mae),
    mae_sd = safe_sd(regr.mae),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        pearson_coverage,
        r_md, r_sd, r_q25, r_q75,
        rsq_md, rsq_sd,
        mae_md, mae_sd
      ),
      ~ round(.x, 3)
    )
  )

############################
#### 16) ORDER FEATURE-FAMILY ROWS
############################

family_table_df <- family_results_sum %>%
  mutate(
    outcome = factor(outcome, levels = c("Trait PA", "Trait NA")),
    context = factor(context, levels = c("Private", "Public")),
    feature_family = factor(
      feature_family,
      levels = c(
        "Word features",
        "Emoji features",
        "Typing-dynamics features"
      )
    ),
    algo = factor(algo, levels = c("RF"))
  ) %>%
  arrange(outcome, context, feature_family, algo) %>%
  mutate(
    `Outcome Variable` = as.character(outcome),
    `Context` = as.character(context),
    `Feature Family` = as.character(feature_family),
    `Algo.` = as.character(algo),
    `Valid folds` = paste0(n_folds_valid_pearson, "/", n_folds_total),
    `Pearson coverage` = pearson_coverage,
    `r (Md)` = r_md,
    `r (SD)` = r_sd,
    `r (Q1)` = r_q25,
    `r (Q3)` = r_q75,
    `R² (Md)` = rsq_md,
    `R² (SD)` = rsq_sd,
    `MAE (Md)` = mae_md,
    `MAE (SD)` = mae_sd
  ) %>%
  select(
    `Outcome Variable`, `Context`, `Feature Family`, `Algo.`,
    `Valid folds`, `Pearson coverage`,
    `r (Md)`, `r (SD)`, `r (Q1)`, `r (Q3)`,
    `R² (Md)`, `R² (SD)`, `MAE (Md)`, `MAE (SD)`
  )


############################
#### 17) CSV EXPORT
#### FEATURE-FAMILY RESULTS
############################

write.csv(
  family_table_df,
  file = "results/feature_family_prediction_performance_table.csv",
  row.names = FALSE,
  na = "NA"
)

############################
#### 18) OPTIONAL: COMBINED OBJECTS IN MEMORY
############################

results_sum_all <- bind_rows(
  results_sum,
  family_results_sum
)

results_table_all <- list(
  main_results = table_df,
  feature_family_results = family_table_df
)

##################################################
#### FIGURE 3: PREDICTION PERFORMANCE (VERTICAL)
##################################################

mes_fig <- c(
  list(msr_pearson),
  msrs(c("regr.rsq", "regr.mae", "regr.rmse"))
)

extract_bmr_results <- function(bmr, measures = mes_fig) {
  as.data.frame(bmr$score(measures))
}

bmr_results_folds_trait  <- extract_bmr_results(bmr_keyboardlanguage_trait)
bmr_results_folds_day    <- extract_bmr_results(bmr_keyboardlanguage_day)
bmr_results_folds_moment <- extract_bmr_results(bmr_keyboardlanguage_moment)
bmr_results_folds_age <- extract_bmr_results(bmr_keyboardlanguage_age)

pred_folds <- bind_rows(
  bmr_results_folds_trait,
  bmr_results_folds_day,
  bmr_results_folds_moment,
  bmr_results_folds_age 
)

pred_folds_plot <- pred_folds %>%
  filter(learner_id == "imputeoor.regr.ranger") %>%
  mutate(
    outcome = case_when(
      task_id == "keyboardlanguage_private_age" ~ "Age",
      task_id == "keyboardlanguage_public_age" ~ "Age",
      
      task_id == "keyboardlanguage_private_pa" ~ "Trait PA",
      task_id == "keyboardlanguage_public_pa" ~ "Trait PA",
      
      task_id == "keyboardlanguage_private_na" ~ "Trait NA",
      task_id == "keyboardlanguage_public_na" ~ "Trait NA",
      
      task_id == "keyboardlanguage_private_day_valence" ~ "Daily valence",
      task_id == "keyboardlanguage_public_day_valence" ~ "Daily valence",
      
      task_id == "keyboardlanguage_private_moment_valence" ~ "Momentary valence",
      task_id == "keyboardlanguage_public_moment_valence" ~ "Momentary valence",
      
      TRUE ~ NA_character_
    ),
    context = case_when(
      str_detect(task_id, "private") ~ "private",
      str_detect(task_id, "public") ~ "public",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(outcome), !is.na(context)) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("Age", "Trait NA", "Trait PA", "Daily valence", "Momentary valence")
    ),
    context = factor(context, levels = c("private", "public"))
  )

context_cols <- c(
  "private" = "#E69F00",
  "public"  = "#56B4E9"
)

############################
#### PERMUTATION TEST: PRIVATE > PUBLIC
############################

set.seed(42)
n_perm <- 10000

outcomes_to_test <- c("Trait PA", "Trait NA", "Daily valence", "Momentary valence")

perm_results <- lapply(outcomes_to_test, function(oc) {
  
  d <- pred_folds_plot %>%
    filter(outcome == oc, context %in% c("private", "public")) %>%
    select(pearson, context) %>%
    filter(!is.na(pearson))
  
  if (
    sum(d$context == "private") < 2 ||
    sum(d$context == "public") < 2
  ) {
    return(data.frame(
      outcome = oc,
      n_private = sum(d$context == "private"),
      n_public = sum(d$context == "public"),
      r_private = NA_real_,
      r_public = NA_real_,
      obs_diff = NA_real_,
      ci_lo = NA_real_,
      ci_hi = NA_real_,
      p_perm = NA_real_
    ))
  }
  
  obs_diff <- median(d$pearson[d$context == "private"], na.rm = TRUE) -
    median(d$pearson[d$context == "public"], na.rm = TRUE)
  
  null_diffs <- replicate(n_perm, {
    shuffled <- sample(d$context)
    median(d$pearson[shuffled == "private"], na.rm = TRUE) -
      median(d$pearson[shuffled == "public"], na.rm = TRUE)
  })
  
  p_val <- mean(null_diffs >= obs_diff)
  
  boot_diffs <- replicate(n_perm, {
    priv <- d$pearson[d$context == "private"]
    pub  <- d$pearson[d$context == "public"]
    
    median(sample(priv, replace = TRUE), na.rm = TRUE) -
      median(sample(pub, replace = TRUE), na.rm = TRUE)
  })
  
  ci <- quantile(boot_diffs, c(0.025, 0.975), na.rm = TRUE)
  
  data.frame(
    outcome = oc,
    n_private = sum(d$context == "private"),
    n_public = sum(d$context == "public"),
    r_private = round(median(d$pearson[d$context == "private"], na.rm = TRUE), 3),
    r_public = round(median(d$pearson[d$context == "public"], na.rm = TRUE), 3),
    obs_diff = round(obs_diff, 3),
    ci_lo = round(ci[1], 3),
    ci_hi = round(ci[2], 3),
    p_perm = p_val
  )
})

perm_table <- bind_rows(perm_results) %>%
  mutate(
    p_holm = p.adjust(p_perm, method = "holm")
  )

print(perm_table, row.names = FALSE)

write.csv(
  perm_table,
  "results/permutation_test_private_vs_public.csv",
  row.names = FALSE
)

#### CREATE FIG 3 #####

base_theme <- theme_custom(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text = element_text(size = 10),
    legend.margin = margin(b = -2),
    plot.margin = margin(8, 10, 6, 6)
  )

pred_plot_sum <- pred_folds_plot %>%
  group_by(outcome, context) %>%
  summarise(
    n_folds_total = n(),
    n_folds_valid_pearson = sum(!is.na(pearson)),
    pearson_coverage = n_folds_valid_pearson / n_folds_total,
    r_md  = safe_median(pearson),
    r_q25 = safe_quantile(pearson, 0.25),
    r_q75 = safe_quantile(pearson, 0.75),
    .groups = "drop"
  ) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("Age", "Trait NA", "Trait PA", "Daily valence", "Momentary valence")
    ),
    context = factor(context, levels = c("private", "public")),
    grp = interaction(outcome, context, drop = TRUE)
  )

############################
#### SIGNIFICANCE STARS FOR FIGURE 3
############################

p_to_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  )
}

sig_df_fig3 <- perm_table %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("Age", "Trait NA", "Trait PA", "Daily valence", "Momentary valence")
    ),
    stars = p_to_stars(p_holm)
  ) %>%
  filter(stars != "") %>%
  left_join(
    pred_plot_sum %>%
      filter(context %in% c("private", "public")) %>%
      group_by(outcome) %>%
      summarise(
        y_pos = max(r_q75, r_md, na.rm = TRUE) + 0.08,
        .groups = "drop"
      ),
    by = "outcome"
  )

pd <- position_dodge(width = 0.55)

fig3_pred <- ggplot(
  pred_plot_sum,
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
  geom_text(
    data = sig_df_fig3,
    aes(
      x = outcome,
      y = y_pos,
      label = stars
    ),
    inherit.aes = FALSE,
    size = 5.2,
    fontface = "bold",
    color = "black",
    vjust = 0
  ) +
  scale_color_manual(
    values = context_cols,
    name = "Context",
    labels = c("private" = "Private", "public" = "Public")
  ) +
  scale_shape_manual(
    values = c("private" = 16, "public" = 17),
    name = "Context",
    labels = c("private" = "Private", "public" = "Public")
  ) +
  scale_y_continuous(
    limits = c(-0.60, 0.80),
    breaks = seq(-0.60, 0.80, by = 0.20),
    labels = scales::label_number(accuracy = 0.01, trim = TRUE),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  labs(
    x = NULL,
    y = "Pearson correlation"
  ) +
  base_theme +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "top",
    legend.box = "vertical"
  )

fig3_pred

ggsave(
  filename = "figures/figure3_prediction_performance_dot_iqr.png",
  plot = fig3_pred,
  width = 7.2,
  height = 5.4,
  dpi = 300
)

# finish