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

keyboard_data_trait_ml <- readRDS(file = "data/results/keyboard_data_trait_ml.rds")
keyboard_data_day_ml   <- readRDS(file = "data/results/keyboard_data_day_ml.rds")
keyboard_data_ema_ml   <- readRDS(file = "data/results/keyboard_data_ema_ml.rds")

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

# finish