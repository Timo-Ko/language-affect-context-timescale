### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "mlr3", "mlr3learners","mlr3pipelines", "mlr3tuning", "mlr3filters", "ranger", "glmnet", "future", "remotes")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

# load required functions
source("r_code/functions/sign_test_folds.R")
source("r_code/functions/bmr_results.R")

### READ IN & PREPARE DATA ####

## read in data frames 

# trait
keyboard_data_trait_ml <- readRDS(file="data/results/ml/keyboard_data_trait_ml.rds") 

keyboard_data_trait_private_ml <- readRDS(file="data/results/ml/keyboard_data_trait_private_ml.rds") 

keyboard_data_trait_public_ml <- readRDS(file="data/results/ml/keyboard_data_trait_public_ml.rds") 

# week
keyboard_data_week_ema_ml <- readRDS(file="data/results/ml/keyboard_data_week_ema_ml.rds") 

# day
keyboard_data_day_ema_ml <- readRDS(file="data/results/ml/keyboard_data_day_ema_ml.rds") 

# moment
keyboard_data_moment_ema_ml <- readRDS(file="data/results/ml/keyboard_data_moment_ema_ml.rds") 

#### CREATE TASKS: TRAIT AFFECT ####

## sanity check predictions

# age
keyboard_data_trait_ml_age <- keyboard_data_trait_ml[!is.na(keyboard_data_trait_ml$age),] # create new df with no missing for gender

keyboardlanguage_age = TaskRegr$new(id = "keyboardlanguage_age", 
                                   backend = keyboard_data_trait_ml_age[,c(which(colnames(keyboard_data_trait_ml_age)=="age"),  
                                                                   which(colnames(keyboard_data_trait_ml_age)=="duration_avg"):last(ncol(keyboard_data_trait_ml_age)))], 
                                   target = "age")

# gender
keyboard_data_trait_ml_gender <- keyboard_data_trait_ml[!is.na(keyboard_data_trait_ml$gender),] # create new df with no missing for gender
keyboard_data_trait_ml_gender$gender <- as.factor(keyboard_data_trait_ml_gender$gender) # convert gender to factor

keyboardlanguage_gender = TaskClassif$new(id = "keyboardlanguage_gender", 
                                    backend = keyboard_data_trait_ml_gender[,c(which(colnames(keyboard_data_trait_ml_gender)=="gender"),  
                                                                    which(colnames(keyboard_data_trait_ml_gender)=="duration_avg"):last(ncol(keyboard_data_trait_ml_gender)))], 
                                    target = "gender")

## trait

# positive affect
keyboardlanguage_pa = TaskRegr$new(id = "keyboardlanguage_pa", 
                                   backend = keyboard_data_trait_ml[,c(which(colnames(keyboard_data_trait_ml)=="pa_panas"),  
                                                                which(colnames(keyboard_data_trait_ml)=="duration_avg"):last(ncol(keyboard_data_trait_ml)))], 
                                   target = "pa_panas")

# negative affect
keyboardlanguage_na = TaskRegr$new(id = "keyboardlanguage_na", 
                                   backend = keyboard_data_trait_ml[,c(which(colnames(keyboard_data_trait_ml)=="na_panas"),  
                                                                which(colnames(keyboard_data_trait_ml)=="duration_avg"):last(ncol(keyboard_data_trait_ml)))], 
                                   target = "na_panas")

## trait - gender

keyboard_data_trait_men_ml <- keyboard_data_trait_ml %>% filter(gender == 1)
keyboard_data_trait_women_ml <- keyboard_data_trait_ml %>% filter(gender == 2)

# positive affect - men 
keyboardlanguage_men_pa = TaskRegr$new(id = "keyboardlanguage_men_pa", 
                                   backend = keyboard_data_trait_men_ml[,c(which(colnames(keyboard_data_trait_men_ml)=="pa_panas"),  
                                                                       which(colnames(keyboard_data_trait_ml)=="duration_avg"):last(ncol(keyboard_data_trait_men_ml)))], 
                                   target = "pa_panas")

# negative affect - men
keyboardlanguage_men_na = TaskRegr$new(id = "keyboardlanguage_men_na", 
                                   backend = keyboard_data_trait_men_ml[,c(which(colnames(keyboard_data_trait_men_ml)=="na_panas"),  
                                                                       which(colnames(keyboard_data_trait_men_ml)=="duration_avg"):last(ncol(keyboard_data_trait_men_ml)))], 
                                   target = "na_panas")

# positive affect - women 
keyboardlanguage_women_pa = TaskRegr$new(id = "keyboardlanguage_women_pa", 
                                       backend = keyboard_data_trait_women_ml[,c(which(colnames(keyboard_data_trait_women_ml)=="pa_panas"),  
                                                                               which(colnames(keyboard_data_trait_ml)=="duration_avg"):last(ncol(keyboard_data_trait_women_ml)))], 
                                       target = "pa_panas")

# negative affect - women
keyboardlanguage_women_na = TaskRegr$new(id = "keyboardlanguage_women_na", 
                                       backend = keyboard_data_trait_women_ml[,c(which(colnames(keyboard_data_trait_women_ml)=="na_panas"),  
                                                                               which(colnames(keyboard_data_trait_women_ml)=="duration_avg"):last(ncol(keyboard_data_trait_women_ml)))], 
                                       target = "na_panas")


## trait - private

# positive affect
keyboardlanguage_private_pa = TaskRegr$new(id = "keyboardlanguage_private_pa", 
                                   backend = keyboard_data_trait_private_ml[,c(which(colnames(keyboard_data_trait_private_ml)=="pa_panas"),  
                                                                   which(colnames(keyboard_data_trait_private_ml)=="duration_avg"):last(ncol(keyboard_data_trait_private_ml)))], 
                                   target = "pa_panas")

# negative affect
keyboardlanguage_private_na = TaskRegr$new(id = "keyboardlanguage_private_na", 
                                   backend = keyboard_data_trait_private_ml[,c(which(colnames(keyboard_data_trait_private_ml)=="na_panas"),  
                                                                   which(colnames(keyboard_data_trait_private_ml)=="duration_avg"):last(ncol(keyboard_data_trait_private_ml)))], 
                                   target = "na_panas")

## trait - public

# positive affect
keyboardlanguage_public_pa = TaskRegr$new(id = "keyboardlanguage_public_pa", 
                                           backend = keyboard_data_trait_public_ml[,c(which(colnames(keyboard_data_trait_public_ml)=="pa_panas"),  
                                                                                       which(colnames(keyboard_data_trait_public_ml)=="duration_avg"):last(ncol(keyboard_data_trait_public_ml)))], 
                                           target = "pa_panas")

# negative affect
keyboardlanguage_public_na = TaskRegr$new(id = "keyboardlanguage_public_na", 
                                           backend = keyboard_data_trait_public_ml[,c(which(colnames(keyboard_data_trait_public_ml)=="na_panas"),  
                                                                                       which(colnames(keyboard_data_trait_public_ml)=="duration_avg"):last(ncol(keyboard_data_trait_public_ml)))], 
                                           target = "na_panas")

## week

# weekly valence
keyboardlanguage_week_valence = TaskRegr$new(id = "keyboardlanguage_week_valence", 
                                          backend = keyboard_data_week_ema_ml[,c(which(colnames(keyboard_data_week_ema_ml)=="user_id"),
                                                                                  which(colnames(keyboard_data_week_ema_ml)=="valence_week"),  
                                                                                 which(colnames(keyboard_data_week_ema_ml)=="duration_avg"):last(ncol(keyboard_data_week_ema_ml)))], 
                                          target = "valence_week")

# weekly arousal (supplement)
keyboardlanguage_week_arousal = TaskRegr$new(id = "keyboardlanguage_week_arousal", 
                                          backend = keyboard_data_week_ema_ml[,c(which(colnames(keyboard_data_week_ema_ml)=="user_id"),
                                                                                  which(colnames(keyboard_data_week_ema_ml)=="arousal_week"),  
                                                                                 which(colnames(keyboard_data_week_ema_ml)=="duration_avg"):last(ncol(keyboard_data_week_ema_ml)))], 
                                          target = "arousal_week")

## day

# daily valence
keyboardlanguage_day_valence = TaskRegr$new(id = "keyboardlanguage_day_valence", 
                                          backend = keyboard_data_day_ema_ml[,c(which(colnames(keyboard_data_day_ema_ml)=="user_id"),
                                                                                 which(colnames(keyboard_data_day_ema_ml)=="valence_day"),  
                                                                       which(colnames(keyboard_data_day_ema_ml)=="duration_avg"):last(ncol(keyboard_data_day_ema_ml)))], 
                                          target = "valence_day")

# daily arousal (supplement)
keyboardlanguage_day_arousal = TaskRegr$new(id = "keyboardlanguage_day_arousal", 
                                          backend = keyboard_data_day_ema_ml[,c(which(colnames(keyboard_data_day_ema_ml)=="user_id"),
                                                                                 which(colnames(keyboard_data_day_ema_ml)=="arousal_day"),  
                                                                       which(colnames(keyboard_data_day_ema_ml)=="duration_avg"):last(ncol(keyboard_data_day_ema_ml)))], 
                                          target = "arousal_day")

## moment

# raw valence score

keyboardlanguage_moment_valence = TaskRegr$new(id = "keyboardlanguage_moment_valence", 
                                                 backend = keyboard_data_moment_ema_ml[,c(which(colnames(keyboard_data_moment_ema_ml)=="user_id"),  
                                                                                             which(colnames(keyboard_data_moment_ema_ml)=="valence"),  
                                                                                          which(colnames(keyboard_data_moment_ema_ml)=="duration_avg"):last(ncol(keyboard_data_moment_ema_ml)))], 
                                                 target = "valence")

# raw arousal score (supplement)

keyboardlanguage_moment_arousal = TaskRegr$new(id = "keyboardlanguage_moment_arousal", 
                                                 backend = keyboard_data_moment_ema_ml[,c(which(colnames(keyboard_data_moment_ema_ml)=="user_id"),  
                                                                                             which(colnames(keyboard_data_moment_ema_ml)=="arousal"),  
                                                                                             which(colnames(keyboard_data_moment_ema_ml)=="duration_avg"):last(ncol(keyboard_data_moment_ema_ml)))], 
                                                 target = "arousal")


# valence fluctuation from baseline (supplement)

keyboardlanguage_moment_valence_diff = TaskRegr$new(id = "keyboardlanguage_moment_valence_diff", 
                                                 backend = keyboard_data_moment_ema_ml[,c(which(colnames(keyboard_data_moment_ema_ml)=="user_id"),  
                                                                                             which(colnames(keyboard_data_moment_ema_ml)=="valence_diff"),  
                                                                                             which(colnames(keyboard_data_moment_ema_ml)=="duration_avg"):last(ncol(keyboard_data_moment_ema_ml)))], 
                                                 target = "valence_diff")


# arousal fluctuation from baseline (supplement)

keyboardlanguage_moment_arousal_diff = TaskRegr$new(id = "keyboardlanguage_moment_arousal_diff", 
                                                      backend = keyboard_data_moment_ema_ml[,c(which(colnames(keyboard_data_moment_ema_ml)=="user_id"),  
                                                                                                  which(colnames(keyboard_data_moment_ema_ml)=="arousal_diff"),  
                                                                                                  which(colnames(keyboard_data_moment_ema_ml)=="duration_avg"):last(ncol(keyboard_data_moment_ema_ml)))], 
                                                      target = "arousal_diff")

#### ADD BLOCKING BY PARTICIPANT ####

# add blocking by participant across prediction tasks with multiple instances per participant

# Use participant id column as block factor

# week
keyboardlanguage_week_valence$col_roles$group = "user_id"
keyboardlanguage_week_arousal$col_roles$group = "user_id"

# day
keyboardlanguage_day_valence$col_roles$group = "user_id"
keyboardlanguage_day_arousal$col_roles$group = "user_id"

# moment

keyboardlanguage_moment_valence$col_roles$group = "user_id"
keyboardlanguage_moment_valence_diff$col_roles$group = "user_id"
keyboardlanguage_moment_arousal$col_roles$group = "user_id"
keyboardlanguage_moment_arousal_diff$col_roles$group = "user_id"

# Remove user id from feature space

# week
keyboardlanguage_week_valence$col_roles$feature = setdiff(keyboardlanguage_week_valence$col_roles$feature, "user_id")
keyboardlanguage_week_arousal$col_roles$feature = setdiff(keyboardlanguage_week_arousal$col_roles$feature, "user_id")

# day
keyboardlanguage_day_valence$col_roles$feature = setdiff(keyboardlanguage_day_valence$col_roles$feature, "user_id")
keyboardlanguage_day_arousal$col_roles$feature = setdiff(keyboardlanguage_day_arousal$col_roles$feature, "user_id")

# moment

keyboardlanguage_moment_valence$col_roles$feature = setdiff(keyboardlanguage_moment_valence$col_roles$feature, "user_id")
keyboardlanguage_moment_valence_diff$col_roles$feature = setdiff(keyboardlanguage_moment_valence_diff$col_roles$feature, "user_id")
keyboardlanguage_moment_arousal$col_roles$feature = setdiff(keyboardlanguage_moment_arousal$col_roles$feature, "user_id")
keyboardlanguage_moment_arousal_diff$col_roles$feature = setdiff(keyboardlanguage_moment_arousal_diff$col_roles$feature, "user_id")

#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", 
             mtry = to_tune(1, 50),
             num.trees =1000) # random forest
lrn_rr = lrn("regr.cv_glmnet",
             alpha= to_tune(0,1)) # lasso

# enable parallelization
set_threads(lrn_fl, n = detectCores())
set_threads(lrn_rr, n = detectCores())
set_threads(lrn_rf, n = detectCores())

#### HYPERPARAMETER TUNING ####

at_rf = AutoTuner$new(
  learner = lrn_rf,
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("random_search"),
  store_models = TRUE
)

at_rr = AutoTuner$new(
  learner = lrn_rr,
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("random_search"),
  store_models = TRUE
)


#### RESAMPLING ####

resampling = rsmp("repeated_cv", folds = 10L, repeats = 1L)

#### SET PERFORMANCE MEASURES ####

# measures for benchmark
mes = msrs(c("regr.rsq", "regr.srho"))

### PREPROCESSING IN CV ####

# target dependent preprocessing

#po_scale = po("scale") # scale features

po_impute = po("imputemedian") # impute NAs with median

#po_filter = po("filter", filter = flt("correlation"), filter.frac = 0.2) # filter features 

# combine training with pre-processing
lrn_rf_po = po_impute  %>>% at_rf 
lrn_rr_po = po_impute  %>>% at_rr 

#### RUN BENCHMARKS ####

## general settings

# avoid console output from mlr3tuning
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## sanity check predictions

# age
bmgrid_keyboardlanguage_age = benchmark_grid(
  task = c(keyboardlanguage_age),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_age = benchmark(bmgrid_keyboardlanguage_age, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_age, "data/results/predictions/bmr_keyboardlanguage_age.RData") # save results

# gender
bmgrid_keyboardlanguage_gender = benchmark_grid(
  task = c(keyboardlanguage_gender),
  learner = list(lrn("classif.featureless", predict_type = "prob"), po_scale %>>% po_impute %>>% lrn("classif.ranger", num.trees =1000, predict_type = "prob"), po_scale %>>% po_impute %>>% lrn("classif.cv_glmnet", predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_gender = benchmark(bmgrid_keyboardlanguage_gender, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_gender, "data/results/predictions/bmr_keyboardlanguage_gender.RData") # save results

## trait - all, private and public

bmgrid_keyboardlanguage_trait = benchmark_grid(
  task = c(keyboardlanguage_pa,
           keyboardlanguage_na,
           keyboardlanguage_men_pa,
           keyboardlanguage_men_na,
           keyboardlanguage_women_pa,
           keyboardlanguage_women_na,
           keyboardlanguage_private_pa,
           keyboardlanguage_private_na,
           keyboardlanguage_public_pa,
           keyboardlanguage_public_na),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_trait = benchmark(bmgrid_keyboardlanguage_trait, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_trait, "data/results/predictions/bmr_keyboardlanguage_trait.RData") # save results

# week

bmgrid_keyboardlanguage_week = benchmark_grid(
  task = c(keyboardlanguage_week_valence,
           keyboardlanguage_week_arousal),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_week = benchmark(bmgrid_keyboardlanguage_week, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_week, "data/results/predictions/bmr_keyboardlanguage_week.RData") # save results

# day 

bmgrid_keyboardlanguage_day = benchmark_grid(
  task = c(keyboardlanguage_day_valence,
           keyboardlanguage_day_arousal),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_day = benchmark(bmgrid_keyboardlanguage_day, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_day, "data/results/predictions/bmr_keyboardlanguage_day.RData") # save results

# moment

bmgrid_keyboardlanguage_moment = benchmark_grid(
  task = c(keyboardlanguage_moment_valence#,
           #keyboardlanguage_moment_valence_diff#,
           #keyboardlanguage_moment_arousal,
           #keyboardlanguage_moment_arousal_diff
           ),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_moment = benchmark(bmgrid_keyboardlanguage_moment, store_models = F, store_backends = F) # execute the benchmark

# save results
saveRDS(bmr_keyboardlanguage_moment, "data/results/predictions/bmr_keyboardlanguage_moment.RData")

#### BENCHMARK RESULTS ####

## read in benchmark results

# age
bmr_keyboardlanguage_age <- readRDS("data/results/predictions/bmr_keyboardlanguage_age.RData")

# gender
bmr_keyboardlanguage_gender <- readRDS("data/results/predictions/bmr_keyboardlanguage_gender.RData")

# trait
bmr_keyboardlanguage_trait <- readRDS("data/results/predictions/bmr_keyboardlanguage_trait.RData")

# week
bmr_keyboardlanguage_week <- readRDS("data/results/predictions/bmr_keyboardlanguage_week.RData")

# day
bmr_keyboardlanguage_day <- readRDS("data/results/predictions/bmr_keyboardlanguage_day.RData")

# moment
bmr_keyboardlanguage_moment <- readRDS("data/results/predictions/bmr_keyboardlanguage_moment.RData")

## view aggregated performance for different tasks

bmr_gender_results <- as.data.frame(bmr_keyboardlanguage_gender$aggregate(msrs(c("classif.acc", "classif.auc", "classif.fbeta")))) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(task_id, learner_id, regr.rsq, regr.srho)

bmr_age_results <- as.data.frame(bmr_keyboardlanguage_age$aggregate(mes)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(task_id, learner_id, regr.rsq, regr.srho)

bmr_trait_results <- as.data.frame(bmr_keyboardlanguage_trait$aggregate(mes)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(task_id, learner_id, regr.rsq, regr.srho)

bmr_week_results <- as.data.frame(bmr_keyboardlanguage_week$aggregate(mes)) %>%
  filter(task_id == "keyboardlanguage_week_valence") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(task_id, learner_id, regr.rsq, regr.srho)

bmr_day_results <- as.data.frame(bmr_keyboardlanguage_day$aggregate(mes)) %>%
  filter(task_id == "keyboardlanguage_day_valence") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(task_id, learner_id, regr.rsq, regr.srho)

bmr_moment_results <- as.data.frame(bmr_keyboardlanguage_moment$aggregate(mes)) %>%
  filter(task_id == "keyboardlanguage_moment_valence") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  select(task_id, learner_id, regr.rsq, regr.srho)

# create single table with results

bmr_results_overview <- rbind(bmr_age_results, bmr_trait_results, bmr_week_results, bmr_day_results, bmr_moment_results)

# remove the featureless learner from table
bmr_results_overview = bmr_results_overview %>% filter(learner_id != "regr.featureless")

# save table 
write.csv2(bmr_results_overview, "data/results/predictions/bmr_results_overview.csv")

## create barplots to show performance across folds

## retrieve benchmark results across tasks and learners for single cv folds (this is needed for barplots)

bmr_results_folds_age <- extract_bmr_results(bmr_keyboardlanguage_age, mes)
bmr_results_folds_trait <- extract_bmr_results(bmr_keyboardlanguage_trait, mes)
bmr_results_folds_week <- extract_bmr_results(bmr_keyboardlanguage_week, mes)
bmr_results_folds_day <- extract_bmr_results(bmr_keyboardlanguage_day, mes)
bmr_results_folds_moment <- extract_bmr_results(bmr_keyboardlanguage_moment, mes)

# create overview table of performance incl. significance tests
pred_table_age <- results_table(affect_language_ml, bmr_results_folds_age)
pred_table_trait <- results_table(affect_language_ml, bmr_results_folds_trait)
pred_table_week <- results_table(affect_language_es_week_ml, bmr_results_folds_week)
pred_table_day <- results_table(affect_language_es_day_ml, bmr_results_folds_day)
pred_table_moment <- results_table(affect_language_es_threehrs_ml, bmr_results_folds_moment)

# add column with p values
bmr_results_folds_age <- dplyr::left_join(bmr_results_folds_age, pred_table_trait[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))
bmr_results_folds_trait <- dplyr::left_join(bmr_results_folds_trait, pred_table_trait[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))
bmr_results_folds_week <- dplyr::left_join(bmr_results_folds_week, pred_table_week[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))
bmr_results_folds_day <- dplyr::left_join(bmr_results_folds_day, pred_table_day[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))
bmr_results_folds_moment <- dplyr::left_join(bmr_results_folds_moment, pred_table_moment[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))

# rbind results together
bmr_results_folds <- rbind(#bmr_results_folds_age,
                           bmr_results_folds_trait, 
                           bmr_results_folds_week, 
                           bmr_results_folds_day, 
                           bmr_results_folds_moment
                           )

# create significance column
bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq_corrected >= 0.05 | is.na(bmr_results_folds$p_rsq_corrected), "no", "yes"))

# none are significant!

# rename 
bmr_results_folds <- bmr_results_folds %>% 
  mutate(learner_id = case_when(
    learner_id == "regr.featureless" ~    "Baseline",
    learner_id == "scale.imputemedian.regr.ranger" ~ "Random Forest",
    learner_id == "scale.imputemedian.regr.cv_glmnet" ~ "LASSO")) %>% 
  mutate(task_id = case_when(
    #task_id == "keyboardlanguage_age" ~ "Age",
    task_id == "keyboardlanguage_pa" ~ "Positive Trait Affect (all text)",
    task_id == "keyboardlanguage_na" ~ "Negative Trait Affect (all text)", 
    task_id == "keyboardlanguage_private_pa" ~ "Positive Trait Affect (private)",
    task_id == "keyboardlanguage_private_na" ~ "Negative Trait Affect (private)", 
    task_id == "keyboardlanguage_public_pa" ~ "Positive Trait Affect (public)",
    task_id == "keyboardlanguage_public_na" ~ "Negative Trait Affect (public)", 
    task_id == "keyboardlanguage_week_valence" ~ "Valence (week)", 
    task_id == "keyboardlanguage_day_valence" ~ "Valence (day)", 
    task_id == "keyboardlanguage_threehrs_valence" ~ "Valence (moment)"
    ))


bmr_keyboardlanguage_plot <- ggplot(bmr_results_folds, aes(x= factor(task_id, levels = c("Arousal Fluct. (moment)" ,"Arousal (moment)" , "Valence Fluct. (moment)", "Valence (moment)", "Arousal (day)", "Valence (day)", "Arousal (week)", "Valence (week)", "Negative Trait Affect (public)", "Positive Trait Affect (public)", "Negative Trait Affect (private)", "Positive Trait Affect (private)", "Negative Trait Affect (all text)", "Positive Trait Affect (all text)")) , y= regr.rsq, color = significance, shape = learner_id)) + 
  geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +
  scale_x_discrete(element_blank()) +
  scale_y_continuous(name = bquote(paste("Out-of-sample ", "R"^2)), limits = c(-0.15, 0.15)) + 
  geom_hline(yintercept=0, linetype='dotted') +
  theme_minimal(base_size = 25) +
  labs(colour = "Significance", shape = "Algorithm") + # change legend title
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + # rotate x axis labels
  coord_flip() + # flip coordinates
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  theme(legend.position="top", legend.key.size = unit(1, "cm"))

bmr_keyboardlanguage_plot

# save figure

png(file="figures/bmr_keyboardlanguage_plot.png",width=1250, height=2000)

bmr_keyboardlanguage_plot

dev.off()

### FINISH