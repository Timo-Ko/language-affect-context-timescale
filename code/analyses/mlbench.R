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
affect_language_ml <- readRDS(file="data/affect_language_features/affect_language_ml.RData") 

# trait - private
affect_language_private_ml <- readRDS(file="data/affect_language_features/affect_language_private_ml.RData") 

# trait - public
affect_language_public_ml <- readRDS(file="data/affect_language_features/affect_language_public_ml.RData") 

# month
#affect_language_month_ml <- readRDS(file="data/affect_language_month_ml.RData") 

# week
affect_language_es_week_ml <- readRDS(file="data/affect_language_features/affect_language_es_week_ml.RData") 

# day
affect_language_es_day_ml <- readRDS(file="data/affect_language_features/affect_language_es_day_ml.RData") 

# moment
affect_language_es_threehrs_ml <- readRDS(file="data/affect_language_features/affect_language_es_threehrs_ml.RData") 

#### CREATE TASKS: TRAIT AFFECT ####

## sanity check predictions

# age
affect_language_ml_age <- affect_language_ml[!is.na(affect_language_ml$Demo_A1),] # create new df with no missing for gender

keyboardlanguage_age = TaskRegr$new(id = "keyboardlanguage_age", 
                                   backend = affect_language_ml_age[,c(which(colnames(affect_language_ml_age)=="Demo_A1"),  
                                                                   which(colnames(affect_language_ml_age)=="typing_sessions_duration_md"):last(ncol(affect_language_ml_age)))], 
                                   target = "Demo_A1")

# gender
affect_language_ml_gender <- affect_language_ml[!is.na(affect_language_ml$Demo_GE1),] # create new df with no missing for gender
affect_language_ml_gender$Demo_GE1 <- as.factor(affect_language_ml_gender$Demo_GE1) # convert gender to factor

keyboardlanguage_gender = TaskClassif$new(id = "keyboardlanguage_gender", 
                                    backend = affect_language_ml_gender[,c(which(colnames(affect_language_ml_gender)=="Demo_GE1"),  
                                                                    which(colnames(affect_language_ml_gender)=="typing_sessions_duration_md"):last(ncol(affect_language_ml_gender)))], 
                                    target = "Demo_GE1")

## trait

# positive affect
keyboardlanguage_pa = TaskRegr$new(id = "keyboardlanguage_pa", 
                                   backend = affect_language_ml[,c(which(colnames(affect_language_ml)=="pa_panas"),  
                                                                which(colnames(affect_language_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_ml)))], 
                                   target = "pa_panas")

# negative affect
keyboardlanguage_na = TaskRegr$new(id = "keyboardlanguage_na", 
                                   backend = affect_language_ml[,c(which(colnames(affect_language_ml)=="pa_panas"),  
                                                                which(colnames(affect_language_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_ml)))], 
                                   target = "pa_panas")

## trait - private

# positive affect
keyboardlanguage_private_pa = TaskRegr$new(id = "keyboardlanguage_private_pa", 
                                   backend = affect_language_private_ml[,c(which(colnames(affect_language_private_ml)=="pa_panas"),  
                                                                   which(colnames(affect_language_private_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_private_ml)))], 
                                   target = "pa_panas")

# negative affect
keyboardlanguage_private_na = TaskRegr$new(id = "keyboardlanguage_private_na", 
                                   backend = affect_language_private_ml[,c(which(colnames(affect_language_private_ml)=="pa_panas"),  
                                                                   which(colnames(affect_language_private_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_private_ml)))], 
                                   target = "pa_panas")

## trait - public

# positive affect
keyboardlanguage_public_pa = TaskRegr$new(id = "keyboardlanguage_public_pa", 
                                   backend = affect_language_public_ml[,c(which(colnames(affect_language_public_ml)=="pa_panas"),  
                                                                   which(colnames(affect_language_public_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_public_ml)))], 
                                   target = "pa_panas")

# negative affect
keyboardlanguage_public_na = TaskRegr$new(id = "keyboardlanguage_public_na", 
                                   backend = affect_language_public_ml[,c(which(colnames(affect_language_public_ml)=="pa_panas"),  
                                                                   which(colnames(affect_language_public_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_public_ml)))], 
                                   target = "pa_panas")


# ## month
# 
# # valence
# keyboardlanguage_month_va = TaskRegr$new(id = "keyboardlanguage_month_va", 
#                                    backend = affect_language_month_ml[,c(which(colnames(affect_language_month_ml)=="p_0001"),
#                                                                          which(colnames(affect_language_month_ml)=="va_panava"),  
#                                                                    which(colnames(affect_language_month_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_month_ml)))], 
#                                    target = "va_panava")
# 
# # positive activation
# keyboardlanguage_month_pa = TaskRegr$new(id = "keyboardlanguage_month_pa", 
#                                    backend = affect_language_month_ml[,c(which(colnames(affect_language_month_ml)=="p_0001"),
#                                                                          which(colnames(affect_language_month_ml)=="pa_panava"),  
#                                                                    which(colnames(affect_language_month_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_month_ml)))], 
#                                    target = "pa_panava")
# 
# # negative activation
# keyboardlanguage_month_na = TaskRegr$new(id = "keyboardlanguage_month_na", 
#                                    backend = affect_language_month_ml[,c(which(colnames(affect_language_month_ml)=="p_0001"),
#                                                                          which(colnames(affect_language_month_ml)=="na_panava"),  
#                                                                    which(colnames(affect_language_month_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_month_ml)))], 
#                                    target = "na_panava")

## week

# weekly valence
keyboardlanguage_week_valence = TaskRegr$new(id = "keyboardlanguage_week_valence", 
                                          backend = affect_language_es_week_ml[,c(which(colnames(affect_language_es_week_ml)=="user_id"),
                                                                                  which(colnames(affect_language_es_week_ml)=="valence_week"),  
                                                                                 which(colnames(affect_language_es_week_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_week_ml)))], 
                                          target = "valence_week")

# weekly arousal
keyboardlanguage_week_arousal = TaskRegr$new(id = "keyboardlanguage_week_arousal", 
                                          backend = affect_language_es_week_ml[,c(which(colnames(affect_language_es_week_ml)=="user_id"),
                                                                                  which(colnames(affect_language_es_week_ml)=="arousal_week"),  
                                                                                 which(colnames(affect_language_es_week_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_week_ml)))], 
                                          target = "arousal_week")

## day

# daily valence
keyboardlanguage_day_valence = TaskRegr$new(id = "keyboardlanguage_day_valence", 
                                          backend = affect_language_es_day_ml[,c(which(colnames(affect_language_es_day_ml)=="user_id"),
                                                                                 which(colnames(affect_language_es_day_ml)=="valence_day"),  
                                                                       which(colnames(affect_language_es_day_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_day_ml)))], 
                                          target = "valence_day")

# daily arousal
keyboardlanguage_day_arousal = TaskRegr$new(id = "keyboardlanguage_day_arousal", 
                                          backend = affect_language_es_day_ml[,c(which(colnames(affect_language_es_day_ml)=="user_id"),
                                                                                 which(colnames(affect_language_es_day_ml)=="arousal_day"),  
                                                                       which(colnames(affect_language_es_day_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_day_ml)))], 
                                          target = "arousal_day")

## moment

# raw valence score

keyboardlanguage_threehrs_valence = TaskRegr$new(id = "keyboardlanguage_threehrs_valence", 
                                                 backend = affect_language_es_threehrs_ml[,c(which(colnames(affect_language_es_threehrs_ml)=="user_id"),  
                                                                                             which(colnames(affect_language_es_threehrs_ml)=="valence"),  
                                                                                          which(colnames(affect_language_es_threehrs_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_threehrs_ml)))], 
                                                 target = "valence")

# raw arousal score

keyboardlanguage_threehrs_arousal = TaskRegr$new(id = "keyboardlanguage_threehrs_arousal", 
                                                 backend = affect_language_es_threehrs_ml[,c(which(colnames(affect_language_es_threehrs_ml)=="user_id"),  
                                                                                             which(colnames(affect_language_es_threehrs_ml)=="arousal"),  
                                                                                             which(colnames(affect_language_es_threehrs_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_threehrs_ml)))], 
                                                 target = "arousal")


# valence fluctuation from baseline

keyboardlanguage_threehrs_valence_diff = TaskRegr$new(id = "keyboardlanguage_threehrs_valence_diff", 
                                                 backend = affect_language_es_threehrs_ml[,c(which(colnames(affect_language_es_threehrs_ml)=="user_id"),  
                                                                                             which(colnames(affect_language_es_threehrs_ml)=="diff_valence"),  
                                                                                             which(colnames(affect_language_es_threehrs_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_threehrs_ml)))], 
                                                 target = "diff_valence")


# arousal fluctuation from baseline

keyboardlanguage_threehrs_arousal_diff = TaskRegr$new(id = "keyboardlanguage_threehrs_arousal_diff", 
                                                      backend = affect_language_es_threehrs_ml[,c(which(colnames(affect_language_es_threehrs_ml)=="user_id"),  
                                                                                                  which(colnames(affect_language_es_threehrs_ml)=="diff_arousal"),  
                                                                                                  which(colnames(affect_language_es_threehrs_ml)=="typing_sessions_duration_md"):last(ncol(affect_language_es_threehrs_ml)))], 
                                                      target = "diff_arousal")

#### ADD BLOCKING BY PARTICIPANT ####

# add blocking by participant across prediction tasks with multiple instances per participant

# Use participant id column as block factor

# # month
# keyboardlanguage_month_va$col_roles$group = "p_0001"
# keyboardlanguage_month_pa$col_roles$group = "p_0001"
# keyboardlanguage_month_na$col_roles$group = "p_0001"

# week
keyboardlanguage_week_valence$col_roles$group = "user_id"
keyboardlanguage_week_arousal$col_roles$group = "user_id"

# day
keyboardlanguage_day_valence$col_roles$group = "user_id"
keyboardlanguage_day_arousal$col_roles$group = "user_id"

# moment

keyboardlanguage_threehrs_valence$col_roles$group = "user_id"
keyboardlanguage_threehrs_valence_diff$col_roles$group = "user_id"
keyboardlanguage_threehrs_arousal$col_roles$group = "user_id"
keyboardlanguage_threehrs_arousal_diff$col_roles$group = "user_id"

# Remove user id from feature space

# # month
# keyboardlanguage_month_va$col_roles$feature = setdiff(keyboardlanguage_month_va$col_roles$feature, "p_0001")
# keyboardlanguage_month_pa$col_roles$feature = setdiff(keyboardlanguage_month_pa$col_roles$feature, "p_0001")
# keyboardlanguage_month_na$col_roles$feature = setdiff(keyboardlanguage_month_na$col_roles$feature, "p_0001")

# week
keyboardlanguage_week_valence$col_roles$feature = setdiff(keyboardlanguage_week_valence$col_roles$feature, "user_id")
keyboardlanguage_week_arousal$col_roles$feature = setdiff(keyboardlanguage_week_arousal$col_roles$feature, "user_id")

# day
keyboardlanguage_day_valence$col_roles$feature = setdiff(keyboardlanguage_day_valence$col_roles$feature, "user_id")
keyboardlanguage_day_arousal$col_roles$feature = setdiff(keyboardlanguage_day_arousal$col_roles$feature, "user_id")

# moment

keyboardlanguage_threehrs_valence$col_roles$feature = setdiff(keyboardlanguage_threehrs_valence$col_roles$feature, "user_id")
keyboardlanguage_threehrs_valence_diff$col_roles$feature = setdiff(keyboardlanguage_threehrs_valence_diff$col_roles$feature, "user_id")
keyboardlanguage_threehrs_arousal$col_roles$feature = setdiff(keyboardlanguage_threehrs_arousal$col_roles$feature, "user_id")
keyboardlanguage_threehrs_arousal_diff$col_roles$feature = setdiff(keyboardlanguage_threehrs_arousal_diff$col_roles$feature, "user_id")

#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")
lrn_rf = lrn("regr.ranger", 
             mtry = to_tune(1, 50),
             num.trees =500) # random forest
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

po_scale = po("scale") # scale features

po_impute = po("imputemedian") # impute NAs with median

po_filter = po("filter", filter = flt("correlation"), filter.frac = 0.2) # filter features 

# combine training with pre-processing
lrn_rf_po = po_scale %>>% po_impute  %>>% at_rf 
lrn_rr_po = po_scale %>>% po_impute  %>>% at_rr 

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

saveRDS(bmr_keyboardlanguage_age, "results/predictions/bmr_keyboardlanguage_age.RData") # save results


# gender
bmgrid_keyboardlanguage_gender = benchmark_grid(
  task = c(keyboardlanguage_gender),
  learner = list(lrn("classif.featureless", predict_type = "prob"), po_scale %>>% po_impute %>>% lrn("classif.ranger", num.trees =1000, predict_type = "prob"), po_scale %>>% po_impute %>>% lrn("classif.cv_glmnet", predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_gender = benchmark(bmgrid_keyboardlanguage_gender, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_gender, "results/predictions/bmr_keyboardlanguage_gender.RData") # save results

## trait - all, private and public

bmgrid_keyboardlanguage_trait = benchmark_grid(
  task = c(keyboardlanguage_pa,
           keyboardlanguage_na,
           keyboardlanguage_private_pa,
           keyboardlanguage_private_na,
           keyboardlanguage_public_pa,
           keyboardlanguage_public_na),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_trait = benchmark(bmgrid_keyboardlanguage_trait, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_trait, "results/predictions/bmr_keyboardlanguage_trait.RData") # save results

# # month
# 
# bmgrid_keyboardlanguage_month = benchmark_grid(
#   task = c(keyboardlanguage_month_va,
#            keyboardlanguage_month_pa,
#            keyboardlanguage_month_na),
#   learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
#   resampling = resampling
# )
# 
# future::plan("multisession", workers = 5) # enable parallelization
# 
# bmr_keyboardlanguage_month = benchmark(bmgrid_keyboardlanguage_month, store_models = F, store_backends = F) # execute the benchmark
# 
# saveRDS(bmr_keyboardlanguage_month, "results/predictions/bmr_keyboardlanguage_month.RData") # save results

# week

bmgrid_keyboardlanguage_week = benchmark_grid(
  task = c(keyboardlanguage_week_valence,
           keyboardlanguage_week_arousal),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_week = benchmark(bmgrid_keyboardlanguage_week, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_week, "results/predictions/bmr_keyboardlanguage_week.RData") # save results

# day 

bmgrid_keyboardlanguage_day = benchmark_grid(
  task = c(keyboardlanguage_day_valence,
           keyboardlanguage_day_arousal),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_day = benchmark(bmgrid_keyboardlanguage_day, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_keyboardlanguage_day, "results/predictions/bmr_keyboardlanguage_day.RData") # save results

# moment

bmgrid_keyboardlanguage_moment = benchmark_grid(
  task = c(keyboardlanguage_threehrs_valence,
           keyboardlanguage_threehrs_valence_diff,
           keyboardlanguage_threehrs_arousal,
           keyboardlanguage_threehrs_arousal_diff
           ),
  learner = list(lrn_fl, lrn_rf_po, lrn_rr_po),
  resampling = resampling
)

future::plan("multisession", workers = 5) # enable parallelization

bmr_keyboardlanguage_moment = benchmark(bmgrid_keyboardlanguage_moment, store_models = F, store_backends = F) # execute the benchmark

# save results
saveRDS(bmr_keyboardlanguage_moment, "results/predictions/bmr_keyboardlanguage_moment.RData")

#### BENCHMARK RESULTS ####

## read in benchmark results

# age
bmr_keyboardlanguage_age <- readRDS("results/predictions/bmr_keyboardlanguage_age.RData")

# gender
bmr_keyboardlanguage_gender <- readRDS("results/predictions/bmr_keyboardlanguage_gender.RData")

# trait
bmr_keyboardlanguage_trait <- readRDS("results/predictions/bmr_keyboardlanguage_trait.RData")

# month
#bmr_keyboardlanguage_month <- readRDS("results/predictions/bmr_keyboardlanguage_month.RData")

# week
bmr_keyboardlanguage_week <- readRDS("results/predictions/bmr_keyboardlanguage_week.RData")

# day
bmr_keyboardlanguage_day <- readRDS("results/predictions/bmr_keyboardlanguage_day.RData")

# moment
bmr_keyboardlanguage_moment <- readRDS("results/predictions/bmr_keyboardlanguage_moment.RData")

## view aggregated performance

bmr_keyboardlanguage_age$aggregate(mes)
bmr_keyboardlanguage_gender$aggregate(msrs(c("classif.acc", "classif.auc", "classif.fbeta")))

bmr_keyboardlanguage_trait$aggregate(mes)
#bmr_keyboardlanguage_month$aggregate(mes)
bmr_keyboardlanguage_week$aggregate(mes)
bmr_keyboardlanguage_day$aggregate(mes)
bmr_keyboardlanguage_moment$aggregate(mes)

## retrieve benchmark results across tasks and learners for single cv folds (this is needed for barplots)

bmr_results_folds_age <- extract_bmr_results(bmr_keyboardlanguage_age, mes)
bmr_results_folds_trait <- extract_bmr_results(bmr_keyboardlanguage_trait, mes)
#bmr_results_folds_month <- extract_bmr_results(bmr_keyboardlanguage_month, mes)
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
    task_id == "keyboardlanguage_week_arousal" ~ "Arousal (week)", 
    task_id == "keyboardlanguage_day_valence" ~ "Valence (day)", 
    task_id == "keyboardlanguage_day_arousal" ~ "Arousal (day)", 
    task_id == "keyboardlanguage_threehrs_valence" ~ "Valence (moment)", 
    task_id == "keyboardlanguage_threehrs_arousal" ~ "Arousal (moment)",
    task_id == "keyboardlanguage_threehrs_valence_diff" ~ "Valence Fluct. (moment)", 
    task_id == "keyboardlanguage_threehrs_arousal_diff" ~ "Arousal Fluct. (moment)"
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

# #### BENCHMARK RESULTS AND SIGNIFICANCE TESTS ####
# 
# # read in benchmark results
# bmr_egemaps_age <- readRDS("study1_ger/results/bmr_egemaps_age.RData")
# bmr_egemaps_gender <- readRDS("study1_ger/results/bmr_egemaps_gender.RData")
# bmr_egemaps <- readRDS("study1_ger/results/bmr_egemaps.RData")
# 
# ## view aggregated performance
# bmr_egemaps_age$aggregate(mes)
# bmr_egemaps_gender$aggregate(msrs(c("classif.acc", "classif.auc")))
# bmr_egemaps$aggregate(mes)
# 
# ## retrieve benchmark results across tasks and learners for single cv folds (this is needed for barplots)
# bmr_results_folds <- extract_bmr_results(bmr_egemaps, mes)
# 
# # create overview table of performance incl. significance tests
# pred_table <- results_table(affect_egemaps, bmr_results_folds)
# 
# # add column with p values
# bmr_results_folds <- dplyr::left_join(bmr_results_folds, pred_table[,c("task_id", "learner_id", "p_rsq", "p_rsq_corrected")], by = c("task_id", "learner_id"))
# 
# # create significance column
# bmr_results_folds$significance <- as.factor(ifelse(bmr_results_folds$p_rsq_corrected >= 0.05 | is.na(bmr_results_folds$p_rsq_corrected), "no", "yes"))
# 
# # rename 
# bmr_results_folds <- bmr_results_folds %>% 
#   mutate(learner_id = case_when(
#     learner_id == "regr.featureless" ~    "Baseline",
#     learner_id == "regr.ranger" ~ "Random Forest",
#     learner_id == "regr.cv_glmnet" ~ "LASSO")) %>% 
#   mutate(task_id = case_when(
#     task_id == "egemaps_valence" ~    "Valence",
#     task_id == "egemaps_valence_diff" ~ "Valence Fluctuation",
#     task_id == "egemaps_arousal" ~ "Arousal",
#     task_id == "egemaps_arousal_diff" ~ "Arousal Fluctuation"))
# 
# # create figure
# 
# bmr_egemaps_plot <- ggplot(bmr_results_folds, aes(x= factor(task_id, levels = c("Arousal Fluctuation","Arousal", "Valence Fluctuation",  "Valence")) , y= regr.rsq, color = significance, shape = learner_id)) + 
#   geom_boxplot(width = 0.3,lwd = 1, aes(color = significance), alpha = 0.3, outlier.shape=NA, position=position_dodge(0.5)) +  
#   geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 2) +
#   scale_x_discrete(element_blank()) +
#   scale_y_continuous(name = bquote(paste("Out-of-sample ", "R"^2)), limits = c(-0.15, 0.15)) + 
#   geom_hline(yintercept=0, linetype='dotted') +
#   theme_minimal(base_size = 20) +
#   labs(colour = "Significance", shape = "Algorithm") + # change legend title
#   theme(axis.text.x=element_text(angle = -45, hjust = 0)) + # rotate x axis labels
#   coord_flip() + # flip coordinates
#   guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
#   theme(legend.position="top")
# 
# bmr_egemaps_plot
# 
# # save figure
# 
# png(file="figures/bmr_ger_egemaps_plot.png",width=1000, height=700)
# 
# bmr_egemaps_plot
# 
# dev.off()
