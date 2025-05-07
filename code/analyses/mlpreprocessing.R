### PREPARATION ####

packages <- c("dplyr", "caret")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load function for target independet preprocessing
source("Analysis/ML_Analyses/target_independent_preproc.R")

# load data

# trait
keyboard_data_trait <- readRDS(file="data/results/cleaned/keyboard_data_trait_cleaned.rds") 
keyboard_data_trait_private <- readRDS(file="data/results/cleaned/keyboard_data_trait_private_cleaned.rds") 
keyboard_data_trait_public <- readRDS(file="data/results/cleaned/keyboard_data_trait_public_cleaned.rds") 

# week
keyboard_data_week_ema <- readRDS(file="data/results/cleaned/keyboard_data_week_ema_cleaned.rds") 

# day
keyboard_data_day_ema <- readRDS(file="data/results/cleaned/keyboard_data_day_ema_cleaned.rds") 

# moment
keyboard_data_moment_ema <- readRDS(file="data/results/cleaned/keyboard_data_moment_ema_cleaned.rds") 

#### TARGET INDEPENDENT PREPROCESSING ####

# define the columns that are not features (here preprocessing is not applied)
no_feature_columns_trait = c("user_uuid", "age", "gender","pa_panas", "na_panas", "session_count")

no_feature_columns_ema_week = c("user_id", "week", "es_count_week", "es_days_week", "valence_week", "arousal_week", "age", "gender", "session_count")

no_feature_columns_ema_day = c("user_id", "date", "es_count_day", "valence_day", "arousal_day", "age", "gender", "session_count")

no_feature_columns_ema_moment = c("user_id", "age", "gender", "e_s_questionnaire_id", "questionnaireStartedTimestamp_corrected", "valence" , "valence_avg", "valence_diff", "arousal", "arousal_avg", "arousal_diff", "session_count")

# apply functions for target-independent preprocessing

keyboard_data_trait_ml <- target_independent_preproc(keyboard_data_trait, no_feature_columns_trait)
keyboard_data_trait_private_ml <- target_independent_preproc(keyboard_data_trait_private, no_feature_columns_trait)
keyboard_data_trait_public_ml <- target_independent_preproc(keyboard_data_trait_public, no_feature_columns_trait)

keyboard_data_week_ema_ml <- target_independent_preproc(keyboard_data_week_ema, no_feature_columns_ema_week)
keyboard_data_day_ema_ml <- target_independent_preproc(keyboard_data_day_ema, no_feature_columns_ema_day)
keyboard_data_moment_ema_ml <- target_independent_preproc(keyboard_data_moment_ema, no_feature_columns_ema_moment)

# save data

# trait
saveRDS(keyboard_data_trait_ml, "data/results/ml/keyboard_data_trait_ml.rds")
saveRDS(keyboard_data_trait_private_ml, "data/results/ml/keyboard_data_trait_private_ml.rds")
saveRDS(keyboard_data_trait_public_ml, "data/results/ml/keyboard_data_trait_public_ml.rds")

# week
saveRDS(keyboard_data_week_ema_ml, "data/results/ml/keyboard_data_week_ema_ml.rds")

# day
saveRDS(keyboard_data_day_ema_ml, "data/results/ml/keyboard_data_day_ema_ml.rds")

# moment
saveRDS(keyboard_data_moment_ema_ml, "data/results/ml/keyboard_data_moment_ema_ml.rds")

# FINISH