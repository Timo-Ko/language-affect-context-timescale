### PREPARATION ####

packages <- c("dplyr", "caret")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load function for target independet preprocessing
source("r_code/functions/target_independent_preproc.R")

# load data

# trait
affect_language <- readRDS(file="data/affect_language_features/affect_language_500.RData") 
affect_language_private <- readRDS(file="data/affect_language_features/affect_language_private_500.RData") 
affect_language_public <- readRDS(file="data/affect_language_features/affect_language_public_500.RData") 

# month
#affect_language_month <- readRDS(file="data/affect_language_month.RData") 

# week
affect_language_es_week <- readRDS(file="data/affect_language_features/affect_language_es_week_500.RData") 

# day
affect_language_es_day <- readRDS(file="data/affect_language_features/affect_language_es_day_100.RData") 

# moment
affect_language_es_threehrs <- readRDS(file="data/affect_language_features/affect_language_es_threehrs_100.RData") 

#### REMOVE UNNEEDED COLUMNS ####

affect_language_es_threehrs$threehrs_lower <- NULL
affect_language_es_threehrs$threehrs_upper <- NULL
affect_language_es_threehrs$onehr_lower <- NULL
affect_language_es_threehrs$onehr_upper <- NULL

#### TARGET INDEPENDENT PREPROCESSING ####

# define the columns that are not features (here preprocessing is not applied)
no_feature_columns = c("p_0001", "Demo_A1", "Demo_GE1","pa_panas", "na_panas", "typing_sessions_count", "amount_words_typed_total")

no_feature_columns_month = c("p_0001", "wave_id", "datetime", "fourweeks_start", "Demo_A1", "Demo_GE1","va_panava", "pa_panava", "na_panava", "typing_sessions_count", "amount_words_typed_total")

no_feature_columns_es_week = c("user_id", "week", "es_days_week", "es_count_week", "Demo_A1", "Demo_GE1", "valence_week", "arousal_week", "typing_sessions_count", "amount_words_typed_total")

no_feature_columns_es_day = c("user_id", "date", "es_count_day", "Demo_A1", "Demo_GE1", "valence_day", "arousal_day", "typing_sessions_count", "amount_words_typed_total")

no_feature_columns_es_threehrs = c("user_id", "Demo_A1", "Demo_GE1", "e_s_questionnaire_id", "questionnaireStartedTimestamp", "pa_panas", "na_panas", "valence" , "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal", "typing_sessions_count", "amount_words_typed_total")

# apply functions for target-independent preprocessing
affect_language_ml <- target_independent_preproc(affect_language, no_feature_columns)
affect_language_private_ml <- target_independent_preproc(affect_language_private, no_feature_columns)
affect_language_public_ml <- target_independent_preproc(affect_language_public, no_feature_columns)

#affect_language_month_ml <- target_independent_preproc(affect_language_month, no_feature_columns_month)

affect_language_es_week_ml <- target_independent_preproc(affect_language_es_week, no_feature_columns_es_week)

affect_language_es_day_ml <- target_independent_preproc(affect_language_es_day, no_feature_columns_es_day)

affect_language_es_threehrs_ml <- target_independent_preproc(affect_language_es_threehrs, no_feature_columns_es_threehrs)

# save data

# trait
saveRDS(affect_language_ml, "data/affect_language_features/affect_language_ml.RData")
saveRDS(affect_language_private_ml, "data/affect_language_features/affect_language_private_ml.RData")
saveRDS(affect_language_public_ml, "data/affect_language_features/affect_language_public_ml.RData")

# month
#affect_language_month_ml  <- saveRDS(affect_language_month_ml, "data/affect_language_month_ml.RData")

# week
saveRDS(affect_language_es_week_ml, "data/affect_language_es_week_ml.RData")

# day
saveRDS(affect_language_es_day_ml, "data/affect_language_es_day_ml.RData")

# moment
affect_language_es_threehrs_ml  <- saveRDS(affect_language_es_threehrs_ml, "data/affect_language_es_threehrs_ml.RData")
#affect_language_es_onehr_ml  <- readRDS("data/affect_language_es_onehr.RData")

# FINISH