### PREPROCESSING AND FEATURE EXTRACTION ###

library(dplyr)
library(lubridate)

source("code/data_processing/keyboard_features/extract_keyboard_features_emoji.R")
source("code/data_processing/helper/ema_labels.R")

# load ema data
ema_data <- readRDS("data/ema/ema_data.rds")

# load lookup tables (names only; used by extract_keyboard_features)
emoticons_df <- readRDS("data/helper/emoticons_df.rds")
emoji_df     <- readRDS("data/helper/emoji_df.rds")

# input files (one .rds per user)
in_dir     <- "/home/rstudio/data/ps_keyboard"
file_names <- list.files(path = in_dir, full.names = TRUE)
users      <- sub("\\.rds$", "", basename(file_names))

# ensure output dirs and log exist
#dir.create("data/results_temp/ema_centered", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results_temp/ema_pre180", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results_temp/ema_pre60", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results_temp/all",    recursive = TRUE, showWarnings = FALSE)
log_file <- "data/errorlog_extraction.txt"
if (!file.exists(log_file)) file.create(log_file)

############ Define Feature Extraction Function #############

keyboard_feature_extraction <- function(user) {
  tryCatch({
    t1 <- Sys.time()
    message("Starting user ", user)
    
    # ---- Step 1: load keyboard data for user ----
    fpath <- file.path(in_dir, paste0(user, ".rds"))
    if (!file.exists(fpath)) {
      write(paste0(Sys.time(), ": SKIPPED: user ", user, " – file not found: ", fpath),
            file = log_file, append = TRUE)
      return(invisible(NULL))
    }
    keyboard_data <- readRDS(fpath)
    
    # minimal schema guards
    for (nm in c("emoji_count", "emoticon_count", "words_typed")) {
      if (!nm %in% names(keyboard_data)) keyboard_data[[nm]] <- 0
    }
    
    # skip if no emoji/emoticon at all
    if (sum(keyboard_data$emoji_count + keyboard_data$emoticon_count, na.rm = TRUE) < 1) {
      write(paste0(Sys.time(), ": SKIPPED: user ", user, " – no emoji/emoticon usage"),
            file = log_file, append = TRUE)
      return(invisible(NULL))
    }
    
    # # Trait / all-text windows
    # if (!"user_uuid" %in% names(keyboard_data)) keyboard_data$user_uuid <- user
    # keyboard_features <- extract_keyboard_features(keyboard_data, "user_uuid")
    # if (is.data.frame(keyboard_features) && nrow(keyboard_features) > 0) {
    #   saveRDS(keyboard_features, file.path("data/results_temp/all", paste0(user, ".rds")))
    # }
    
    # subset ema data to user
    es_user <- ema_data %>% filter(user_id == user) %>% ungroup()
    
    # skip if no ema data from user
    if (nrow(es_user) == 0) {
      write(paste0(Sys.time(), ": SKIPPED: user ", user, " – no ema data"),
            file = log_file, append = TRUE)
      return(invisible(NULL))
    }
    
    # ## state : +/- 90 mins time window
    # 
    # #label EMA moments 
    # if (nrow(es_user) > 0) {
    #   keyboard_data_centered <- label_ema_in_keyboard(keyboard_data, es_user, 90, 90)
    # }
    # # subset to es windows
    # keyboard_data_es_centered <- keyboard_data_centered %>% filter(!is.na(es_questionnaire_id))
    # 
    # # extract symbol features
    # keyboard_features_es_centered <- extract_keyboard_features(keyboard_data_es_centered, "es_questionnaire_id")
    # 
    # # save features
    # if (is.data.frame(keyboard_features_es_centered) && nrow(keyboard_features_es_centered) > 0) {
    #   saveRDS(keyboard_features_es_centered, file.path("data/results_temp/ema_centered", paste0(user, ".rds")))
    # }
    # 
    
    ## state : pre 60 mins time window
    
    #label EMA moments 
    if (nrow(es_user) > 0) {
      keyboard_data_pre <- label_ema_in_keyboard(keyboard_data, es_user, 60, 0)
    }
    
    # subset to es windows
    keyboard_data_es_pre <- keyboard_data_pre %>% filter(!is.na(es_questionnaire_id))
    
    # skip if no emoji/emoticon in es windows
    if (sum(keyboard_data_es_pre$emoji_count + keyboard_data_es_pre$emoticon_count, na.rm = TRUE) < 1) {
      write(paste0(Sys.time(), ": SKIPPED: user ", user, " – no emoji/emoticon usage"),
            file = log_file, append = TRUE)
      return(invisible(NULL))
    }
    
    # extract symbol features
    keyboard_features_es_pre <- extract_keyboard_features(keyboard_data_es_pre, "es_questionnaire_id")
    
    # save features
    if (is.data.frame(keyboard_features_es_pre) && nrow(keyboard_features_es_pre) > 0) {
      saveRDS(keyboard_features_es_pre, file.path("data/results_temp/ema_pre60", paste0(user, ".rds")))
    }
    
    
    # ---- logging ----
    t2 <- Sys.time()
    processing_time <- round(as.numeric(difftime(t2, t1, units = "mins")), 2)
    idx <- match(user, users)
    message("Done user ", user, " in ", processing_time, " min [", idx, "/", length(users), "]")
    write(paste0(Sys.time(), ": SUCCESS: user ", user, " completed in ", processing_time, " min"),
          file = log_file, append = TRUE)
    
    rm(keyboard_data, keyboard_data_es_pre, keyboard_features_es_pre)
    invisible(NULL)
  },
  error = function(e) {
    write(paste0(Sys.time(), ": ERROR: ", conditionMessage(e), " --> user: ", user),
          file = log_file, append = TRUE)
    invisible(NULL)
  })
}


############ Execute Feature Extraction Function #############

keyboard_feature_extraction(user) # test single feature extraction

# Load the packages for parallelization

library(doParallel)
library(foreach)
library(iterators)
library(tidyverse)

# Setup parallelization
n_cores = 2
doParallel::registerDoParallel(cores = n_cores)

# Start feature extraction for each user

foreach(
  iter_id = iter(users, by = "value"),
  .packages = c("tidyverse"),
  .errorhandling = "pass"
) %dopar% {
  tryCatch({
    keyboard_feature_extraction(user = iter_id)
  }, error=function(e) {
    list(error=toString(e))
  })
}

# Stop the parallel backend after completing the tasks
stopImplicitCluster()

### Continue with 03_SOURCE_feature_combination.R ###
