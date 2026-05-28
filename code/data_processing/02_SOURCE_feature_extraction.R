### PREPROCESSING AND FEATURE EXTRACTION ###

library(dplyr)
library(lubridate)
library(readr)

# helper scripts

source("code/data_processing/keyboard_features/extract_keyboard_features.R", local = TRUE)
source("code/data_processing/helper/ema_labels.R", local = TRUE)

# load ema data
ema_data <- readRDS("data/ema/ema_data.rds")

# load lookup tables (names only; used by extract_keyboard_features)
emoticons_df <- readRDS("data/helper/emoticons_df.rds")
emoji_df     <- readRDS("data/helper/emoji_df.rds")

## Load Category names for LIWC
liwc.names = read_delim("data/helper/DE-LIWC2015.rimealiases", delim="\t", col_names=c("LIWC.cat","C.cat"))
unknown = c("unknown", "unknown")
liwc.names = rbind(liwc.names, unknown)
liwc.names$LIWC.name = paste0("LIWC_", liwc.names$LIWC.cat)

# input files (one .rds per user)
in_dir     <- "/home/rstudio/data/ps_keyboard"
file_names <- list.files(path = in_dir, full.names = TRUE)
users      <- sub("\\.rds$", "", basename(file_names))

# ensure output dirs and log exist
dir.create("data/results_temp/all",    recursive = TRUE, showWarnings = FALSE)
dir.create("data/results_temp/day",    recursive = TRUE, showWarnings = FALSE)
dir.create("data/results_temp/ema", recursive = TRUE, showWarnings = FALSE)

log_file <- "data/errorlog_extraction.txt"
if (file.exists(log_file)) file.remove(log_file)
file.create(log_file)

# daily EMA aggregates per user × day
ema_day <- ema_data %>%
  filter(!is.na(valence), !is.na(date)) %>%
  group_by(user_id, date) %>%
  summarise(
    daily_valence = mean(valence, na.rm = TRUE),
    n_ema_day = sum(!is.na(valence)),
    .groups = "drop"
  )

saveRDS(ema_day, "data/ema/ema_day.rds")

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
    
    keyboard_data <- keyboard_data %>%
      mutate(
        chars_typed = dplyr::coalesce(character_count_added, 0) +
          dplyr::coalesce(character_count_altered, 0)
      )
    
    required_cols <- c("words_typed", "action_category")
    missing_cols <- setdiff(required_cols, names(keyboard_data))
    
    if (length(missing_cols) > 0) {
      write(paste0(Sys.time(), ": SKIPPED: user ", user, " – missing columns: ",
                   paste(missing_cols, collapse = ", ")),
            file = log_file, append = TRUE)
      return(invisible(NULL))
    }

    # skip if no language data at all
    if (sum(keyboard_data$words_typed, na.rm = TRUE) < 1) {
      write(paste0(Sys.time(), ": SKIPPED: user ", user, " – no words typed"),
            file = log_file, append = TRUE)
      return(invisible(NULL))
    }
    
    ## Trait 
    
    if (!"user_uuid" %in% names(keyboard_data)) keyboard_data$user_uuid <- user
    
    keyboard_features_all <- extract_keyboard_features(
      keyboard_data = keyboard_data,
      window_identifier = "user_uuid",
      filter_var = "all"
    )
    
    keyboard_features_all_context <- extract_keyboard_features(
      keyboard_data = keyboard_data,
      window_identifier = "user_uuid",
      filter_var = c("private", "public")
    )
    
    # save features
    
    trait_out <- dplyr::bind_rows(keyboard_features_all, keyboard_features_all_context) %>%
      mutate(user_id = user)
    
    if (is.data.frame(trait_out) && nrow(trait_out) > 0) {
      saveRDS(trait_out, file.path("data/results_temp/all", paste0(user, ".rds")))
    }
    
    ## Day
    
    # subset day-level EMA data to user
    ema_day_user <- ema_day %>%
      filter(user_id == user)
    
    if (nrow(ema_day_user) > 0) {
      
      # keep only keyboard records from days with at least one EMA valence report
      keyboard_data_day <- keyboard_data %>%
        filter(date %in% ema_day_user$date)
      
      # only proceed if there is any keyboard data on EMA days
      if (nrow(keyboard_data_day) > 0 && sum(keyboard_data_day$words_typed, na.rm = TRUE) > 0) {
        
        keyboard_features_day_all <- extract_keyboard_features(
          keyboard_data = keyboard_data_day,
          window_identifier = "date",
          filter_var = "all"
        )
        
        keyboard_features_day_context <- extract_keyboard_features(
          keyboard_data = keyboard_data_day,
          window_identifier = "date",
          filter_var = c("private", "public")
        )
        
        day_out <- dplyr::bind_rows(
          keyboard_features_day_all,
          keyboard_features_day_context
        )
        
        if (is.data.frame(day_out) && nrow(day_out) > 0) {
          # keep user id explicitly for later merges
          day_out <- day_out %>%
            mutate(user_id = user)
          
          saveRDS(day_out, file.path("data/results_temp/day", paste0(user, ".rds")))
        }
        
      }
    }
  
    ## Moment
    
    # subset ema data to user
    es_user <- ema_data %>% filter(user_id == user) %>% ungroup()
    
    if (nrow(es_user) == 0) {
      write(paste0(Sys.time(), ": INFO: user ", user, " – no ema data; skipping EMA extraction"),
            file = log_file, append = TRUE)
    } else {
      
      keyboard_data_ema <- label_ema_in_keyboard(keyboard_data, es_user, 60, 0)
      keyboard_data_ema <- keyboard_data_ema %>% filter(!is.na(es_questionnaire_id))
      
      if (nrow(keyboard_data_ema) == 0 || sum(keyboard_data_ema$words_typed, na.rm = TRUE) < 1) {
        write(paste0(Sys.time(), ": INFO: user ", user, " – no usable keyboard data in EMA windows"),
              file = log_file, append = TRUE)
      } else {
        
        keyboard_features_ema <- extract_keyboard_features(
          keyboard_data = keyboard_data_ema,
          window_identifier = "es_questionnaire_id",
          filter_var = "all"
        )
        
        keyboard_features_ema_context <- extract_keyboard_features(
          keyboard_data = keyboard_data_ema,
          window_identifier = "es_questionnaire_id",
          filter_var = c("private", "public")
        )
        
        ema_out <- dplyr::bind_rows(keyboard_features_ema, keyboard_features_ema_context) %>%
          mutate(user_id = user)
        
        if (is.data.frame(ema_out) && nrow(ema_out) > 0) {
          saveRDS(ema_out, file.path("data/results_temp/ema", paste0(user, ".rds")))
        }
      }
    }
    # ---- logging ----
    t2 <- Sys.time()
    processing_time <- round(as.numeric(difftime(t2, t1, units = "mins")), 2)
    idx <- match(user, users)
    message("Done user ", user, " in ", processing_time, " min [", idx, "/", length(users), "]")
    write(paste0(Sys.time(), ": SUCCESS: user ", user, " completed in ", processing_time, " min"),
          file = log_file, append = TRUE)
    
    invisible(NULL)
  },
  error = function(e) {
    write(paste0(Sys.time(), ": ERROR: ", conditionMessage(e), " --> user: ", user),
          file = log_file, append = TRUE)
    invisible(NULL)
  })
}

############ Execute Feature Extraction Function #############

unlink("data/results_temp/all", recursive = TRUE)
unlink("data/results_temp/day", recursive = TRUE)
unlink("data/results_temp/ema", recursive = TRUE)

dir.create("data/results_temp/all", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results_temp/day", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results_temp/ema", recursive = TRUE, showWarnings = FALSE)

write(paste0(Sys.time(), ": Starting sequential extraction for ", length(users), " users"),
      file = log_file, append = TRUE)

first_user <- users[1]
keyboard_feature_extraction(first_user)

tmp_check <- readRDS(file.path("data/results_temp/all", paste0(first_user, ".rds")))

write(paste0(Sys.time(), ": Sanity check saved file ncol = ", ncol(tmp_check)),
      file = log_file, append = TRUE)

write(paste0(Sys.time(), ": Sanity check saved file n_liwc = ",
             sum(grepl("^liwc_", names(tmp_check)))),
      file = log_file, append = TRUE)

if (ncol(tmp_check) < 1000 || sum(grepl("^liwc_", names(tmp_check))) < 50) {
  stop("Saved output failed sanity check; aborting extraction run.")
}

invisible(lapply(users[-1], keyboard_feature_extraction))

write(paste0(Sys.time(), ": JOB FINISHED"), file = log_file, append = TRUE)

### Continue with 03_SOURCE_feature_combination.R ###

## parallelized script

# # Load the packages for parallelization
# 
# library(doParallel)
# library(foreach)
# library(iterators)
# 
# # Setup parallelization
# n_cores = 2
# doParallel::registerDoParallel(cores = n_cores)
# 
# # Start feature extraction for each user
# 
# foreach(
#   iter_id = iter(users, by = "value"),
#   .packages = c("dplyr", "lubridate", "tibble"),
#   .export = c(
#     "keyboard_feature_extraction",
#     "extract_keyboard_features",
#     "label_ema_in_keyboard",
#     "ema_data",
#     "ema_day",
#     "emoji_df",
#     "emoticons_df",
#     "liwc.names",
#     "in_dir",
#     "log_file",
#     "users"
#   ),
#   .errorhandling = "pass"
# ) %dopar% {
#   tryCatch({
#     keyboard_feature_extraction(user = iter_id)
#   }, error = function(e) {
#     list(error = toString(e))
#   })
# }
# 
# # Stop the parallel backend after completing the tasks
# stopImplicitCluster()

### Continue with 03_SOURCE_data_combination.R ###