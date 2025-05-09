### PREPROCESSING AND FEATURE EXTRACTION ###

# load ema data
ema_data = readRDS("data/ema/ema_data.rds")
ema_day = readRDS("data/ema/ema_day.rds")
ema_week = readRDS("data/ema/ema_week.rds")

# get files names of user with keyboard data
file_names = list.files(path = "/home/rstudio/data/ps_keyboard", full.names = T)

# do some data checks 
#file_details <- file.info(file_names) # get file infos

#data_list <- lapply(file_names, readRDS) # get list with dfs

# get user ids
users = sub("\\.rds$", "", basename(file_names))

############ Define Feature Extraction Function #############

keyboard_feature_extraction = function(user) {

  tryCatch({
    
    t1 = Sys.time()
  
    # print info
    print(paste("Staring with user", user))
    
    # Step 1: Pull keyboard data for that user 
    keyboard_data <- readRDS(paste0("/home/rstudio/data/ps_keyboard/", user, ".rds"))
    
    if (sum(keyboard_data$words_typed, na.rm = TRUE) < 1) {
      message(paste(user, "did not type any words"))
      write(paste0(Sys.time(), ": SKIPPED: user ", user,
                   " – no keyboard events"), 
            file = "data/errorlog_extraction.txt", append = TRUE)
      return(invisible(NULL))        # <-- early exit, no error
    }
    
    # Step 2: Label esm moments in keyboard data 
    es_user = ema_data %>% filter(user_id == user) %>% ungroup() # filter out es data from that user
    
    if(nrow(es_user) > 0){ # execute this code if there is esm data for that user
    
    # label experience sampling snippets in keyboard data (this function assumes already corrected time stamps)
    keyboard_data = label_ema_in_keyboard(keyboard_data, es_user, 90, 90)  %>% as.data.frame()
    
    } else { keyboard_data$es_questionnaire_id = NA }
    
    # Step 3: Keyboard feature extraction
    
    # es moment
    keyboard_data_es <- keyboard_data %>% filter(!is.na(es_questionnaire_id)) # filter out sessions rows with corresponding es data

    keyboard_features_es = extract_keyboard_features(keyboard_data_es, "es_questionnaire_id", "all", 100)

    if(nrow(keyboard_features_es) > 0){
      saveRDS(keyboard_features_es, paste0("data/results_temp/moment/", user, ".rds"))
    }

    # days in es period
    keyboard_data_esdays <- keyboard_data %>% filter(date %in% ema_day[ema_day$user_id == user, "date"]$date) # filter keyboard_data for days with es data

    keyboard_features_day = extract_keyboard_features(keyboard_data_esdays, "date", "all", 100)

    if(nrow(keyboard_features_day) > 0){
      saveRDS(keyboard_features_day, paste0("data/results_temp/day/", user, ".rds"))
    }

    # weeks in es period
    keyboard_data_esweeks <- keyboard_data %>% filter(week %in% ema_week[ema_week$user_id == user, "week"]$week) # filter keyboard_data for weeks with es data

    keyboard_features_week = extract_keyboard_features(keyboard_data_esweeks,  "week", "all", 100)

    if(nrow(keyboard_features_week) > 0){
      saveRDS(keyboard_features_week, paste0("data/results_temp/week/", user, ".rds"))
    }

    # all produced text during study period
    keyboard_features = extract_keyboard_features(keyboard_data, "user_uuid", "all", 100)

    if(nrow(keyboard_features) > 0){
      saveRDS(keyboard_features,  paste0("data/results_temp/all/", user, ".rds"))
    }

    # all produced text during study period - private communication
    keyboard_features_private = extract_keyboard_features(keyboard_data, "user_uuid", "private", 100)

    if(nrow(keyboard_features_private) > 0){
      saveRDS(keyboard_features_private, paste0("data/results_temp/all_private/", user, ".rds"))
    }

    # all produced text during study period - public communication
    keyboard_features_public = extract_keyboard_features(keyboard_data, "user_uuid", "public", 100)

    if(nrow(keyboard_features_public) > 0){
      saveRDS(keyboard_features_public, paste0("data/results_temp/all_public/", user, ".rds"))
    }
    
    t2 = Sys.time()
    processing_time <- round(as.numeric(difftime(t2, t1, units = "mins")),2)
    
    # print extraction status update 
    print(paste("Features of user", user, "taking", processing_time, "minutes were extracted. This is user number", which(users==user), "out of", length(users)))
    write(paste0(Sys.time(),": SUCCESS: user", user," completed"), file = "data/errorlog_extraction.txt", append = TRUE)
    
    # remove sensing, keyboard and es data frames from that user after feature extraction
    rm( keyboard_data, 
         keyboard_features_es, keyboard_features_day, keyboard_features_week, 
         keyboard_features, keyboard_features_private, keyboard_features_public)

  },
  
  # Tell tryCatch what do in case of an error
  error=function(e) {
    write(paste0(Sys.time(),": ERROR: ",conditionMessage(e)," --> Current user is: ", user), file = "data/errorlog_extraction.txt", append = TRUE)
  })
  
} # end of function


############ Execute Feature Extraction Function #############

# keyboard_feature_extraction(user) # test single feature extraction

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
