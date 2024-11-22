###### Extract Word List Features #######

library(dplyr)

### connect to db

keyboard = dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "rstudio",
  password = rstudioapi::askForPassword("Enter your password"),
  host = 'mc-ibt01.unisg.ch',
  port = 3306,
  dbname = "researchime")

###  Load data  ###

# word list of users
wordlist_all = dplyr::tbl(keyboard, "word_frequency") %>% 
  data.frame() %>% 
  mutate(user_uuid = as.character(user_uuid))

# load mfd 2.0 dictionary
mfd_german <- read.csv("data/helper/MFD2.0_german.csv")

# user list of preprocessed keyboard data
users.key <- list.files("/home/rstudio/data/ps_keyboard/", pattern = "^\\d+\\.rds$", full.names = TRUE)
users.key <- basename(users.key)
users.key <- sub("\\.rds$", "", users.key)

# initialize summary data frame
mfd_features <- tibble(user_uuid = character(), count_total = numeric(), rel_authority.virtue = numeric(), rel_care.vice = numeric(), rel_care.virtue = numeric(), rel_fairness.virtue = numeric(), rel_loyalty.virtue = numeric(), rel_sanctity.virtue = numeric(), rel_NA = numeric(), rel_sanctity.vice = numeric(), rel_authority.vice = numeric(), rel_fairness.vice = numeric(), rel_loyalty.vice = numeric())

### Extraction by User ###

# # one user
# user.loop.word(user)
# 
# # all users
# lapply(users, user.loop.word)

# Start user loop
user.loop.word = function(user){
  
  tryCatch({
    Sys.time()
    
    ## Admin
    paste("Started user", user, "at", Sys.time(), "This is Number", which(user == users), "of", length(users), "(", round(which(users == user)/length(users)*100) ,"% )") %>% print()
    
    t1 = Sys.time()
    
    ## Load Word List Data for a Specified User
    
    # filter for user
    wordlist = wordlist_all %>% filter(user_uuid == user)
    
    # skip user if no data is available
    if(nrow(words) == 0){
      print(paste("User", user, "has no wordlist data"))
      stop("execution stopped, user has no WORD data")
    }
    
    if(!user %in% users.key){
      print(paste("User", user, "has no preprocessed keyboard data"))
      stop("execution stopped, user has no preprocessed KEYBOARD data")
    }
    
    ## 02 Preprocess User Data
    
    # Add mfd dictionary to word list data
    wordlist = left_join(wordlist, mfd_german[, c("dimension", "term_ger1")], by = c("word" = "term_ger1"))
    
        
    # # Add number of days of keyboard logging 
    # days = readRDS(paste0("/home/rstudio/data/ps_keyboard/",user , ".rds"))[,"timestamp_type_start_corrected"]
    # days = as.numeric(round_date(max(days), unit = "day") - round_date(min(days), unit = "day"))
    # words = words %>% 
    #   mutate(freq_cat_rel = ifelse(!is.na(freq_cat), as.numeric(freq_cat) / days, NA),
    #          count_rel = count / days)
    
    # # Change columns to numeric
    # words = words %>% 
    #   mutate(count = as.numeric(count),
    #          freq_cat = as.numeric(freq_cat))
    
    ## 03 Extract mfd Feature
    
    ### Create new df that summarizes the outcome of the dimension assignemnt per user
    summarized_df <- wordlist %>%
      group_by(user_uuid, dimension) %>%
      summarise(dimension_count = sum(count), .groups = 'drop') %>%
      mutate(across(-c(user_uuid, dimension), as.numeric))
    
    # add dimension that were not used by this participant
    
    # Create a dataframe of all user_uuid and dimension combinations
    complete_user_dimension <- expand_grid(
      user_uuid = unique(summarized_df$user_uuid),
      dimension = c(unique(mfd_german$dimension), NA))
    
    # Left join this with your summarized_df
    summarized_df_complete <- complete_user_dimension %>%
      left_join(summarized_df, by = c("user_uuid", "dimension")) %>%
      # Replace NA in dimension_count with 0 (for missing dimensions, not "NA" the dimension)
      mutate(dimension_count = ifelse(is.na(dimension_count), 0, dimension_count))
    
    # pivot wider
    mfd_feats_user <- summarized_df_complete %>%
      pivot_wider(names_from = dimension, values_from = dimension_count, names_prefix = "count_")
    
    # compute mfd features per user
    mfd_feats_user$count_total <- rowSums(select(mfd_feats_user, -user_uuid)) # total count of words captured with mfd dictionary
    mfd_feats_user$rel_authority.virtue <- mfd_feats_user$count_authority.virtue/mfd_feats_user$count_total
    mfd_feats_user$rel_care.vice <- mfd_feats_user$count_care.vice/mfd_feats_user$count_total
    mfd_feats_user$rel_care.virtue <- mfd_feats_user$count_care.virtue/mfd_feats_user$count_total
    mfd_feats_user$rel_fairness.virtue <- mfd_feats_user$count_fairness.virtue/mfd_feats_user$count_total
    mfd_feats_user$rel_loyalty.virtue <- mfd_feats_user$count_loyalty.virtue/mfd_feats_user$count_total
    mfd_feats_user$rel_sanctity.virtue <- mfd_feats_user$count_sanctity.virtue/mfd_feats_user$count_total
    mfd_feats_user$rel_NA <- mfd_feats_user$count_NA/mfd_feats_user$count_total
    mfd_feats_user$rel_sanctity.vice <- mfd_feats_user$count_sanctity.vice/mfd_feats_user$count_total
    mfd_feats_user$rel_authority.vice <- mfd_feats_user$count_authority.vice/mfd_feats_user$count_total
    mfd_feats_user$rel_fairness.vice <- mfd_feats_user$count_fairness.vice/mfd_feats_user$count_total
    mfd_feats_user$rel_loyalty.vice <- mfd_feats_user$count_loyalty.vice/mfd_feats_user$count_total
    
    ## 05 Save Features for specific User
    
    # # combine different wordlist features
    # feat.user = cbind(data.frame(user_id = as.character(user)), 
    #                   feat.distinct, 
    #                   feat.used)
    
    #saveRDS(feat.user, file = paste0("/home/max/projects/MobileSensing/data/results_temp/word/INT", user, ".RDS"))
    
    print(paste("Features of user", user, "were extracted at", Sys.time(), "This is user number", which(users==user), "out of", length(users)))
    
    return(mfd_feats_user)
    
  },
  
  # Tell tryCatch what do in case of an error
  error = function(e){
    write(paste0(Sys.time(),": ERROR: ", conditionMessage(e)," --> Current user is: ", user, ". The error ocurred after successful extraction of ", lastSuccess), file = "data/FeatureExtractionErrorlog.txt", append = TRUE)
  }
  
  )}

mfd_features <- bind_rows(lapply(users, user.loop.word), .id = NULL)

# save data frame with mfd features for all users 
saveRDS(mfd_features, "data/results_temp/keyboard/mfd_features.rds")

# finish