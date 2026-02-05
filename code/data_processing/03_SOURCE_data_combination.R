### COMBINE EXTRACTED KEYBOARD EATURES WITH AFFECT DATA ###

## Load demographics data from wave 1 
wave1 = read.csv2("data/helper/wave1_2021_02_19.csv")
demographics = wave1 %>% dplyr::select(p_0001, Demo_A1, Demo_GE1)
colnames(demographics) = c("user_id", "age", "gender")
demographics <- demographics[!duplicated(demographics), ] # remove duplicates
demographics = demographics %>% mutate(user_id = as.character(user_id))

## load panas data
panas_df = readRDS("data/helper/panas.RData")
names(panas_df)[names(panas_df) == "p_0001"] <- "user_uuid" # rename column
panas_df = panas_df %>% mutate(user_uuid = as.character(user_uuid))

## load ema data 
ema_data = readRDS("data/ema/ema_data.rds") %>% mutate(user_id = as.character(user_id))

## load all keyboard data for different es time windows 
keyboard_data_ema_centered <- data.frame() # initialize df
keyboard_data_ema_pre180 <- data.frame() # initialize df
keyboard_data_ema_pre60 <- data.frame() # initialize df

# es moment - ema centered and pre ema 
for(file in list.files("data/results_temp/ema_centered/")){ # iterate through files
  df1 = readRDS(paste0("data/results_temp/ema_centered/", file)) # load user df
  df1 <- df1 %>%
    mutate(user_id = as.character(sub("\\.rds$", "", file))) %>%
    select(user_id, everything()) # add user_id
  keyboard_data_ema_centered = dplyr::bind_rows(keyboard_data_ema_centered, df1) # append user data
}

for(file in list.files("data/results_temp/ema_pre180/")){ # iterate through files
  df1 = readRDS(paste0("data/results_temp/ema_pre180/", file)) # load user df
  df1 <- df1 %>%
    mutate(user_id = as.character(sub("\\.rds$", "", file))) %>%
    select(user_id, everything()) # add user_id
  keyboard_data_ema_pre = dplyr::bind_rows(keyboard_data_ema_pre, df1) # append user data
}

for(file in list.files("data/results_temp/ema_pre60/")){ # iterate through files
  df1 = readRDS(paste0("data/results_temp/ema_pre60/", file)) # load user df
  df1 <- df1 %>%
    mutate(user_id = as.character(sub("\\.rds$", "", file))) %>%
    select(user_id, everything()) # add user_id
  keyboard_data_ema_pre60 = dplyr::bind_rows(keyboard_data_ema_pre60, df1) # append user data
}

# combine keyboard data and ema data and demographics
keyboard_data_ema_centered = left_join(ema_data, demographics, by = "user_id", relationship = "many-to-one") %>% inner_join(keyboard_data_ema_centered, by = c ("es_questionnaire_id", "user_id"))
keyboard_data_ema_pre180 = left_join(ema_data, demographics, by = "user_id", relationship = "many-to-one") %>% inner_join(keyboard_data_ema_pre180, by = c ("es_questionnaire_id", "user_id"))
keyboard_data_ema_pre60 = left_join(ema_data, demographics, by = "user_id", relationship = "many-to-one") %>% inner_join(keyboard_data_ema_pre60, by = c ("es_questionnaire_id", "user_id"))

# drop irrelevant cols
keyboard_data_ema_centered <- keyboard_data_ema_centered %>%
  select(
    -c(
      notificationTimestamp,
      questionnaireStartedTimestamp,
      questionnaireEndedTimestamp,
      arousal,
      valence_avg,
      arousal_avg,
      weekday,
      nr,
      date,
      week,
      arousal_diff,
      valence_diff
    )
  )

keyboard_data_ema_pre60 <- keyboard_data_ema_pre60 %>%
  select(
    -c(
      notificationTimestamp,
      questionnaireStartedTimestamp,
      questionnaireEndedTimestamp,
      arousal,
      valence_avg,
      arousal_avg,
      weekday,
      nr,
      date,
      week,
      arousal_diff,
      valence_diff
    )
  )

# save combined data sets 
saveRDS(keyboard_data_ema_centered, "data/results/keyboard_data_ema_centered.rds") # es moment 
saveRDS(keyboard_data_ema_pre180, "data/results/keyboard_data_ema_pre.rds") # es moment 
saveRDS(keyboard_data_ema_pre60, "data/results/keyboard_data_ema_pre60.rds") # es moment 

## combine all keyboard data with trait affect (panas) data

# initiate dfs
keyboard_data = data.frame() 

# fill dfs
for (file.within in list.files(paste0("data/results_temp/all"))) {
  df1 = readRDS(paste0("data/results_temp/all/", file.within)) # load user df
  keyboard_data = dplyr::bind_rows(keyboard_data, df1) # append user df
}

# join keyboard data sets w demographics and trait affect data
keyboard_data_trait = inner_join(panas_df, keyboard_data, by = "user_uuid") %>%
  dplyr::left_join(demographics, by = c("user_uuid" = "user_id" ))  %>%
  dplyr::select(user_uuid, age, gender, pa_panas, na_panas, everything()) %>%
  dplyr::rename(user_id = user_uuid)

# save combined data sets 

saveRDS(keyboard_data_trait, "data/results/keyboard_data_trait.rds") 

### continue with 04_SOURCE_smartphone_chnagers.R ###