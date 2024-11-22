### COMBINE EXTRACTED KEYBOARD EATURES WITH AFFECT DATA ###

## Load demographics data from wave 1 
wave1 = read.csv2("data/raw/wave1_2021_02_19.csv")
demographics = wave1 %>% dplyr::select(p_0001, Demo_A1, Demo_GE1)
colnames(demographics) = c("user_id", "age", "gender")
demographics <- demographics[!duplicated(demographics), ] # remove duplicates

## load panas data
panas_df = readRDS("data/results/panas.RData")
names(panas_df)[names(panas_df) == "p_0001"] <- "user_uuid" # rename column

## load ema data 
ema_data = readRDS("data/results/ema/ema_data.rds")
ema_day = readRDS("data/results/ema/ema_day.rds")
ema_week = readRDS("data/results/ema/ema_week.rds")

## load all keyboard data for different es time windows 
keyboard_data_moment <- data.frame() # initialize df
keyboard_data_day <- data.frame() # initialize df
keyboard_data_week <- data.frame() # initialize df

# es 
for(file in list.files("data/results_temp/keyboard/es/")){ # iterate through files
  df1 = read.csv2(paste0("data/results_temp/keyboard/es/", file)) # load user df
  df1 <- df1 %>%
    mutate(user_id = as.integer(sub("\\.csv$", "", file))) %>%
    select(user_id, everything()) # add user_id
  keyboard_data_moment = dplyr::bind_rows(keyboard_data_moment, df1) # append user data
}

# es day
for(file in list.files("data/results_temp/keyboard/day")){ # iterate through files
  df1 = read.csv2(paste0("data/results_temp/keyboard/day/", file)) # load user df
  df1 <- df1 %>%
    mutate(user_id = as.integer(sub("\\.csv$", "", file))) %>%
    select(user_id, everything()) # add user_id
  keyboard_data_day = dplyr::bind_rows(keyboard_data_day, df1) # append user data
}

# es week
for(file in list.files("data/results_temp/keyboard/week")){ # iterate through files
  df1 = read.csv2(paste0("data/results_temp/keyboard/week/", file)) # load user df
  df1 <- df1 %>%
    mutate(user_id = as.integer(sub("\\.csv$", "", file))) %>%
    select(user_id, everything()) # add user_id
  keyboard_data_week = dplyr::bind_rows(keyboard_data_week, df1) # append user data
}

# covert date to date format
keyboard_data_day$date <- as.Date(keyboard_data_day$date, origin = "1970-01-01")

# combine keyboard data and ema data and demographics
keyboard_data_moment_ema = left_join(ema_data, demographics, by = "user_id", relationship = "many-to-one") %>% inner_join(keyboard_data_moment, by = c ("es_questionnaire_id", "user_id"))
keyboard_data_day_ema = left_join(ema_day, demographics, by = "user_id", relationship = "many-to-one") %>% inner_join(keyboard_data_day, by = c ("date", "user_id"))
keyboard_data_week_ema = left_join(ema_week, demographics, by = "user_id", relationship = "many-to-one") %>% inner_join(keyboard_data_week, by = c ("week", "user_id"))

# save combined data sets 
saveRDS(keyboard_data_moment_ema, "data/results/raw/keyboard_data_moment_ema.rds") # es moment 
saveRDS(keyboard_data_day_ema, "data/results/raw/keyboard_data_day_ema.rds") # es day
saveRDS(keyboard_data_week_ema, "data/results/raw/keyboard_data_week_ema.rds") # es week

## combine all keyboard data with trait affect (panas) data

# initiate dfs
keyboard_data = data.frame() 
keyboard_data_private = data.frame() 
keyboard_data_public = data.frame()

# fill dfs
for (file.within in list.files(paste0("data/results_temp/keyboard/all/all"))) {
  df1 = read.csv2(paste0("data/results_temp/keyboard/all/all/", file.within)) # load user df
  keyboard_data = dplyr::bind_rows(keyboard_data, df1) # append user df
}

for (file.within in list.files(paste0("data/results_temp/keyboard/all/private"))) {
  df1 = read.csv2(paste0("data/results_temp/keyboard/all/private/", file.within)) # load user df
  keyboard_data_private = dplyr::bind_rows(keyboard_data_private, df1) # append user df
}

for (file.within in list.files(paste0("data/results_temp/keyboard/all/public"))) {
  df1 = read.csv2(paste0("data/results_temp/keyboard/all/public/", file.within)) # load user df
  keyboard_data_public = dplyr::bind_rows(keyboard_data_public, df1) # append user df
}

# join keyboard data sets w demographics and trait affect data
keyboard_data_trait = inner_join(panas_df, keyboard_data, by = "user_uuid") %>%
  dplyr::left_join(demographics, by = c("user_uuid" = "user_id" ))  %>%
  dplyr::select(user_uuid, age, gender, pa_panas, na_panas, everything())

keyboard_data_trait_private = inner_join(panas_df, keyboard_data_private, by = "user_uuid") %>%
  dplyr::left_join(demographics, by = c("user_uuid" = "user_id" ))  %>%
  dplyr::select(user_uuid, age, gender, pa_panas, na_panas, everything())

keyboard_data_trait_public = inner_join(panas_df, keyboard_data_public, by = "user_uuid") %>%
  dplyr::left_join(demographics, by = c("user_uuid" = "user_id" ))  %>%
  dplyr::select(user_uuid, age, gender, pa_panas, na_panas, everything())

# save combined data sets 
saveRDS(keyboard_data, "data/results/raw/keyboard_data.rds") 

saveRDS(keyboard_data_trait, "data/results/raw/keyboard_data_trait.rds") 
saveRDS(keyboard_data_trait_private, "data/results/raw/keyboard_data_trait_private.rds") 
saveRDS(keyboard_data_trait_public, "data/results/raw/keyboard_data_trait_public.rds") 

# ## delete temporary files 
# unlink("data/results/RDS_subsets", recursive=TRUE)
# unlink("data/results/results_temp", recursive=TRUE)

### continue with 04_SOURCE_exclusion_criteria.R ###
