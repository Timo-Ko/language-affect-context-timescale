### COMBINE EXTRACTED KEYBOARD FEATURES WITH AFFECT DATA ###

library(dplyr)

dir.create("data/results", recursive = TRUE, showWarnings = FALSE)

## Load demographics data from wave 1 
wave1 <- read.csv2("data/helper/wave1_2021_02_19.csv")
demographics <- wave1 %>%
  dplyr::select(p_0001, Demo_A1, Demo_GE1)

colnames(demographics) <- c("user_id", "age", "gender")
demographics <- demographics[!duplicated(demographics), ]
demographics <- demographics %>% mutate(user_id = as.character(user_id))

## Load PANAS data
panas_df <- readRDS("data/helper/panas.RData")
names(panas_df)[names(panas_df) == "p_0001"] <- "user_uuid"
panas_df <- panas_df %>% mutate(user_uuid = as.character(user_uuid))

## Load EMA data
ema_data <- readRDS("data/ema/ema_data.rds") %>%
  mutate(user_id = as.character(user_id))

## Load daily EMA data
ema_day <- readRDS("data/ema/ema_day.rds") %>%
  mutate(
    user_id = as.character(user_id),
    date = as.Date(date)
  )

############################################################
## 1) MOMENTARY-LEVEL DATA
############################################################

keyboard_data_ema <- data.frame()

for (file in list.files("data/results_temp/ema/", full.names = TRUE)) {
  df1 <- readRDS(file)
  
  # add user_id from filename only if not already there
  if (!"user_id" %in% names(df1)) {
    df1 <- df1 %>%
      mutate(user_id = as.character(sub("\\.rds$", "", basename(file))))
  } else {
    df1 <- df1 %>% mutate(user_id = as.character(user_id))
  }
  
  keyboard_data_ema <- dplyr::bind_rows(keyboard_data_ema, df1)
}

# combine keyboard data with EMA data and demographics
keyboard_data_ema <- ema_data %>%
  left_join(demographics, by = "user_id", relationship = "many-to-one") %>%
  inner_join(keyboard_data_ema, by = c("es_questionnaire_id", "user_id"))

# drop irrelevant cols
keyboard_data_ema <- keyboard_data_ema %>%
  select(
    -c(
      notificationTimestamp,
      questionnaireStartedTimestamp,
      questionnaireEndedTimestamp,
      weekday,
      nr,
      date,
      week,
      arousal_diff,
      valence_diff
    )
  )

saveRDS(keyboard_data_ema, "data/results/keyboard_data_ema.rds")

############################################################
## 2) TRAIT-LEVEL DATA
############################################################

keyboard_data_trait_raw <- data.frame()

for (file in list.files("data/results_temp/all/", full.names = TRUE)) {
  df1 <- readRDS(file)
  
  # add user_id from filename only if not already there
  if (!"user_id" %in% names(df1)) {
    df1 <- df1 %>%
      mutate(user_id = as.character(sub("\\.rds$", "", basename(file))))
  } else {
    df1 <- df1 %>% mutate(user_id = as.character(user_id))
  }
  
  keyboard_data_trait_raw <- dplyr::bind_rows(keyboard_data_trait_raw, df1)
}

# make sure join key matches PANAS
keyboard_data_trait_raw <- keyboard_data_trait_raw %>%
  mutate(user_uuid = as.character(user_id))

keyboard_data_trait <- panas_df %>%
  inner_join(keyboard_data_trait_raw, by = "user_uuid") %>%
  left_join(demographics, by = c("user_uuid" = "user_id")) %>%
  select(-any_of("user_id")) %>%
  select(user_uuid, age, gender, pa_panas, na_panas, everything()) %>%
  rename(user_id = user_uuid)

saveRDS(keyboard_data_trait, "data/results/keyboard_data_trait.rds")

############################################################
## 3) DAILY-LEVEL DATA
############################################################

keyboard_data_day <- data.frame()

for (file in list.files("data/results_temp/day/", full.names = TRUE)) {
  df1 <- readRDS(file)
  
  # add user_id from filename only if not already there
  if (!"user_id" %in% names(df1)) {
    df1 <- df1 %>%
      mutate(user_id = as.character(sub("\\.rds$", "", basename(file))))
  } else {
    df1 <- df1 %>% mutate(user_id = as.character(user_id))
  }
  
  keyboard_data_day <- dplyr::bind_rows(keyboard_data_day, df1)
}

# ensure date is Date
keyboard_data_day <- keyboard_data_day %>%
  mutate(date = as.Date(date))

# combine daily keyboard data with daily EMA data and demographics
keyboard_data_day <- ema_day %>%
  left_join(demographics, by = "user_id", relationship = "many-to-one") %>%
  inner_join(keyboard_data_day, by = c("user_id", "date"))

saveRDS(keyboard_data_day, "data/results/keyboard_data_day.rds")

### continue with 04_SOURCE_smartphone_chnagers.R ###