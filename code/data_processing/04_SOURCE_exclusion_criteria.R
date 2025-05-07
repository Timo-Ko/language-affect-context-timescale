#### APPLY EXCLUSION CRITERIA TO KEYBOARD AND AFFECT DATA ####

# load data sets
keyboard_data_moment_ema = readRDS("data/results/keyboard_data_moment_ema.rds")
keyboard_data_day_ema = readRDS("data/results/keyboard_data_day_ema.rds")
keyboard_data_week_ema = readRDS("data/results/keyboard_data_week_ema.rds")

keyboard_data_trait = readRDS("data/results/keyboard_data_trait.rds")
keyboard_data_trait_private = readRDS("data/results/keyboard_data_trait_private.rds")
keyboard_data_trait_public = readRDS("data/results/keyboard_data_trait_public.rds")

## fix smartphone changers
# 
# # load file on smartphone changers
# changers_data = read.csv2("data/helper/Smartphonewechsel_20210219.csv")
# 
# # find changers in our data
# changers = keyboard_data_trait %>% dplyr::filter(user_uuid %in% changers_data$NewId)
# 
# # for trait, we cannot rbind data as scores are already relativized - rbind changers before feature extraction?
# # for state, we could just rbind the sessions
# # for now we just remove smartphone changers
# 
# user_ids_changers <- changers_data %>%
#   select(p_0001_2, p_0001_3, p_0001_new) %>%  # Select the first three columns
#   unlist() %>%  # Convert it into a vector
#   na.omit() %>%  # Remove NA values
#   unique()  # Get unique values
# 
# # Filter out the ids
# keyboard_data_trait <- keyboard_data_trait %>%
#   filter(!user_uuid %in% user_ids_changers)
# 
# ## exclude participants with less than 5 EMA days
# # to do
# 
# ## Exclude straightliners (no variance in responses)
# 
# # compute variance in valence and arousal responses per participant
# var_es_user <- ema_data %>%
#   dplyr::group_by(user_id) %>%
#   dplyr::summarise(var_valence = var(valence, na.rm = T), var_arousal = var(arousal, na.rm = T)) 
# 
# # find participants with zero variance in their valence AND arousal responses across all their emas (they were probably straightlining)
# 
# novariance_user <- var_es_user[ var_es_user$var_valence == 0  & var_es_user$var_arousal == 0, "user_id"]$user_id # find users that have variance in their responses
# 
# # remove NAs (when users filled out only one EMA)
# 
# ema_data <- ema_data[ema_data$user_id %in% variancees_user ,] # keep only users with variance in their affect responses


## drop rare emoji

# Calculate the proportion of participants with non-zero/non-NA usage for each emoji 
emoji_cols <- paste0(emoji_df$variable_name, "_avg")

proportion_emoji_used <- sapply(keyboard_data[emoji_cols], function(column) {
  sum(!is.na(column) & column != 0) / nrow(keyboard_data)
})

# Convert proportion_emoji_used to a data frame
proportion_emoji_df <- data.frame(
  variable_name = sub("_avg", "", names(proportion_emoji_used)),
  proportion_used = proportion_emoji_used
)

# Join with emoji_df
emoji_df_extended <- merge(emoji_df, proportion_emoji_df, by = "variable_name", all.x = TRUE)

# Filter for emojis used by less than 5% of users
rare_emoji <- emoji_df_extended[emoji_df_extended$proportion_used < 0.05, ]$variable_name

# Step 2: Create new entries with "_var", "_min", and "_max" suffixes
rare_emoji_expanded <- lapply(rare_emoji, function(x) {
  c(paste0(x, "_avg"), paste0(x, "_var"), paste0(x, "_min"), paste0(x, "_max"))
})

# Step 3: Combine into a single vector
expanded_emoji <- unlist(rare_emoji_expanded)

# Remove these rarely used emoji columns from the dataframes
keyboard_data_moment_ema_cleaned  <- keyboard_data_moment_ema [ , !(names(keyboard_data_moment_ema ) %in% expanded_emoji)]
keyboard_data_day_ema_cleaned  <- keyboard_data_day_ema [ , !(names(keyboard_data_day_ema ) %in% expanded_emoji)]
keyboard_data_week_ema_cleaned  <- keyboard_data_week_ema [ , !(names(keyboard_data_week_ema ) %in% expanded_emoji)]

keyboard_data_trait_cleaned  <- keyboard_data_trait [ , !(names(keyboard_data_trait) %in% expanded_emoji)]
keyboard_data_trait_private_cleaned  <- keyboard_data_trait_private [ , !(names(keyboard_data_trait_private ) %in% expanded_emoji)]
keyboard_data_trait_public_cleaned  <- keyboard_data_trait_public [ , !(names(keyboard_data_trait_public ) %in% expanded_emoji)]

## save cleaned data files 

saveRDS(keyboard_data_moment_ema_cleaned, "data/results/cleaned/keyboard_data_moment_ema_cleaned.rds") # es moment 
saveRDS(keyboard_data_day_ema_cleaned, "data/results/cleaned/keyboard_data_day_ema_cleaned.rds") # es day
saveRDS(keyboard_data_week_ema_cleaned, "data/results/cleaned/keyboard_data_week_ema_cleaned.rds") # es week

saveRDS(keyboard_data_trait_cleaned, "data/results/cleaned/keyboard_data_trait_cleaned.rds") # trait
saveRDS(keyboard_data_trait_private_cleaned, "data/results/cleaned/keyboard_data_trait_private_cleaned.rds") # trait
saveRDS(keyboard_data_trait_public_cleaned, "data/results/cleaned/keyboard_data_trait_public_cleaned.rds") # trait

### preprocessing and feature extraction competed ###