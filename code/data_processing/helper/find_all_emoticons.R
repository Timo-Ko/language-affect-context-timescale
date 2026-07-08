#' Find all unqiue emoticons in the data set 
#' 
#' @family Preprocessing function 
#' @description this script finds all unique emoticons that have been used in the data set
#' @return a data frame containing the low and high surrogate as well as a picture of the respective emoticons and a unique variable name
#' @export 
#' 
#' 
#' # get all unique emoticon keyboard matches
emoticon_events = dplyr::tbl(keyboard, "abstracted_action_event") %>% 
  dplyr::filter(logical_category_list_id == 2) %>% # get all emoticon events
  data.frame()

# find all unique emoticon events and keep one row per event (reduced computational load)
unique_emoticon_events <- emoticon_events %>%
  group_by(event_json) %>%
  slice(1)

# emoticon are in the json string, parse json
emoticon.data.paersed = parseJsonColumnSensing(unique_emoticon_events, "event_json")  

unique_emoticon <- unique(emoticon.data.paersed$categoryAfter) # get all used unique emoticons

# drop unknown and NA
unique_emoticon <- unique_emoticon[unique_emoticon != "unknown" & !is.na(unique_emoticon)]

# remove double escaping
unique_emoticon <- gsub("\\\\", "\\", unique_emoticon)

# load file with all unique emoticon names that could have been logged
emoticons_unique_names <- read.csv2("data/helper/emoticons_unique_names.csv", row.names = NULL)

# merge them to create df with used unique emoticons and corresponding names that have been used in the keyboard data set
emoticons_df <- emoticons_unique_names[emoticons_unique_names$emoticon %in% unique_emoticon, ]

# created a column with escaped emoticons for pattern matching
emoticons_df$emoticon_escaped = sapply(emoticons_df$emoticon, function(x) paste0("\\", x))

# save data frame 
saveRDS(emoticons_df, "data/helper/emoticons_df.rds")

## Finished 