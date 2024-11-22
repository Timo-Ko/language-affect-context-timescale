#' Find all unqiue emoji in the data set 
#' 
#' @author Timo Koch
#' @family Preprocessing step
#' @import dplyr
#' @import stringr
#' @import utf8
#' @import readxl
#' @description this script finds all unique emoji that have been used in the data set (you only need to run this script once)
#' @return a data frame called emoji_df containing the low and high surrogate as well as a picture of the respective emoji and a unique variable name
#' @export 
#' 
#' 
#' # get all unique emoji keyboard regex matches
emoji_events = dplyr::tbl(keyboard, "abstracted_action_event") %>% 
  dplyr::filter(regex_matcher_id == 1) %>% # get all emoji events, i.e. regex matcher id = 1
  data.frame()

# emoji are in the json string, parse json
emoji_data_parsed = parseJsonColumnSensing(emoji_events, "event_json")  

unique_emoji_sequences <- unique(emoji_data_parsed$rawContentAfter) # get all used unique emoji sequences in the data set

# some of these sequences are very long! these are multiple emoji (one emoji = two integers of a total of 10 digits)

# Step 1: Remove unwanted elements
filtered_sequences <- lapply(unique_emoji_sequences, function(sublist) {
  sublist <- sublist[!sublist %in% c(45, 65039, 65532)]
  return(sublist)
})

# Step 2: Create pairs
create_pairs <- function(sublist) {
  if (length(sublist) == 0) {
    return(NULL)  # Return NULL for empty sublists
  }
  if (length(sublist) %% 2 != 0) {
    sublist <- sublist[-length(sublist)]  # Remove last element if odd
  }
  matrix(sublist, ncol = 2, byrow = TRUE)
}

pairs_list <- lapply(filtered_sequences, create_pairs)

# Filter out NULL values from pairs_list
pairs_list <- pairs_list[!sapply(pairs_list, is.null)]

# Step 3: Aggregate and find unique pairs
all_pairs <- do.call(rbind, pairs_list)
unique_pairs <- unique(all_pairs)

dim(unique_pairs)

## translate utf16 codes into feature names and actual emoji pictures

# Create an empty data frame with the desired column names
emoji_df <- data.frame(
  high_surrogate = integer(0),
  low_surrogate = integer(0),
  unicode_code_point_dec = integer(0),
  unicode_code_point_hex = character(0),
  emoji = character(0),
  variable_name = character(0)
)

# Iterate through unique sequences
for (i in 1:nrow(unique_pairs)) {
  sequence <- unique_pairs[i,]
  
  # Extract the high and low surrogates
  high_surrogate <- sequence[1]
  low_surrogate <- sequence[2]
  
  # Check if both surrogates are valid
  if (high_surrogate >= 0xD800 && high_surrogate <= 0xDBFF &&
      low_surrogate >= 0xDC00 && low_surrogate <= 0xDFFF) {
    # Calculate the Unicode code point from the surrogate pair
    unicode_code_point_dec <- (high_surrogate - 0xD800) * 0x400 + (low_surrogate - 0xDC00) + 0x10000
    
    # Convert the decimal code point to a hexadecimal string
    unicode_code_point_hex <- sprintf("0x%X", unicode_code_point_dec)
    
    # Convert the code point to an emoji character
    emoji <- intToUtf8(unicode_code_point_dec)
    
    # Create the variable name
    variable_name <- paste("emoji", unicode_code_point_dec, sep = "_")
    
    # Add the data to the data frame
    new_row <- data.frame(
      high_surrogate = high_surrogate,
      low_surrogate = low_surrogate,
      unicode_code_point_dec = unicode_code_point_dec,
      unicode_code_point_hex = unicode_code_point_hex,
      emoji = emoji,
      variable_name = variable_name
    )
    
    emoji_df <- rbind(emoji_df, new_row)
    
  }
}

dim(emoji_df)

# Reset row names
rownames(emoji_df) <- NULL

## add emoji sentiment 

# Read the Excel file into a data frame
emoji_sentiment  <- read.csv("data/helper/SentimentOfEmojis_edited.csv", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "character","character"))

# capitalize all letters in unicode for proper matching
emoji_sentiment$unicode_codepoint <- str_to_upper(emoji_sentiment$unicode_codepoint)
emoji_df$unicode_code_point_hex <- str_to_upper(emoji_df$unicode_code_point_hex )

# append emoji sentiment column to emoji_df
emoji_df <- left_join(emoji_df, emoji_sentiment[, c("unicode_codepoint", "Sentiment.score")], by = c("unicode_code_point_hex" ="unicode_codepoint"))

# rename sentiment score column
names(emoji_df)[names(emoji_df) == "Sentiment.score"] <- "sentiment_score"

# save data frame 
saveRDS(emoji_df, "data/helper/emoji_df.RData")

## Finished 