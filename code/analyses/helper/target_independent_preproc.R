#' @param data df containing features and targets
#' @param no_feature_columns a vector containing the colnames that are not targets
#' @return dataframe with preprocessed features

target_independent_preproc <- function(data, no_feature_columns){
  
  ## transform all features to numerics (if they are not already)
  
  # convert difftime cols to numeric 
  difftime_cols <- sapply(data, function(x) inherits(x, "difftime"))
  data[difftime_cols] <- lapply(data[difftime_cols], function(x) as.numeric(x, units = "secs"))
  
  # convert all others to numeric
  data[, which(!colnames(data) %in% no_feature_columns)] = apply(data[, which(!colnames(data) %in% no_feature_columns)], 2, function(x) as.numeric(x))
  
  ## replace extreme outliers (M+-6SD) with NA (will be imputed in target-dependent preprocessing)
  data[, which(!colnames(data) %in% no_feature_columns)] = apply(data[, which(!colnames(data) %in% no_feature_columns)], 2, 
                                                                 function(x) ifelse(x > (mean(x, na.rm = TRUE)+6*sd(x, na.rm = TRUE)) | x < (mean(x, na.rm = TRUE)-6*sd(x, na.rm = TRUE)), NA, x))
  
  ## exclude features with more than 90% missing values (NAs)
  na_count = sapply(data[, which(!colnames(data) %in% no_feature_columns)], function(y) sum(length(which(is.na(y)))))
  exclude_na.90Perc = colnames(data[, which(!colnames(data) %in% no_feature_columns)])[which(na_count > nrow(data)*0.90)] 
  print(paste(length(exclude_na.90Perc), "features excluded: More than 90% missings."))
  data = data %>% dplyr::select(-all_of(exclude_na.90Perc))
  
  ## exclude features with >98% same non-na values (i.e., less than 2% different non-na values)
  columns_to_remove <- data %>%
    select(where(~ {
      non_na_vals <- .[!is.na(.)]
      most_frequent_proportion <- max(table(non_na_vals)) / length(non_na_vals)
      most_frequent_proportion >= 0.98
    })) %>%
    colnames()
  
  # Print the number of columns that will be excluded
  print(paste(length(columns_to_remove), "features excluded: Less than 2% different non-na values."))
  
  #  Select the remaining columns
  data <- data %>%
    select(-all_of(columns_to_remove))
  
  # ## exclude highly correlated features
  # # compute correlations between features
  # cors_feat = cor(data[, which(!colnames(data) %in% no_feature_columns)], use="pairwise.complete.obs")
  # cors_feat[is.na(cors_feat)] = 0
  # 
  # # find features that are highly correlated (> 0.90) and drop them 
  # exclude_high.cor = caret::findCorrelation(cors_feat, cutoff = 0.90, names = TRUE)
  # print(paste(length(exclude_high.cor), "features excluded: Highly correlated (>.90)."))
  # data = data %>% dplyr::select(-all_of(exclude_high.cor))
  # 
  # data$user_id = as.character(data$user_id)
  
  return(data)
}
