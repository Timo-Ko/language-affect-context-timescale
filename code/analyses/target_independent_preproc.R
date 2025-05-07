#' Gets the emoticon scores for all words that were typed in the given typing sessions.
#'
#' @param data df containing features and targets
#' @param no_feature_columns a vector containing the colnames that are not targets
#' @return dataframe with preprocessed features

target_independent_preproc <- function(data, no_feature_columns){
  
  ## transform all features to numerics (if they are not already)
  data[, which(!colnames(data) %in% no_feature_columns)] = apply(data[, which(!colnames(data) %in% no_feature_columns)], 2, function(x) as.numeric(x))
  
  ## replace extreme outliers (M+-4SD) with NA (will be median imputed in target-dependent preprocessing)
  data[, which(!colnames(data) %in% no_feature_columns)] = apply(data[, which(!colnames(data) %in% no_feature_columns)], 2, 
                                                                 function(x) ifelse(x > (mean(x, na.rm = TRUE)+4*sd(x, na.rm = TRUE)) | x < (mean(x, na.rm = TRUE)-4*sd(x, na.rm = TRUE)), NA, x))
  
  ## exclude features with more than 90% missing values
  na_count = sapply(data[, which(!colnames(data) %in% no_feature_columns)], function(y) sum(length(which(is.na(y)))))
  exclude_na.90Perc = colnames(data[, which(!colnames(data) %in% no_feature_columns)])[which(na_count > nrow(data)*0.90)] 
  length(exclude_na.90Perc)
  data = data %>% dplyr::select(-all_of(exclude_na.90Perc))
  
  ## exclude features with zero or near-zero (< 5%) variance 
  exclude_zero.var = nearZeroVar(data[, which(!colnames(data) %in% no_feature_columns)], freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, allowParallel = TRUE)
  length(exclude_zero.var)
  exclude_zero.var = colnames(data[, which(!colnames(data) %in% no_feature_columns)])[exclude_zero.var]
  data = data %>% dplyr::select(-all_of(exclude_zero.var))
  
  return(data)
}
