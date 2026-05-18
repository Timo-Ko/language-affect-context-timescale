#' @param bmr a mlr3 benchmark object
#' @param mes performance measures
#' @return df_results - a data frame with the perfromance measures per fold

extract_bmr_results = function(bmr, mes){
  
  df = as.data.frame(bmr$aggregate(mes)) # get aggregated performance across folds
  uni.learner = unique(df$learner_id) # get unique learner ids
  uni.task = unique(df$task_id) # get unique task ids
  df_results = data.frame() # create empty df with results
  
  for(i in 1:length(uni.task)){ #iterate through task ids
    
    ## standard fl, elastic net, rf
    df_helper = as.data.frame(bmr$aggregate(mes)[learner_id == uni.learner[1]]$resample_result[[i]]$score(mes)) # fl
    df_helper = rbind(df_helper, bmr$aggregate(mes)[learner_id == uni.learner[2]]$resample_result[[i]]$score(mes)) # elastic net
    df_helper = rbind(df_helper, bmr$aggregate(mes)[learner_id == uni.learner[3]]$resample_result[[i]]$score(mes)) # rf
    
    df_results = rbind(df_results, df_helper)
    
    }
  
  return(df_results) # return df results data frame 
}


#' @param data the data frame the ml algos had been trained and tested on
#' @param bmr_results a data frame with performances measures per fold (output of function above)
#' @return results.table - a summary table with all performance measures and p values

# results_table = function(data, bmr_results){
#   
#   n = nrow(data) # get n from data for t tests
#   uni.learner = unique(bmr_results$learner_id) # get the unique learner ids
#   uni.tasks = unique(bmr_results$task_id) # get the unique task ids
#   
#   # create empty results table that is to be filled
#   
#   results.table = data.frame(task_id = rep(uni.tasks, length(uni.learner)),
#                              learner_id = rep(uni.learner,  each = length(uni.tasks)),
# 
#                              Md_srho = rep(NA, length(uni.tasks)*length(uni.learner)),
#                              SD_srho = rep(NA, length(uni.tasks)*length(uni.learner)),
#                              
#                              Md_rsq = rep(NA, length(uni.tasks)*length(uni.learner)), 
#                              SD_rsq = rep(NA, length(uni.tasks)*length(uni.learner)), 
#                              p_rsq = rep(NA, length(uni.tasks)*length(uni.learner)), 
# 
#                              Md_mae = rep(NA, length(uni.tasks)*length(uni.learner)), 
#                              SD_mae = rep(NA, length(uni.tasks)*length(uni.learner)), 
#                              
#                              Md_rmse = rep(NA, length(uni.tasks)*length(uni.learner)), 
#                              SD_rmse = rep(NA, length(uni.tasks)*length(uni.learner))
#                              
#                             )
#   
#   for(uni.task in uni.tasks){ #iterate through single tasks and fill results table
#     
#     df_test = bmr_results %>% dplyr::filter(task_id == uni.task) # filter out rows for that task
#     
#     ### featureless learner
#     
#     results.table$Md_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = median(regr.srho, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = sd(regr.srho, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = median(regr.rsq, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = sd(regr.rsq, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_mae[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = median(regr.mae, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_mae[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = sd(regr.mae, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_rmse[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = median(regr.rmse, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_rmse[results.table$task_id == uni.task & results.table$learner_id == uni.learner[1]] = df_test %>% dplyr::filter(learner_id == uni.learner[1]) %>% summarise(value = sd(regr.rmse, na.rm = TRUE)) %>% pull(value)
#     
#     
#     ### elastic net 
#     
#     results.table$Md_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = median(regr.srho, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = sd(regr.srho, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = median(regr.rsq, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = sd(regr.rsq, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_mae[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = median(regr.mae, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_mae[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = sd(regr.mae, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_rmse[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = median(regr.rmse, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_rmse[results.table$task_id == uni.task & results.table$learner_id == uni.learner[3]] = df_test %>% dplyr::filter(learner_id == uni.learner[3]) %>% summarise(value = sd(regr.rmse, na.rm = TRUE)) %>% pull(value)
#     
#     
#     ### rf
# 
#     results.table$Md_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = median(regr.srho, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_srho[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = sd(regr.srho, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = median(regr.rsq, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_rsq[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = sd(regr.rsq, na.rm = TRUE)) %>% pull(value)
# 
#     results.table$Md_mae[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = median(regr.mae, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_mae[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = sd(regr.mae, na.rm = TRUE)) %>% pull(value)
#     
#     results.table$Md_rmse[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = median(regr.rmse, na.rm = TRUE)) %>% pull(value)
#     results.table$SD_rmse[results.table$task_id == uni.task & results.table$learner_id == uni.learner[2]] = df_test %>% dplyr::filter(learner_id == uni.learner[2]) %>% summarise(value = sd(regr.rmse, na.rm = TRUE)) %>% pull(value)
# 
#       }
#   
#   results.table[,3:ncol(results.table)] = apply(results.table[,3:ncol(results.table)], 2, function(x) round(x, 3)) # round results
#   
#   # Order the table by task_id
#   results.table <- results.table %>%
#     arrange(task_id)
#   
#   return(results.table)
# }


results_table <- function(data, bmr_results) {
  library(dplyr)
  
  out <- bmr_results %>%
    group_by(task_id, learner_id) %>%
    summarise(
      # Spearman's rho
      Md_srho  = median(regr.srho,  na.rm = TRUE),
      LCI_srho = quantile(regr.srho, 0.025, na.rm = TRUE),
      UCI_srho = quantile(regr.srho, 0.975, na.rm = TRUE),
      
      # R^2
      Md_rsq  = median(regr.rsq,  na.rm = TRUE),
      LCI_rsq = quantile(regr.rsq, 0.025, na.rm = TRUE),
      UCI_rsq = quantile(regr.rsq, 0.975, na.rm = TRUE),
      
      # MAE
      Md_mae  = median(regr.mae,  na.rm = TRUE),
      LCI_mae = quantile(regr.mae, 0.025, na.rm = TRUE),
      UCI_mae = quantile(regr.mae, 0.975, na.rm = TRUE),
      
      # RMSE
      Md_rmse  = median(regr.rmse,  na.rm = TRUE),
      LCI_rmse = quantile(regr.rmse, 0.025, na.rm = TRUE),
      UCI_rmse = quantile(regr.rmse, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(task_id, learner_id) %>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  return(out)
}


