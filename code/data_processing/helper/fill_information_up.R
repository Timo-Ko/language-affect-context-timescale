#' Helper function to fill up information  
#' 
#' @author R. Schoedel
#' @family Preprocessing function 
#' @import dplyr
#' @description this function fills NAs with previous entry until new event occurs; label as consecutive event sessions
#' @return sensing data with new column for the filled up information
#' @export 

fill_info = function(sensing.data, var, var_desc, on){
  
  sensing.data$df_session = NA
  i = 1
  df_b = which(!is.na(sensing.data[,var]))
  
  bs = 1:(length(df_b)-1)
  
  for(b in bs){
    firstb = df_b[b]
    lastb = df_b[b+1]
    
    if(lastb-firstb <= 1 && sensing.data[firstb, var] == sensing.data[lastb, var]){
      sensing.data$df_session[firstb] = i
    }
    
    if(lastb-firstb <= 1 && sensing.data[firstb, var] != sensing.data[lastb, var]){
      sensing.data$df_session[firstb] = i
      i = i+1
    }
    
    if(lastb-firstb > 1 && sensing.data[firstb, var] == sensing.data[lastb, var]){
      sensing.data[(firstb+1):(lastb-1), var] = sensing.data[firstb, var]
      if(is.null(var_desc == FALSE)){
        if(sensing.data[firstb, var] %in% on) sensing.data[(firstb+1):(lastb-1), var_desc] = sensing.data[firstb, var_desc]
      }
      sensing.data$df_session[firstb:(lastb-1)] = i
    }
    
    if(lastb-firstb > 1 && sensing.data[firstb, var] != sensing.data[lastb, var]){
      sensing.data[(firstb+1):(lastb-1), var] = sensing.data[firstb, var]
      if(is.null(var_desc) == FALSE){
        if(sensing.data[firstb, var] %in% on) sensing.data[(firstb+1):(lastb-1), var_desc] = sensing.data[firstb, var_desc]
      }
      sensing.data$df_session[firstb:(lastb-1)] = i
      i = i+1
    }
    
  }
  
  colnames(sensing.data)[which(colnames(sensing.data) == "df_session")] = paste0(var, "_session")
  
  return(sensing.data)
}
