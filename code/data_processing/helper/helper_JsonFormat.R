#' Helper function to prepare raw keyboard data
#' 
#' @author F. Bemmann
#' @family Preprocessing function 
#' @description this function unfolds json data into one column per key-value pair 
#' @export 

parseJsonColumnSensing = function(df, column_name){
  
  df2 = df %>% select(user_uuid, client_event_id, !!column_name) %>% filter(!is.na(!!rlang::sym(column_name))) %>% map_dfc(.f = parseJsonColumn) %>% distinct()
  
  colnames(df2)[1:2] = c("user_uuid", "client_event_id")
  
  df = left_join(df, df2, by = c("user_uuid", "client_event_id"))
  df[,column_name] = NULL
  
  return(df)
}

# this function is used in the function above
parseJsonColumn = function(x){
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% jsonlite::fromJSON(flatten = T) %>% as_tibble()
}


## little support function: create list with entries, except the entry in unknown (then code as NA)
helper_cell = function(xs){
  map(xs, function(x){x %>% na_if("unknown")})
} 






