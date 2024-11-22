# helper functions: customized aggregation functions

my.n = function(x){
  if(length(x) == 0) {y = 0}
  if(length(x) != 0) {y = length(x)}  
  return(y)
}

my.sum = function(x){
  if(length(x) == 0) {y = 0} ## 0 if no entries can be found
  if(length(x) != 0) {y = sum(x, na.rm = TRUE)} ## data available
  if(length(x) != 0 && all(is.na(x))) {y = NA} ## NA if there are only NA's (technical logging issues)
  return(y)
}


my.median = function(x){
  if(length(x) == 0) {y = 0} ## 0 if no entries can be found
  if(length(x) != 0) {y = median(x, na.rm = TRUE)} ## data available
  if(length(x) != 0 && all(is.na(x))){y = NA} ## NA if there are only NA's (technical logging issues)
  return(y)
}

my.mad = function(x){
  if(length(x) == 0) {y = 0} ## 0 if no entries can be found
  if(length(x) != 0) {y = mad(x, na.rm = TRUE)} ## data available
  if(length(x) != 0 && all(is.na(x))){y = NA} ## NA if there are only NA's (technical logging issues)
  return(y)
}

my.min = function(x){
  if(length(x) == 0) {y = 0} ## 0 if no entries can be found
  if(length(x) != 0) {y = min(x, na.rm = TRUE)} ## data available
  if(length(x) != 0 && all(is.na(x))){y = NA} ## NA if there are only NA's (technical logging issues)
  return(y)
}

my.max = function(x){
  if(length(x) == 0) {y = 0} ## 0 if no entries can be found
  if(length(x) != 0) {y = max(x, na.rm = TRUE)} ## data available
  if(length(x) != 0 && all(is.na(x))){y = NA} ## NA if there are only NA's (technical logging issues)
  return(y)
}

sum.positive = function(x){
  x = x[which(x >= 0)]
  y = sum(x)
  return(y)
}

sum.negative = function(x){
  x = x[which(x < 0)]
  y = sum(x)
  return(y)
}


