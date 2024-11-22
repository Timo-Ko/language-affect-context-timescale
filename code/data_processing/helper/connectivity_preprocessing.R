# Connectivity Preprocessing

### BLUETOOTH, WIFI, POWER, AIRPLANE (flight mode): preprocessing helper function

connectivity_preprocessing = function(sensing.data, event.type){
  sensing.data$event_name = NA
  sensing.data$event_name[which(sensing.data$activityName == event.type)] = sensing.data$event[which(sensing.data$activityName == event.type)]
  sensing.data$event_descr = NA 
  sensing.data$event_descr[which(sensing.data$activityName == event.type)] = sensing.data$description[which(sensing.data$activityName == event.type)]
  
  colnames(sensing.data)[which(colnames(sensing.data) == "event_name")] = event.type
  colnames(sensing.data)[which(colnames(sensing.data) == "event_descr")] = paste0(event.type, "_descr")
  
  return(sensing.data)
}




