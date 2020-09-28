#'Segment
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export segment_new


segment_new <- function(access_token, instance_url, object, field, data_type, newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myobject <- object
  myquery <- paste0('Select Id, ', field,' FROM ', myobject)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)

  data1 <- rforcecom.bulkQuery(session, myquery, myobject)
  #data1 <- na.omit(data1)

  # Select Missing values , Add new field and filled with MISSING level
  new_DF <- data1[is.na(data1[,2]),]
  if(nrow(new_DF) > 0){
    new_DF$dist = "MISSING"
  }

  # select Zero Values, Add new field and filled with ZERO VALUES level
  new_DF1 <- data1[data1[,2] == 0 & !(is.na(data1[,2])),]
  if(nrow(new_DF1) > 0){
    new_DF1$dist = "ZERO VALUES"
  }
  # Data Treatment starts Here
  data2 <- data1[data1[,2] != 0 & !(is.na(data1[,2])),]
  data3 <- subset(data2, select = c(2))
  if(data_type == 0) {
    data3 <- slider(data3, 5)
  } else {
    data3 <- past_date_recency(data3,5)
  }

  data3 <- cbind(data2, data3) # Derived values are binded to the original data
  data3 <- data3[,-2] # Remove replicate fields

  # Add missing values and zero values
  if(nrow(new_DF) > 0){
    data3 = rbind(data3, new_DF)
  }

  if(nrow(new_DF1) > 0){
    data3 = rbind(data3, new_DF1)
  }
  data1 <- subset(data3, select = c("Id", "dist"))
  colnames(data1) <- c("strId", "dist")

  updater(access_token, instance_url, myobject, data1)
}
