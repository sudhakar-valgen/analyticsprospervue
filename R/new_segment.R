#'Segment
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export segment1


segment1 <- function(access_token, instance_url, object, field, newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myobject <- object
  myquery <- paste0('Select Id, ', field,' FROM ', myobject)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)

  data1 <- rforcecom.bulkQuery(session, myquery, myobject)
  #data1 <- na.omit(data[,c(1,2)])

  if(is.numeric(data1[,2]) == T){
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
    data3 <- slider(data3, 5)
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
  } else{
    data1 <- past_date_recency(data1)
    colnames(data1) <- c("strId", "dist")
  }

  # data1 <- subset(data3, select = c("Id", "dist"))
  # colnames(data1) <- c("strId", "dist")

  updater(access_token, instance_url, myobject, data1)
}
