#'SIC Profiling
#'
#'This function indexes one dependent variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export deciling




deciling <- function(access_token, instance_url, object, catfield, numfield,non_core,dynamic,data_split,newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myquery <- paste0('Select Id, ', catfield,', ',numfield,',',non_core,'FROM' , object)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)
  #data1 <- na.omit(data1)

  data1[,2] <- as.factor(data1[,2])
  data1[,3] <- as.numeric(data1[,3])

  data5 <- ranker_decile(data1, dynamic,data_split)
  data1 <- subset(data5, select = c("Id", "decile"))
  colnames(data1) <- c("Id", newname)

  updater(access_token, instance_url, object, data1)
  return(data1)

}
