#'Industry ranking
#'
#'This function ranks a categorical variable from a numeric variable

#' @import RForcecom
#' @import jsonlite
#' @import dplyr
#' @export ranks

ranks <- function(access_token, instance_url, object, depfield, data_type1,indfield, data_type2, newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myquery <- paste0('Select Id, ', depfield,', ',indfield,' FROM ', object)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)
  data1 <- na.omit(data1)

  if(data_type1 == "B"){
    data1[,2] <- date_recency(data1[,2],5)
  }else{
    #data3 <- past_date_recency(data1[,2],5)
    data1[,2] <- data1[,2]
  }

  if(data_type2 == "B"){
    data3 <- past_date_recency(data1[,c(1,3)],5)
    data3 <- merge(data1, data3, all.x = T)
    data1[,3] <- data3$dist
  }else{
    #data3 <- past_date_recency(data1[,3],5)
    data1[,3] <- data1[,3]

  }

  data1 <- ranker(data1)
  data1 <- subset(data1, select = c("Id", "decile"))
  data1$decile[data1$decile == 5 | data1$decile == 6 | data1$decile == 7 | data1$decile == 8] <- 5
  data1$decile[data1$decile == 9 | data1$decile == 10 | data1$decile == 11] <- 6

  colnames(data1) <- c("Id", newname)

  updater(access_token, instance_url, object, data1)
  return(data1)

}
