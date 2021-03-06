#'Past Recency
#'
#'This function will do the past recency of the date variable

#' @import RForcecom
#' @import jsonlite
#' @import dplyr
#' @export Future_recency

Future_recency <- function(access_token, instance_url, object, field1, newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myobject <- object
  myquery <- paste0('Select Id, ', field1,'FROM ', myobject)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, myobject)

  # To  todays date
  today <- Sys.Date()
  #date_field <- data$Future_date
  # Convert and fixed to date formats
  a = data.frame()
  a <- as.Date(data1[,2], "%m-%d-%Y")
  a = as.data.frame(a)
  colnames(a)[1] = "Future_date"
  a$Past_recency <- a$Future_date  - today

  ### Substract Max date value by min - 1
  #a$Past_recency_min <- a$Past_recency - (min(a$Past_recency, na.rm = T) - 1)
  max_value <- max(a$Past_recency, na.rm = T)

  ### Count the missing value in the past recency
  #count_missing <- table(is.na(a$Past_recency) == TRUE)

  a$Past_recency_min <- as.numeric(a$Past_recency)
  missing_data <- a[is.na(a$Past_recency),]
  a = na.omit(a)
  zeros_data <-  a[a$Past_recency_min == 0,]

  Percentage <- ((nrow(missing_data) + nrow(zeros_data)) / nrow(data)) * 100
  for (i in 1:nrow(a)) {
    if(Percentage < 10) {
      b1 = quantile(a$Past_recency_min, c(.15))
      b2 = quantile(a$Past_recency_min, c(.30))
      b3 = quantile(a$Past_recency_min, c(.45))
      b4 = quantile(a$Past_recency_min, c(.60))
      b5 = quantile(a$Past_recency_min, c(.75))
      b6 = quantile(a$Past_recency_min, c(.90))


      b1 <- ifelse(b1 < 7,7,ifelse(b1 < 14,14,ifelse(b1 < 21,21,ifelse(b1 < 30,30,ifelse(b1 < 60, 60,
                                                                                         ifelse(b1 < 90, 90,ifelse(b1 < 120, 120, ifelse(b1 < 150,150,
                                                                                                                                         ifelse(b1 < 180,180,
                                                                                                                                                ifelse(b1 < 210, 210,
                                                                                                                                                       ifelse(b1 < 240,240,
                                                                                                                                                              ifelse(b1 < 270, 270,
                                                                                                                                                                     ifelse(b1 < 300,300,
                                                                                                                                                                            ifelse(b1 < 365, 365,
                                                                                                                                                                                   ifelse(b1 < 730, 730,
                                                                                                                                                                                          ifelse(b1 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b1 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b1 < 3650, 3650, 5475))))))))))))))))))

      b2 <- ifelse(b2 < 7,7,ifelse(b2 < 14,14,ifelse(b2 < 21,21,ifelse(b2 < 30,30,ifelse(b2 < 60, 60,
                                                                                         ifelse(b2 < 90, 90,ifelse(b2 < 120, 120, ifelse(b2 < 150,150,
                                                                                                                                         ifelse(b2 < 180,180,
                                                                                                                                                ifelse(b2 < 210, 210,
                                                                                                                                                       ifelse(b2 < 240,240,
                                                                                                                                                              ifelse(b2 < 270, 270,
                                                                                                                                                                     ifelse(b2 < 300,300,
                                                                                                                                                                            ifelse(b2 < 365, 365,
                                                                                                                                                                                   ifelse(b2 < 730, 730,
                                                                                                                                                                                          ifelse(b2 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b2 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b2 < 3650, 3650, 5475))))))))))))))))))
      b3 <- ifelse(b3 < 7,7,ifelse(b3 < 14,14,ifelse(b3 < 21,21,ifelse(b3 < 30,30,ifelse(b3 < 60, 60,
                                                                                         ifelse(b3 < 90, 90,ifelse(b3 < 120, 120, ifelse(b3 < 150,150,
                                                                                                                                         ifelse(b3 < 180,180,
                                                                                                                                                ifelse(b3 < 210, 210,
                                                                                                                                                       ifelse(b3 < 240,240,
                                                                                                                                                              ifelse(b3 < 270, 270,
                                                                                                                                                                     ifelse(b3 < 300,300,
                                                                                                                                                                            ifelse(b3 < 365, 365,
                                                                                                                                                                                   ifelse(b3 < 730, 730,
                                                                                                                                                                                          ifelse(b3 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b3 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b3 < 3650, 3650, 5475))))))))))))))))))
      b4 <- ifelse(b4 < 7,7,ifelse(b4 < 14,14,ifelse(b4 < 21,21,ifelse(b4 < 30,30,ifelse(b4 < 60, 60,
                                                                                         ifelse(b4 < 90, 90,ifelse(b4 < 120, 120, ifelse(b4 < 150,150,
                                                                                                                                         ifelse(b4 < 180,180,
                                                                                                                                                ifelse(b4 < 210, 210,
                                                                                                                                                       ifelse(b4 < 240,240,
                                                                                                                                                              ifelse(b4 < 270, 270,
                                                                                                                                                                     ifelse(b4 < 300,300,
                                                                                                                                                                            ifelse(b4 < 365, 365,
                                                                                                                                                                                   ifelse(b4 < 730, 730,
                                                                                                                                                                                          ifelse(b4 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b4 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b4 < 3650, 3650, 5475))))))))))))))))))
      b5 <- ifelse(b5 < 7,7,ifelse(b5 < 14,14,ifelse(b5 < 21,21,ifelse(b5 < 30,30,ifelse(b5 < 60, 60,
                                                                                         ifelse(b5 < 90, 90,ifelse(b5 < 120, 120, ifelse(b5 < 150,150,
                                                                                                                                         ifelse(b5 < 180,180,
                                                                                                                                                ifelse(b5 < 210, 210,
                                                                                                                                                       ifelse(b5 < 240,240,
                                                                                                                                                              ifelse(b5 < 270, 270,
                                                                                                                                                                     ifelse(b5 < 300,300,
                                                                                                                                                                            ifelse(b5 < 365, 365,
                                                                                                                                                                                   ifelse(b5 < 730, 730,
                                                                                                                                                                                          ifelse(b5 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b5 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b5 < 3650, 3650, 5475))))))))))))))))))
      b6 <- ifelse(b6 < 7,7,ifelse(b6 < 14,14,ifelse(b6 < 21,21,ifelse(b6 < 30,30,ifelse(b6 < 60, 60,
                                                                                         ifelse(b6 < 90, 90,ifelse(b6 < 120, 120, ifelse(b6 < 150,150,
                                                                                                                                         ifelse(b6 < 180,180,
                                                                                                                                                ifelse(b6 < 210, 210,
                                                                                                                                                       ifelse(b6 < 240,240,
                                                                                                                                                              ifelse(b6 < 270, 270,
                                                                                                                                                                     ifelse(b6 < 300,300,
                                                                                                                                                                            ifelse(b6 < 365, 365,
                                                                                                                                                                                   ifelse(b6 < 730, 730,
                                                                                                                                                                                          ifelse(b6 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b6 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b6 < 3650, 3650, 5475))))))))))))))))))

      # b1 = round(b1, digits = -1)
      # b2 = round(b2, digits = -1)
      # b3 = round(b3, digits = -1)
      # b4 = round(b4, digits = -1)
      # b5 = round(b5, digits = -1)
      # b6 = round(b6, digits = -1)
      if(a$Past_recency_min[i] <= b1){
        a$new_Segment[i] <- (paste0("1 to ", b1))
      } else if(a$Past_recency_min[i] > b1 & a$Past_recency_min[i] <= b2){
        a$new_Segment[i] <- (paste0(b1 + 1," to ", b2))
      } else if(a$Past_recency_min[i] > b2 & a$Past_recency_min[i] <= b3){
        a$new_Segment[i] <- (paste0(b2 + 1," to ", b3))
      } else if(a$Past_recency_min[i] > b3 & a$Past_recency_min[i] <= b4){
        a$new_Segment[i] <- (paste0(b3 + 1," to ", b4))
      } else if(a$Past_recency_min[i] > b4 & a$Past_recency_min[i] <= b5){
        a$new_Segment[i] <- (paste0(b4 + 1," to ", b5))
      } else if(a$Past_recency_min[i] > b5 & a$Past_recency_min[i] <= b6){
        a$new_Segment[i] <- (paste0(b5 + 1," to ", b6))
      } else {
        a$new_Segment [i]<- (paste0(b6,"+"))
      }
    } else if(Percentage >= 10 & Percentage < 20){
      b1 = quantile(a$Past_recency_min, c(.20))
      b2 = quantile(a$Past_recency_min, c(.40))
      b3 = quantile(a$Past_recency_min, c(.60))
      b4 = quantile(a$Past_recency_min, c(.80))


      b1 <- ifelse(b1 < 7,7,ifelse(b1 < 14,14,ifelse(b1 < 21,21,ifelse(b1 < 30,30,ifelse(b1 < 60, 60,
                                                                                         ifelse(b1 < 90, 90,ifelse(b1 < 120, 120, ifelse(b1 < 150,150,
                                                                                                                                         ifelse(b1 < 180,180,
                                                                                                                                                ifelse(b1 < 210, 210,
                                                                                                                                                       ifelse(b1 < 240,240,
                                                                                                                                                              ifelse(b1 < 270, 270,
                                                                                                                                                                     ifelse(b1 < 300,300,
                                                                                                                                                                            ifelse(b1 < 365, 365,
                                                                                                                                                                                   ifelse(b1 < 730, 730,
                                                                                                                                                                                          ifelse(b1 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b1 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b1 < 3650, 3650, 5475))))))))))))))))))

      b2 <- ifelse(b2 < 7,7,ifelse(b2 < 14,14,ifelse(b2 < 21,21,ifelse(b2 < 30,30,ifelse(b2 < 60, 60,
                                                                                         ifelse(b2 < 90, 90,ifelse(b2 < 120, 120, ifelse(b2 < 150,150,
                                                                                                                                         ifelse(b2 < 180,180,
                                                                                                                                                ifelse(b2 < 210, 210,
                                                                                                                                                       ifelse(b2 < 240,240,
                                                                                                                                                              ifelse(b2 < 270, 270,
                                                                                                                                                                     ifelse(b2 < 300,
                                                                                                                                                                            ifelse(b2 < 365, 365,
                                                                                                                                                                                   ifelse(b2 < 730, 730,
                                                                                                                                                                                          ifelse(b2 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b2 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b2 < 3650, 3650, 5475))))))))))))))))))
      b3 <- ifelse(b3 < 7,7,ifelse(b3 < 14,14,ifelse(b3 < 21,21,ifelse(b3 < 30,30,ifelse(b3 < 60, 60,
                                                                                         ifelse(b3 < 90, 90,ifelse(b3 < 120, 120, ifelse(b3 < 150,150,
                                                                                                                                         ifelse(b3 < 180,180,
                                                                                                                                                ifelse(b3 < 210, 210,
                                                                                                                                                       ifelse(b3 < 240,240,
                                                                                                                                                              ifelse(b3 < 270, 270,
                                                                                                                                                                     ifelse(b3 < 300,
                                                                                                                                                                            ifelse(b3 < 365, 365,
                                                                                                                                                                                   ifelse(b3 < 730, 730,
                                                                                                                                                                                          ifelse(b3 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b3 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b3 < 3650, 3650, 5475))))))))))))))))))
      b4 <- ifelse(b4 < 7,7,ifelse(b4 < 14,14,ifelse(b4 < 21,21,ifelse(b4 < 30,30,ifelse(b4 < 60, 60,
                                                                                         ifelse(b4 < 90, 90,ifelse(b4 < 120, 120, ifelse(b4 < 150,150,
                                                                                                                                         ifelse(b4 < 180,180,
                                                                                                                                                ifelse(b4 < 210, 210,
                                                                                                                                                       ifelse(b4 < 240,240,
                                                                                                                                                              ifelse(b4 < 270, 270,
                                                                                                                                                                     ifelse(b4 < 300,300,
                                                                                                                                                                            ifelse(b4 < 365, 365,
                                                                                                                                                                                   ifelse(b4 < 730, 730,
                                                                                                                                                                                          ifelse(b4 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b4 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b4 < 3650, 3650, 5475))))))))))))))))))

      # b1 = round(b1, digits = -1)
      # b2 = round(b2, digits = -1)
      # b3 = round(b3, digits = -1)
      # b4 = round(b4, digits = -1)
      if(a$Past_recency_min[i] <= b1){
        a$new_Segment[i] <- (paste0("0 to ", b1))
      } else if(a$Past_recency_min[i] > b1 & a$Past_recency_min[i] <= b2){
        a$new_Segment[i] <- (paste0(b1 + 1," to ", b2))
      } else if(a$Past_recency_min[i] > b2 & a$Past_recency_min[i] <= b3){
        a$new_Segment[i] <- (paste0(b2 + 1," to ", b3))
      } else if(a$Past_recency_min[i] > b3 & a$Past_recency_min[i] <= b4){
        a$new_Segment[i] <- (paste0(b3 + 1," to ", b4))
      } else {
        a$new_Segment[i] <- paste0(b4,"+")
      }

    } else {
      b1 = quantile(a$Past_recency_min, c(.25))
      b2 = quantile(a$Past_recency_min, c(.50))
      b3 = quantile(a$Past_recency_min, c(.75))

      b1 <- ifelse(b1 < 7,7,ifelse(b1 < 14,14,ifelse(b1 < 21,21,ifelse(b1 < 30,30,ifelse(b1 < 60, 60,
                                                                                         ifelse(b1 < 90, 90,ifelse(b1 < 120, 120, ifelse(b1 < 150,150,
                                                                                                                                         ifelse(b1 < 180,180,
                                                                                                                                                ifelse(b1 < 210, 210,
                                                                                                                                                       ifelse(b1 < 240,240,
                                                                                                                                                              ifelse(b1 < 270, 270,
                                                                                                                                                                     ifelse(b1 < 300,
                                                                                                                                                                            ifelse(b1 < 365, 365,
                                                                                                                                                                                   ifelse(b1 < 730, 730,
                                                                                                                                                                                          ifelse(b1 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b1 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b1 < 3650, 3650, 5475))))))))))))))))))

      b2 <- ifelse(b2 < 7,7,ifelse(b2 < 14,14,ifelse(b2 < 21,21,ifelse(b2 < 30,30,ifelse(b2 < 60, 60,
                                                                                         ifelse(b2 < 90, 90,ifelse(b2 < 120, 120, ifelse(b2 < 150,150,
                                                                                                                                         ifelse(b2 < 180,180,
                                                                                                                                                ifelse(b2 < 210, 210,
                                                                                                                                                       ifelse(b2 < 240,240,
                                                                                                                                                              ifelse(b2 < 270, 270,
                                                                                                                                                                     ifelse(b2 < 300,
                                                                                                                                                                            ifelse(b2 < 365, 365,
                                                                                                                                                                                   ifelse(b2 < 730, 730,
                                                                                                                                                                                          ifelse(b2 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b2 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b2 < 3650, 3650, 5475))))))))))))))))))
      b3 <- ifelse(b3 < 7,7,ifelse(b3 < 14,14,ifelse(b3 < 21,21,ifelse(b3 < 30,30,ifelse(b3 < 60, 60,
                                                                                         ifelse(b3 < 90, 90,ifelse(b3 < 120, 120, ifelse(b3 < 150,150,
                                                                                                                                         ifelse(b3 < 180,180,
                                                                                                                                                ifelse(b3 < 210, 210,
                                                                                                                                                       ifelse(b3 < 240,240,
                                                                                                                                                              ifelse(b3 < 270, 270,
                                                                                                                                                                     ifelse(b3 < 300,
                                                                                                                                                                            ifelse(b3 < 365, 365,
                                                                                                                                                                                   ifelse(b3 < 730, 730,
                                                                                                                                                                                          ifelse(b3 < 1095, 1095,
                                                                                                                                                                                                 ifelse(b3 < 1825, 1825,
                                                                                                                                                                                                        ifelse(b3 < 3650, 3650, 5475))))))))))))))))))
      # b1 = round(b1, digits = -1)
      # b2 = round(b2, digits = -1)
      # b3 = round(b3, digits = -1)
      if(a$Past_recency_min[i] <= b1){
        a$new_Segment[i] <- (paste0("0 to ", b1))
      } else if(a$Past_recency_min[i] > b1 &  a$Past_recency_min[i] <= b2){
        a$new_Segment[i] <- (paste0(b1 + 1," to ", b2))
      } else if(a$Past_recency_min[i] > b2 & a$Past_recency_min[i] <= b3){
        a$new_Segment[i] <- (paste0(b2 + 1," to ", b3))
      } else {
        a$new_Segment[i] <- (paste0(b3,"+"))
      }
    }
  }

  missing_data$new_Segment <- "Data is Not Available"

  #zeros_data$new_segment <- " "
  n <- nrow(zeros_data)
  if(n > 0 ){
    zeros_data$new_Segment <- "Zeros"
    new_data <- rbind(a,missing_data, zeros_data)
  } else {
    new_data <- rbind(a, missing_data)
  }

  data1 <- subset(new_data, select = c("Id", "new_segment"))
  colnames(data1) <- c("strId", "dist")
  updater(access_token, instance_url, myobject, newdata)
  return(new_data)
}
