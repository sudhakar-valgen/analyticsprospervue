#'Segment
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export segment_old
segment_old <- function(access_token,instance_url,object,field,data_type,newname){
  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myobject <- object
  myquery <- paste0('Select Id, ', field,' FROM ', myobject)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)

  data1 <- rforcecom.bulkQuery(session, myquery, myobject)
  #data1 <- na.omit(data1)

  #data1 = data[,c(1,2)]
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
  #data_type = c("B")

  if(data_type == "B") {
    data3 <- date_recency(data3,5)
    data3 = as.data.frame(data3)
    ################################################################################
    a = data3
    colnames(a)[1] = "Past_recency_min"
    a$Past_recency_min <- as.numeric(a$Past_recency_min)
    missing_data <- a[is.na(a$Past_recency),]
    a = na.omit(a)
    zeros_data <-  a[a$Past_recency_min == 0,]
    #Percentage <- (nrow(zeros_data) / nrow(a)) * 100
    Percentage = 5
    for (i in 1:nrow(a)) {
      if(Percentage < 10) {
        b1 = quantile(a$Past_recency_min, c(.15))
        b2 = quantile(a$Past_recency_min, c(.30))
        b3 = quantile(a$Past_recency_min, c(.45))
        b4 = quantile(a$Past_recency_min, c(.60))
        b5 = quantile(a$Past_recency_min, c(.75))
        b6 = quantile(a$Past_recency_min, c(.90))
        if(b1 < 30){
          z = round(b1 / 7)
          b1 <- z * 7
        } else if(b1 < 180){
          z = round(b1 / 15)
          b1 <- z * 15
        } else{
          z = round(b1 / 30)
          b1 <- z * 30
        }
        if(b2 < 30){
          z = round(b2 / 7)
          b2 <- z * 7
        } else if(b2 < 180){
          z = round(b2 / 15)
          b2 <- z * 15
        } else{
          z = round(b2 / 30)
          b2 <- z * 30
        }
        if(b3 < 30){
          z = round(b3 / 7)
          b3 <- z * 7
        } else if(b3 < 180){
          z = round(b3 / 15)
          b3 <- z * 15
        } else{
          z = round(b3 / 30)
          b3 <- z * 30
        }
        if(b4 < 30){
          z = round(b4 / 7)
          b4 <- z * 7
        } else if(b4 < 180){
          z = round(b4 / 15)
          b4 <- z * 15
        } else{
          z = round(b4 / 30)
          b4 <- z * 30
        }
        if(b5 < 30){
          z = round(b5 / 7)
          b5 <- z * 7
        } else if(b5 < 180){
          z = round(b5 / 15)
          b5 <- z * 15
        } else{
          z = round(b5 / 30)
          b5 <- z * 30
        }
        if(b6 < 30){
          z = round(b6 / 7)
          b6 <- z * 7
        } else if(b6 < 180){
          z = round(b6 / 15)
          b6 <- z * 15
        } else{
          z = round(b6 / 30)
          b6 <- z * 30
        }
        if(a$Past_recency_min[i] <= b1){
          a$new_Segment[i] <- (paste0("0 to ", b1))
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
        if(b1 < 30){
          z = round(b1 / 7)
          b1 <- z * 7
        } else if(b1 < 180){
          z = round(b1 / 15)
          b1 <- z * 15
        } else{
          z = round(b1 / 30)
          b1 <- z * 30
        }
        if(b2 < 30){
          z = round(b2 / 7)
          b2 <- z * 7
        } else if(b2 < 180){
          z = round(b2 / 15)
          b2 <- z * 15
        } else{
          z = round(b2 / 30)
          b2 <- z * 30
        }
        if(b3 < 30){
          z = round(b3 / 7)
          b3 <- z * 7
        } else if(b3 < 180){
          z = round(b3 / 15)
          b3 <- z * 15
        } else{
          z = round(b3 / 30)
          b3 <- z * 30
        }
        if(b4 < 30){
          z = round(b4 / 7)
          b4 <- z * 7
        } else if(b4 < 180){
          z = round(b4 / 15)
          b4 <- z * 15
        } else{
          z = round(b4 / 30)
          b4 <- z * 30
        }
        if(a$Past_recency_min[i] <= b1){
          a$new_Segment[i] <- (paste0("1 to ", b1))
        } else if(a$Past_recency_min[i] > b1 & a$Past_recency_min[i] <= b2){
          a$new_Segment[i] <- (paste0(b1 + 1," to ", b2))
        } else if(a$Past_recency_min[i] > b2 & a$Past_recency_min[i] <= b3){
          a$new_Segment[i] <- (paste0(b2 + 1," to ", b3))
        } else if(a$Past_recency_min[i] > b3 & a$Past_recency_min[i] <= b4){
          a$new_Segment[i] <- (paste0(b3 + 1," to ", b4))
        } else {
          a$new_Segment[i] <- paste0(b4,"+")
        }
      }
      else {
        b1 = quantile(a$Past_recency_min, c(.25))
        b2 = quantile(a$Past_recency_min, c(.50))
        b3 = quantile(a$Past_recency_min, c(.75))
        if(b1 < 30){
          z = round(b1 / 7)
          b1 <- z * 7
        } else if(b1 < 180){
          z = round(b1 / 15)
          b1 <- z * 15
        } else{
          z = round(b1 / 30)
          b1 <- z * 30
        }
        if(b2 < 30){
          z = round(b2 / 7)
          b2 <- z * 7
        } else if(b2 < 180){
          z = round(b2 / 15)
          b2 <- z * 15
        } else{
          z = round(b2 / 30)
          b2 <- z * 30
        }
        if(b3 < 30){
          z = round(b3 / 7)
          b3 <- z * 7
        } else if(b3 < 180){
          z = round(b3 / 15)
          b3 <- z * 15
        } else{
          z = round(b3 / 30)
          b3 <- z * 30
        }
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
    data3 <- a
    data3 <- cbind(data2, data3) # Derived values are binded to the original data
    data3 <- data3[,-3] # Remove replicate fields
    # Add missing values and zero values
    colnames(data3)[3] = "dist"

    if(nrow(new_DF) > 0){
      data3 = rbind(data3, new_DF)
    }
    if(nrow(new_DF1) > 0){
      data3 = rbind(data3, new_DF1)
    }
    data1 <- subset(data3, select = c("Id", "dist"))
    colnames(data1) <- c("strId", "dist")
    colnames(data1) <- c("Id", newname)

    updater(access_token, instance_url, myobject, data1)
    #data3 <- data3[,2]
  } else {

    data3[,1] <- as.numeric(as.character(data3[,1]))
    data3 <- slider(data3, 5)
    data3 <- cbind(data2, data3) # Derived values are binded to the original data
    data3 <- data3[,-2] # Remove replicate fields
    # Add missing values and zero values
    colnames(data3)[2] = field

    if(nrow(new_DF) > 0){
      data3 = rbind(data3, new_DF)
    }
    if(nrow(new_DF1) > 0){
      data3 = rbind(data3, new_DF1)
    }
    data1 <- subset(data3, select = c("Id", "dist"))
    colnames(data1) <- c("strId", "dist")
    colnames(data1) <- c("Id", newname)

    updater(access_token, instance_url, myobject, data1)

  }

}
