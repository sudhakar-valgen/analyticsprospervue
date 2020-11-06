#'Past Recency
#'
#'This function will do the past recency of the date variable

#' @import RForcecom
#' @import jsonlite
#' @import dplyr
#' @export past_date_recency
#'


past_date_recency <- function(data5, b){
  data2 <- as.data.frame(data5)
  today <- Sys.Date()
  if(b == 1){
    a <- as.Date(data2[,2], format = "%m/%d/%Y")
  } else if(b == 2){
    a = as.Date(data2[,2], format = "%Y/%m/%d")
  }
  else if(b == 3){
    a = as.Date(data2[,2], format = "%d/%m/%Y")
  }
  else if(b == 4){
    a = as.Date(data2[,2], format = "%d-%m-%Y")
  }
  else if(b == 5){
    a = as.Date(data2[,2], format = "%Y-%m-%d")
  } else{
    a = as.Date(data2[,2], format = "%m-%d-%Y")
  }

  #a <- as.POSIXct(myorder[,2], format = "%m-%d-%Y")
  a = as.data.frame(a)
  colnames(a)[1] = "Past_date"
  # Converting negative values into positive for future recency
  a$Past_recency <- abs(today - a$Past_date)

  ### Substract Max date value by min - 1
  a$Past_recency_min <- a$Past_recency - (min(a$Past_recency, na.rm = T) - 1)
  a$Past_recency_min <- ifelse(is.na(a$Past_recency_min),max(a$Past_recency_min, na.rm = T)+1, a$Past_recency_min)

  max_value <- max(a$Past_recency_min, na.rm = T)

  ### Count the missing value in the past recency
  #count_missing <- table(is.na(a$Past_recency) == TRUE)

  a$Past_recency_min <- as.numeric(a$Past_recency_min)
  missing_data <- a[is.na(a$Past_recency),]
  #a = na.omit(a)
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
  a = cbind.data.frame(id = data2[,1], new_segment = a[,4])
  colnames(a)[1] = "Id"
  #e <- merge(x = data2, y = a, all.x = TRUE)
  new_data <- a
  colnames(new_data)[2] = "dist"

  return(new_data)
}
