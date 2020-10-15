#'Past Recency
#'
#'This function will do the past recency of the date variable

#' @import RForcecom
#' @import jsonlite
#' @import dplyr
#' @export date_recency
#'


date_recency <- function(data5, b){
  data2 <- as.data.frame(data5)
  today <- Sys.Date()
  if(b == 1){
    a <- as.Date(data2[,1], format = "%m/%d/%Y")
  } else if(b == 2){
    a = as.Date(data2[,1], format = "%Y/%m/%d")
  }
  else if(b == 3){
    a = as.Date(data2[,1], format = "%d/%m/%Y")
  }
  else if(b == 4){
    a = as.Date(data2[,1], format = "%d-%m-%Y")
  }
  else if(b == 5){
    a = as.Date(data2[,1], format = "%Y-%m-%d")
  } else{
    a = as.Date(data2[,1], format = "%m-%d-%Y")
  }

  #a <- as.POSIXct(myorder[,2], format = "%m-%d-%Y")
  a = as.data.frame(a)
  colnames(a)[1] = "Past_date"
  # Converting negative values into positive for future recency
  a$Past_recency <- abs(today - a$Past_date)

  ### Substract Max date value by min - 1
  a$Past_recency_min <- a$Past_recency - (min(a$Past_recency, na.rm = T) - 1)
  max_value <- max(a$Past_recency_min, na.rm = T)

  ### Count the missing value in the past recency
  #count_missing <- table(is.na(a$Past_recency) == TRUE)

  a$Past_recency_min <- as.numeric(a$Past_recency_min)
  a$Past_recency_min <- ifelse(is.na(a$Past_recency_min),max(a$Past_recency_min, na.rm = T)+1, a$Past_recency_min)
  new_data <- a$Past_recency_min
  return(new_data)
}
