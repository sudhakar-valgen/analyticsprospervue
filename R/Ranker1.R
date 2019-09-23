#'Industry ranking
#'
#'This function ranks a categorical variable from a numeric variable

#' @import RForcecom
#' @import jsonlite
#' @import dplyr

#' @export ranking

ranking <- function(dataFrame)
  {
    df <- data1

    #Creating a table for use through dplyr
    myorder <-  as.data.frame(df)

    colnames(myorder) <- c('Id', 'quant', 'qual') #Colnames generalized for ease of use
    myorder <- na.omit(myorder)

    # Handling What to predict? variable , it has type numeric and date type.
    if(is.numeric(myorder[,2]) == T){
      myorder$quant[is.na(myorder$quant)] <- 0# Numeric NAs are considered as 0s

    } else {
      today <- Sys.Date()
      myorder[,2] <- as.Date(myorder[,2], format = "%Y-%m-%d")
      myorder[,2] <- as.numeric(today - myorder[,2])
    }

    # Handling Where to apply prediction?, it has type categorical, numeric and date type
    #myorder = data

    a = unique(myorder[,3])
     if(is.numeric(myorder[,3])){
      new_DF <- myorder[is.na(myorder[,3]),]
      if(nrow(new_DF) > 0){
        new_DF$dist = "MISSING"
      }

      # select Zero Values, Add new field and filled with ZERO VALUES level
      new_DF1 <- myorder[myorder[,3] == 0 & !(is.na(myorder[,3])),]
      if(nrow(new_DF1) > 0){
        new_DF1$dist = "ZERO VALUES"
      }
      # Data Treatment starts Here
      data2 <- myorder[myorder[,3] != 0 & !(is.na(myorder[,3])),]
      data3 <- subset(data2, select = c(3))
      data3 <- slider(data3, 5)
      data3 <- cbind(data2, data3) # Derived values are binded to the original data
      data3 <- data3[,-3] # Remove replicate fields

      # Add missing values and zero values
      if(nrow(new_DF) > 0){
        data3 = rbind(data3, new_DF)
      }

      if(nrow(new_DF1) > 0){
        data3 = rbind(data3, new_DF1)
      }

      myorder[,3] <- data3$dist

    } else if(str_length(myorder[,3][1]) == str_length(myorder[,3][2]) &&  (str_length(myorder[,3][3])) == (str_length(myorder[,3][4])) && (str_length(myorder[,3][5])) == (str_length(myorder[,3][6]))
              && (str_length(myorder[,3][15])) == (str_length(myorder[,3][20]))
              && (str_length(myorder[,3][25])) == (str_length(myorder[,3][30])) && length(a) >= 10){
      today <- Sys.Date()
      a <- as.Date(myorder[,3], format = "%Y-%m-%d")
      #a <- as.POSIXct(myorder[,2], format = "%m-%d-%Y")
      a = as.data.frame(a)
      colnames(a)[1] = "Past_date"
      a$Past_recency <- today - a$Past_date

      ### Substract Max date value by min - 1
      a$Past_recency_min <- a$Past_recency - (min(a$Past_recency, na.rm = T) - 1)
      max_value <- max(a$Past_recency_min, na.rm = T)

      ### Count the missing value in the past recency
      #count_missing <- table(is.na(a$Past_recency) == TRUE)

      a$Past_recency_min <- as.numeric(a$Past_recency_min)
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

      # missing_data$new_Segment <- "Data is Not Available"
      #
      # #zeros_data$new_segment <- " "
      # n <- nrow(zeros_data)
      # if(n > 0 ){
      #   zeros_data$new_Segment <- "Zeros"
      #   new_data <- rbind(a,missing_data, zeros_data)
      # } else {
      #   new_data <- rbind(a, missing_data)
      # }

      #data1 <- subset(new_data, select = c("Id", "new_segment"))
      myorder[,3] <- a$new_Segment
    } else{
      myorder <- myorder %>% arrange(desc(qual))
    }

    #myorder <- myorder %>% arrange(desc(qual))
    myorder$qual <- factor(myorder$qual)

    # Summarization and grouping by independent variable
    summary <- myorder %>% group_by(qual) %>%
      summarise(Records = n(), total = mean(quant))
    summary <- summary %>% arrange(desc(total))

    if(nrow(summary) <= 10){

      summary$decile <- rank(-summary$total)
      final <- merge(myorder, summary, by = 'qual')
      final <- final %>% arrange(decile)
      return(as.data.frame(final))
    }else{

      summary$decile <- as.numeric('NA')

      # Core algorithm; setting up values and creating decile rankings
      totalcount <- nrow(summary)
      order1 <- tail(summary, totalcount %% 10)
      order1$decile <- 11 # Anything not belonging to specified parameters will go to 6
      order2 <- summary[-seq(nrow(summary),nrow(summary)-((totalcount %% 10)-1)),]
      totalcount <- nrow(order2)

      order2$decile <- rep(1:10, each = totalcount/10) #Create Declies

      #Binding the two split tables
      order <- rbind(order2,order1)

      final <- merge(myorder, order, by = 'qual')
      final$Records <- NULL
      final$total <- NULL
      final <- final %>% arrange(decile)
      return(as.data.frame(final)) # Returns the data frame at record levels with decile values from 1: 11 appended
    }
  }

#
#  data = read.csv("C:/Users/Sudhakar/Desktop/data/date.csv")
#  new_data <- read.csv("C:/Users/Sudhakar/Desktop/data/SAMPLE_CITY_AMOUNT.csv")

