#'SIC Deciling
#'
#'This function ranks a categorical variable from a numeric variable

#' @import RForcecom
#' @import jsonlite
#' @import dplyr
#' @export ranker_decile

ranker_decile <- function(dataFrame, dynamic, split_)
{
  df <- dataFrame

  #Creating a table for use through dplyr
  myorder <-  tbl_df(df)

  colnames(myorder) <- c('Id','qual', 'quant','non_core') #Colnames generalized for ease of use
  #myorder <- na.omit(myorder)
  myorder$quant <- as.numeric(myorder$quant)
  myorder$quant[is.na(myorder$quant)] <- 0# Numeric NAs are considered as 0s
  #myorder$qual <- factor(myorder$qual)
  myorder <- myorder %>% arrange(desc(qual))

  # data split core and non_core
  core = myorder[is.na(myorder[,4]),]
  non_core = myorder[! is.na(myorder[,4]),]
  non_core$decile = "Non Core"

  order1 <- core[core[,3] <= dynamic,]
  if(nrow(order1) > 0){
    order1$decile = paste('Less than',dynamic,'units')
  }
  order3 <- core[is.na(core[,2]),]
  if(nrow(order3) > 0){
    order3$decile = "Blank"
  }

  if(nrow(order1) > 0){
    order <- rbind(non_core,order1)
  }
  if(nrow(order3) > 0){
    order <- rbind(order,order3)
  }

  core = core[! core[,3] <= dynamic & !is.na(core[,2]),]

  # Summarization and grouping by independent variable
  summary <- core %>% group_by(qual) %>%
    summarise(Records = n(), total = sum(quant))
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


   # order1$decile <- 11 # Anything not belonging to specified parameters will go to 6
    order2 <- summary[summary[,3] > dynamic & !is.na(summary[,1]),]
    totalcount <- nrow(order2)
    if(split_data == 10){
      #order2$decile <- rep(1:10, each = round(totalcount/10)) #Create Declies
    a1 = round(nrow(order2)*.1)
    a2 = round(nrow(order2)*.2)
    a3 = round(nrow(order2)*.3)
    a4 = round(nrow(order2)*.4)
    a5 = round(nrow(order2)*.5)
    a6 = round(nrow(order2)*.6)
    a7 = round(nrow(order2)*.7)
    a8 = round(nrow(order2)*.8)
    a9 = round(nrow(order2)*.9)
    #Binding the two split tables
    order2$decile[1:a1] = "Top 10%"
    order2$decile[(a1 + 1):a2] = "Top 20%"
    order2$decile[(a2 + 1):a3] = "Top 30%"
    order2$decile[(a3 + 1):a4] = "Top 40%"
    order2$decile[(a4 + 1): a5] = "Top 50%"
    order2$decile[(a5 + 1): a6] = "Top 60%"
    order2$decile[(a6 + 1): a7] = "Top 70%"
    order2$decile[(a7 + 1): a8] = "Top 80%"
    order2$decile[(a8 + 1): a9] = "Top 90%"
    order2$decile[(a9 + 1): nrow(order2)] = "Bottom 10%"
    } else if(split_data == 5){
      a1 = round(nrow(order2)*.2)
      a2 = round(nrow(order2)*.4)
      a3 = round(nrow(order2)*.6)
      a4 = round(nrow(order2)*.8)

      #Binding the two split tables
      order2$decile[1:a1] = "Top 20%"
      order2$decile[(a1 + 1):a2] = "Top 40%"
      order2$decile[(a2 + 1): a3] = "Top 60%"
      order2$decile[(a3 + 1): a4] = "Top 80%"
      order2$decile[(a4 + 1): nrow(order2)] = "Bottom 20%"

    } else {
      a1 = round(nrow(order2)*.05)
      a2 = round(nrow(order2)*.10)
      a3 = round(nrow(order2)*.15)
      a4 = round(nrow(order2)*.20)
      a5 = round(nrow(order2)*.25)
      a6 = round(nrow(order2)*.30)
      a7 = round(nrow(order2)*.35)
      a8 = round(nrow(order2)*.40)
      a9 = round(nrow(order2)*.45)
      a10 = round(nrow(order2)*.50)
      a11 = round(nrow(order2)*.55)
      a12 = round(nrow(order2)*.60)
      a13 = round(nrow(order2)*.65)
      a14 = round(nrow(order2)*.70)
      a15 = round(nrow(order2)*.75)
      a16 = round(nrow(order2)*.80)
      a17 = round(nrow(order2)*.85)
      a18 = round(nrow(order2)*.90)
      a19 = round(nrow(order2)*.95)
      #Binding the two split tables
      order2$decile[1:a1] = "Top 5%"
      order2$decile[(a1 + 1):a2] = "Top 10%"
      order2$decile[(a2 + 1):a3] = "Top 15%"
      order2$decile[(a3 + 1):a4] = "Top 20%"
      order2$decile[(a4 + 1): a5] = "Top 25%"
      order2$decile[(a5 + 1): a6] = "Top 30%"
      order2$decile[(a6 + 1): a7] = "Top 35%"
      order2$decile[(a7 + 1): a8] = "Top 40%"
      order2$decile[(a8 + 1): a9] = "Top 45%"
      order2$decile[(a9 + 1):a10] = "Top 50%"
      order2$decile[(a10 + 1):a11] = "Top 55%"
      order2$decile[(a11 + 1):a12] = "Top 60%"
      order2$decile[(a12 + 1): a13] = "Top 65%"
      order2$decile[(a13 + 1): a14] = "Top 70%"
      order2$decile[(a14 + 1): a15] = "Top 75%"
      order2$decile[(a15 + 1): a16] = "Top 80%"
      order2$decile[(a16 + 1): a17] = "Top 85%"
      order2$decile[(a17 + 1): a18] = "Top 90%"
      order2$decile[(a18 + 1): a19] = "Top 95%"
      order2$decile[(a19 + 1): nrow(order2)] = "Bottom 5%"
    }




    final <- merge(core, order2, by = 'qual')
    final <- final[,c(2,1,3,4,7)]
    #final$Records <- NULL
    #final$total <- NULL
    #final <- final %>% arrange(decile)
    #return(as.data.frame(final)) # Returns the data frame at record levels with decile values from 1: 11 appended
  }

  final_new <- rbind(final, order)
  return(final_new)
}


# library(dplyr)
# library(rsample)
# data <-  read.csv("E:\\Valgen\\Valgen Internal\\SIC Deciling\\Sample Dataset for Testing CSV.csv")
# data = data[,c(1,2,5,6)]
# n = nrow(data)
# trainIndex = sample(1:n, size = round(0.5*n), replace=FALSE)
# train1 = data[trainIndex ,]
# train2 = data[-trainIndex ,]
# df = train1
# #df = train1[,c(1,4,5)]

# data5 <- ranker_decile(train2, 10,9)

# z = data5 %>%
#   group_by(decile) %>%
#   summarise(Account_count = n(), Sum_of_amount = sum(quant), SIC_Count=n_distinct(qual)) %>%
#   mutate(Accounts_Pct = (Account_count / sum(Account_count))*100, Amount_Pct = (Sum_of_amount/sum(Sum_of_amount))*100)
#
#
# z$Accounts_Pct = z$Accounts_Pct * 100
# z$Amount_Pct = z$Amount_Pct * 100
#
#
# y = data1 %>%
#   group_by(decile) %>%
#   summarise(n = n())
#
