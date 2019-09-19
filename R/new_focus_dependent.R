ranker1 <- function(dataFrame)
{
  df <- dataFrame

  #Creating a table for use through dplyr
  myorder <-  tbl_df(df)

  colnames(myorder) <- c('Id', 'quant', 'qual') #Colnames generalized for ease of use
  myorder <- na.omit(myorder)
  if(is.numeric(myorder[,2]) == T){
    myorder$quant[is.na(myorder$quant)] <- 0# Numeric NAs are considered as 0s

  } else {
    today <- Sys.Date()
    myorder[,2] <- as.Date(myorder[,2], format = "%m-%d-%Y")
    myorder[,2] <- as.numeric(today - myorder[,2])
  }

  myorder <- myorder %>% arrange(desc(qual))
  myorder$qual <- factor(myorder$qual)
  # Summarization and grouping by independent variable
  summary <- myorder %>% group_by(qual) %>%
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
