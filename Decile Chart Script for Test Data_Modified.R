######################################### Decile Chart for Test Dataset ########################################

#### Decile Chart Function by Count of Attrition Flag ####

decile_chart <- function(x, y){ #input the field of probability score for x and input the field of actual classification for y
  deciles <- ntile(x, 10)
  decile_reverse <- 
    actuals <- as.numeric(y)
  decile_vars <- data.frame(
    Decile = deciles,
    Actuals = actuals
  )
  decile_vars %>%
    mutate(
      Decile = case_when(Decile == 1 ~ 10,
                         Decile == 2 ~ 9,
                         Decile == 3 ~ 8,
                         Decile == 4 ~ 7,
                         Decile == 5 ~ 6,
                         Decile == 6 ~ 5,
                         Decile == 7 ~ 4,
                         Decile == 8 ~ 3,
                         Decile == 9 ~ 2,
                         Decile == 10 ~ 1)
    ) %>%
    group_by(Decile) %>%
    summarise(
      Total = sum(Actuals)
    ) %>%
    ggplot() +
    ggtitle("Decile Chart") +
    geom_bar(aes(x = Decile, y = Total), stat = 'identity', fill = "#2b8cbe") +
    geom_label(aes(Decile, Total, label = Total), vjust = -0.4) + 
    theme_minimal() + theme(plot.title = element_text(hjust=0.5))
}

#### Decile Chart Function by Average Attrition Flag ####
decile_chart_mean <- function(x, y){ #input the field of probability score for x and input the field of actual classification for y
  deciles <- ntile(x, 10)
  decile_reverse <- 
    actuals <- as.numeric(y)
  decile_vars <- data.frame(
    Decile = deciles,
    Actuals = actuals
  )
  decile_vars %>%
    mutate(
      Decile = case_when(Decile == 1 ~ 10,
                         Decile == 2 ~ 9,
                         Decile == 3 ~ 8,
                         Decile == 4 ~ 7,
                         Decile == 5 ~ 6,
                         Decile == 6 ~ 5,
                         Decile == 7 ~ 4,
                         Decile == 8 ~ 3,
                         Decile == 9 ~ 2,
                         Decile == 10 ~ 1)
    ) %>%
    group_by(Decile) %>%
    summarise(
      Average_Attrition_Flag = mean(Actuals),
      Average_Attrition_Flag = round(Average_Attrition_Flag, 2)
    ) %>%
    ggplot() +
    ggtitle("Decile Chart") + 
    geom_bar(aes(x = Decile, y = Average_Attrition_Flag), stat = 'identity', fill = "#2b8cbe") +
    geom_label(aes(Decile, Average_Attrition_Flag, label = Average_Attrition_Flag), vjust = -0.4) + 
    theme_minimal() + theme(plot.title = element_text(hjust=0.5))
}

#### Enter Function Values ####
#Actual
actual <- x.test[c("TARGET_FLAG")]
actual$TARGET_FLAG <- as.numeric(actual$TARGET_FLAG)
actual$TARGET_FLAG[actual$TARGET_FLAG=="1"] <- "0"
actual$TARGET_FLAG[actual$TARGET_FLAG=="2"] <- "1"
actual$TARGET_FLAG <- as.numeric(actual$TARGET_FLAG)
str(actual)

#Predicted
predicted <- x.test[c("P_TARGET_FLAG")]
str(predicted)

#Produce the Charts
decile_chart (predicted$P_TARGET_FLAG,actual$TARGET_FLAG)
decile_chart_mean (predicted$P_TARGET_FLAG,actual$TARGET_FLAG)
