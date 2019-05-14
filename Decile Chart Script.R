decile_chart <- function(x, y){ #input the field of probability score for x and input the field of actual classification for y
  deciles <- ntile(x, 10)
  decile_reverse <- 
    actuals <- as.numeric(y) - 1
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
    geom_bar(aes(x = Decile, y = Total), stat = 'identity') +
    geom_label(aes(Decile, Total, label = Total), vjust = -0.4) + 
    theme_classic()
}
