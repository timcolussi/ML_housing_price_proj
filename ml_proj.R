library(readr)
library(tidyverse)
library(ggplot2)
housing_data_raw <- read_csv("./train.csv")

names(housing_data_raw)

perc_missingness <- function(x) {
  data.frame("Percent.Missing" = floor(apply(is.na(x), 2, sum)/nrow(x)*100))
}


# num_na <- apply(is.na(housing_data_raw), 2, sum)
# sum(is.na(housing_data_raw$Fence))/nrow(housing_data_raw)
# nrow(housing_data_raw)
# 
# data.frame("Percent.Missing" = floor(apply(is.na(x), 2, sum)/nrow(x)*100))

perc_missingness(housing_data_raw)
missing <- perc_missingness(housing_data_raw)
missing$Column.Names <- row.names(missing)
missing
missing <- missing[missing$Percent.Missing > 0, ]

ggplot(missing, aes(x = reorder(Column.Names, Percent.Missing), y = Percent.Missing)) +
  geom_bar(stat="identity") + 
  coord_flip(ylim = c(4,100)) + 
  labs(title = "Percent of Data Missing for each Feature",
       x = "Feature",
       y = "Percent Missing") +
  theme_bw()









