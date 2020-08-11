setwd("~/NYCDSA/ML_project/ML_housing_price_proj")
library(readr)
housing_data_raw <- read_csv("./train.csv")



library(tidyverse)
library(ggplot2)

perc_missingness <- function(x) {
  data.frame("Percent.Missing" = floor(apply(is.na(x), 2, sum)/nrow(x)*100))
}

missing <- perc_missingness(housing_data_raw)
missing$Column.Names <- row.names(missing)
missing <- missing[missing$Percent.Missing > 0, ]

# plotting the missingness

ggplot(missing, aes(x = reorder(Column.Names, Percent.Missing), y = Percent.Missing)) +
  geom_bar(stat="identity") + 
  coord_flip(ylim = c(4,100)) + 
  labs(title = "Percent of Data Missing for each Feature",
       x = "Feature",
       y = "Percent Missing") +
  theme_bw()

# Three features did not show up because % was less than 1

housing_data <- housing_data_raw

housing_data$TotPorchSF <- housing_data$`3SsnPorch` + housing_data$OpenPorchSF + 
  housing_data$ScreenPorch + housing_data$EnclosedPorch

# adding a column for percent of basement that is finished
housing_data$BsmtPercFin <- (housing_data$BsmtFinSF1 +
                               housing_data$BsmtFinSF2)/housing_data$TotalBsmtSF

# added TotSF and TotSFAbvGrd columns 
housing_data <- housing_data %>% 
  rename(FrstFlrSF = `1stFlrSF`,
         ScndFlrSF = `2ndFlrSF`)
housing_data$TotSF <- housing_data$GrLivArea  + housing_data$TotalBsmtSF

# adding columns for TotFullBath and TotHalfBath
housing_data$TotFullBath <- housing_data$FullBath + housing_data$BsmtFullBath
housing_data$TotHalfBath <- housing_data$HalfBath + housing_data$BsmtHalfBath

# removing some columns
housing_data <- housing_data %>% 
  rename(ThrScn = `3SsnPorch`)
columns_to_drop <- c("PoolQC", "MiscFeature", "MiscValue", "GarageYrBlt",
                     "BsmtFinType1", "BsmtFinSF1", "BsmtFinType2", "BsmtFinSF2", "FrstFlrSF",
                     "ScndFlrSF", "BsmtUnfSF", "FullBath", "HalfBath", "BsmtFullBath",
                     "BsmtHalfBath", "OpenPorchSF", "ThrScn", "ScreenPorch", "EnclosedPorch")
housing_data[, columns_to_drop] <- list(NULL)

# imputing NAs with "None" or 0
library(data.table)

columns_to_convert_na_none <- c("Fence", "FireplaceQu", "GarageType", "GarageFinish", "GarageCond",
                                "MasVnrType", "Alley", "BsmtQual", "BsmtCond", "BsmtExposure", 
                                "GarageQual", "Garage")

columns_to_convert_na_zero <- c("MasVnrArea", "BsmtPercFin", "PoolArea")

for(k in names(housing_data)){
  if(k %in% columns_to_convert_na_none){
    set(x = housing_data, which(is.na(housing_data[[k]])), k, "None")
  }
  else if(k %in% columns_to_convert_na_zero){
    set(x = housing_data, which(is.na(housing_data[[k]])), k, 0)
  }
}

# imputing LotFrontage
housing_data <- housing_data %>% 
  group_by(Neighborhood, LotConfig) %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage), mean(LotFrontage, na.rm = TRUE), LotFrontage))
nwames <- housing_data %>% 
  filter(Neighborhood == 'NWAmes')
mean_lot_nwames <- mean(nwames$LotFrontage, na.rm = TRUE)
crawfor <- housing_data %>% 
  filter(Neighborhood == 'Crawfor')
mean_lot_crawfor <- mean(crawfor$LotFrontage, na.rm = TRUE)
nwames_na_idx <- c(365, 422, 612)
crawfor_na_idx <- c(530, 1162)
housing_data[nwames_na_idx, "LotFrontage"] <- mean_lot_nwames
housing_data[crawfor_na_idx, "LotFrontage"] <- mean_lot_crawfor

# imputing Electrical
which(is.na(housing_data$Electrical))
housing_data[1380, c('Neighborhood', 'Electrical')]
timber <- housing_data %>% 
  filter(Neighborhood == 'Timber')
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_elec_timber <- getmode(timber$Electrical)
housing_data[1380, "Electrical"] <- mode_elec_timber

ggplot(housing_data, aes(x = SalePrice)) + geom_histogram(bins = 100) +
  scale_x_continuous(labels=scales::dollar_format()) +
  labs(title = "Frequency of Sale Price",
       x = "Sale Price",
       y = "Freqeuncy")

target <- log(housing_data$SalePrice)
target_df <- data.frame(target)

ggplot(target_df, aes(x = target)) + geom_histogram(bins = 100) +
  labs(title = "Frequency of Sale Price",
       x = "Log of Sale Price",
       y = "Freqeuncy")


