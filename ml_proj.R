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

sum(housing_data_raw$PoolArea == 0)
nrow(housing_data_raw)
sum(!(is.na(housing_data_raw$PoolQC)))

housing_data <- housing_data_raw
housing_data$HasPool <- ifelse(housing_data$PoolArea > 0, "Yes", "No")

columns_to_drop <- c("PoolQC", "PoolArea", "MiscFeature", "MiscValue", "GarageYrBlt",
                     "BsmtFinType1", "BsmtFinSF1", "BsmtFinType2", "BsmtFinSF2", "FrstFlrSF",
                     "ScndFlrSF", "BsmtUnfSF", "FullBath", "HalfBath", "BsmtFullBath",
                     "BsmtHalfBath")

unique(housing_data$MiscFeature)
housing_data$HasMiscFeature <- ifelse(!(is.na(housing_data$MiscFeature)), "Yes", "No")

table(housing_data$HasMiscFeature)

columns_to_convert_na_none <- c("Fence", "FireplaceQu", "GarageType", "GarageFinish", "GarageCond",
                                "MasVnrType", "Alley", "BsmtQual", "BsmtCond", "BsmtExposure", 
                                "GarageQual", "Garage")

columns_to_convert_na_zero <- c("MasVnrArea", "BsmtPercFin")

# LotFronatge and Electrical to be done differently

colnames(housing_data)[colSums(is.na(housing_data)) > 0]
sum(is.na(housing_data$MasVnrArea))

which(housing_data$YearRemodAdd != housing_data$GarageYrBlt)
which(is.na(housing_data$GarageYrBlt))
which(is.na(housing_data$GarageType))

sum(housing_data$GarageYrBlt == housing_data$YearBuilt, na.rm = TRUE)
sum((housing_data$GarageYrBlt != housing_data$YearBuilt) & !(is.na(housing_data$GarageYrBlt)))
sum(is.na(housing_data$GarageYrBlt))

housing_data$GarageYrBlt == housing_data$YearBuilt

housing_data$Garage <- ifelse(housing_data$GarageYrBlt == housing_data$YearBuilt, "orig",
                              ifelse((housing_data$GarageYrBlt != housing_data$YearBuilt) & !(is.na(housing_data$GarageYrBlt)),
                                     "remod", "None"))

housing_data$Porch <- ifelse(housing_data$OpenPorchSF > 0 & housing_data$`3SsnPorch` == 0 & 
                               housing_data$ScreenPorch == 0 & housing_data$EnclosedPorch == 0, 'open',
                             ifelse(housing_data$`3SsnPorch` > 0 & housing_data$OpenPorchSF == 0 & housing_data$ScreenPorch == 0 &
                                      housing_data$EnclosedPorch == 0, 'threeSeason',
                                    ifelse(housing_data$ScreenPorch > 0 & housing_data$OpenPorchSF == 0 & 
                                             housing_data$`3SsnPorch` == 0 & housing_data$EnclosedPorch == 0, 'screen',
                                           ifelse(housing_data$EnclosedPorch > 0 & housing_data$OpenPorchSF == 0 & 
                                                    housing_data$`3SsnPorch` == 0 & housing_data$EnclosedPorch == 0, 
                                                  'enclosed', 'multiple'))))
porch_to_drop <- housing_data %>% 
  select(OpenPorchSF, `3SsnPorch`, ScreenPorch, EnclosedPorch)

housing_data$BsmtPercFin <- (housing_data$BsmtFinSF1 + housing_data$BsmtFinSF2)/housing_data$TotalBsmtSF
housing_data <- housing_data %>% 
  rename(FrstFlrSF = `1stFlrSF`,
         ScndFlrSF = `2ndFlrSF`)
housing_data$TotSF <- housing_data$FrstFlrSF + housing_data$ScndFlrSF + housing_data$TotalBsmtSF
housing_data$TotSFAbvGrd <- housing_data$FrstFlrSF + housing_data$ScndFlrSF

housing_data$TotFullBath <- housing_data$FullBath + housing_data$BsmtFullBath
housing_data$TotHalfBath <- housing_data$HalfBath + housing_data$BsmtHalfBath

housing_data2 <- housing_data
housing_data2[, columns_to_drop] <- list(NULL)

housing_data[, columns_to_drop] <- list(NULL)
housing_data <- housing_data %>% 
  rename(ThrScn = `3SsnPorch`)
housing_data[, c("OpenPorchSF", "ThrScn", "ScreenPorch", "EnclosedPorch")] <- list(NULL)

library(data.table)

for(k in names(housing_data)){
  if(k %in% columns_to_convert_na_none){
    set(x = housing_data, which(is.na(housing_data[[k]])), k, "None")
  }
  else if(k %in% columns_to_convert_na_zero){
    set(x = housing_data, which(is.na(housing_data[[k]])), k, 0)
  }
}
sum(housing_data$Fence == 'None')
sum(is.na(housing_data))

housing_data %>% 
  group_by(Neighborhood, LotConfig) %>% 
  summarise(mean(LotFrontage, na.rm = TRUE))

housing_data2 <- housing_data2 %>% 
  group_by(Neighborhood, LotConfig) %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage), mean(LotFrontage, na.rm = TRUE), LotFrontage))
sum(is.na(housing_data2$LotFrontage))
names(housing_data2)
sum(housing_data$LotFrontage != housing_data2$LotFrontage)
setdiff(housing_data$LotFrontage, housing_data2$LotFrontage)
head(housing_data$LotFrontage, 10)
head(housing_data2$LotFrontage, 10)
na_front <- which(is.na(housing_data2$LotFrontage))
housing_data2[na_front, c('Neighborhood', 'LotConfig', 'LotFrontage')]

housing_data2 %>% 
  filter(Neighborhood == 'NWAmes', LotConfig == 'FR2') %>% 
  select(LotFrontage)

nwames <- housing_data2 %>% 
  filter(Neighborhood == 'NWAmes')
mean_lot_nwames <- mean(nwames$LotFrontage, na.rm = TRUE)
crawfor <- housing_data2 %>% 
  filter(Neighborhood == 'Crawfor')
mean_lot_crawfor <- mean(crawfor$LotFrontage, na.rm = TRUE)
na_front

housing_data2[1162, 'Neighborhood']
nwames_na_idx <- c(365, 422, 612)
crawfor_na_idx <- c(530, 1162)

housing_data2[nwames_na_idx, "LotFrontage"] <- mean_lot_nwames
housing_data2[crawfor_na_idx, "LotFrontage"] <- mean_lot_crawfor
sum(is.na(housing_data2$LotFrontage))

housing_data <- housing_data %>% 
  group_by(Neighborhood, LotConfig) %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage), mean(LotFrontage, na.rm = TRUE), LotFrontage))
nwames <- housing_data %>% 
  filter(Neighborhood == 'NWAmes')
mean_lot_nwames <- mean(nwames$LotFrontage, na.rm = TRUE)
crawfor <- housing_data %>% 
  filter(Neighborhood == 'Crawfor')
mean_lot_crawfor <- mean(crawfor$LotFrontage, na.rm = TRUE)
housing_data[nwames_na_idx, "LotFrontage"] <- mean_lot_nwames
housing_data[crawfor_na_idx, "LotFrontage"] <- mean_lot_crawfor
sum(is.na(housing_data$LotFrontage))

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
sum(is.na(housing_data))

ncol(housing_data)

ggplot(housing_data, aes(x = SalePrice)) + geom_histogram(bins = 100) +
  scale_x_continuous(labels=scales::dollar_format()) +
  labs(title = "Frequency of Sale Price",
       x = "Sale Price",
       y = "Freqeuncy")

ggplot(housing_data, aes(x = SalePrice)) + geom_histogram(bins = 100) +
  scale_x_log10(labels=scales::dollar_format()) +
  labs(title = "Frequency of Sale Price",
       x = "Sale Price (Log Scale)",
       y = "Freqeuncy")

target <- log(housing_data$SalePrice)
summary(target)
target_df <- data.frame(target)
hist(target, breaks = 50)
hist(housing_data$SalePrice, breaks = 50)
head(target_df)

ggplot(housing_data, aes(x = SalePrice)) + geom_histogram(bins = 100) +
  scale_x_continuous(labels=scales::dollar_format()) +
  labs(title = "Frequency of Sale Price",
       x = "Sale Price",
       y = "Freqeuncy")

ggplot(target_df, aes(x = target)) + geom_histogram(bins = 100) +
  labs(title = "Frequency of Sale Price",
       x = "Log of Sale Price",
       y = "Freqeuncy")


