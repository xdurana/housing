library(ggplot2)
library(readr)
library(dplyr)
library(caret)
library(corrplot)
library(mice)
library(VIM)

transform <- function(do_impute = TRUE) {
  
  # Read train dataset
  
  train <- read_csv("input/train.csv")
  test  <- read_csv("input/test.csv")
  
  # Missings
  
  all <- rbind(train %>% mutate(train = 1) %>% select(-SalePrice), test %>% mutate(train = 0))
  
  aggr_plot <- aggr(
    all,
    col=c('navyblue','red'), 
    numbers=TRUE, 
    sortVars=TRUE, 
    labels=names(all),
    cex.axis=.7,
    gap=0,
    ylab=c("Histogram of missing data","Pattern")
  )
  
  exclude <- c('PoolQC', 'MiscFeature', 'Alley', 'Fence', 'FireplaceQu')
  include <- setdiff(names(all), exclude)
  all <- all[include]
  
  # Transform categorical variables
  
  all <- all %>%
    mutate(
      GarageFinish = as.factor(ifelse(is.na(GarageFinish), 'NA', GarageFinish)),
      GarageQual = as.factor(ifelse(is.na(GarageQual), 'NA', GarageQual)),
      GarageCond = as.factor(ifelse(is.na(GarageCond), 'NA', GarageCond)),
      GarageType = as.factor(ifelse(is.na(GarageType), 'NA', GarageType)),
      BsmtCond = as.factor(ifelse(is.na(BsmtCond), 'NA', BsmtCond)),
      BsmtExposure = as.factor(ifelse(is.na(BsmtExposure), 'NA', BsmtExposure)),
      BsmtQual = as.factor(ifelse(is.na(BsmtQual), 'NA', BsmtQual)),
      BsmtFinType2 = as.factor(ifelse(is.na(BsmtFinType2), 'NA', BsmtFinType2)),
      BsmtFinType1 = as.factor(ifelse(is.na(BsmtFinType1), 'NA', BsmtFinType1)),
      Electrical = as.factor(ifelse(is.na(Electrical), 'SBrkr', Electrical)),
      MSZoning = as.factor(ifelse(is.na(MSZoning), 'RL', MSZoning)),
      KitchenQual = as.factor(ifelse(is.na(KitchenQual), 'TA', KitchenQual)),
      Functional = as.factor(ifelse(is.na(Functional), 'Typ', Functional)),
      SaleType = as.factor(ifelse(is.na(SaleType), 'WD', SaleType)),
      Exterior1st = as.factor(ifelse(is.na(Exterior1st), 'VinylSd', Exterior1st)),
      Exterior2nd = as.factor(ifelse(is.na(Exterior2nd), 'VinylSd', Exterior2nd)),
      MasVnrType = as.factor(ifelse(is.na(MasVnrType), 'None', MasVnrType))
    ) %>%
    select(
      -Utilities
    )
  
  aggr_plot <- aggr(
    all,
    col=c('navyblue','red'), 
    numbers=TRUE, 
    sortVars=TRUE, 
    labels=names(all),
    cex.axis=.7,
    gap=0,
    ylab=c("Histogram of missing data","Pattern")
  )
  
  # Impute missings
  
  if (do_impute) {
    imp.all <- mice(all, m = 1, method = 'rf', printFlag = FALSE)
    all <- complete(imp.all)
  }
  
  # Create dummy variables
  
  dmy <- dummyVars(" ~ .", data = all)
  all <- data.frame(predict(dmy, newdata = all))
  
  # Create new features
  
  all <- all %>%
    mutate(
      TotalSF = TotalBsmtSF + X.1stFlrSF. + X.2ndFlrSF.
    )
  
  # Training set
  
  train_trans <- all %>%
    filter(train == 1) %>%
    left_join(train %>% select(Id, SalePrice)) %>%
    mutate(
      SalePrice = log1p(SalePrice)
    ) %>%
    select(
      -Id,
      -train
    )
  
  # Test set
  
  test_trans <- all %>%
    filter(train == 0) %>%
    mutate(
      Id = as.factor(as.character(Id))
    ) %>%
    select(
      -train
    )
  
  train_trans %>% write_csv(sprintf('output/train%s.csv', ifelse(do_impute, '_imputed', '')), na = '')
  test_trans %>% write_csv(sprintf('output/test%s.csv', ifelse(do_impute, '_imputed', '')), na = '')
}

