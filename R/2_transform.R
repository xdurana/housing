library(ggplot2)
library(readr)
library(caret)
library(corrplot)
library(mice)
library(VIM)
library(moments)
library(MASS)
library(dplyr)

transform <- function(do_impute = TRUE) {
  
  # Read train dataset
  
  train <- read_csv("input/train.csv")
  test  <- read_csv("input/test.csv")
  
  # Missings
  
  all <- rbind(train %>% mutate(train = 1) %>% dplyr::select(-SalePrice), test %>% mutate(train = 0))
  
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
  dplyr::select(
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
  
  # Not numeric variables
  
  all <- all %>% mutate(
    MSSubClass = as.factor(MSSubClass)
  )
  
  # Impute missings
  
  if (do_impute) {
    imp.all <- mice(all, m = 1, method = 'pmm', printFlag = FALSE)
    all <- complete(imp.all)
  }
  
  # Correct skewness of numeric variables
  
  feature_classes <- sapply(names(all), function(x) {
    class(all[[x]])
  })
  
  numeric_feats <- names(feature_classes[feature_classes == "integer"])
  
  skewed_feats <- sapply(numeric_feats, function(x) {
    skewness(all[[x]], na.rm = TRUE)
  })
  
  ## Keep only features that exceed a threshold (0.75) for skewness
  skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]
  
  ## Transform skewed features with boxcox transformation
  for (x in names(skewed_feats)) {
    bc = BoxCoxTrans(all[[x]], lambda = 0.15)
    all[[x]] = predict(bc, all[[x]])
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
    left_join(train %>% dplyr::select(Id, SalePrice)) %>%
    mutate(
      SalePrice = log1p(SalePrice)
    ) %>%
    dplyr::select(
      -Id,
      -train
    )
  
  # Test set
  
  test_trans <- all %>%
    filter(train == 0) %>%
    mutate(
      Id = as.factor(as.character(Id))
    ) %>%
    dplyr::select(
      -train
    )
  
  train_trans %>% write_csv(sprintf('output/train%s.csv', ifelse(do_impute, '_imputed', '')), na = '')
  test_trans %>% write_csv(sprintf('output/test%s.csv', ifelse(do_impute, '_imputed', '')), na = '')
}

