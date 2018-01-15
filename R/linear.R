library(ggplot2)
library(readr)
library(dplyr)
library(caret)
library(corrplot)
library(mice)
library(VIM)

# Read train dataset

train <- read_csv("input/train.csv")
test  <- read_csv("input/test.csv")

# Explore correlations with SalePrice

correlations <- train[, sapply(train, is.numeric)] %>%
  select(-Id) %>%
  na.omit %>%
  cor

correlations %>%
  corrplot.mixed(
    lower = "circle",
    upper = "circle",
    tl.pos = "lt",
    diag = "n",
    order = "hclust",
    hclust.method = "complete"
  )

highcorr <- c(names(correlations[,'SalePrice'])[
  which(correlations[,'SalePrice'] > 0.5)],
  names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])

highcorr <- highcorr[highcorr != 'SalePrice']

# Missings

all <- rbind(train %>% mutate(train = 1), test %>% mutate(train = 0, SalePrice = NA))

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
    Electrical = as.factor(ifelse(is.na(Electrical), 'NA', Electrical)),
    KitchenQual = as.factor(ifelse(is.na(KitchenQual), 'NA', KitchenQual)),
    SaleType = as.factor(ifelse(is.na(SaleType), 'NA', SaleType))
  )

all <- all %>%
  mutate(
    MasVnrType = as.factor(ifelse(is.na(MasVnrType), 'NA', MasVnrType)),
    MSZoning = as.factor(ifelse(is.na(MSZoning), 'NA', MSZoning)),
    Utilities = as.factor(ifelse(is.na(Utilities), 'NA', Utilities)),
    Functional = as.factor(ifelse(is.na(Functional), 'NA', Functional)),
    Exterior1st = as.factor(ifelse(is.na(Exterior1st), 'NA', Exterior1st)),
    Exterior2nd = as.factor(ifelse(is.na(Exterior2nd), 'NA', Exterior2nd))
  )

# Transform train and test data

dmy <- dummyVars(" ~ .", data = all)
all <- data.frame(predict(dmy, newdata = all)) %>%
  select(-SalePrice)

imp.all <- mice(all, m=1, method='cart', printFlag=FALSE)
all <- complete(imp.all)

all <- all %>%
  mutate(
    YearBuiltAgo = max(YearBuilt) - YearBuilt
  )

# Training set

train_trans <- all %>%
  filter(train == 1) %>%
  left_join(train %>% select(Id, SalePrice)) %>%
  mutate(
    SalePrice = log1p(SalePrice)
  ) %>%
  select(
    -Id
  )

test_trans <- all %>%
  filter(train == 0) %>%
  mutate(
    Id = as.factor(as.character(Id))
  )


# Models

linear_regression <- function(train) {
  
  linear <- lm(SalePrice ~ ., data = train)
  summary(linear)
  linear
}

regularized_linear_regression <- function(train) {
  
  tc <- trainControl(method = "repeatedcv", number = 10, repeats = 4)
  fit.glmnet <- train(
    SalePrice ~ .,
    train,
    trControl = tc,
    method = "glmnet",
    tuneGrid = expand.grid(.alpha = seq(0,1,by=0.05), .lambda = seq(0, 0.08, by = 0.01)))
}

# Submission

submit <- function(test, model) {
  
  predicted <- expm1(predict(model, test))
  output <- data.frame(test$Id, predicted)
  colnames(output) <- cbind("Id", "SalePrice")
  output %>% write_csv('submission/submission.csv')
}

linear = linear_regression(train_trans)
regularized = regularized_linear_regression(train_trans)

submit(test_trans, linear)
submit(test_trans, regularized)
