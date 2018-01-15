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

# Explore

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

all <- rbind(train, test %>% mutate(SalePrice = 0))

aggr_plot <- aggr(
  all,
  col=c('navyblue','red'), 
  numbers=TRUE, 
  sortVars=TRUE, 
  labels=names(train),
  cex.axis=.7,
  gap=0,
  ylab=c("Histogram of missing data","Pattern")
)

exclude <- c('PoolQC', 'MiscFeature', 'Alley', 'Fence', 'FireplaceQu')
include <- setdiff(names(train), exclude)
train <- train[include]

imp.train <- mice(train, m=1, method='cart', printFlag=FALSE)
train <- complete(imp.train)

# Transform train data

train_trans <- train %>%
  mutate(
    SalePrice = log1p(SalePrice),
    YearBuiltAgo = max(YearBuilt) - YearBuilt
  )

# Linear regression

paste("SalePrice", paste(highcorr, collapse = ' + '), sep = ' ~ ')

linear <- lm(
  SalePrice ~
    OverallQual +
    YearBuilt +
    YearRemodAdd +
    TotalBsmtSF +
    `1stFlrSF` +
    GrLivArea +
    FullBath +
    TotRmsAbvGrd +
    GarageYrBlt +
    GarageCars +
    GarageArea,
  data = train_trans)

summary(linear)

# Transform test data

imp.test <- mice(test, m=1, method='cart', printFlag=FALSE)
test <- complete(imp.test)

test_trans <- test %>%
  mutate(
    YearBuiltAgo = max(YearBuilt) - YearBuilt
  )

# Generate submission

predicted <- expm1(predict(linear, test_trans))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")

output %>% write_csv('submission/linear.csv')
