library(readr)
library(dplyr)
library(corrplot)

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
