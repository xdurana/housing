library(readr)
library(iterators)
library(parallel)
library(doMC)
library(data.table)

train <- fread('output/train_imputed.csv')
test <- fread('output/test_imputed.csv')

registerDoMC(detectCores())

CARET.TRAIN.CTRL <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = FALSE,
  allowParallel = TRUE
)

gbmFit <- train(
  SalePrice ~ .,
  method = "gbm",
  metric = "RMSE",
  maximize = FALSE,
  trControl = CARET.TRAIN.CTRL,
  tuneGrid = expand.grid(n.trees = (4:10) *50,
                         interaction.depth = c(5),
                         shrinkage = c(0.05), n.minobsinnode = c(10)),
  data = train, verbose = FALSE
)

print(gbmFit)

# submit

predicted <- expm1(predict(gbmFit, newdata = test))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")
output %>% write_csv('submission/gbm.csv')
