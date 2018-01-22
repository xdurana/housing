library(xgboost)
library(caret)
library(pROC)
library(data.table)
library(dplyr)

train <- fread('output/train.csv')
test <- fread('output/test.csv')

outcome <- c('SalePrice')
predictors <- names(train)[!names(train) %in% outcome]

mx.train <- as.matrix(train, rownames.force = NA)
mx.test <- as.matrix(test, rownames.force = NA)

train_dmatrix <- xgb.DMatrix(data = mx.train[, predictors], label = mx.train[, outcome] )

fit.xgb <- xgboost(
  data = train_dmatrix,
  nrounds = 20,
  objective = "reg:linear"
)

xgb.dump(fit.xgb)

# submit

predicted <- expm1(predict(fit.xgb, as.matrix(mx.test[, predictors])))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")
output %>% write_csv('submission/xgb.csv')
