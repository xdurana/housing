library(caret)

train <- read_csv('output/train.csv')
test <- read_csv('output/test.csv')

CARET.TRAIN.CTRL <- trainControl(method = "repeatedcv", number = 10, repeats = 4)
fit.glmnet <- train(
  SalePrice ~ .,
  train,
  trControl = CARET.TRAIN.CTRL,
  method = "glmnet",
  metric = "RMSE",
  maximize = FALSE,
  tuneGrid = expand.grid(.alpha = seq(0, 1, by = 0.05), .lambda = seq(0, 1, by = 0.01)))

# submit

predicted <- expm1(predict(fit.glmnet, test))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")
output %>% write_csv('submission/glmnet.csv')