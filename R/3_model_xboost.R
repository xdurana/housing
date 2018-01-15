library(xgboost)

train <- read_csv('output/train.csv')
test <- read_csv('output/test.csv')

outcomes <- train$SalePrice

dtrain <- xgb.DMatrix(as.matrix(train %>% select(-SalePrice)), label = train$SalePrice)
dtest <- xgb.DMatrix(as.matrix(test))

param <- list(
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=6,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  seed=5
)

xgb_model <- xgb.train(
  data = dtrain,
  params = param,
  watchlist = list(train = dtrain),
  nrounds = 7000,
  verbose = 1,
  print_every_n = 100
)

# submit

predicted = expm1(predict(xgb_model, dtest))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")
output %>% write_csv('submission/xgb.csv')


