library(xgboost)

dtrain <- xgb.DMatrix(as.matrix(train_trans %>% select(-SalePrice)), label = train_trans$SalePrice)
dtest <- xgb.DMatrix(as.matrix(test_trans))

cv.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 1,
  number = 4,
  allowParallel = T
)

xgb.grid <- expand.grid(
  nrounds = 750,
  eta = c(0.01,0.005,0.001),
  max_depth = c(4,6,8),
  colsample_bytree=c(0,1,10),
  min_child_weight = 2,
  subsample=c(0,0.2,0.4,0.6),
  gamma=0.01
)

set.seed(45)

xgb_params <- list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=4,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  seed=5,
  silent=TRUE
)

#xgb.cv(xgb_params, dtrain, nrounds = 5000, nfold = 4, early_stopping_rounds = 500)
bst <- xgb.train(xgb_params, dtrain, nrounds = 10)#, early_stopping_rounds = 300, watchlist = list(train=dtrain))