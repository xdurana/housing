library(xgboost)

train <- read_csv('output/train.csv')
test <- read_csv('output/test.csv')

outcome <- c('SalePrice')
predictors <- names(train)[!names(train) %in% outcome]

train_matrix <- as.matrix(train,rownames.force = NA)
test_matrix <- as.matrix(test, rownames.force = NA)

train_cv <- xgb.cv(
  data = train_matrix[,predictors],
  label = train_matrix[,outcome],
  nrounds = 100,
  max_depth = 8,
  eta = 0.02,
  subsample = .7,
  booster = "gbtree",
  eval_metric = "rmse",
  verbose = TRUE,
  print_every_n = 50,
  nfold = 4,
  nthread = 2,
  objective="reg:linear"
)

train_sparse <- as(train_matrix,"sparseMatrix")
test_sparse <- as(test_matrix,"sparseMatrix")

train_dmatrix <- xgb.DMatrix(data = train_matrix[, predictors], label = train_matrix[, outcome] )

param <- list(
  objective = "reg:linear",
  eval_metric = "rmse",
  booster = "gbtree",
  max_depth = 8,
  eta = 0.02,
  subsample = 0.7
)

xgbFit <- xgb.train(
  params = param,
  data = train_dmatrix,
  nrounds = 10000,
  verbose = TRUE,
  watchlist = list(train = train_dmatrix),
  print_every_n = 100,
  nfold = 4,
  nthread = 16
)

predictions <- predict(xgbFit, as.matrix(train[, predictors]))

rmse <- sqrt((mean(train$SalePrice-predictions))^2)
rmse

# submit

predicted <- expm1(predict(xgbFit, test_sparse))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")
output %>% write_csv('submission/xgb.csv')