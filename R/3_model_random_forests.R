library(randomForest)
library(caret)

train <- read_csv('output/train_imputed.csv') %>% select(-train) %>% as.data.frame()
test <- read_csv('output/test_imputed.csv') %>% as.data.frame()

rfFit <- randomForest(
  SalePrice ~. ,
  data = train,
  method = "anova",
  ntree = 1500,
  mtry = 30,
  replace = F,
  importance = T
)

predicted <- expm1(predict(rfFit, newdata = test))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")
output %>% write_csv('submission/rf.csv')
