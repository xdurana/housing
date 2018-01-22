library(caret)

train <- read_csv('output/train_imputed.csv')
test <- read_csv('output/test_imputed.csv')

linear <- lm(SalePrice ~ ., data = train)
summary(linear)

# submit

predicted <- expm1(predict(linear, test))
output <- data.frame(test$Id, predicted)
colnames(output) <- cbind("Id", "SalePrice")
output %>% write_csv('submission/linear.csv')
