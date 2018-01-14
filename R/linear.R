library(ggplot2)
library(readr)
library(dplyr)

train <- read_csv("input/train.csv")
test  <- read_csv("input/test.csv")

linear <- lm(SalePrice ~ YearBuilt, data=train)

summary(linear)

predicted <- predict(linear, test)

output <- data.frame(test$Id, predicted)

colnames(output) <- cbind("Id", "SalePrice")

summary(output)

output %>% write_csv('submission/linear.csv')
