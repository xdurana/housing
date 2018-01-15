library(readr)

submission.gbm <- read_csv('submission/gbm.csv')
submission.regularized <- read_csv('submission/regularized.csv')

df <- data.frame(Id = submission.gbm$Id, SalePrice = 0.6 * submission.gbm$SalePrice + 0.4 * submission.regularized$SalePrice)
write_csv(df, 'submission/weighted.csv')
