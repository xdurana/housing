library(readr)

submission.gbm <- read_csv('submission/gbm.csv')
submission.regularized <- read_csv('submission/regularized.csv')
submission.rf <- read_csv('submission/rf.csv')
submission.xgb <- read_csv('submission/xgb.csv')

df <- data.frame(
  Id = submission.gbm$Id,
  SalePrice = 
    0.5 * submission.gbm$SalePrice +
    0.2 * submission.regularized$SalePrice +
    0.3 * submission.xgb$SalePrice
)

write_csv(df, 'submission/weighted.csv')
