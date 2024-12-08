library(midr)
library(ggplot2)
library(gridExtra)
library(ranger)
theme_set(theme_midr())

xy <- ISLR2::Boston
set.seed(42)
train_idx <- sample(nrow(xy), nrow(xy) * .75)
train <- xy[ train_idx, ]
valid <- xy[-train_idx, ]

set.seed(42)
model <- ranger(medv ~ ., train, mtry = 5)
weighted.rmse(valid$medv, get.yhat(model, valid))
# rmse 3.295881

mid <- interpret(medv ~ .^2, train, lambda = 1)
weighted.rmse(valid$medv, get.yhat(mid, valid))
# rmse 3.303718

ggmid(mid.importance(mid), max = 20) +
  aes(alpha = degree)
grid.arrange(grobs = mid.plots(mid))
ggmid(mid, "lstat:dis", include = TRUE) +
  geom_point(data = train)
ggmid(mid, "lstat:age", include = TRUE) +
  geom_point(data = train)
