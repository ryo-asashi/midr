#'
#' DALEX::apartments
#'
library(midr)
library(DALEX)
library(ggplot2)
library(gridExtra)
library(e1071)
# import data
xy <- DALEX::apartments
set.seed(42)
train_rows <- sample(nrow(xy), nrow(xy) * .75)
train <- xy[ train_rows, ]
valid <- xy[-train_rows, ]
test <- DALEX::apartments_test
#'
#' fit MID models to the data
#'
mid2d <- interpret(m2.price ~ .^2, train)
weighted.rmse(get.yhat(mid2d, valid), valid$m2.price)
autoplot(mid.importance(mid2d))
#> mid2d vs valid : RMSE 80.79291
mid1d <- interpret(m2.price ~ ., train)
summary(mid1d)
weighted.rmse(get.yhat(mid1d, valid), valid$m2.price)
#> mid1d vs valid : RMSE 62.36172
# visualize important term effects
grid.arrange(grobs = mid.plots(mid))
weighted.rmse(get.yhat(mid1d, test), test$m2.price, na.rm = TRUE)
#> mid1d vs test : RMSE 63.88086
# custom plots using DALEX theme
ggmid(mid1d, "construction.year") + theme_default_dalex()
ggmid(mid1d, "district") + theme_vertical_default_dalex() + coord_flip()
#
# construct a SVM model and a surrogate of it
#
model_svm <- svm(m2.price ~ ., train)
weighted.rmse(get.yhat(model_svm, valid), valid$m2.price)
#> svm vs valid : RMSE 168.8267
mid_svm <- interpret(m2.price ~ ., train, model_svm)
weighted.rmse(get.yhat(mid_svm, valid), get.yhat(model_svm, valid))
#> mid_svm vs svm : RMSE 84.08171
weighted.rmse(get.yhat(mid_svm, valid), valid$m2.price)
#> mid_svm vs valid : RMSE 134.6396
ggmid(mid_svm, "construction.year") + theme_default_dalex()
#
# construct a linear model and a surrogate of it
#
model_lm <- lm(m2.price ~ ., train)
weighted.rmse(get.yhat(model_lm, valid), valid$m2.price)
#> lm vs valid : RMSE 288.6719
weighted.rmse(get.yhat(mid_lm, valid), get.yhat(model_lm, valid))
#> mid_lm vs lm : RMSE 0.01...
weighted.rmse(get.yhat(mid_lm, valid), valid$m2.price)
#> mid_lm vs valid : RMSE 288.6724
grid.arrange(grobs = mid.plots(mid_lm))
