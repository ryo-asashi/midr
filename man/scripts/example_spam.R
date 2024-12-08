#'
#' kernlab::spam
#'
devtools::document()
library(midr)
library(ggplot2)
library(gridExtra)
library(kernlab)
library(pROC)
# import data
data(spam, package = "kernlab")
xy <- spam
xy$type <- relevel(xy$type, "spam")
set.seed(42)
train_rows <- sample(nrow(xy), nrow(xy) * .75)
train <- xy[ train_rows, ]
valid <- xy[-train_rows, ]
# fit MID models to the data
mid <- interpret(type ~ ., train, lambda = 1)
summary(mid)
autoplot(imp <- mid.importance(mid), max.terms = 20)
grid.arrange(grobs = mid.plots(mid, terms(imp)[1:9]))
# evaluate the fitted mid model by ROC-AUC
roc_mid <- roc(valid$type, predict(mid, valid))
ggroc(roc_mid) + theme_midr() +
  labs(subtitle = paste("mid auc : ", round(roc_mid$auc, 4)))

