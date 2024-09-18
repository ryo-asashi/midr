#'
#' datasets::airquality
#' 
library(midr)
library(ggplot2)
library(ranger)
library(gridExtra)
# import data
xy <- datasets::airquality
set.seed(42)
train_rows <- sample(nrow(xy), nrow(xy) * .75)
train <- xy[ train_rows, ]
valid <- xy[-train_rows, ]
# construct ranger model with tuned parameters
set.seed(42)
model <- ranger(Ozone ~ ., na.omit(train), importance = "permutation",
                num.trees = 100, mtry = 5)
weighted.rmse(get.yhat(model, valid), valid$Ozone, na.rm = TRUE)
#> model vs valid : RMSE 20.75258
# fit a MID surrogate
set.seed(42)
mid <- interpret(Ozone ~ .^2, shuffled(train, size = 2e4), model)
weighted.rmse(get.yhat(mid, valid), get.yhat(model, valid), na.rm = TRUE)
#> mid vs model : RMSE 5.738283
weighted.rmse(get.yhat(mid, valid), valid$Ozone, na.rm = TRUE)
#> mid vs valid : RMSE 21.83065
# visualize important term effects
autoplot(mid.importance(mid))
grid.arrange(
  ggmid(mid, "Temp", add = TRUE) +
    geom_point(aes(y = Ozone), data = train),
  ggmid(mid, "Wind", add = TRUE) +
    geom_point(aes(y = Ozone), data = train),
  ggmid(mid, "Solar.R", add = TRUE) +
    geom_point(aes(y = Ozone), data = train),
  ggmid(mid, "Wind:Temp", add = TRUE, scale.type = "viridis") +
    geom_point(data = na.omit(train))
) |> suppressWarnings()
