#'
#' ISLR2::Bikeshare
#'
library(midr)
library(gridExtra)
library(ggplot2)
xy <- ISLR2::Bikeshare
set.seed(42)
train_rows <- sample(nrow(xy), nrow(xy) * .75)
train <- xy[ train_rows, ]
valid <- xy[-train_rows, ]
# fit a two-dimensional MID model
mid <- interpret(bikers ~ (mnth + factor(workingday) + hr +
                   weathersit + temp + hum + windspeed)^2,
                 data = train, lambda = .01, link = "log")
pred <- get.yhat(mid, valid)
weighted.rmse(valid$bikers, pred)
# visualize main effects
grid.arrange(grobs = mid.plots(mid))
# visualize term importance
imp <- mid.importance(mid)
ggmid(imp)
ggmid(imp, "heatmap")
# visualize the most important interaction
ggmid(mid, "factor(workingday):hr")
plot(mid, "factor(workingday):hr")
# create an object for ICE plots
set.seed(42)
ice.rows <- sample(nrow(valid), 200L)
mc <- mid.conditional(mid, valid[ice.rows,], "hr")
mc2 <- mid.conditional(mid, valid[ice.rows,], "factor(workingday)")
# visualize ICE plots
plot(mc, variable.colour = factor(workingday))
ggmid(mc, variable.colour = factor(workingday))
ggmid(mc, variable.colour = factor(workingday)) +
  coord_polar()
plot(mc2, variable.colour = hr)
ggmid(mc2, variable.colour = hr)