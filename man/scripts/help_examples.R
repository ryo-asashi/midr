# factor_encoder.R
data(iris, package = "datasets")
enc <- factor.encoder(x = iris$Species, use.catchall = FALSE)
enc$frame
enc$encode(new_x = c("setosa", "virginica", "ensata", NA, "versicolor"))

# numeric_encoder.R
data(iris, package = "datasets")
enc <- numeric.encoder(x = iris$Sepal.Length, k = 5L)
enc$frame
enc$encode(new_x = 4:8)

# get_yhat.R
model <- glm(Volume ~ ., trees, family = Gamma(log))
predict(model, trees, "response")
get.yhat(model, trees)

# ggmid.R
data(diamonds, package = "ggplot2")
model <- lm(price ~ carat + cut + color + clarity + carat:clarity, diamonds)
mid <- interpret(price ~ carat + cut + color + clarity + carat:clarity,
                 data = diamonds, model = model)
ggmid(mid, "carat")
ggplot2::autoplot(mid, "clarity")
ggmid(mid, "carat:clarity")
ggmid(mid, "carat:clarity", add.intercept = TRUE,
      include.main.effects = TRUE, scale.type = "viridis")

# ggmid_mid_conditional.R
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
mc <- mid.conditional(mid, "Wind", airquality)
ggmid(mc, variable.colour = "Solar.R", centered = TRUE)

# ggmid_mid_importance.R
data(diamonds, package = "ggplot2")
set.seed(42)
mid <- interpret(price ~ (carat + cut + color + clarity)^2,
                 diamonds[sample(nrow(diamonds), 1e4), ])
imp <- mid.importance(mid)
ggmid(imp)
ggmid(imp, type = "heatmap")

# mid_conditional.R
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
mc <- mid.conditional(mid, "Wind", airquality)
mc

# mid_extract.R

data(trees, package = "datasets")
mid <- interpret(Volume ~ ., trees, k = 10)
mid.extract(mid, encoding.info)
mid.extract(mid, uninterpreted.rate)
mid.extract(mid, frames)
mid.extract(mid, Girth)
mid.extract(mid, intercept)

# mid_importance.R
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
imp <- mid.importance(mid)
imp

# mid_plots.R
data(diamonds, package = "ggplot2")
set.seed(42)
idx <- sample(nrow(diamonds), 1e4L)
mid <- interpret(price ~ (carat + cut + color + clarity) ^ 2, diamonds[idx, ])
mid.plots(mid, c("carat", "color", "carat:color", "clarity:color"), limits = NULL)

# plot_mid.R
data(airquality, package = "datasets")
airquality$Month <- factor(airquality$Month)
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
plot(mid, "Temp")
plot(mid, "Month")
plot(mid, "Wind:Temp")
plot(mid, "Solar.R:Month", scale.type = "viridis",
     add.intercept = TRUE, include.main.effects = TRUE)

# misc_functions.R
# weighted quantile
weighted.quantile(x = 1:10, w = 1:10, probs = c(0, .25, .50, .75, 1))
# weighted tabulate
weighted.tabulate(bin = c(2, 2, 3, 5), w = 1:4)
# weighted loss functions
weighted.rmse(x = c(0, 10), y = c(0, 0), w = c(99, 1))
weighted.mae(x = c(0, 10), y = c(0, 0), w = c(99, 1))
weighted.medae(x = c(0, 10), y = c(0, 0), w = c(99, 1))
# theme midr
ggplot2::theme_set(theme_midr())

# plot_mid_conditional.R
data(airquality, package = "datasets")
mid <- interpret(Ozone ~ .^2, airquality, lambda = 1)
mc <- mid.conditional(mid, "Wind", na.omit(airquality))
plot(mc, variable.colour = "Solar.R", centered = TRUE)

# plot_mid_importance.R
data(diamonds, package = "ggplot2")
set.seed(42)
mid <- interpret(price ~ (carat + cut + color + clarity)^2,
                 diamonds[sample(nrow(diamonds), 1e4), ])
imp <- mid.importance(mid)
plot(imp)
plot(imp, type = "heatmap")

# predict_mid.R
data(cars, package = "datasets")
mid <- interpret(dist ~ speed, cars, lambda = 1)
predict(mid, newdata = data.frame(speed = 5:25))
mid.f(mid, "speed", 5:25) + mid$intercept

# print_mid.R
data(cars, package = "datasets")
print(interpret(dist ~ speed, cars))

# summary_mid.R
data(cars, package = "datasets")
summary(interpret(dist ~ speed, cars))

# interpret.R
data(cars, package = "datasets")
model <- lm(dist ~ I(speed^2) + speed, cars)
mid <- interpret(dist ~ speed, cars, model)
plot(mid, "speed", add.intercept = TRUE) +
  points(cars)
summary(mid)

data(Nile, package = "datasets")
mid <- interpret(x = 1L:100L, y = Nile, k = 100L)
plot(mid, "x", add.intercept = TRUE, ylim = c(600L, 1300L)) +
  points(x = 1L:100L, y = Nile)
# reduce number of knots by k parameter
mid <- interpret(x = 1L:100L, y = Nile, k = 10L)
plot(mid, "x", add.intercept = TRUE, ylim = c(600L, 1300L)) +
  points(x = 1L:100L, y = Nile)
# pseudo-smoothing by lambda parameter
mid <- interpret(x = 1L:100L, y = Nile, k = 100L, lambda = 100L)
plot(mid, "x", add.intercept = TRUE, ylim = c(600L, 1300L)) +
  points(x = 1L:100L, y = Nile)

data(airquality, package = "datasets")
airquality$Month <- factor(airquality$Month)
model <- glm(Ozone ~ .^2, Gamma(log), airquality)
mid <- interpret(Ozone ~ .^2, na.omit(airquality), model, lambda = .1)
summary(mid)
plot(mid, "Wind")
plot(mid, "Temp")
plot(mid, "Wind:Month", include.main.effects = TRUE)
