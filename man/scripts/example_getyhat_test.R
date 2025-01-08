library(midr)
nrow(airquality)
#> 153
.iris <- iris
.iris$Species <- relevel(.iris$Species, "virginica")
nrow(.iris)
#> 150

library(e1071)
m_svm <- svm(Ozone ~ ., airquality)
get.yhat(m_svm, airquality)
p_svm <- svm(Species ~ ., .iris, probability = TRUE)
get.yhat(p_svm, .iris, target = "virginica")

library(kernlab)
m_ksvm <- ksvm(Ozone ~ ., airquality)
get.yhat(m_ksvm, airquality)
p_ksvm <- ksvm(Species ~ ., .iris, prob.model = TRUE)
get.yhat(p_ksvm, .iris)

library(rpart)
m_rpart <- rpart(Ozone ~ ., airquality)
get.yhat(m_rpart, airquality)
p_rpart <- rpart(Species ~ ., .iris)
get.yhat(p_rpart, .iris)

library(ranger)
m_ranger <- ranger(Ozone ~ ., na.omit(airquality))
get.yhat(m_ranger, airquality)
p_ranger <- rpart(Species ~ ., .iris)
get.yhat(p_ranger, .iris)

library(randomForest)
m_randomForest <- randomForest(Ozone ~ ., na.omit(airquality))
get.yhat(m_randomForest, airquality)
p_randomForest <- randomForest(Species ~ ., .iris)
get.yhat(p_randomForest, .iris)

library(tree)
m_tree <- tree(Ozone ~ ., airquality)
get.yhat(m_tree, airquality)
p_tree <- tree(Species ~ ., .iris)
get.yhat(p_tree, .iris)
