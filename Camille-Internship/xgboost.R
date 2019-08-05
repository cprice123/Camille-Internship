library(xgboost)

mtcarsm <- as.matrix(mtcars[,-1])
bst <- xgboost(data = mtcarsm, label = mtcars$mpg, nrounds = 20)

importance <- xgb.importance(feature_names = colnames(mtcarsm), model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)

chisq.test(mtcars$hp, mtcars$mpg) #somewhat unreliable because of small numbers

