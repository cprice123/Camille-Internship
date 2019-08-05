library(xgboost)
#------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars[,-1])
bst <- xgboost(data = mtcarsm, label = mtcars$mpg, nrounds = 20)

importance <- xgb.importance(feature_names = colnames(mtcarsm), model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)

chisq.test(mtcars$hp, mtcars$mpg) #somewhat unreliable because of small numbers
# whole mtcars dataframe
#------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars[,2:6])
bst <- xgboost(data = mtcarsm, label = mtcars$mpg, nrounds = 20)

importance <- xgb.importance(feature_names = colnames(mtcarsm), model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)

# first half (cyl, disp, hp, drat, wt)
#------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars[,7:11])
bst <- xgboost(data = mtcarsm, label = mtcars$mpg, nrounds = 20)

importance <- xgb.importance(feature_names = colnames(mtcarsm), model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)

#second half (qsec, vs, am, gear, carb)
#------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars[,8:10])
bst <- xgboost(data = mtcarsm, label = mtcars$mpg, nrounds = 20)

importance <- xgb.importance(feature_names = colnames(mtcarsm), model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)

#ones taken out by everything model (vs, am, gear)
#------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars[,c(2,6,3)])
bst <- xgboost(data = mtcarsm, label = mtcars$mpg, nrounds = 20)

importance <- xgb.importance(feature_names = colnames(mtcarsm), model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)

#three best predictors (cyl, wt, disp)
#------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars[,c(2,3,6,8:10)])
bst <- xgboost(data = mtcarsm, label = mtcars$mpg, nrounds = 20)

importance <- xgb.importance(feature_names = colnames(mtcarsm), model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)

#best and ones taken out (cyl, disp, wt, vs, am, gear)
#------------------------------------------------------------------------------
