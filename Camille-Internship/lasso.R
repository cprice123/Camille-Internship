library(glmnet)

mtcarsm <- as.matrix(mtcars)
fit = glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 1)

cvfit = cv.glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 1)
coef(cvfit, s="lambda.min") #27
coef(cvfit, s="lambda.1se") #16
plot(cvfit)
#lambda increase, log(lambda) increase(in + direction, not abs val)