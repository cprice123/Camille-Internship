library(glmnet)

#-------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars)
fit = glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 1)
plot(fit) #doesn't change without seed

set.seed(123)
cvfit = cv.glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 1)
coef(cvfit, s="lambda.min") #changes without seed
coef(cvfit, s="lambda.1se") #changes without seed
plot(cvfit)
#lambda increase, log(lambda) increase(in + direction, not abs val)

summary(coef(cvfit, s="lambda.1se"))#condensed coef() info
#lasso
#-------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars)
fit = glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0.75)
plot(fit)

cvfit = cv.glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0.75)
coef(cvfit, s="lambda.min") 
coef(cvfit, s="lambda.1se")
plot(cvfit)

mtcarsm <- as.matrix(mtcars)
fit = glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0.5)
plot(fit)

cvfit = cv.glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0.5)
coef(cvfit, s="lambda.min") 
coef(cvfit, s="lambda.1se")
plot(cvfit)

mtcarsm <- as.matrix(mtcars)
fit = glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0.25)
plot(fit)

cvfit = cv.glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0.25)
coef(cvfit, s="lambda.min") 
coef(cvfit, s="lambda.1se")
plot(cvfit)
#elastic net
#-------------------------------------------------------------------------------
mtcarsm <- as.matrix(mtcars)
fit = glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0)
plot(fit)

cvfit = cv.glmnet(mtcarsm[,-1],mtcarsm[,1],alpha = 0)
coef(cvfit, s="lambda.min") 
coef(cvfit, s="lambda.1se")
plot(cvfit)
#ridge
#-------------------------------------------------------------------------------

