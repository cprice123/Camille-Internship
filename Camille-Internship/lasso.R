library(glmnet)

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

