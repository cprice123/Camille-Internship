library(randomForest)
#-----------------------------------------------------------------------------
x <- t(mtx[tfs,,drop=FALSE])      
y <- as.vector(t(mtx[target.gene,])) # Change y to a vector to avoid RF warning      

fit <- randomForest( x = x, y = y )

# Grab output and sort it
edges <- fit$importance             
edges <- edges[order(edges, decreasing=TRUE),]
#example
#------------------------------------------------------------------------------
set.seed(4543)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, importance=TRUE)
importance(mtcars.rf)
importance(mtcars.rf, type=1)
#my try (based on website example)
#------------------------------------------------------------------------------
x <- mtcars[,-1]
y <- mtcars$mpg

fit <- randomForest(x = x, y = y)

edges <- fit$importance
edges <- edges[order(edges, decreasing=TRUE),]#ordered importance

#my try (based on example)
#------------------------------------------------------------------------------
