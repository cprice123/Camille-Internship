library(trena)
library(xgboost)
set.seed(122113)
# Load matrix and transform via arcsinh
load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))
mtx.asinh <- asinh(mtx.sub)
#print(fivenum(mtx.asinh)  # [1] 0.000000 1.327453 3.208193 4.460219 7.628290)

target.gene <- "MEF2C"
tfs <- setdiff(rownames(mtx.asinh), "MEF2C")
solvers <- c("lasso", "ridge", "lassopv", "pearson", "spearman", "randomForest") # "sqrtlasso",

solver <- EnsembleSolver(mtx.asinh,target.gene,tfs,solverNames=solvers)
tbl.trena <- run(solver)
#-----------------------------------------------------------------------------------
#set up variables for xgboost
x = t(mtx.asinh[tfs,,drop=FALSE])
y = as.vector(mtx.asinh[target.gene,])
tbl.xgboost <- data.frame()
#-----------------------------------------------------------------------------------
#add normal xgboost example
set.seed(123)
bst <- xgboost(data = x, label = y, nrounds = 200, eta = 0.1, subsample = 0.5)
tbl.importance.1 <- xgb.importance(feature_names = colnames(x), model = bst)

#tbl.xgboost.1 <- data.frame(row.names = importance$Feature, normalModelImportance = importance[,2])
#-----------------------------------------------------------------------------------
#more nrounds example
set.seed(123)
bst <- xgboost(data = x, label = y, nrounds = 1000, eta = 0.1, subsample = 0.5)
tbl.importance.2 <-importance <- xgb.importance(feature_names = colnames(x), model = bst)

#tbl.xgboost <- cbind(data.frame(row.names = importance$Feature, moreNrounds = importance[,2]),tbl.xgboost)
#-----------------------------------------------------------------------------------
#fewer nrounds, larger eta example--not working because creates
#importance values for many that are too small to be displayed, so will
#not cbind because of having too few rows
set.seed(123)
bst <- xgboost(data = x, label = y, nrounds = 10, eta = 0.5, subsample = 0.5)
tbl.importance.3 <- xgb.importance(feature_names = colnames(x), model = bst)

#tbl.xgboost <- cbind(data.frame(row.names = importance$Feature,
#                                lessNroundsMoreEta = importance[,2]),tbl.xgboost)
#-----------------------------------------------------------------------------------
#bigger subsample example
set.seed(123)
bst <- xgboost(data = x, label = y, nrounds = 200, eta = 0.1, subsample = 0.9)
tbl.importance.3 <-  xgb.importance(feature_names = colnames(x), model = bst)

#tbl.xgboost <- cbind(data.frame(row.names = importance$Feature,
#                                bigSubsample = importance[,2]),tbl.xgboost)
#-----------------------------------------------------------------------------------
#small subsample example--also not working for same reason
set.seed(123)
bst <- xgboost(data = x, label = y, nrounds = 200, eta = 0.1, subsample = 0.1)
tbl.importance.4 <- xgb.importance(feature_names = colnames(x), model = bst)

#tbl.xgboost <- cbind(data.frame(row.names = importance$Feature,
#                                smallSubsample = importance[,2]),tbl.xgboost)


#I don't know how to get the column names to be normalModelImportance, moreNrounds,
#bigSubsample etc like I want them to be


# "goi" shorthand for "genes of interest"
# for a quick, first look:
#   add boost results only for the TFs indentified by trena
goi <- tbl.trena$gene
all(goi %in% tbl.importance.1$Feature)
all(goi %in% tbl.importance.2$Feature)
all(goi %in% tbl.importance.3$Feature)
all(goi %in% tbl.importance.4$Feature)

tbl.extended <- merge(tbl.trena, tbl.importance.1[, 1:2], by.x="gene", by.y="Feature")
colnames(tbl.extended)[ncol(tbl.extended)] <- "boost.iter10.eta0.5sub0.5"

tbl.extended <- merge(tbl.extended, tbl.importance.2[, 1:2], by.x="gene", by.y="Feature")
colnames(tbl.extended)[ncol(tbl.extended)] <- "boost.iter.1000.eta0.1sub0.5"

tbl.extended <- merge(tbl.extended, tbl.importance.3[, 1:2], by.x="gene", by.y="Feature")
colnames(tbl.extended)[ncol(tbl.extended)] <- "boost.iter.200.eta0.1sub0.9"

tbl.extended <- merge(tbl.extended, tbl.importance.4[, 1:2], by.x="gene", by.y="Feature")
colnames(tbl.extended)[ncol(tbl.extended)] <- "boost.iter.200.eta0.1sub0.1"



