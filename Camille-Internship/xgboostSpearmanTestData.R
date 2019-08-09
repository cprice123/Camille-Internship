# Load matrix and transform via arcsinh
load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))

mtx.asinh <- asinh(mtx.sub)
target.gene <- "MEF2C"
tfs <- setdiff(rownames(mtx.asinh), "MEF2C")
#print(fivenum(mtx.asinh)  # [1] 0.000000 1.327453 3.208193 4.460219 7.628290)   

spearman.solver <- SpearmanSolver(mtx.asinh, target.gene, tfs)
tbl <- run(spearman.solver)

#code from test_SpearmanSolver.R
#-------------------------------------------------------------------------------
load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))
target.gene <- mtx.sub["MEF2C",]
Regulators <- setdiff(rownames(mtx.sub), "MEF2C")
bst <- xgboost(data = t(mtx.sub[Regulators,]), label = target.gene, eta = 0.1, nrounds = 100)

importance <- xgb.importance(feature_names = Regulators, model = bst)
plot.importance <- xgb.plot.importance(importance_matrix = importance)
#xgboost with spearman test data
#-------------------------------------------------------------------------------