library(trena)
library(RUnit)
library(xgboost)
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
runTests <- function()
{
  test_ParamXGBoostSolverConstructor()
  test_ampAD.mef2c.154tfs.278samples.ParamXGBoost()
  
} # runTests
#----------------------------------------------------------------------------------------------------
test_ParamXGBoostSolverConstructor <- function()
{
  printf("--- test_ParamXGBoostSolverConstructor")
  
  mtx <- matrix(1:9,nrow=3)
  rownames(mtx) <- c("gene1","gene2","gene3")
  solver <- ParamXGBoostSolver(mtx,targetGene = "gene1",
                          candidateRegulators = c("gene2","gene3"))
  
  checkEquals(class(solver)[1], "ParamXGBoostSolver")
  checkTrue(all(c("ParamXGBoostSolver", "Solver") %in% is(solver)))
  
} # test_ParamXGBoostSolverConstructor
#----------------------------------------------------------------------------------------------------
test_ampAD.mef2c.154tfs.278samples.ParamXGBoost <- function()
{
  printf("--- test_ampAD.mef2c.154tfs.278samples.ParamXGBoost")
  
  # Load matrix and transform via arcsinh
  load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))
  
  mtx.asinh <- asinh(mtx.sub)
  target.gene <- "MEF2C"
  tfs <- setdiff(rownames(mtx.asinh), "MEF2C")
  #print(fivenum(mtx.asinh)  # [1] 0.000000 1.327453 3.208193 4.460219 7.628290)
  
  ParamXGBoost.solver <- ParamXGBoostSolver(mtx.asinh, target.gene, tfs)
  tbl <- run(ParamXGBoost.solver)
  
  # Check for empirical values
  checkTrue(nrow(subset(tbl_eval, abs(rounds) < 101)) == 48)
  
} # test_ampAD.mef2c.154tfs.278samples.ParamXGBoost
#----------------------------------------------------------------------------------------------------
if(!interactive()) runTests()
