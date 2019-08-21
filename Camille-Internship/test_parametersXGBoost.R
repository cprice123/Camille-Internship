library(trena)
library(RUnit)
library(xgboost)
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
runTests <- function()
{
  test_ParameterizeXGBoostSolverConstructor()
  test_ampAD.mef2c.154tfs.278samples.ParameterizeXGBoost()
  
} # runTests
#----------------------------------------------------------------------------------------------------
test_ParameterizeXGBoostSolverConstructor <- function()
{
  printf("--- test_ParameterizeXGBoostSolverConstructor")
  
  mtx <- matrix(1:9,nrow=3)
  rownames(mtx) <- c("gene1","gene2","gene3")
  solver <- ParameterizeXGBoostSolver(mtx,targetGene = "gene1",
                          candidateRegulators = c("gene2","gene3"))
  
  checkEquals(class(solver)[1], "ParameterizeXGBoostSolver")
  checkTrue(all(c("ParameterizeXGBoostSolver", "Solver") %in% is(solver)))
  
} # test_ParameterizeXGBoostSolverConstructor
#----------------------------------------------------------------------------------------------------
test_ampAD.mef2c.154tfs.278samples.ParameterizeXGBoost <- function()
{
  printf("--- test_ampAD.mef2c.154tfs.278samples.ParameterizeXGBoost")
  
  # Load matrix and transform via arcsinh
  load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))
  
  mtx.asinh <- asinh(mtx.sub)
  target.gene <- "MEF2C"
  tfs <- setdiff(rownames(mtx.asinh), "MEF2C")
  #print(fivenum(mtx.asinh)  # [1] 0.000000 1.327453 3.208193 4.460219 7.628290)
  
  ParameterizeXGBoost.solver <- ParameterizeXGBoostSolver(mtx.asinh, target.gene, tfs)
  tbl <- run(ParameterizeXGBoost.solver)
  
  # Check for empirical values
  checkTrue(nrow(subset(tbl_eval, abs(rounds) < 101)) == 8)
  
} # test_ampAD.mef2c.154tfs.278samples.ParameterizeXGBoost
#----------------------------------------------------------------------------------------------------
if(!interactive()) runTests()
