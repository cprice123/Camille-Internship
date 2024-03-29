#----------------------------------------------------------------------------------------------------
#' An S4 class to represent parameters for a XGBoost solver
#'
#' @import xgboost    
#' @include Solver.R
#' @import methods
#'
#' @name ParameterizeXGBoostSolver-class
#'

.ParameterizeXGBoostSolver <- setClass("ParameterizeXGBoostSolver", contains = "Solver")
#----------------------------------------------------------------------------------------------------
#' Create a Solver class using gradient boosting (a regression technique) and the XGBoost library
#'
#' @param mtx.assay An assay matrix of gene expression data
#' @param targetGene A designated target gene that should be part of the mtx.assay data
#' @param candidateRegulators The designated set of transcription factors that could be associated
#' with the target gene
#' @param quiet A logical denoting whether or not the solver should print output
#'
#' @return A Solver class object of parameters for XGBoost
#'
#' @seealso  \code{\link{solve.XGBoost}}, \code{\link{getAssayData}}
#'
#' @family Solver class objects
#'
#' @export
#'
#' @examples
#' load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))
#' target.gene <- "MEF2C"
#' tfs <- setdiff(rownames(mtx.sub), target.gene)
#' ParameterizeXGBoost.solver <- ParameterizeXGBoostSolver(mtx.sub, target.gene, tfs)

ParameterizeXGBoostSolver <- function(mtx.assay = matrix(), targetGene, candidateRegulators, quiet=TRUE)
{
  # Remove the targetGene from candidateRegulators
  if(any(grepl(targetGene, candidateRegulators)))
    candidateRegulators <- candidateRegulators[-grep(targetGene, candidateRegulators)]
  
  # Check to make sure the matrix contains some of the candidates
  candidateRegulators <- intersect(candidateRegulators, rownames(mtx.assay))
  stopifnot(length(candidateRegulators) > 0)
  
  obj <- .ParameterizeXGBoostSolver(Solver(mtx.assay=mtx.assay,
                               quiet=quiet,
                               targetGene=targetGene,
                               candidateRegulators=candidateRegulators))
  
  # Send a warning if there's a row of zeros
  if(!is.na(max(mtx.assay)) & any(rowSums(mtx.assay) == 0))
    warning("One or more gene has zero expression; this may yield 'NA' results and warnings when using XGBoost correlations")
  
  obj
  
} #ParameterizeXGBoostSolver, the constructor
#----------------------------------------------------------------------------------------------------
#' Show the ParameterizeXGBoost Solver
#'
#' @rdname show.ParameterizeXGBoostSolver
#' @aliases show.ParameterizeXGBoostSolver
#'
#' @param object An object of the class ParameterizeXGBoostSolver
#'
#' @return A truncated view of the supplied object
#'
#' @examples
#' load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))
#' target.gene <- "MEF2C"
#' tfs <- setdiff(rownames(mtx.sub), target.gene)
#' ParameterizeXGBoost.solver <- ParameterizeXGBoostSolver(mtx.sub, target.gene, tfs)
#' show(ParameterizeXGBoost.solver)

setMethod('show', 'ParameterizeXGBoostSolver',
          
          function(object) {
            regulator.count <- length(getRegulators(object))
            if(regulator.count > 10){
              regulatorString <- paste(getRegulators(object)[1:10], collapse=",")
              regulatorString <- sprintf("%s...", regulatorString);
            }
            else
              regulatorString <- paste(getRegulators(object), collapse=",")
            
            msg = sprintf("ParameterizeXGBoostSolver with mtx.assay (%d, %d), targetGene %s, %d candidate regulators %s",
                          nrow(getAssayData(object)), ncol(getAssayData(object)),
                          getTarget(object), regulator.count, regulatorString)
            cat (msg, '\n', sep='')
          })
#----------------------------------------------------------------------------------------------------
#' Run the ParameterizeXGBoost Solver
#'
#' @rdname solve.ParameterizeXGBoost
#' @aliases run.ParameterizeXGBoostSolver solve.ParameterizeXGBoost
#'
#' @description Given a TReNA object with XGBoost as the solver, use the \code{\link{cor}}
#' function with \code{method = "XGBoost"} to find parameters for XGBoostSolver.
#'
#' @param obj An object of class XGBoostSolver
#'
#' @return The set of XGBoost parameters for XGBoostSolver
#' 
#' @seealso \code{\link{cor}}, \code{\link{XGBoostSolver}}
#'
#' @family solver methods
#'
#' @examples
#' # Load included Alzheimer's data, create a TReNA object with Bayes Spike as solver, and solve
#' load(system.file(package="trena", "extdata/ampAD.154genes.mef2cTFs.278samples.RData"))
#' target.gene <- "MEF2C"
#' tfs <- setdiff(rownames(mtx.sub), target.gene)
#' ParameterizeXGBoost.solver <- ParameterizeXGBoostSolver(mtx.sub, target.gene, tfs)
#' tbl <- run(ParameterizeXGBoost.solver)

setMethod("run", "ParameterizeXGBoostSolver",
          
          function (obj){
            
            mtx <- getAssayData(obj)
            target.gene <- getTarget(obj)
            tfs <- getRegulators(obj)
            
            # Check that target gene and tfs are all part of the matrix
            stopifnot(target.gene %in% rownames(mtx))
            stopifnot(all(tfs %in% rownames(mtx)))
            
            # If given no tfs, return nothing
            if (length(tfs)==0) return(data.frame())
            
            # Don't handle tf self-regulation, so take target gene out of tfs
            deleters <- grep(target.gene, tfs)
            if(length(deleters) > 0){
              tfs <- tfs[-deleters]
            }
            # If target gene was the only tf, then return nothing
            if(length(tfs)==0) return(NULL)
            
            x = t(mtx[tfs,,drop=FALSE])
            y = as.vector(t(mtx[target.gene,])) # Make target gene levels into a vector
            
            #ParameterizeXGBoostSolver
            tbl_eval <- data.frame("Rounds" = numeric(),
                                      "Depth" = numeric(),
                                      "r_sample" = numeric(),
                                      "c_sample" = numeric(),
                                      "Eval" = numeric(),
                                      "best_round" = numeric())
            
            for (rounds in seq(100, 800, 100)){
              
              for (depth in c(5, 10)) {
                
                for (r_sample in c(0.5, 1)) {
                  
                  for (c_sample in c(0.4, 1)) {
                    
                    set.seed(123)
                    eta_val <- ((-2/7000)*(rounds-100)) + 0.3
                    cv <- xgb.cv(data = x, nfold = 10, label = y, 
                                    nrounds = rounds, 
                                    eta = eta_val, 
                                    max_depth = depth,
                                    subsample = r_sample, 
                                    colsample_bytree = c_sample,
                                    early_stopping_rounds = 0.5*rounds,
                                    #objective = regression_type,
                                    verbose = FALSE)
                    
                    print(paste(rounds, depth, r_sample, c_sample, cv$best_iteration, cv$evaluation_log[cv$best_iteration,test_rmse_mean]))
                    tbl_eval[nrow(tbl_eval)+1, ] <- c(rounds, 
                                                         depth, 
                                                         r_sample, 
                                                         c_sample, 
                                                         cv$evaluation_log[cv$best_iteration,test_rmse_mean], 
                                                         cv$best_iteration)
                     if (nrow(tbl_eval) == 64) {
                       tbl_eval_ordered <- tbl_eval[order(tbl_eval$Eval),]
                       print(tbl_eval_ordered)
                       print(tbl_eval_ordered[1,])
                       
                       #parameters to be used by XGBoostSolver
                       rounds <- tbl_eval_ordered[1,"Rounds"]
                       XGparams <- list(eta = ((-2/7000)*(rounds-100)) + 0.3,
                                      max_depth = tbl_eval_ordered[1,"Depth"],
                                      subsample = tbl_eval_ordered[1,"r_sample"],
                                      colsample_bytree = tbl_eval_ordered[1,"c_sample"])
                    }else{
                      #continues through loop
                    }
                  }
                }
              }
            }
          })
#----------------------------------------------------------------------------------------------------
