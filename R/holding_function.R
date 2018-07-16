#' A slightly modified version of the knockoff function
#' 
#' 
#' 2 major differences being that the knockoff variables
#' are to be passed directly to the function, not made inside
#' of it; and secondly you can pass the amount of cores wanted
#' directly to the statistic function chosen. Be careful as this
#' may need to be 'NULL'-ed if the statistic function you choose
#' does not allow for multi-cores. 
#' 
#' @export
#' @param resp The response variable, a matrix the same length as expl The first col must be the identifier
#' @param expl The data frame to be the explanatory variables, the first col must be the identifier 
#' @param Xko The knockoff variables created seperately (see the 'knockoff' package for more details). Also must be the same length as expl, resp.
#' @param offset The offset is used to control conservative-ness. 0 for liberal and best for smaller data sets, 1 for conservative and larger data sets. 1 can be a very poor choice if you do not have many, many explanatory variables. Passed to the my_kn function.
#' @param fdr The "False Discovery Rate", allowed to be between 0,1 (non-inclusive). Defaults to .2, implying that 1/5 of variables returned will be a type 1 error. This will be passed to the my_kn function.
#' @param cores The cores to be used in the doMC section of the code, passed to the my_kn function.
#' @param ... Various other arguments to be passed to the my_kn function.
#' 
#' @return This will return of data frame 5 columns. The first being phenotype, the signficant variable selected, the estimate of a glm regression, se for standard error of the regression and the p-value. 
#' 

holding_function <- function(resp, expl, Xko, offset = 0, fdr = .2, cores = 2, ...){
  colnames(resp)[1] <- "ID"
  colnames(expl)[1] <- "ID"
  
  res.cache <- data.table(
    phenotype=rep("", res.cache.nrow),
    snp="", est=0.0, se=0.0, p_value=0.0)
  res.cache.idx <- 0
  
  expl_names <- setdiff(names(expl), "ID")
  
for (phename in colnames(resp)[-1]){
  
  diti <- data.frame(resp[,1], resp[,phename])
  colnames(diti) <- c("ID", phename)
  diti <- diti[diti$ID %in% expl$ID,]
  
  
  non_missing <- !is.na(diti[,phename])
  doto <- diti[non_missing, -1]
  kn_var <- Xko[non_missing,]
  covar <- expl[non_missing, -1]
  
  
  kn <- my_kn((covar), doto, kn_var, offset = 0, fdr = .9)
  chosen <- kn$selected
  chosen
  
  holding <- NULL
  if(length(chosen) == 0) {
    result <- t(rep(NA, 4))
  } else{
    result <- NULL
    stuff <- matrix(unlist(data[,names(chosen)]), nrow = dim(data)[1])
    colnames(stuff) <- names(chosen)
    model <- glm( doto ~ as.matrix(expl[,chosen]), family = gaussian(), data = covar, na.action=na.exclude)
    
    result <- as.data.frame(summary(model)$coefficients)[-1,]
  }
  
  expl_names <- names(chosen)
  if(length(chosen) == 0) {expl_names <- "No Sig. Expl. Var."}
  

  result <- as.matrix(result, ncol = 4)
  for(i in 1:(dim(result)[1])){
    res.cache.idx <- res.cache.idx + 1
    res.cache[res.cache.idx, ] <- list(phenotype = phename, 
                                       snp = expl_names[i], 
                                       est = result[i,1], 
                                       se = result[i,2], 
                                       p_value = result[i,4])
  }   
}
  return(res.cache)
  }
  
  







































    
     