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
#' @param expl The data frame to be the explanatory variables, the first col must be "ID"
#' @param resp The response variable, a matrix the same length as expl The first col must be "ID"
#' @param Xko The knockoff variables created seperately (see the 'knockoff' package for more details). Also must be the same length as expl, resp.
#' @param ... Various other arguments to be passed to the my_kn function.
#' 
#' @return This will return of data frame 5 columns. The first being phenotype, the signficant variable selected, the estimate of a glm regression, se for standard error of the regression and the p-value. 
#' 

holding_function <- function(resp, expl, Xko, offset = 0, fdr = .2, cores = 2){
 
  
  res.cache <- data.table(
    phenotype=rep("", res.cache.nrow),
    snp="", est=0.0, se=0.0, p_value=0.0)
  res.cache.idx <- 0
  
  expl_names <- setdiff(names(expl), "ID")
  
for (phename in colnames(resp)[-1]){
  
  diti <- data.frame(resp[,1], resp[,phename, with = FALSE])
  colnames(diti) <- c("ID", phename)
  diti <- diti[diti$ID %in% expl$ID,]
  
  
  non_missing <- !is.na(diti[,phename])
  doto <- diti[non_missing, -1]
  kn_var <- Xko[non_missing,]
  covar <- expl[non_missing, -1]
  
  
  kn <- my_kn(as.matrix(covar), doto, kn_var, ...)
  chosen <- kn$selected
  
  holding <- NULL
  if(length(chosen) == 0) {
    result <- t(rep(NA, 4))
  } else{
    result <- NULL
    stuff <- matrix(unlist(data[,names(chosen)]), nrow = dim(data)[1])
    colnames(stuff) <- names(chosen)
    model <- glm( doto ~ as.matrix(expl[,chosen]), family = gaussian(), data = covar, na.action=na.exclude)
    
    result <- as.data.frame(summary(model)$coefficients)[-1,]
    holding <- rbind(result, holding)
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
  
  
  
  
  








































    
     