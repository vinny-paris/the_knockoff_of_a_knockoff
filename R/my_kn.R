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
#' @param X The data frame to be the explanatory variables
#' @param y The response variable, a single vector the same length as X
#' @param Xk The knockoff variables created seperately (see the 'knockoff' package for more details) 
#' @param statistic The statistic to be used in the variable selection process. Defaults to the difference in coeffiecents for a basic linear model.
#' @param fdr The False Discovery Rate bounded between (0,1). The default is .1
#' @param offset Allows for more or less conservative selections. 1 for more (default) , 0 for less. 
#' @param cores The number of cores you would like ot use. The default is 2. If more is stated than is possible an error will be returned. 
#' 
#' 
#' @return Same as with the standard knockoff package function 'knockoff.filter', see that for more. 
#' 
#' @note This funtion can only be used *after* creating the knockoff variables, the goal was to split those two functions inorder to get a a faster speed up on the loop for response variables with the same explanatory variables.



my_kn <-  function (X, y, Xk = Xk, statistic = stat.glmnet_coefdiff, 
          fdr = 0.1, offset = 1, cores = 2) {
  if (is.data.frame(X)) {
    X.names = names(X)
    X = as.matrix(X, rownames.force = F)
  }else if (is.matrix(X)) {
    X.names = colnames(X)
  }else {
    stop("Input X must be a numeric matrix or data frame")
  }
  n = nrow(X)
  p = ncol(X)
  stopifnot(length(y) == n)
  W = statistic(X, Xk, y, cores = cores)
  t = knockoff.threshold(W, fdr = fdr, offset = offset)
  selected = which(W >= t)
  if (!is.null(X.names)) 
    names(selected) = X.names[selected]
  structure(list(call = match.call(), X = X, Xk = Xk, y = y, 
                 statistic = W, threshold = t, selected = selected), class = "knockoff.result")
}

