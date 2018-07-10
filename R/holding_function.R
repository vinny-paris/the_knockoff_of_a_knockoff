
#function to convert demo to useable on generalized code/data

#resp's first col is identifier


holding_function <- function(resp, expl, Xko, offset = 0, fdr = .2){
 
  
  res.cache <- data.table(
    phenotype=rep("", res.cache.nrow),
    snp="", est=0.0, se=0.0, p_value=0.0, file = "")
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
  
  
  kn <- my_kn(as.matrix(covar), doto, kn_var, offset = 0, fdr = .2)
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
  
  
  
  
  








































    
     