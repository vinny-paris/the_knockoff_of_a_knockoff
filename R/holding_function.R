
#function to convert demo to useable on generalized code/data


holding_function <- function(resp, expl, Xko){

for (phename in colnames(resp)){
  
  diti <- data.frame(resp[,1], resp[,phename, with = FALSE])
  colnames(diti) <- c("ID", phename)

  non-missing <- !is.na(diti[,phename, with = FALSE])
  diti <- diti[non-missing,]
  kn_var <- Xko[non-missing,]
  expl <- expl[non-missing,]
  
  kn <- my_kn(as.matrix(expl[,-"ID"]), y = diti[,-"ID"], kn_var, offset = 0)
  chosen <- kn$selected
  
  
  if(length(chosen) == 0) {
    result <- t(rep(NA, 4))
  } else{
    result <- NULL
    stuff <- matrix(unlist(data[,names(chosen)]), nrow = dim(data)[1])
    colnames(stuff) <- names(chosen)
    model <- glm( diti[,-"ID"] ~ as.matrix(expl[,chosen]), family = gaussian(), data = expl, na.action=na.exclude)
    
    result <- as.data.frame(summary(model)$coefficients)[-1,]
    holding <- rbind(result, holding)
  }

  
  result <- as.matrix(result, ncol = 4)
  for(i in 1:(dim(result)[1])){
    res.cache.idx <- res.cache.idx + 1
    res.cache[res.cache.idx, ] <- list(phenotype = phename, 
                                       scp = snpname[i], 
                                       est = result[i,1], 
                                       se = result[i,2], 
                                       p_value = result[i,4],
                                       file = snpfname)
  }    
}
}
  
  
  
  
  
    
    
    
     