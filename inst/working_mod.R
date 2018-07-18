#Please, Please, Please read "supplemental_readme"
#in the inst folder. It gives a much better idea
#of what is happening in this file. 

#directory of stuff
datadir <- paste0("./data/")

#get the phenome traits
phefname <- paste0(datadir, "ClinicalTraits.csv")
phetype <- fread(phefname)
phetype <- phetype[, -c(31, 16)]
phetype <- phetype[, c(2, 11:36) ]

#join the phenomes names with the mice identifiers
phenames <- setdiff(colnames(phetype), "Mice")

#result file getting prepped
resfname <- paste0(datadir , "result -mpi.csv")
if(file.exists(resfname)) invisible(file.remove(resfname))

#collect the files the snps are stored in
snpdir <- paste0(datadir)
wanted <- grep('Liver', list.files(snpdir))
snpflist <- list.files(snpdir)[wanted]

#break snp's up 
if (comm.size() > 1) {
  snpflist <- split(snpflist , cut(seq_along(snpflist),
                                   comm.size()))
} else  {
  snpflist <- list(snpflist)
}


get.node.resfname <- function(rank) {
  paste0(datadir ,sprintf("result -mpi -%04d.csv",rank)) }

#used for result storage
node.resfname <- get.node.resfname(comm.rank())


#prep the data frame for the collumns
res.cache <- NULL
res.cache.nrow <- 10000 
res.cache.idx <- 0

res.cache <- data.table(
  phenotype=rep("", res.cache.nrow),
  snp="", est=0.0, se=0.0, p_value=0.0, file = "")
res.cache.idx <- 0





#outer most loop over the snp files (male vs female)
for (snpfname in snpflist[[comm.rank()+1]]) {
  Dit <- fread(paste0(snpdir , snpfname))
  
  snp = as.data.frame(t(Dit[, -c(1:8)]));
  names(snp) = Dit$substanceBXH;
   
  
  
  #remove any cols with NA's
  snp <- snp[,!is.na(colSums(snp))]
  snp = data.frame(names(Dit)[-c(1:8)], snp)
  colnames(snp)[1] <- "Mice"
  
  #get names of genes
  snpnames <- setdiff(names(snp), "Mice")
  
  
  #create the knockoff variables for all snps in the file 
  #have to leave off the "Mice" identifier col though
  Xko <- create.second_order(as.matrix(snp[,2:199]))




  #Inner loop for each resonse variable phename
for (phename in phenames) {

  ##fix stuff
  #the particular phenotype
  resp <- data.frame(phetype$Mice, phetype[,phename, with = FALSE])
  colnames(resp) <- c("Mice", phename)
  data <- merge(resp, snp, by = "Mice")
  
  #collect the correct rows for knockoffs and for the general data
  kn_var <- Xko[!is.na(data[,2]),]
  data <- data[!is.na(data[,2]),]
 
  #Run the knockoff
  kn <- my_kn(as.matrix(data[,3:200]), y = data[,2], kn_var, offset = 0, fdr = .2)
  chosen <- kn$selected
  
  #Ugly function that runs a regression with the knockoff chosen variables
  if(length(chosen) == 0) {
    result <- t(rep(NA, 4))
    } else{
      result <- NULL
      stuff <- matrix(unlist(data[,names(chosen)]), nrow = dim(data)[1])
      colnames(stuff) <- names(chosen)
      model <- glm( data[,phename] ~ as.matrix(data[,chosen + 2]), family = gaussian(), data = data, na.action=na.exclude)
      
      result <- as.data.frame(summary(model)$coefficients)[-1,]
    }
  
  
    
    snpname <- names(chosen)
    if(length(chosen) == 0) {snpname <- "No Sig. Expl. Var."}
    

    #take results and put them in the res.cache
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

      fwrite(res.cache , file = paste0(node.resfname, snpfname, sep = "") , append=ifelse(file.exists(node.resfname),T,F))

}


