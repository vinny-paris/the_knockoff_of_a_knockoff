pbd_kn <- function(dir){
  
  #get the file list for analysis to be ran on
  files <- list.files(dir, ...)
  
  
  #split the files among the nodes
  if (comm.size() > 1) {
    files <- split(files , cut(seq_along(files),
                                     comm.size()))
  } else  {
    files <- list(files)
  }
  
  
 
  #prep data frame for coming loop
  result <- NULL
  
  for (file in files[[comm.rank()+1]]) {
    #read in files
    resp <- fread(paste0(dir , file, "/resp"))
    expl <- fread(paste0(dir , file, "/expl"))
    
    #clean up the expl var
    expl <- expl[,!is.na(colSum(expl))]
    
    #create knockoff variables
    Xko <- create.second_order(expl)
    
    #do the actual analysis
    holding <- matrix_kn(resp, expl, Xko, ...)
    
    #appending which file it came from
    holding <- data.frame(holding, file)
    colnames(holding)[dim(holding)[2]] <- 'file'

    
    #get all results together on one data frame withen node
    result <- rbind(result, holding)
  }
  
  #get the results together between all nodes
  final <- reduce(result)
  
  #return the final product
  return(final)
}
