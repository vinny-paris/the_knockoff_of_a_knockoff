#'Access to pbdMPI in use of matrix_kn among mulitple files. 
#'
#'Major thing is that we are assuming we have a directory with any number of folders. 
#'Inside each of these folders there needs to be two files,
#'one called "resp" with the response variables and one called 
#'"expl" for the explanatory variables. The folders will be divided among
#'the nodes and each node will do the analysis of it's folders,
#'recombining them at the end. 
#'
#'@export
#'@param dir This is the directory where the folders (AND ONLY THE FOLDERS) are located
#'@param ... Any other argument to be passed to the matrix_kn function, and in turn my_kn
#'@return One large data frame of 6 columns 
#'\itemize{
#'\item{Resp: } What ever response variable was being tested
#'\item{Expl: } Gives the name of the signficant explanatory variable chosen
#'\item{Est: } Estimate of the glm model, defaults to normal
#'\item{se: } Gives the standard error of the parameter estimate from the glm
#'\item{p-value: } Gives the p-value associated with that variable estimate from the glm for the null hypothesis the effect magnitude is 0.
#'\item{file: } Which file the data was used from
#'}




pbd_kn <- function(dir, ...){
  
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
    resp <- fread(paste0(dir , file, "/resp.csv"))
    expl <- fread(paste0(dir , file, "/expl.csv"))
    
    #clean up the expl var
    expl <- expl[,c(TRUE, !is.na(colSums(expl[,-1])))]
    
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
