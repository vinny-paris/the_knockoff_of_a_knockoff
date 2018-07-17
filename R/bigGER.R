#' Will expand or shirnk (2 dim) matrices or data frames for whatever purpose you want
#'
#'This function works in two steps. Firstly, it will either expand or shrink the colums randomly. If it expands the orinial columns will be untouched and left in the orginial order. If you are shrinking then the columns will disappear in a roandom order. 
#'
#'The second step is with the rows. These do not get copied identically lik eth ecolumns do becauase of the issues with singularities. Instead, inside of each column, we will randomly sample the lements with replacement and then use these randomly sampled elements as the new rows for the colun. The practical upshot being the new rows we create will be unique and have been generated with the data we have seen before but without have to worry about columns being perfect linear combinations of each other. Again as with the columns, if you exapnd the number of rows the the original rows will remain unchanged. If you shrink the rows we will randomly sample from the whole matrix/data.frame to get a smaller subsetted matrix.
#'
#'@export 
#'@param x A matrix or data.frame like object with two dimensions
#'@param width The number of columns of x you would like, or the "width" if you will
#'@param length The number of rows of x you would like, or the "length" if you would
#'@return A matrix with the same data as x of dimension "length" x "width"
#'@note Width and length will be rounded if they do not come in as natural numbers
#'

bigGER <- function(x, width, length){
  size <- dim(as.data.frame(x))
  
  #error messages
  if (length(size) != 2) {stop("This is only for a two diminensional object")}
  if (width <= 0) {stop("Width must be greater than 0! (that's an exclamation point to show anger, not factorial...)")}
  if (length <= 0) {stop("Length must be greater than 0! (that's an exclamation point to show anger, not factorial...)")}
  if (!is.numeric(c(width, length))) {stop("We need numbers for width, length")}
  
  #round to appopropriate values
  width <- round(width)
  length <- round(length)
  
  #growing the columns
  if (width > size[2]) {
    #find the new rows to take in
    excess <- width - size[2]
    
    #sample the colums and attach to original x
    sampled_cols <- sample(1:size[2], excess, replace = TRUE)
    new_cols <- c(1:size[2], sampled_cols)
    x_updated <- x[,new_cols]
    
    
    
    #For shrinking cols
  } else {
    
    #Sample the new number of cols, no replacement, at shrink 
    sampled_cols <- sample(1:size[2], width, replace = FALSE)
    x_updated <- x[, sort(sampled_cols)]
  
  }
  
  #For adding rows
  if (length > size[1]) {
    #find excess length, apply over colums to
    #randomly sample, add on to x
    excess <- length - size[1]
    new_rows <- apply(as.data.frame(x_updated), 2, sample, size = excess, replace = TRUE)
    x_updated <- rbind(x_updated, new_rows)
    
  } else {
    
    #Randomly showrtens the x matrix, doesn't "scramble"
    #the rows though
    new_rows <- sample(1:(size[1]), length, replace = FALSE)
    x_updated <- as.data.frame(x_updated)[new_rows, ]
  }
  
  return(x_updated)
}
    
    
    