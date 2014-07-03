#' Matches hash of target file to target file in dependency tree
#' @param tree Dependency data frame
#' @param file.for.hashing In filesystem to compare hash with dependency tree
#' @return Returns rows of tree that match file.for.hasing
#' @details Only for result files.
#' @export
#' 
Match.file.hash <- function(tree,file.for.hashing){
  
  # Identify the branch of the tree that owns the hash of file.hashed
  
  hash <- digest(file=file.for.hashing,serialize=FALSE)
  
  matched.leaves <- subset(tree,target.hash==hash)
  
  return(matched.leaves	)
  
  
  
}


