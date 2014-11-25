#' Matches hash of target file to target file in depenency tree
#' @param git.dir git directory
#' @param file.for.hashing in filesystem to compare hash with git tracked files
#' @return git log search results
#' @details Uses git.history.search
#' @export
#' 
Match.file.hash.history <- function(git.dir,file.for.hashing){
  
  # Identify the branch of the tree that owns the hash of file.hashed
  
  hash <- Digest(file=file.for.hashing,serialize=FALSE)
  
  matched.commits <- git.history.search(project.dir,hash)
  
  return(matched.commits	)
  
  
  
}
