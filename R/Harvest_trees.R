#' Collect trees from dependency directory
#' @param dependency.dir Directory with dependency files
#' @return data frame of stacked dependency files
#' @export
#' 
Harvest.trees <- function(dependency.dir){
  
  #collects all trees in dependency.dir
  
  dep.files <- list.files(dependency.dir,full.names=TRUE)
  
  list.deps <- lapply(dep.files,read.dependency)
  
  trees <- rbind.fill(list.deps)
  
  return(trees)
}
