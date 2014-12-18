#' Collect trees from dependency directory
#' @param dependency.dir Directory with dependency files
#' @return data frame of stacked dependency files
#' @export
#' 
Harvest.trees <- function(dependency.dir){
  
  #collects all trees in dependency.dir
  
  dep.files <- list.files(dependency.dir,full.names=TRUE)
  
  trees <- NULL
  
  if(length(dep.files)>0){
    
  list.deps <- lapply(dep.files,read.dependency)
  
  trees <- rbind.fill(list.deps)
    
  project.id <-  list.deps[[1]]$project.id[1] 
    
  old.project.path <- unique(trees$project.path)[1]
  
  
  new.path <- get.project.path(project.id)
  
  if((old.project.path!=new.path)|(length(old.project.path>1))){
      
    shaved.variables <- c("path","source.file.path","target.path","project.path")
  
    for(char.shave in shaved.variables){
      
      for(file.iter in 1:nrow(trees)){
        
        old.project.path <- trees$project.path[file.iter]   
        
        trees[[char.shave]][file.iter] <- gsub(old.project.path,"",trees[[char.shave]][file.iter],fixed=TRUE)
        
        trees[[char.shave]][file.iter] <- paste0(new.path,trees[[char.shave]][file.iter])
        
      }
      
    }
    
    
    
    
  } # if project path mismatch
  
  
  } # if there exist dependency files
  
  
  return(trees)
}
