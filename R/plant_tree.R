#' initialize project
#' @param project.id Project name
#' @param project.path Project home directory
#' @param swap.directory Project branch exchange directory
#' @return logical for success or not
#' @export
plant.tree <- function(project.id,project.path,swap.directory){
  
  
  
  empty.orchard <- data.frame(project.id=project.id,project.path=project.path,swap.directory=swap.directory)
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv")	
  
  if(!file.exists(orchard.site)){plant.orchard()}
  
  all.orchards <- read.csv(file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv"),as.is=TRUE)
  
  if(project.id %in% all.orchards$project.id){
    
    print("Project Exists")
    
    return(FALSE)
  }else{
    
    
    orchards.old <- read.csv(orchard.site,as.is=TRUE)
    
    write.csv(rbind(orchards.old,empty.orchard),orchard.site,row.names=FALSE)
    
    dir.create(swap.directory)
    
    dir.create(project.path)
    
    return(TRUE)
    
  }
  
  
  
}

