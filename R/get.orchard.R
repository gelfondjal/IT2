#' Returns the primary hub file with project location and id information
#' @return orchard
#' @export
#' 
get_orchard <- function(){
  
  # 
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv")  
  
  if(!file.exists(orchard.site)){
        
      
    return(NULL)
  }
  
  orchard <- read.csv(orchard.site,as.is=TRUE)  
  
  return(orchard)
}
