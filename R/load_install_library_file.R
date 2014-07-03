#' Loads librarys within the file library.list.file
#' @param library.data.file CSV File with a set of library names and repository locations
#' @return Library information data
#' @details Installs and loads all packages
#' @export
#' 
#' 
load.install.library.file <- function(library.data.file){
 
  
  packages.info <- read.csv(library.data.file,as.is=TRUE)
  
  
  
  
  for(library.iter in 1:nrow(packages.info)){
    
    
    packages.info$install.check[library.iter]  <- 	require(packages.info$Package[library.iter],character.only=TRUE)
    
    if(packages.info$install.check[library.iter]){
      library(packages.info$Package[library.iter],character.only=TRUE)
      print(paste(packages.info$Package[library.iter],"Installed, loaded"))
    }
  }
  
  # install biocounductor packages
  
  if(sum((packages.info$install.check==FALSE)&(packages.info$repos=="bioC"),na.rm=TRUE)){
    
    
    try({
      
      
      bioc.list <- subset(packages.info,(repos=="bioC")&(install.check==FALSE))$Package
      
      print(paste("Installing",bioc.list))
      
      
      source("http://bioconductor.org/biocLite.R")
      
      biocLite(bioc.list,ask=TRUE)
      lapply(bioc.list, require, character.only=TRUE)
      
      print(paste("Loaded",bioc.list))
      
      packages.info$install.check[packages.info$repos=="bioC"] <- TRUE
      
    })	
    
  }
  
  
  
  for(library.iter in 1:nrow(packages.info)){
    
    
    try({
      
      if(!packages.info$install.check[library.iter] ){
        
        print(paste("Installing",packages.info$Package[library.iter]))
        
        repository <- packages.info$repos[library.iter]
        
        repository <- ifelse(is.na(repository)|(repository==""),getOption("repos"),repository)
        
        install.packages(packages.info$Package[library.iter],repos=repository)
        
        library(packages.info$Package[library.iter],character=TRUE)		
        
        print(paste("Loaded",packages.info$Package[library.iter]))
        
        
        packages.info$install.check[library.iter] <- TRUE
      }
    })
    
  }
  
  return(packages.info)	
  
  
}
