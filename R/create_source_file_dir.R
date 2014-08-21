#' Create source file directories 
#' @param project.id is the project id name string
#' @param source.file is the filename of the source
#' @param description is the string description of what the source file does 
#' @param project.path is the home directory of the project
#' @param project.directory.tree is the list denoting the relative directories within the project
#' @param git.path path to git version control command
#' @return source_info list describing the project
#' @details Intializes git for the project, adds program git tracking, and initializes dependency tracking
#' @export
#' 
create.source.file.dir <- function(project.id="",source.file,source.description="",project.path=get.project.path(project.id),project.tree=project.directory.tree,
                                   
                                   git.path=NULL){
  require(devtools)
  require(plyr)
  require(igraph)
  require(digest)
  require(DBI)
  require(RSQLite)
  require(fields)
  require(R2HTML)
  require(gplots)
  require(rCharts)
  
  
  analysis.dir <- file.path(project.path,project.tree$analysis) # where the programs are
  data.dir <- file.path(project.path,project.tree$data)  # where the data are
  results.dir <- file.path(project.path,project.tree$results,source.file) # Standard output
  tex.dir <- file.path(results.dir,project.directory.tree$tex.dir) # Publication quality output
  dependency.dir <- file.path(project.path,project.tree$dependency.dir)
  support.dir <- file.path(project.path,project.tree$support)
  library.dir <- file.path(support.dir,project.tree$library.bank)
  apps.dir <- file.path(support.dir,"Apps")
  
  
  # Create necessary directories
  
  apply(matrix(c(analysis.dir,data.dir,results.dir,tex.dir,dependency.dir,support.dir,library.dir,apps.dir   )),1,dir.create,showWarnings=FALSE,recursive=TRUE)
  
  source.file.info <- Create.file.info(analysis.dir,source.file,description=source.description)	
  
  source_info <- list(analysis.dir=analysis.dir,data.dir=data.dir,tex.dir=tex.dir,results.dir=results.dir,support.dir = support.dir,library.dir=library.dir,
                      dependency.dir=dependency.dir,file=source.file.info,support.library.file="common_libs.csv")
  
  source_info$project.id <- project.id
  source_info$project.path <- project.path	
  
  try({
    not.this.source <- subset(Harvest.trees(dependency.dir),(source.file!=source_info$file[["file"]])&(!is.na(dependency)))
    if (nrow(not.this.source)){source_info$all.files<- Condense.file.info(not.this.source)}
  },silent=TRUE)
  
  source_info$dependency.file <- paste(source.file.info[2],".txt",sep="")	
  
  source_info$git.path <- git.path		
  
  # create depedency ref class instance
  
  source_info$dependency <- dependency(data= data.frame())
  
  initialize.dependency.info(source_info)
  
  #Start html markup tracking
  
  panderOptions("table.split.table",Inf)
  evalsOptions("cache.dir",source_info$tex.dir)
  
  source_info$report <- Pandoc$new()
  source_info$pandoc <- FALSE
  author <- ""
  try({
    git_binary_path <- git_path(NULL)
    author  <- system2(git_binary_path, paste("config --global user.name"),stdout=TRUE)
  })
  try(pandocinstalled <- source_info$report$export("test",open=FALSE))
  if(!exists("pandocinstalled")){
    return("Error: Pandoc is not installed on this computer")}else {
      source_info$pandoc <- TRUE
      source_info$report$title <- paste(source_info$project.id,source.file)
      source_info$report$author <-paste("IT2",author)
      
    }
  return(source_info)
  
}	
