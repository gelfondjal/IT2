#' Initializes dependency object source_info
#' @param source_info_arg is a source_info list with describing R script and project
#' @return Depedency file location
#' @export
#' 
initialize.dependency.info <- function(source_info_arg){
  
  # initialize dependency file R object, store filename
  # project.path is for git
  
  source.file.info <- source_info_arg$file
  project.path <- source_info_arg$project.path
  project.id <- source_info_arg$project.id
  dependency.path <- source_info_arg$dependency.dir
  
  dependency.file <- file.path(source_info_arg$dependency.dir,source_info_arg$dependency.file)  #file.path(dependency.path,paste(source.file.info[2],".txt",sep=""))
  
  
  
  dependency.out <- data.frame(source.file.path=source.file.info[["path"]],source.file=source.file.info[["file"]],source.file.description=source.file.info[["description"]],
                               source.run.time=as.POSIXct(Sys.time(),tz = Sys.timezone()),
                               path=dependency.path,project.path=project.path,project.id=project.id,
                               target.path=NA,
                               target.file=NA,
                               dependency=NA,
                               target.description=NA,
                               stringsAsFactors=FALSE)
  
  
  source_info_arg$dependency$update(dependency.out)
  
  #  write.dependency(dependency.out,dependency.file)	
  
  try({
    
    gitout <- git.init(project.path)
    
    setwd(source_info_arg$analysis.dir)
    
    git_binary_path <- git_path(NULL)
    if(source_info_arg$git.log){temp <- system2(git_binary_path,"log",stdout="")}else{
    temp <- system2(git_binary_path,"log",stdout=NULL)}
    
    if(temp==128){
      
      git.add(project.path,file.path(source.file.info[["path"]],source.file.info[["file"]]))	
      
      git.commit(project.path,"Intitialize git")
      
      print("Initialized git repo")
      
    }
    
    git.add(project.path,file.path(source.file.info[["path"]],source.file.info[["file"]]))	
    
  })#try get
  
  
  #  git.add(project.path,file.path(dependency.file))	
  
  
  
  # load the libraries and the dependent source functions
  
  
  load.install.library.file(file.path(source_info_arg$support.dir,source_info_arg$support.library.file))
  load.source.directory(source_info_arg$support.dir)
  
  support.files <- list.files(source_info_arg$support.dir,recursive=TRUE)
  
  
  print(dependency.file)
  
  #	return(NULL)	
  
  # add source files to dependency set was	
  
  for(file.name in support.files){
    
    try({ git.add(project.path,file.path(source_info_arg$support.dir,file.name)) })
    
    Read.cap(Create.file.info(source_info_arg$support.dir,file.name,"Support file"),I,source_info_arg)
    
  }		  
  
  
  
  return(dependency.file)
  
}	

