#' Copy dependent programs from swap directory into Program directory
#' @details Runs as test and to create dependency files
#' @param branch_name Name of directory containing branch data to graft
#' @param run logical Run scripts or not
#' @param project.id Name of project to locate swap and project home directory
#' @param start.up = TRUE create program directory
#' @param overwriteTF logical Overwrite existing programs or not
#' @return Branch information
#' @export
#' 
graft.branch <- function(branch_name,run=TRUE,start.up=FALSE,project.id=NULL,overwriteTF=FALSE){
  
  # copy dependent programs into Program directory
  
  # Runs as test and to create dependency files
  # start.up = TRUE create program directory
  
  if(start.up){
    
    analysis.dir <- file.path(get.project.path(project.id),project.directory.tree$analysis)
    support.dir <- file.path(get.project.path(project.id),project.directory.tree$support)
    dependency.dir <- file.path(get.project.path(project.id),project.directory.tree$dependency.dir)
    project.dir <- get.project.path(project.id)
    sapply(c(analysis.dir,support.dir,dependency.dir),dir.create,showWarnings=FALSE)
    
    
  }else{
    project.id <- source_info$project.id
    analysis.dir <- source_info$analysis.dir
    dependency.dir <- source_info$dependency.dir
    support.dir <- source_info$support.dir
    project.dir <- source_info$project.path
  }
  
  swap.directory <- get.project.swap.directory(project.id)
  
  
  branch.dir <- file.path(swap.directory,"Branches",branch_name)
  
  
  prog.dir <-file.path(branch.dir,"Programs")
  dep.dir <- file.path(branch.dir,"Dependency")
  sup.dir <- file.path(branch.dir,"support_functions_dir")
  
  
  branch.info.out <- read.table(file.path(branch.dir,paste0(branch_name,".txt")),as.is=TRUE)
  
  program.paths <- file.path(prog.dir,branch.info.out$branch.names)
  
  
  dir.create(analysis.dir,showWarnings=FALSE)
  dir.create(support.dir,showWarnings=FALSE)
  dir.create(dependency.dir,showWarnings=FALSE)
  dir.create(project.dir,showWarnings=FALSE)
  
  supports <- list.files(sup.dir,full.names=TRUE,recursive=TRUE)	
  supports <- supports[(!file.info(supports)$size<1)&(basename(supports)!="Icon\r")]
  
  
  file.copy(supports,file.path(analysis.dir,gsub(sup.dir,"",supports,fixed=TRUE)),recursive=TRUE,overwrite=overwriteTF)
  
  
  
  deps <- file.path(dep.dir,list.files(dep.dir))
  deps <- grep("\\.txt$",deps,value=TRUE)
  
  file.copy(deps,file.path(dependency.dir,basename(deps)),overwrite=overwriteTF)
  
  program.paths <- program.paths[grepl("r$|R$",program.paths)]	
  
  rework.project.path(dependency.dir,new.path=project.dir)
  
  file.copy(program.paths,file.path(analysis.dir,basename(program.paths)),overwrite=TRUE)
  
  rework.project.path(dependency.dir,new.path=project.dir)
  
  for(prog.iter in file.path(analysis.dir,branch.info.out$branch.names)){
    
    if(run){clean_source(prog.iter)}
    
  }
  
  
  
  return(branch.info.out)
}
