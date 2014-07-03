#' Write object and capture file information
#' @param file.name file to write to the source "Result" directory
#' @param description describes object to write
#' @param write.fcn function for writing file of object type
#' @return File information list 
#' @export


Write <- function(obj=NULL,file.name="data.csv",description="Result file",write.fcn=guess.write.fcn(file.name),...){
  
  # lightweight Write.cap take small number of args
  # used file.name and description create file.information
  
  
  if(dirname(file.name)!="."){
    outpath <- file.path(source_info$results.dir,dirname(file.name))
    
  }else{outpath <- source_info$results.dir}
  
  
  outfile <- basename(file.name)
  
  file.info <- Create.file.info(outpath,outfile,description)
  
  write.obj <-Write.cap(obj,file.info,write.fcn,source_info,...)
  
  return(file.info)
  
}
