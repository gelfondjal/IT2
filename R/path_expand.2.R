#' Swap / for \\ in path expand
#' @param x file path, could be relative or ~
#' @return Full filepath to x
#' @details See path.expand() in base R
path.expand.2 <- function(x){
  
  # Swap / for \\ in path expand
  
  temp <- path.expand(x)
  
  out <- gsub("\\\\","/",temp)
  
  return(out)
  
}