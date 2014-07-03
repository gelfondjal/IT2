#' Get project dependency database
#' @param project.id
#' @details Assumes project.id is the name of a database.
#' @return data.frame dump of database
get.project.dependency.db <- function(project.id){
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname=project.id)
  res <- dbSendQuery(con, paste("SELECT * FROM",project.id))
  db.dependency <- fetch(res,n=1e5)
  
  return(db.dependency)
  
} # END: get.project.dependency.db