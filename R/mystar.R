#' Star function for igraph plotting
#' @param coords coordinates for plotting
#' @param v vertex
#' @param params graphics parameters
#' @return nothing returned
#' @export

mystar <- function(coords, v=NULL, params) {
 # library(igraph)
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }
  mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
         FUN=function(x, y, bg, size, nor) {
           symbols(x=x, y=y, bg=bg,
                   stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
                   add=TRUE, inches=FALSE)
         })
  
  return()
}


# no clipping, edges will be below the vertices anyway
#igraph::add.vertex.shape("star", clip=igraph::igraph.shape.noclip,
#                 plot=mystar, parameters=list(vertex.norays=5))

