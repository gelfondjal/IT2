#' Generates plot of large number of dependencies in relatively compart format
#' @param g.all Project igraph objet
#' @param legend.tf Logical indicates whether to plot legend
#' @param title of graph
#' @param vertex.sizes of nodes representing files
#' @param vertex.dist Distance from vertex for labes
#' @param label.cex size of label text
#' @param xstreatch Aspect ratio
#' @param legend.cex Size of legen font
#' @return list with graph element and graph made for DAG layout
#' @export

Plot.biggraph.horizontal <- function(g.all,legend.tf=FALSE,title="Dependencies",vertex.sizes=c(10,0.5),vertex.dist=0,label.cex=0.75,xstretch=1,legend.cex=1,black.vertex.list=NULL){
  
  # plots the depedency tree described by g.all
  # does not plot names
  
  g.all.2 <- g.all
  V(g.all.2)$name <- paste0(1:length(V(g.all.2)$name),".",(V(g.all.2)$file))
  V(g.all.2)$old.name <- V(g.all)$name
  V(g.all.2)$color <- "white"
  tree.layout <- layout.sugiyama(g.all.2,attributes="all")
  
  g2 <- tree.layout$extd_graph
  
  V(g2)$shape <- ifelse(is.na(V(g2)$shape),"circle",V(g2)$shape)
  
  
  g2$layout[,2:1] <- g2$layout[,1:2]
  
  g2$layout[,1] <- (max(g2$layout[,1])-g2$layout[,1])*xstretch
  
  if(!is.null(black.vertex.list)){
    
    V(g2)$color <- ifelse(V(g2)$old.name %in% black.vertex.list, "black" ,V(g2)$color)
    
    
  }
  
  plot(g2,vertex.size=ifelse(!is.na(V(g2)$name),vertex.sizes[1],vertex.sizes[2]),main=title,vertex.label.dist=vertex.dist,vertex.label.cex=label.cex,vertex.label.color="red")
  
  
  if(legend.tf){
    plot.new()
    
    legend("center",legend=paste(V(g.all.2)$name,V(g.all.2)$old.name),col=V(g.all.2)$color,pch=16,cex=legend.cex)
  }
  
  return(list(gout=g2,g.key=g.all.2))
  
}


Plot.biggraph.horizontal <- function(g.all,legend.tf=FALSE,title="Dependencies",vertex.sizes=c(10,0.5),vertex.dist=0,label.cex=0.75,xstretch=1,legend.cex=1,black.vertex.list=NULL){
  
  # plots the depedency tree described by g.all
  # does not plot names
  
  g.all.2 <- g.all
  V(g.all.2)$name <- paste0(1:length(V(g.all.2)$name),".",(V(g.all.2)$file))
  V(g.all.2)$old.name <- V(g.all)$name
  V(g.all.2)$color <- "white"
  tree.layout <- layout.sugiyama(g.all.2,attributes="all")
  
  g2 <- tree.layout$extd_graph
  
  V(g2)$shape <- ifelse(is.na(V(g2)$shape),"circle",V(g2)$shape)
  
  
  g2$layout[,2:1] <- g2$layout[,1:2]
  
  g2$layout[,1] <- (max(g2$layout[,1])-g2$layout[,1])*xstretch
  
  if(!is.null(black.vertex.list)){
    
    V(g2)$color <- ifelse(V(g2)$old.name %in% black.vertex.list, "black" ,V(g2)$color)
    
    
  }
  
  plot(g2,vertex.size=ifelse(!is.na(V(g2)$name),vertex.sizes[1],vertex.sizes[2]),main=title,vertex.label.dist=vertex.dist,vertex.label.cex=label.cex,vertex.label.color="red")
  
  
  if(legend.tf){
    plot.new()
    
    legend("center",legend=paste(V(g.all.2)$name,V(g.all.2)$old.name),col=V(g.all.2)$color,pch=16,cex=legend.cex)
  }
  
  return(list(gout=g2,g.key=g.all.2))
  
}





Plot.biggraph.horizontal <- function(g.all,legend.tf=FALSE,title="Dependencies",vertex.sizes=c(10,0.5),vertex.dist=0,label.cex=0.75,xstretch=1,legend.cex=1,black.vertex.list=NULL){
  
  # plots the depedency tree described by g.all
  # does not plot names
  
  g.all.2 <- g.all
  V(g.all.2)$name <- paste0(1:length(V(g.all.2)$name),".",(V(g.all.2)$file))
  V(g.all.2)$old.name <- V(g.all)$name
  V(g.all.2)$color <- "white"
  tree.layout <- layout.sugiyama(g.all.2,attributes="all")
  
  g2 <- tree.layout$extd_graph
  
  V(g2)$shape <- ifelse(is.na(V(g2)$shape),"circle",V(g2)$shape)
  
  
  g2$layout[,2:1] <- g2$layout[,1:2]
  
  g2$layout[,1] <- (max(g2$layout[,1])-g2$layout[,1])*xstretch
  
  if(!is.null(black.vertex.list)){
    
    V(g2)$color <- ifelse(V(g2)$old.name %in% black.vertex.list, "black" ,V(g2)$color)
    
    
  }
  
  plot(g2,vertex.size=ifelse(!is.na(V(g2)$name),vertex.sizes[1],vertex.sizes[2]),main=title,vertex.label.dist=vertex.dist,vertex.label.cex=label.cex,vertex.label.color="red")
  
  
  if(legend.tf){
    plot.new()
    
    legend("center",legend=paste(V(g.all.2)$name,V(g.all.2)$old.name),col=V(g.all.2)$color,pch=16,cex=legend.cex)
  }
  
  return(list(gout=g2,g.key=g.all.2))
  
}


Plot.biggraph.horizontal <- function(g.all,legend.tf=FALSE,title="Dependencies",vertex.sizes=c(10,0.5),vertex.dist=0,label.cex=0.75,xstretch=1,legend.cex=1,black.vertex.list=NULL){
  
  # plots the depedency tree described by g.all
  # does not plot names
  
  g.all.2 <- g.all
  V(g.all.2)$name <- paste0(1:length(V(g.all.2)$name),".",(V(g.all.2)$file))
  V(g.all.2)$old.name <- V(g.all)$name
  V(g.all.2)$color <- "white"
  tree.layout <- layout.sugiyama(g.all.2,attributes="all")
  
  g2 <- tree.layout$extd_graph
  
  V(g2)$shape <- ifelse(is.na(V(g2)$shape),"circle",V(g2)$shape)
  
  
  g2$layout[,2:1] <- g2$layout[,1:2]
  
  g2$layout[,1] <- (max(g2$layout[,1])-g2$layout[,1])*xstretch
  
  if(!is.null(black.vertex.list)){
    
    V(g2)$color <- ifelse(V(g2)$old.name %in% black.vertex.list, "black" ,V(g2)$color)
    
    
  }
  
  plot(g2,vertex.size=ifelse(!is.na(V(g2)$name),vertex.sizes[1],vertex.sizes[2]),main=title,vertex.label.dist=vertex.dist,vertex.label.cex=label.cex,vertex.label.color="red")
  
  
  if(legend.tf){
    plot.new()
    
    legend("center",legend=paste(V(g.all.2)$name,V(g.all.2)$old.name),col=V(g.all.2)$color,pch=16,cex=legend.cex)
  }
  
  return(list(gout=g2,g.key=g.all.2))
  
}


Plot.biggraph.horizontal <- function(g.all,legend.tf=FALSE,title="Dependencies",vertex.sizes=c(10,0.5),vertex.dist=0,label.cex=0.75,xstretch=1,legend.cex=1,black.vertex.list=NULL){
  
  # plots the depedency tree described by g.all
  # does not plot names
  
  g.all.2 <- g.all
  V(g.all.2)$name <- paste0(1:length(V(g.all.2)$name),".",(V(g.all.2)$file))
  V(g.all.2)$old.name <- V(g.all)$name
  V(g.all.2)$color <- "white"
  tree.layout <- layout.sugiyama(g.all.2,attributes="all")
  
  g2 <- tree.layout$extd_graph
  
  V(g2)$shape <- ifelse(is.na(V(g2)$shape),"circle",V(g2)$shape)
  
  
  g2$layout[,2:1] <- g2$layout[,1:2]
  
  g2$layout[,1] <- (max(g2$layout[,1])-g2$layout[,1])*xstretch
  
  if(!is.null(black.vertex.list)){
    
    V(g2)$color <- ifelse(V(g2)$old.name %in% black.vertex.list, "black" ,V(g2)$color)
    
    
  }
  
  plot(g2,vertex.size=ifelse(!is.na(V(g2)$name),vertex.sizes[1],vertex.sizes[2]),main=title,vertex.label.dist=vertex.dist,vertex.label.cex=label.cex,vertex.label.color="red")
  
  
  if(legend.tf){
    plot.new()
    
    legend("center",legend=paste(V(g.all.2)$name,V(g.all.2)$old.name),col=V(g.all.2)$color,pch=16,cex=legend.cex)
  }
  
  return(list(gout=g2,g.key=g.all.2))
  
}


Plot.biggraph.horizontal <- function(g.all,legend.tf=FALSE,title="Dependencies",vertex.sizes=c(10,0.5),vertex.dist=0,label.cex=0.75,xstretch=1,legend.cex=1,black.vertex.list=NULL){
  
  # plots the depedency tree described by g.all
  # does not plot names
  
  g.all.2 <- g.all
  V(g.all.2)$name <- paste0(1:length(V(g.all.2)$name),".",(V(g.all.2)$file))
  V(g.all.2)$old.name <- V(g.all)$name
  V(g.all.2)$color <- "white"
  tree.layout <- layout.sugiyama(g.all.2,attributes="all")
  
  g2 <- tree.layout$extd_graph
  
  V(g2)$shape <- ifelse(is.na(V(g2)$shape),"circle",V(g2)$shape)
  
  
  g2$layout[,2:1] <- g2$layout[,1:2]
  
  g2$layout[,1] <- (max(g2$layout[,1])-g2$layout[,1])*xstretch
  
  if(!is.null(black.vertex.list)){
    
    V(g2)$color <- ifelse(V(g2)$old.name %in% black.vertex.list, "black" ,V(g2)$color)
    
    
  }
  
  plot(g2,vertex.size=ifelse(!is.na(V(g2)$name),vertex.sizes[1],vertex.sizes[2]),main=title,vertex.label.dist=vertex.dist,vertex.label.cex=label.cex,vertex.label.color="red")
  
  
  if(legend.tf){
    plot.new()
    
    legend("center",legend=paste(V(g.all.2)$name,V(g.all.2)$old.name),col=V(g.all.2)$color,pch=16,cex=legend.cex)
  }
  
  return(list(gout=g2,g.key=g.all.2))
  
}

