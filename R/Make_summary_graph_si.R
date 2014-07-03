#' Returns summary graph of dependencies given source_info object
#' @param source_info Source information list
#' @param plot.graph logical indicating requet to make plot
#' @return graph of dependency relationships
#' @export
#' 
Make.summary.graph.si <- function(source_info,plot.graph=FALSE){
  
  
  project_info <- get.project.info(source_info$dependency.dir)	
  
  g.all <- project_info$graph
  
  suffixes <- gsub(".*\\.","",V(g.all)$name)		
  
  outputs <- c("tex","Robj","pdf")
  data.types <- c("csv","txt")
  sources <- c("R")
  
  file.class <- rbind(cbind("output",outputs),cbind("data",data.types),cbind("source",sources))
  file.type.df <- data.frame(file.type=file.class[,1],suffix=file.class[,2])
  
  shape.vector <- c("circle","star","square")
  names(shape.vector) <- c("data","source","output")
  
  
  file.type.df$shape <- shape.vector[as.character(file.type.df$file.type)]
  
  
  all.file.info <- project_info$all.files
  
  all.file.info$fullname.abbr <- file.path(all.file.info$path.abbr,all.file.info$file)
  
  all.file.info$time <- as.POSIXct(all.file.info$time)
  
  all.file.info$rank <- rank(all.file.info$tim)
  
  #V(g.all)$color <- file.type.df$color[match(suffixes,file.type.df$suffix)]
  
  V(g.all)$shape <- file.type.df$shape[match(suffixes,as.character(file.type.df$suffix))]
  
  V(g.all)$color <- bluered(nrow(all.file.info))[all.file.info$rank[match(V(g.all)$name,all.file.info$fullname.abbr)]]
  
  V(g.all)$mod.time <-  all.file.info$time[match(V(g.all)$name,all.file.info$fullname.abbr)]
  
  V(g.all)$run.time <-  as.POSIXct(all.file.info$source.run.time[match(V(g.all)$name,all.file.info$fullname.abbr)])
  
  V(g.all)$file.class <-  all.file.info$file.class[match(V(g.all)$name,all.file.info$fullname.abbr)]
  
  V(g.all)$file.hash <-  all.file.info$file.hash[match(V(g.all)$name,all.file.info$fullname.abbr)]
  
  V(g.all)$file <-  all.file.info$file[match(V(g.all)$name,all.file.info$fullname.abbr)]
  
  V(g.all)$path <-  all.file.info$path[match(V(g.all)$name,all.file.info$fullname.abbr)]
  
  
  V(g.all)$description <-  all.file.info$description[match(V(g.all)$name,all.file.info$fullname.abbr)]
  
  V(g.all)$shape <- ifelse(is.na(V(g.all)$shape),"circle",V(g.all)$shape)
  
  #plot(g.all,main="Dependencies",vertex.label=get.vertex.attribute(g.all,"name"))
  
  tree.layout <- layout.sugiyama(g.all,attributes="all")
  
  
  
  g2 <- tree.layout$extd_graph
  suffixes <-  gsub(".*\\.","",V(g2)$name)
  file.type.df <- rbind(file.type.df,data.frame(file.type="dummy",suffix="",shape="circle"))
  
  V(g2)$shape <- file.type.df$shape[match(suffixes,as.character(file.type.df$suffix))]
  
  V(g2)$shape <- ifelse(is.na(V(g2)$shape),"circle",V(g2)$shape)
  
  V(g2)$color <-  bluered(nrow(all.file.info))[all.file.info$rank[match(V(g2)$name,all.file.info$fullname.abbr)]]
  
  V(g2)$color <- ifelse(is.na(V(g2)$color),"black",V(g2)$color)
  
  g2$layout[,2] <- g2$layout[,2]+ifelse(!is.na(V(g2)$name),runif(length(g2$layout[,2]),min=-0.25,max=0.25),0)
  
  if(plot.graph){
    
    plot(g2,vertex.size=ifelse(!is.na(V(g2)$name),10,1),main="Analytical Dependencies",vertex.label.dist=0.25,vertex.label.color="black")
    
    par(mar=c(14,5,5,10))
    
    plot(all.file.info$rank,all.file.info$time,col=bluered(nrow(all.file.info))[all.file.info$rank],pch=19,main="File Modification Time",ylab="Modification Time",xlab="Rank")
    
    points(all.file.info$rank,all.file.info$time,pch=1)
    
    image.plot( zlim=c(1,max(all.file.info$rank)) ,add=TRUE,col=bluered(256),legend.only=TRUE,axes=FALSE,ylab="")
    
  }
  
  #legend("topright",legend=c("Older Files","New files"),pch=15,col=c("blue","red"))
  
  
  
  return(g.all)
  
}#Make.summary.graph.pi
