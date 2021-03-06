% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plot_biggraph_horizontal_si.R
\name{plot.biggraph.horizontal.si}
\alias{plot.biggraph.horizontal.si}
\title{Generates plot of large number of dependencies in relatively compart format}
\usage{
\method{plot}{biggraph.horizontal.si}(source_info, legend.tf = FALSE,
  title = "Dependencies", vertex.sizes = c(10, 0.5), vertex.dist = 0,
  label.cex = 0.75, xstretch = 1, legend.cex = 1)
}
\arguments{
\item{source_info}{Source information list}

\item{legend.tf}{Logical indicates whether to plot legend}

\item{title}{of graph}

\item{vertex.sizes}{of nodes representing files}

\item{vertex.dist}{Distance from vertex for labes}

\item{label.cex}{size of label text}

\item{legend.cex}{Size of legen font}

\item{xstreatch}{Aspect ratio}
}
\value{
list with graph element and graph made for DAG layout
}
\description{
Generates plot of large number of dependencies in relatively compart format
}

