% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myUMAPPlot.R
\name{myUMAPPlot}
\alias{myUMAPPlot}
\title{Given an exisiting UMAP embedding of scRNAseq data, returns UMAP embedding colored by the relative expression of marker genes of interest}
\usage{
myUMAPPlot(cds, markers = NULL, logMode = T, color_by = "Cluster",
  alpha = 0.5, shape_by = NULL, scaled = FALSE, size = 1)
}
\arguments{
\item{cds}{a cell data set}

\item{markers}{a list of gene short names}

\item{logMode}{choose to log10 transform gene expression counts with a pseudocount of 1}

\item{color_by}{choose a color for the cells expressing marker gene of interest}

\item{alpha}{determine the transparency of the points}

\item{size}{change the size of the points used}
}
\value{
Returns a facet wrapped UMAP embeddings of the same number as the number of marker genes chosen, colored by relative gene expression
}
\description{
Given an exisiting UMAP embedding of scRNAseq data, returns UMAP embedding colored by the relative expression of marker genes of interest
}
\examples{

}
