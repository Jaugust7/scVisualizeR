% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patternGenes.R
\name{patternGenes}
\alias{patternGenes}
\title{Generate a list of data frames with ranked ordered genes from patterns found using NNLM::nnmf()}
\usage{
patternGenes(eset, decomp, num_genes)
}
\arguments{
\item{eset}{an expression set or cell data set (CDS)}

\item{decomp}{output from NNLM::nnmf()}

\item{num_genes}{the number of genes per pattern you want to see}
}
\value{
Returns a list of data frames containing gene short names and gene weights for every patter
}
\description{
Generate a list of data frames with ranked ordered genes from patterns found using NNLM::nnmf()
}
