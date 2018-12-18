#' Normalize gene counts by sequencing depth per cell
#' 
#' @param cds a cell data set
#' 
#' @export
#' 
#' @return Returns an expression matrix that is the log of the normalized by sequencing depth converted to counts per 10K with a pseudocount of 1 
#' 
#' @examples
#' 

scnorm<-function(cds){
  log(((as.matrix(Biobase::exprs(cds))/Biobase::pData(cds)$Total_mRNAs)*10000) + 1)
}
