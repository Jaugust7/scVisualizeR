#' Generate new cds with UMAP coordinates in pData
#' 
#' @description Generate 2D UMAP coordinates based on count normalized expression matrix and append to CDS
#' 
#' @param cds a cell data set
#' @param nComp a vector consisting of the number of PCs to be used
#' 
#' @export
#' 
#' @return Returns a CDS with UMAP coordinates in pData
#' 
#' @examples
#' 

cordNormUMAP<-function (cds, nComp = NULL) 
{
  x <- cds
  tmp <- irlba::prcomp_irlba(t(scnorm(x[row.names(subset(fData(x), use_for_ordering == TRUE))])), n = tail(nComp, n = 1))
  tmp1 <- umap::umap(tmp$x[, nComp])
  pData(x)$UMAP1 <- tmp1$layout[, 1]
  pData(x)$UMAP2 <- tmp1$layout[, 2]
  return(x)
}