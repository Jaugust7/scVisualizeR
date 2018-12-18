#' Generate new cds with UMAP coordinates in pData
#' 
#' @description Generate a 2D UMAP coordinates and append to CDS
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

cordUMAP<-function(cds, nComp=NULL){
  x<-cds
  tmp<-irlba::prcomp_irlba(log(t(as.matrix(exprs(x[row.names(subset(fData(x), use_for_ordering == TRUE))]))+1)), n = tail(nComp, n=1))
  tmp1<-umap::umap(tmp$x[,nComp]) 
  
  pData(x)$UMAP1<-tmp1$layout[,1]
  pData(x)$UMAP2<-tmp1$layout[,2]
  return(x)
}