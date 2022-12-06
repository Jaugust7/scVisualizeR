#' Returns the gene ID given a gene short name as input
#' 
#' @param cds an expression set or cell data set (CDS)
#' @param gene_names a list of gene short names
#' 
#' @export
#' 
#' @return Returns a vector of gene IDs
#' 
#' 
#' 

lookupGeneId<-function(cds,gene_names){
  res <- rownames(monocle3::fData(cds))[monocle3::fData(cds)$gene_short_name %in% gene_names]
  res <- c(res,rownames(monocle3::fData(cds))[rownames(monocle3::fData(cds)) %in% gene_names])
  res <- unique(res)
  res
}