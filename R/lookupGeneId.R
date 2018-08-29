#' Returns the gene ID given a gene short name as input
#' 
#' @param eset an expression set or cell data set (CDS)
#' @param gene_names a list of gene short names
#' 
#' @export
#' 
#' @return Returns a vector of gene IDs
#' 
#' @examples
#' 

lookupGeneId<-function(eset,gene_names){
  res <- rownames(fData(eset))[fData(eset)$gene_short_name %in% gene_names]
  res <- c(res,rownames(fData(eset))[rownames(fData(eset)) %in% gene_names])
  res <- unique(res)
  res
}