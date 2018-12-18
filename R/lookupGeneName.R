#' Returns the gene short name given a gene ID as input
#' 
#' @param eset an expression set or cell data set (CDS)
#' @param gene_id a list of gene IDs
#' 
#' @export
#' 
#' @return Returns a vector of gene short names
#' 
#' @examples
#' 

lookupGeneName<-function(eset,gene_id){
  res <-Biobase::fData(eset[gene_id,])$gene_short_name
  res <- unique(res)
  res
}