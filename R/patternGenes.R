#' Generate a list of data frames with ranked ordered genes from patterns found using NNLM::nnmf()
#'
#' @param eset an expression set or cell data set (CDS)
#' @param decomp output from NNLM::nnmf()
#' @param num_genes the number of genes per pattern you want to see
#'
#' @export
#'
#' @return Returns a list of data frames containing gene short names and gene weights for every patter
#'
#' @examples
#'

patternGenes<-function(eset, decomp, num_genes){
  colnames(decomp$W)<-1:length(decomp$W[1,]) #I'll remove this line later if need be
  lapply(c(1:length(colnames(decomp$W))), function(x){
  data.frame("gene_name" = lookupGeneName(eset, names(sort(decomp$W[,x], decreasing =TRUE)[1:num_genes])), "gene_weight" = sort(decomp$W[,x], decreasing = TRUE)[1:num_genes])})
}
