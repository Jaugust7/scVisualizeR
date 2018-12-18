#' Visualize PC variance
#' 
#' @param cds a cell data set
#' @param num_pcs the number of PCs you'd like to calculate
#' 
#' @export
#' 
#' @return Returns a screeplot with the amount of variance explained by each PC
#' 
#' @examples
#' 

PCpicker<-function(cds, num_pcs=NULL){
  tmp<-irlba::prcomp_irlba(log(t(as.matrix(Biobase::exprs(cds[row.names(subset(Biobase::fData(cds), use_for_ordering == TRUE))]))+1)), n = num_pcs)
  screeplot(tmp, npcs=num_pcs)
  }
