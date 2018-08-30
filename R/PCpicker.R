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
  tmp<-prcomp_irlba(log(t(as.matrix(exprs(cds[row.names(subset(fData(cds), use_for_ordering == TRUE))]))+1)), n = num_pcs)
  screeplot(tmp, npcs=num_pcs)
  }
