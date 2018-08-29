#' Generate a UMAP visualiztion using ggplot2
#' 
#' @param cds a cell data set
#' @param nComp a vector consisting of the number of PCs to be used
#' @param cell_color the parameter in pData you'd like to color your cells by
#' @param scale_color a vector of colors you'd like to use for your data set
#' 
#' @export
#' 
#' @return Returns a screeplot with the amount of variance explained by each PC
#' 
#' @examples
#' 

scUMAP<-function(cds, nComp=NULL, cell_color=NULL, scale_color=c("#377EB8", "#4DAF4A", "#E41A1C")){
  tmp<-umap(prcomp_irlba(log(t(exprs(cds[row.names(subset(fData(cds), use_for_ordering == TRUE))])+1)), n = tail(nComp, n=1))$x[,nComp]) 
  
  pData(cds)$UMAP1<-tmp$layout[,1]
  pData(cds)$UMAP2<-tmp$layout[,2]
  
  tmp2<-ggplot(pData(cds)) + geom_point(aes(x=UMAP1, y=UMAP2, color=pData(cds)$cell_color), size=1, alpha=0.5) + theme_bw() + scale_color_manual(values=scale_color) + labs(x="UMAP 1", y ="UMAP 2", color = "cell_color") + monocle:::monocle_theme_opts()
  return(tmp2) 
}

