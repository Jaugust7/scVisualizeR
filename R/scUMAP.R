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

scUMAP<-function(cds, nComp=NULL, cell_color="color", scale_color=c("#377EB8", "#4DAF4A", "#E41A1C")){
  x<-cds
  tmp<-prcomp_irlba(log(t(as.matrix(exprs(x[row.names(subset(fData(x), use_for_ordering == TRUE))]))+1)), n = tail(nComp, n=1))
  tmp1<-umap(tmp$x[,nComp]) 
  
  pData(x)$UMAP1<-tmp1$layout[,1]
  pData(x)$UMAP2<-tmp1$layout[,2]
  
  p<-ggplot(pData(x), aes(x=UMAP1, y=UMAP2))
  p + geom_point(aes_string(color=cell_color), size=1, alpha=0.5) +
    theme_bw() + 
    scale_color_manual(values=scale_color) + 
    labs(x="UMAP 1", y ="UMAP 2", color = "cell_color") + 
    monocle:::monocle_theme_opts()
}

