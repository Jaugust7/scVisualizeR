#' Given an exisiting UMAP embedding of scRNAseq data, returns UMAP embedding colored by the relative expression of marker gene of interest
#' 
#' @param cds a cell data set
#' @param markers a list of gene short names
#' @param logMode choose to log10 transform gene expression counts with a pseudocount of 1
#' @param x the x coordinates that you would like to use
#' @param y the y coordinates that you would like to use 
#' @param size adjust the size of the points used
#'
#' @export
#' 
#' @return Returns a facet wrapped UMAP embeddings of the same number as the number of marker genes chosen, colored by relative gene expression
#' 
#' @examples
#' 

myUMAPPlotMarkers<-function(cds,markers=NULL,logMode=T,shape_by=NULL,scaled=FALSE, x_cord="UMAP1", y_cord="UMAP2", size=0.75){
  tmp<-Biobase::pData(cds)
  genes<-as.matrix(Biobase::exprs(cds[rownames(Biobase::fData(cds)) %in% lookupGeneId(cds,markers)]))
  if(logMode){
    genes<-log10(genes+1)
  }
  geneMeans<-rowMax(genes)
  if(scaled){
    genes<-genes/geneMeans
  }
  genes<-t(genes)
  genes<-reshape2::melt(genes)
  colnames(genes)<-c("cell_id","gene_id","value")
  genes<-merge(genes,Biobase::fData(cds),by.x="gene_id",by.y="gene_id",all.x=TRUE,sort=FALSE)
  #print(head(genes))
  tmp<-merge(tmp,genes,by.x=0,by.y="cell_id")
  #print(head(tmp))
  p<-ggplot2::ggplot(tmp,aes_string(x=x_cord,y=y_cord))
  if(is.null(shape_by)){
    p + ggplot2::geom_point(aes_string(color="value"),size=size) + ggplot2::facet_wrap('gene_short_name')+ ggplot2::theme_bw() + ggplot2::scale_color_gradient(low="gray95", high="red") + monocle:::monocle_theme_opts() 
  }else{
    p + ggplot2::geom_point(aes_string(color="value",shape=shape_by),size=size) + ggplot2::facet_wrap('gene_short_name')+ ggplot2::theme_bw() + ggplot2::scale_color_gradient(low="gray95", high="red") + monocle:::monocle_theme_opts() 
  }
}