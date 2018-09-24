#' Given an exisiting UMAP embedding of scRNAseq data, returns UMAP embedding colored by the relative expression of marker gene of interest
#' 
#' @param cds a cell data set
#' @param markers a list of gene short names
#' @param logMode whether the log of expression data will be used
#' @param x the x coordinates that you would like to use
#' @param y the y coordinates that you would like to use 
#'
#' @export
#' 
#' @return Returns a facet wrapped UMAP embeddings of the same number as the number of marker genes chosen, colored by relative gene expression
#' 
#' @examples
#' 

myUMAPPlotMarkers<-function(cds,markers=NULL,logMode=T,shape_by=NULL,scaled=FALSE, x_cord="UMAP1", y_cord="UMAP2"){
  tmp<-pData(cds)
  genes<-as.matrix(exprs(cds[rownames(fData(cds)) %in% lookupGeneId(cds,markers)]))
  if(logMode){
    genes<-log10(genes+1)
  }
  geneMeans<-rowMax(genes)
  if(scaled){
    genes<-genes/geneMeans
  }
  genes<-t(genes)
  genes<-melt(genes)
  colnames(genes)<-c("cell_id","gene_id","value")
  genes<-merge(genes,fData(cds),by.x="gene_id",by.y="gene_id",all.x=TRUE,sort=FALSE)
  #print(head(genes))
  tmp<-merge(tmp,genes,by.x=0,by.y="cell_id")
  #print(head(tmp))
  p<-ggplot(tmp,aes_string(x=x_cord,y=y_cord))
  if(is.null(shape_by)){
    p + geom_point(aes_string(color="value"),size=0.5) + facet_wrap('gene_short_name')+ theme_bw() + scale_color_viridis() + monocle:::monocle_theme_opts() 
  }else{
    p + geom_point(aes_string(color="value",shape=shape_by),size=0.5) + facet_wrap('gene_short_name')+ theme_bw() + scale_color_viridis()+ monocle:::monocle_theme_opts() 
  }
}