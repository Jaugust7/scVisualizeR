#' Given an exisiting UMAP embedding of scRNAseq data, returns UMAP embedding colored by the relative expression of marker genes of interest
#' 
#' @param cds a cell data set
#' @param markers a list of gene short names
#' @param logMode whether the log of expression data will be used
#' @param color_by choose a color for the cells expressing marker gene of interest
#' @param alpha determine the transparency of the points
#' @param size change the size of the points used
#' 
#' 
#' @export
#' 
#' @return Returns a facet wrapped UMAP embeddings of the same number as the number of marker genes chosen, colored by relative gene expression
#' 
#' @examples
#' 

myUMAPPlot<-function(cds,markers=NULL,logMode=T,color_by="Cluster", alpha = 0.5, shape_by=NULL,scaled=FALSE,size=1){
  tmp<-pData(cds)
  if(!is.null(markers)){
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
    p<-ggplot(tmp,aes(x=UMAP1,y=UMAP2))
    if(is.null(shape_by)){
      p + geom_point(aes_string(color=color_by), size=size, alpha=alpha) + facet_wrap('gene_short_name')+ theme_bw() + scale_color_brewer(palette="Set1") + monocle:::monocle_theme_opts() 
    }else{
      p + geom_point(aes_string(color=color_by, shape=shape_by),size=size, alpha=alpha) + facet_wrap('gene_short_name')+ theme_bw() + scale_color_brewer(palette="Set1")+ monocle:::monocle_theme_opts() 
    }
  }else{
    p<-ggplot(tmp,aes(x=UMAP1,y=UMAP2))
    if(is.null(shape_by)){
      p + geom_point(aes_string(color=color_by),size=size, alpha=alpha) + theme_bw() + scale_color_brewer(palette="Set1")+ monocle:::monocle_theme_opts() 
    }else{
      p + geom_point(aes_string(color=color_by,shape=shape_by),size=size, alpha=alpha) + theme_bw() + scale_color_brewer(palette="Set1")+ monocle:::monocle_theme_opts() 
    }
  }
}