#' Generate a UMAP visualiztion using ggplot2
#' 
#' @description Generate a UMAP visualization from read depth normalized expression matrix
#' 
#' @param cds a cell data set
#' @param nComp a vector consisting of the number of PCs to be used
#' @param cell_color the parameter in pData you'd like to color your cells by
#' @param scale_color a vector of colors you'd like to use for your data set
#' 
#' @export
#' 
#' @return Returns a UMAP visualization colored by "cell_color" using the colors indicated in "scale_color"
#' 
#' @examples
#' 

plotUMAP<-function (cds, cell_color = "color", scale_color = c("#377EB8", "#4DAF4A", "#E41A1C")){
p <- ggplot(pData(cds), aes(x = UMAP1, y = UMAP2))
p + geom_point(aes_string(color = cell_color), size = 1, 
               alpha = 0.5) + theme_bw() + scale_color_manual(values = scale_color) + 
  labs(x = "UMAP 1", y = "UMAP 2", color = "cell_color") + 
  monocle:::monocle_theme_opts()
}