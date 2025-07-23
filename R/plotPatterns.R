#' Returns UMAP visualizations colored by pattern weight as determined by NNLM::nnmf()
#'
#' @param eset an expression set or cell data set (CDS)
#' @param decomp output from NNLM::nnmf()
#' @param nPatterns a vector of patterns that you'd like to visualize
#' @param x_cord the x cordinates to use for visualization ("UMAP1" by default)
#' @param y_cord the y cordinates to use for visualization ("UMAP2" by default)
#' @param size the size to use for the points
#'
#' @export
#'
#' @return Returns UMAP visualizations colored by pattern weight as determined by NNLM::nnmf()
#'
#'
#'

plotPatterns<-function (cds, decomp, nPatterns = c(1:5), x_cord = "UMAP1", y_cord = "UMAP2", size = 0.5){
  tmp <- monocle3::pData(cds) 
  tmp$UMAP1 <-SingleCellExperiment::reducedDims(cds)$UMAP[,1]
  tmp$UMAP2 <-SingleCellExperiment::reducedDims(cds)$UMAP[,2]
  tmp2<-t(as.data.frame(decomp$w))
  colnames(tmp2)<-pData(cds)$barcode
  patterns<-tmp2[nPatterns,]
  rownames(patterns)<-nPatterns
  patterns <- t(patterns)
  patterns <- reshape2::melt(patterns)
  colnames(patterns) <- c("barcode", "pattern_id", "value")
  tmp <- as.data.frame(left_join(as.data.frame(tmp), patterns))
  p <- ggplot2::ggplot(tmp, aes_string(x = "UMAP1", y = "UMAP2"))

  p + ggplot2::geom_point(aes(color = scale(value)), size = size) +
      ggplot2::facet_wrap("pattern_id") +
      ggplot2::theme_bw() + ggplot2::scale_color_gradient(low = "gray95",
      high = "darkblue") + monocle3:::monocle_theme_opts()
}
