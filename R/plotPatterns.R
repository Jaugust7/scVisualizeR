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

plotPatterns<-function (cds, decomp, nPatterns = c(1:5), x_cord = "UMAP1", y_cord = "UMAP2", size = 0.5)
{
  tmp <- monocle3::pData(eset)
  tmp2<-as.data.frame(decomp$H)
  colnames(tmp2)<-pData(eset)$cell_id
  patterns<-tmp2[nPatterns,]
  rownames(patterns)<-nPatterns
  patterns <- t(patterns)
  patterns <- reshape2::melt(patterns)
  colnames(patterns) <- c("cell_id", "pattern_id", "value")
  tmp <- merge(tmp, patterns, by.x = 0, by.y = "cell_id")
  p <- ggplot2::ggplot(tmp, aes_string(x = "UMAP1", y = "UMAP2"))

  p + ggplot2::geom_point(aes_string(color = "value")) +
      ggplot2::facet_wrap("pattern_id") +
      ggplot2::theme_bw() + ggplot2::scale_color_gradient(low = "gray95",
      high = "darkblue") + monocle:::monocle_theme_opts()
    }
