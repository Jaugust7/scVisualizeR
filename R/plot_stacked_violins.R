#' Returns a stacked violin plot of log2 normalized expression subsetted by items in pData. Compatible with ggplot2
#' 
#' @param cds a cell data set object
#' @param gene a vector of gene short names
#' @param group_by a character string indicating the column in pData used for grouping cells
#' @param assay_name a character string used to indicate which expression values to use from the Assay slot in the CDS
#' 
#' @export
#' 
#' @return Returns a stacked violin plot of normalized expression values subsetted by a category in pData
#' 
#' 
#' 
plot_stacked_violins<-function(cds = NULL, genes = c("NULL"), group_by = "cluster", assay_name = "counts" ){
  cds<-cds[monocle3::fData(cds)$gene_short_name %in% genes]
  features<-unique(monocle3::fData(cds)$gene_short_name)
  list<-lapply(features, function(x){
    tmp<-cds[monocle3::fData(cds)$gene_short_name == x,]
    data.frame("Cell" = colnames(tmp), "Idents" = as.factor(monocle3::pData(tmp)[[group_by]]), "Feat" = rep(x), "Expr" = exprs(tmp)[1,])
  })
  tmp<-do.call(rbind, list)
  tmp$Expr<-log2(tmp$Expr + 0.1)
  avg <- sapply(X = split(x = tmp, f = tmp$Idents),
                FUN = function(df) { return(tapply(X = df$Expr, INDEX = df$Feat, FUN = mean)) })
  L2Norm <- function(mat, MARGIN){
    normalized <- sweep(x = mat, MARGIN = MARGIN,
                        STATS = apply(X = mat, MARGIN = MARGIN,
                                      FUN = function(x){ sqrt(x = sum(x ^ 2)) }), FUN = "/")
    normalized[!is.finite(x = normalized)] <- 0
    return(normalized)
  }
  idents.order <- hclust(d = dist(t(L2Norm(mat = avg, MARGIN = 2))))$order
  avg <- avg[,idents.order]
  avg <- L2Norm(mat = avg, MARGIN = 1)
  mat <- stats::hclust(d = stats::dist(avg))$merge
  position <- apply(X = avg, MARGIN = 1, FUN = which.max)
  orderings <- list()
  for (i in 1:nrow(mat)) {
    x <- if (mat[i,1] < 0) -mat[i,1] else orderings[[mat[i,1]]]
    y <- if (mat[i,2] < 0) -mat[i,2] else orderings[[mat[i,2]]]
    x.pos <- min(x = position[x])
    y.pos <- min(x = position[y])
    orderings[[i]] <- if (x.pos < y.pos) { c(x, y) } else { c(y, x) }
  }
  features.order <- orderings[[length(orderings)]]
  tmp$Idents <- factor(tmp$Idents, levels = levels(tmp$Idents)[idents.order])
  tmp$Feat <- as.factor(tmp$Feat)
  tmp$Feat <- factor(tmp$Feat, levels = levels(tmp$Feat)[features.order])
  ggplot2::ggplot(tmp, aes(factor(Idents), Expr, fill = Idents)) +
    ggplot2::geom_violin(scale = "width", adjust = 1, trim = TRUE) +
    ggplot2::scale_y_continuous(expand = c(0, 0), position="right", labels = function(x)
      c(rep(x = "", times = length(x)-2), x[length(x) - 1], "")) +
    ggplot2::facet_grid(rows = vars(Feat), scales = "free", switch = "y") +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(legend.position = "none", panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = NA, color = "black"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          strip.text.y.left = element_text(angle = 0),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::xlab(group_by) + ylab("Expression Level")
}