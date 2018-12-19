plot_tsne_meta_gradient <- function(tsne.df, meta, meta.col=NULL,show.legend=TRUE, cex=0.15, legend.size=5)
{
  tsne.df$meta = meta
  p=ggplot(tsne.df, aes(Lim1, Lim2)) + geom_point(aes(color=meta),size=cex)
  if(is.factor(meta)){
    if(is.null(meta.col)){
      meta.col = setNames(jet.colors(length(levels(meta))), levels(meta))
    }
    p = p+ scale_color_manual(values=as.vector(meta.col[levels(tsne.df$meta)]))
    p = p+ theme(panel.background=element_blank(),axis.line.x = element_line(colour = "black"),axis.line.y = element_line(colour = "black"))
  }
  else{
    p = p+ scale_color_gradient(low="blue",high="red", limits=c(0,1)) 
  }
  if(!show.legend){
    p = p + theme(legend.position="none") 
  }
  else{
    p = p + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
  }
  return(p)
}