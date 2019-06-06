Logcounts_DimRed <- function(geneName, DimRed, SingleCellExperiment_object) {
   stopifnot(!missing(SingleCellExperiment_object))
   stopifnot(!missing(DimRed))
   
   
   GeneExp <- logcounts(sce)[geneName,]
   Cluster <- as.factor(SingleCellExperiment_Object@colData$Cluster)
   Sample <- as.factor(SingleCellExperiment_Object@colData$Sample)
   
   if(DimRed == "UMAP"){
      as.tibble(SingleCellExperiment_Object@reducedDims$UMAP) %>%
         dplyr::mutate("Gene Expression" = GeneExp) %>% 
         dplyr::mutate("Sample" = Sample) %>%
         ggplot(aes(V1, V2, color=GeneExp)) +
         geom_point(size=0.25,alpha=0.75,aes(colour = GeneExp)) +
         #scale_colour_gradientn(colours=terrain.colors(7)) +
         scale_color_gradientn(colours=c("gray75", "red3")) +
         xlab("") + ylab("") + 
         ggtitle(geneName) +
         coord_fixed() +
         theme_classic(base_size=14) +
         theme(strip.background = element_blank(),
               #strip.text.x     = element_blank(),
               strip.text.x     = element_text(size=18),
               axis.text.x      = element_blank(),
               axis.text.y      = element_blank(),
               axis.ticks       = element_blank(),
               axis.line        = element_blank(),
               #panel.border     = element_blank(),
               #legend.key.size = unit(1,"line"),
               legend.text=element_text(color=Sample,size=12),
               legend.title = element_text(size = 18),
               plot.title = element_text(size = 18),
               panel.border     = element_rect(colour = "gray50", fill=NA, size=0.25),
               panel.spacing.x=unit(0, "lines"), 
               panel.spacing.y=unit(0,"lines"),
               #legend.position = ("none")) +
               panel.background =  element_blank(), 
               panel.grid.major =  element_blank(),
               panel.grid.minor =  element_blank()) +
         #scale_color_manual(values = c30) +
         
         facet_wrap(Sample, ncol = 2, nrow = 2)
      
   } else if (DimRed == "TSNE") {
      as.tibble(SingleCellExperiment_Object@reducedDims$TSNE) %>%
         dplyr::mutate("Gene Expression" = GeneExp) %>% 
         dplyr::mutate("Sample" = Sample) %>%
         ggplot(aes(V1, V2, color=GeneExp)) +
         geom_point(size=0.25,alpha=0.75,aes(colour = GeneExp)) +
         #scale_colour_gradientn(colours=terrain.colors(7)) +
         scale_color_gradientn(colours=c("gray75", "red3")) +
         xlab("") + ylab("") + 
         ggtitle(geneName) +
         coord_fixed() +
         theme_classic(base_size=14) +
         theme(strip.background = element_blank(),
               #strip.text.x     = element_blank(),
               strip.text.x     = element_text(size=18),
               axis.text.x      = element_blank(),
               axis.text.y      = element_blank(),
               axis.ticks       = element_blank(),
               axis.line        = element_blank(),
               #panel.border     = element_blank(),
               #legend.key.size = unit(1,"line"),
               legend.text=element_text(color=Sample,size=12),
               legend.title = element_text(size = 18),
               plot.title = element_text(size = 18),
               panel.border     = element_rect(colour = "gray50", fill=NA, size=0.25),
               panel.spacing.x=unit(0, "lines"), 
               panel.spacing.y=unit(0,"lines"),
               #legend.position = ("none")) +
               panel.background =  element_blank(), 
               panel.grid.major =  element_blank(),
               panel.grid.minor =  element_blank()) +
         #scale_color_manual(values = c30) +
         
         facet_wrap(Sample, ncol = 2, nrow = 2)
      
   } else if (DimRed == "PCA") {
      as.tibble(SingleCellExperiment_Object@reducedDims$PCA) %>%
         dplyr::mutate("Gene Expression" = GeneExp) %>% 
         dplyr::mutate("Sample" = Sample) %>%
         ggplot(aes(PC1, PC2, color=GeneExp)) +
         geom_point(size=0.25,alpha=0.75,aes(colour = GeneExp)) +
         #scale_colour_gradientn(colours=terrain.colors(7)) +
         scale_color_gradientn(colours=c("gray75", "red3")) +
         xlab("") + ylab("") + 
         ggtitle(geneName) +
         coord_fixed() +
         theme_classic(base_size=14) +
         theme(strip.background = element_blank(),
               #strip.text.x     = element_blank(),
               strip.text.x     = element_text(size=18),
               axis.text.x      = element_blank(),
               axis.text.y      = element_blank(),
               axis.ticks       = element_blank(),
               axis.line        = element_blank(),
               #panel.border     = element_blank(),
               #legend.key.size = unit(1,"line"),
               legend.text=element_text(color=Sample,size=12),
               legend.title = element_text(size = 18),
               plot.title = element_text(size = 18),
               panel.border     = element_rect(colour = "gray50", fill=NA, size=0.25),
               panel.spacing.x=unit(0, "lines"), 
               panel.spacing.y=unit(0,"lines"),
               #legend.position = ("none")) +
               panel.background =  element_blank(), 
               panel.grid.major =  element_blank(),
               panel.grid.minor =  element_blank()) +
         #scale_color_manual(values = c30) +
         
         facet_wrap(Sample, ncol = 2, nrow = 2)
   } else {
      print("DimRed error have you placed the option in quotes? Options include TSNE, UMAP, PCA")
   }
   
}
