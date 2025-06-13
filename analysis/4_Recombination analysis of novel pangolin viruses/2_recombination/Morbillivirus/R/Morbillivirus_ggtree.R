library(ggtree)
library(ape)
library(dplyr)
library(ggplot2)
library(treeio)
library(gridExtra)

rm(list=ls())


minor_parent <- c("UN_MBA191024-Paramyxoviridae-1")
major_parent <- c("JN896331-dog")
recombination <- c("MF926599-Dog")


tree_before <- read.newick("before.nwk", node.lab="support")
tree_before@data$support <- ifelse(tree_before@data$support<90,NA,tree_before@data$support)
before_collapse <- c(131,151,164,169)

p_before <- ggtree(tree_before, size = 1, layout = "rectangular", color = "black", ladderize = FALSE)

for (node in before_collapse) {
  p_before <- collapse(p_before, node = node)
}

p_before <- p_before + 
  geom_point(data = filter(p_before$data, node %in% before_collapse), 
             aes(x = x, y = y), shape = 17, size = 8, color = "grey") +
  geom_tiplab(align=FALSE,size=4) +
  geom_treescale(x = 0, y = 0, offset = 0.5)
p_before <- p_before +
  geom_point(data = filter(p_before$data, label %in% minor_parent), color = "#8b669e", size = 6) +  
  geom_point(data = filter(p_before$data, label %in% major_parent), color = "#c53867", size = 6) +  
  geom_point(data = filter(p_before$data, label %in% recombination), color = "#df857a", size = 6) +  
      theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("before")+
  hexpand(0.3)
p_before

tree_recombine <- read.newick("recombine.nwk", node.lab="support")
tree_recombine@data$support <- ifelse(tree_recombine@data$support<90,NA,tree_recombine@data$support)
recombine_collapse <- c(127,146,160,167)

p_recombine <- ggtree(tree_recombine, size = 1, layout = "rectangular", color = "black", ladderize = FALSE)

for (node in recombine_collapse) {
  p_recombine <- collapse(p_recombine, node = node)
}
p_recombine <- p_recombine + 
  geom_point(data = filter(p_recombine$data, node %in% recombine_collapse), 
             aes(x = x, y = y), shape = 17, size = 8, color = "grey") +
  geom_tiplab(align=FALSE,size=4) +
  geom_treescale(x = 0, y = 0, offset = 0.5)
p_recombine <- p_recombine +
  geom_point(data = filter(p_recombine$data, label %in% minor_parent), color = "#8b669e", size = 6) +  
  geom_point(data = filter(p_recombine$data, label %in% major_parent), color = "#c53867", size = 6) +  
  geom_point(data = filter(p_recombine$data, label %in% recombination), color = "#df857a", size = 6) +  
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("recombine")+
  hexpand(0.3)
p_recombine

tree_after <- read.newick("after.nwk", node.lab="support")
tree_after@data$support <- ifelse(tree_after@data$support<90,NA,tree_after@data$support)
after_collapse <- c(111, 161, 168)

p_after <- ggtree(tree_after, size = 1, layout = "rectangular", color = "black", ladderize = FALSE)

for (node in after_collapse) {
  p_after <- collapse(p_after, node = node)
}
p_after <- p_after + 
  geom_point(data = filter(p_after$data, node %in% after_collapse), 
             aes(x = x, y = y), shape = 17, size = 8, color = "grey") +
  geom_tiplab(align=FALSE,size=4) +
  geom_treescale(x = 0, y = 0, offset = 0.5)
p_after <- p_after +
  geom_point(data = filter(p_after$data, label %in% minor_parent), color = "#8b669e", size = 6) +  
  geom_point(data = filter(p_after$data, label %in% major_parent), color = "#c53867", size = 6) +  
  geom_point(data = filter(p_after$data, label %in% recombination), color = "#df857a", size = 6) +  
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("after")+
  hexpand(0.3)
p_after

p1 <- grid.arrange(p_before, p_recombine,p_after, ncol = 3)


ggsave("RdRp_treefile1.pdf", plot = p1, width = 13, height = 10)
