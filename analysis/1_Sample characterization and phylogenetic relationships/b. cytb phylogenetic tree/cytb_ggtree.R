library(ggtree)
library(ape)
library(dplyr)
library(ggplot2)
library(treeio)

rm(list=ls())


tree <- read.newick("phylo_cytb.nwk", node.lab="support")
tree@data$support <- ifelse(tree@data$support<90,NA,tree@data$support)


highlight_Healthy <- c("UN_MBA191000_211227_cytb",
                       "UN_MBA191001_211227_cytb",
                       "UN_MBA191002_211227_cytb",
                       "UN_MBA191003_211227_cytb",
                       "UN_MBA191004_211227_cytb",
                       "UN_MBA191005_211227_cytb",
                       "UN_MBA191007_211227_cytb",
                       "UN_MBA191009_211227_cytb",
                       "UN_MBA191010_211227_cytb",
                       "UN_MBA191012_211229_cytb",
                       "UN_MBO191013_220216_cytb",
                       "UN_MBA191014_211229_cytb",
                       "UN_MBA191016_211229_cytb",
                       "UN_MBA191017_211229_cytb",
                       "UN_MBA191018_211229_cytb",
                       "UN_MBO191019_220217_cytb",
                       "UN_MBO191020_220217_cytb",
                       "UN_MBA191021_211229_cytb",
                       "UN_MBA191022_211229_cytb",
                       "UN_MBO191023_220217_cytb",
                       "UN_MBA191024_211229_cytb",
                       "UN_MBA191025_211229_cytb",
                       "UN_MBA191027_211229_cytb",
                       "UN_MBA191028_220105_cytb",
                       "UN_MBA191031_220105_cytb",
                       "UN_MBO191033_220218_cytb",
                       "UN_MBO191034_220218_cytb",
                       "UN_MBO191035_220218_cytb",
                       "UN_MBA191037_220105_cytb",
                       "UN_MBO191038_220218_cytb",
                       "UN_MBA191039_220105_cytb",
                       "UN_MBA191040_220105_cytb",
                       "UN_MBA191042_220105_cytb",
                       "UN_MBA191043_220105_cytb",
                       "UN_MBA191044_220107_cytb",
                       "UN_MBO191047_220218_cytb",
                       "UN_MBO191049_220221_cytb",
                       "UN_MBA191051_220107_cytb",
                       "UN_MBA191052_220107_cytb",
                       "UN_MBO191053_220221_cytb",
                       "UN_MBA191055_220107_cytb",
                       "UN_MBA191056_220107_cytb",
                       "UN_MBA191058_220107_cytb",
                       "UN_MBA191060_220110_cytb",
                       "UN_MBA191062_220110_cytb",
                       "UN_MBA191063_220110_cytb",
                       "UN_MBA191064_220110_cytb",
                       "UN_MBA191065_220110_cytb",
                       "UN_MBO191067_220223_cytb",
                       "UN_MBO191069_220223_cytb",
                       "UN_MBA191070_220110_cytb",
                       "UN_MBA191071_220110_cytb",
                       "UN_MBO191073_220223_cytb",
                       "UN_MBA191074_220110_cytb",
                       "UN_MBA191078_220228_cytb",
                       "UN_MBA190999_211227_cytb",
                       "UN_7D11_4O_220228_cytb",
                       "UN_7D7_2O_2_220228_cytb",
                       "UN_MBA191006_211227_cytb",
                       "UN_MBA191008_211227_cytb",
                       "UN_MBA191011_211227_cytb",
                       "UN_MBA191015_211229_cytb",
                       "UN_MBA191026_211229_cytb",
                       "UN_MBA191029_220105_cytb",
                       "UN_MBA191030_220105_cytb",
                       "UN_MBA191032_220105_cytb",
                       "UN_MBA191036_220105_cytb",
                       "UN_MBA191041_220105_cytb",
                       "UN_MBO191045_220218_cytb",
                       "UN_MBA191046_220107_cytb",
                       "UN_MBA191048_220107_cytb",
                       "UN_MBA191050_220107_cytb",
                       "UN_MBA191054_220107_cytb",
                       "UN_MBA191057_220107_cytb",
                       "UN_MBA191059_220107_cytb",
                       "UN_MBA191061_220110_cytb",
                       "UN_MBA191066_220110_cytb",
                       "UN_MBA191068_220110_cytb",
                       "UN_MBO191072_220223_cytb",
                       "UN_MBA191075_220110_cytb",
                       "UN_MBA191076_220228_cytb",
                       "UN_MBA191077_220228_cytb",
                       "UN_MBO191079_220223_cytb")
highlight_Unhealthy <- c("MJ_Lung15_cytb",
                         "MJ_Lung16_cytb",
                         "MJ_Lung17_cytb",
                         "MJ_Lung1_cytb",
                         "MJ_Lung20_cytb",
                         "MJ_Lung22_cytb",
                         "MJ_Lung26_cytb",
                         "MJ_Lung3_cytb",
                         "MJ_Lung4_cytb",
                         "MJ_Lung5_cytb",
                         "MJ_Lung6_cytb",
                         "MJ_Lung7_cytb",
                         "MJ_Lung9_cytb",
                         "MJ_Mix1_cytb",
                         "MJ_Mix2_cytb",
                         "MJ_Mix3_cytb",
                         "MJ_Mix4_cytb",
                         "MJ_Mix8_cytb",
                         "MJ_Skin1_cytb",
                         "MP_Fecal2_cytb",
                         "MP_Lung1_cytb",
                         "MP_Lung2_cytb",
                         "MP_Mix1_cytb",
                         "MP_Mix4_cytb",
                         "MP_Mix5_cytb",
                         "MJ_Lung10_cytb",
                         "MJ_Lung11_cytb",
                         "MJ_Lung12_cytb",
                         "MJ_Lung13_cytb",
                         "MJ_Lung14_cytb",
                         "MJ_Lung18_cytb",
                         "MJ_Lung23_cytb",
                         "MJ_Lung2_cytb",
                         "MJ_Lung8_cytb",
                         "MJ_Mix7_cytb",
                         "MP_Muscle1_cytb",
                         "MP_Mix2_cytb")

p <- ggtree(tree, size = 1, layout = "circular", color = "#646464", ladderize = FALSE) + 
  geom_text(aes(label = support), hjust = 1.5, vjust = 0, size = 2) +
#  geom_tiplab(align = TRUE, size = 4, fontface = "italic") +
  geom_treescale(x = 0, y = 0, offset = 0.5)

p1 <- p + 
  geom_point(data = filter(p$data, label %in% highlight_Healthy), color = "#6BAED6", size = 4) +  # 标记特定的标签
  geom_point(data = filter(p$data, label %in% highlight_Unhealthy), color = "#E34A33", size = 4) +
  geom_cladelabel(node=349, label = "Manis pentadactyla", align = T, offset = 0.025, color = "#FF69B4", barsize = 4, fontsize = 8, fontface = "bold" ) +
  geom_cladelabel(node=192, label = "Manis javanica", align = T, offset = 0.025, color = "#3182bd", barsize = 4, fontsize = 8, fontface = "bold" ) +
  geom_cladelabel(node=377, label = "India", align = T, offset = 0.01, color = "#646464", barsize = 2.5, fontsize = 6) +
  geom_cladelabel(node=350, label = "China", align = T, offset = 0.01, color = "#646464", barsize = 2.5, fontsize = 6) +
  geom_cladelabel(node=327, label = "Indonesia Malaysia", align = T, offset = 0.01, color = "#646464", barsize = 2.5, fontsize = 6) +
  geom_cladelabel(node=296, label = "India", align = T, offset = 0.01, color = "#646464", barsize = 2.5, fontsize = 6) +
  geom_cladelabel(node=199, label = "Malaysia Indonesia China", align = T, offset = 0.014, color = "#646464", barsize = 2.5, fontsize = 6) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Phylogenetic Tree of Sample CYTB")

p1
dev.off()

