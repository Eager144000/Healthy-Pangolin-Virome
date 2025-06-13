library(ggtree)
library(ape)
library(dplyr)
library(ggplot2)
library(treeio)
library(gridExtra)

rm(list=ls())



tree_Circoviridae <- read.newick("Circoviridae_rep.nwk", node.lab="support")
tree_Circoviridae@data$support <- ifelse(tree_Circoviridae@data$support<90,NA,tree_Circoviridae@data$support)



highlight_Circoviridae_Healthy <- c("UN_MBO191005-Circoviridae-1",
                                    "UN_MBO191007-Circoviridae-1",
                                    "UN_MBO191009-Circoviridae-1",
                                    "UN_MBO191010-Circoviridae-1",
                                    "UN_MBA191015-Circoviridae-1",
                                    "UN_MBO191022-Circoviridae-1",
                                    "UN_MBA191023-Circoviridae-1",
                                    "UN_MBA191023-Circoviridae-2",
                                    "UN_MBA191026-Circoviridae-1",
                                    "UN_MBA191028-Circoviridae-1",
                                    "UN_MBA191031-Circoviridae-1",
                                    "UN_MBO191018-Circoviridae-1",
                                    "UN_7D11_4O-Circoviridae-1",
                                    "UN_7D7_2A-Circoviridae-1")
highlight_Circoviridae_Unhealthy <- c("MP_Fecal2-Circoviridae-1")

Circoviridae_collapse <- c(235,229,221,209,192,189,186,174)


p_Circoviridae <- ggtree(tree_Circoviridae, size = 1, layout = "rectangular", color = "black", ladderize = FALSE) 

for (node in Circoviridae_collapse) {
  p_Circoviridae <- collapse(p_Circoviridae, node = node)
}

p_Circoviridae <- p_Circoviridae +  
  geom_point(data = filter(p_Circoviridae$data, node %in% Circoviridae_collapse), 
             aes(x = x, y = y), shape = 22, size = 4, color = "grey") + 

  geom_tiplab(align=FALSE,size=3) +
  geom_treescale(x = 0, y = 0, offset = 0.5)
p_Circoviridae <- p_Circoviridae +
  geom_point(data = filter(p_Circoviridae$data, label %in% highlight_Circoviridae_Healthy), color = "#6BAED6", size = 4) + 
  geom_point(data = filter(p_Circoviridae$data, label %in% highlight_Circoviridae_Unhealthy), color = "#E34A33", size = 4) +
  geom_cladelabel(node=147, label = "Circovirus", align = T, offset = 1, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic" ) +
  geom_cladelabel(node=141, label = "Cyclovirus", align = T, offset = -0.3, color = "#FF69B4", barsize = 3,fontsize = 5,fontface = "italic" ) +
  geom_cladelabel(node=275, label = "unclassified Circoviridae", align = T, offset = 0.2, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic" ) +
  geom_cladelabel(node=261, label = "Cyclovirus risi", align = T, offset = -1.4, color = "black", barsize = 0,fontsize = 4,fontface = "italic" ) +
  geom_cladelabel(node=159, label = "Circovirus porcine1", align = T, offset = -0.3, color = "black", barsize = 0,fontsize = 4,fontface = "italic" ) +
    theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Circoviridae")+
  hexpand(0.3)
p_Circoviridae



tree_Flaviviridae <- read.newick("Flaviviridae_rep.nwk", node.lab="support")
tree_Flaviviridae@data$support <- ifelse(tree_Flaviviridae@data$support<90,NA,tree_Flaviviridae@data$support)



highlight_Flaviviridae_Healthy <- c("UN_MBA191027-Flaviviridae-1",
                                 "UN_MBO191054-Flaviviridae-1",
                                 "UN_MBO191068-Flaviviridae-1",
                                 "UN_MBA191059-Flaviviridae-1",
                                 "UN_MBO191071-Flaviviridae-1")
highlight_Flaviviridae_Unhealthy <- c("MJ_Lung1-Flaviviridae-1",
                                      "MJ_Lung16-Flaviviridae-1",
                                      "MJ_Lung17-Flaviviridae-1",
                                      "MJ_Lung18-Flaviviridae-1",
                                      "MJ_Lung20-Flaviviridae-1",
                                      "MJ_Lung22-Flaviviridae-1",
                                      "MJ_Lung5-Flaviviridae-1",
                                      "MJ_Lung7-Flaviviridae-1",
                                      "MJ_Lung8-Flaviviridae-1")

Flaviviridae_collapse <- c(298,273,162)

p_Flaviviridae <- ggtree(tree_Flaviviridae, size = 1, layout = "rectangular", color = "black", ladderize = FALSE) 

for (node in Flaviviridae_collapse) {
  p_Flaviviridae <- collapse(p_Flaviviridae, node = node)
}

p_Flaviviridae <- p_Flaviviridae +
  geom_point(data = filter(p_Flaviviridae$data, node %in% Flaviviridae_collapse), 
             aes(x = x, y = y), shape = 17, size = 4, color = "grey") + 

  geom_tiplab(align=FALSE,size=3) +
  geom_treescale(x = 0, y = 0, offset = 0.5) 
p_Flaviviridae <- p_Flaviviridae +
  geom_point(data = filter(p_Flaviviridae$data, label %in% highlight_Flaviviridae_Healthy), color = "#6BAED6", size = 4) + 
  geom_point(data = filter(p_Flaviviridae$data, label %in% highlight_Flaviviridae_Unhealthy), color = "#E34A33", size = 4) +
  geom_cladelabel(node=237, label = "Pestivirus", align = T, offset = 1, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=243, label = "Pangolin pestivirus", align = T, offset = 0.4, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  geom_cladelabel(node=162, label = "Orthoflavivirus", align = T, offset = 0, color = "black", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=298, label = "Pegivirus", align = T, offset = 0, color = "black", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=273, label = "Hepacivirus", align = T, offset = 0, color = "black", barsize = 3,fontsize = 5,fontface = "italic") +
    theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Flaviviridae")+
  hexpand(0.3)
p_Flaviviridae



tree_Papillomaviridae <- read.newick("Papillomaviridae_L1.nwk", node.lab="support")
tree_Papillomaviridae@data$support <- ifelse(tree_Papillomaviridae@data$support<90,NA,tree_Papillomaviridae@data$support)


highlight_Papillomaviridae_Healthy <- c("UN_MBO191052-Papillomaviridae-1",
                                        "UN_MBA191059-Papillomaviridae-1",
                                        "UN_MBA190999-Papillomaviridae-1",
                                        "UN_MBO191000-Papillomaviridae-1",
                                        "UN_MBA191038-Papillomaviridae-1",
                                        "UN_MBO191076-Papillomaviridae-1")
Papillomaviridae_collapse <- c(681,679,656,655,642,639,635,632,281,280,619,613,252,251,
                               248,247,597,520,506,494,443,434,427,358,281,280,267,259,
                               608,256,607,253,164,513,159,158,138,88,87,78)

p_Papillomaviridae <- ggtree(tree_Papillomaviridae, size = 1, layout = "rectangular", color = "black", ladderize = FALSE) 
for (node in Papillomaviridae_collapse) {
  p_Papillomaviridae <- collapse(p_Papillomaviridae, node = node)
}

p_Papillomaviridae <- p_Papillomaviridae +
  geom_point(data = filter(p_Papillomaviridae$data, node %in% Papillomaviridae_collapse), 
             aes(x = x, y = y), shape = 17, size = 4, color = "grey") + 

  geom_tiplab(align=FALSE,size=3) +
  geom_treescale(x = 0, y = 0, offset = 0.5) 

p_Papillomaviridae <- p_Papillomaviridae +
  geom_point(data = filter(p_Papillomaviridae$data, label %in% highlight_Papillomaviridae_Healthy), color = "#6BAED6", size = 4) + 
  geom_cladelabel(node=639, label = "Iotapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=635, label = "Dyokappapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=632, label = "Mupapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=281, label = "Dyosigmapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=280, label = "Kappapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=613, label = "Lambdapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=252, label = "Treisiotapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=251, label = "Omegapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=248, label = "Dyopipapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=247, label = "Dyodeltapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=597, label = "Dyoomikronpapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=520, label = "Alphapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=506, label = "Upsilonpapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=494, label = "Xipapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=443, label = "Betapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=434, label = "Taupapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=427, label = "Pipapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=358, label = "Gammapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=267, label = "Dyophipapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=259, label = "Dyothetapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=608, label = "Rhopapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=256, label = "Dyoomegapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=607, label = "Rhopapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=253, label = "Dyomupapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=164, label = "Dyolambdapapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=513, label = "Omikronpapillomavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=603, label = "unclassified Papillomaviridae", align = T, offset = 0.5, color = "#3182bd", barsize = 3, fontsize = 5, fontface = "italic" ) +
  geom_cladelabel(node=625, label = "unclassified Papillomaviridae", align = T, offset = 0.5, color = "#3182bd", barsize = 3, fontsize = 5, fontface = "italic" ) +
  geom_cladelabel(node=629, label = "Manis javanica papillomavirus 1", align = T, offset = 0, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  geom_cladelabel(node=626, label = "Papillomavirus manis7551", align = T, offset = 0, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  geom_cladelabel(node=603, label = "Manis pentadactyla papillomavirus 1", align = T, offset = 0, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
    theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Papillomaviridae")+
  hexpand(0.3)

p_Papillomaviridae



tree_Paramyxoviridae <- read.newick("Paramyxovirdae_rep.nwk", node.lab="support")
tree_Paramyxoviridae@data$support <- ifelse(tree_Paramyxoviridae@data$support<90,NA,tree_Paramyxoviridae@data$support)



highlight_Paramyxoviridae_Healthy <- c("UN_MBA191024-Paramyxoviridae-1")
highlight_Paramyxoviridae_Unhealthy <- c("MJ_Lung17-Paramyxoviridae-1",
                                      "MP_Lung2-Paramyxoviridae-1")

Paramyxoviridae_collapse <- c(311,302,281,279,266,105,104,262,254,92,91,238,74,73,233,66,230,166)

p_Paramyxoviridae <- ggtree(tree_Paramyxoviridae, size = 1, layout = "rectangular", color = "black", ladderize = FALSE) 
for (node in Paramyxoviridae_collapse) {
  p_Paramyxoviridae <- collapse(p_Paramyxoviridae, node = node)
}  
p_Paramyxoviridae <- p_Paramyxoviridae +
  geom_point(data = filter(p_Paramyxoviridae$data, node %in% Paramyxoviridae_collapse), 
             aes(x = x, y = y), shape = 17, size = 4, color = "grey") + 

  geom_tiplab(align=FALSE,size=3) +
  geom_treescale(x = 0, y = 0, offset = 0.5) 
p_Paramyxoviridae <- p_Paramyxoviridae +
  geom_point(data = filter(p_Paramyxoviridae$data, label %in% highlight_Paramyxoviridae_Healthy), color = "#6BAED6", size = 4) + 
  geom_point(data = filter(p_Paramyxoviridae$data, label %in% highlight_Paramyxoviridae_Unhealthy), color = "#E34A33", size = 4) +
  geom_cladelabel(node=302, label = "Pararubulavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=281, label = "Orthoavulavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=279, label = "Paraavulavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=266, label = "Metaavulavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=105, label = "Scoliodonvirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=104, label = "Synodonvirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=262, label = "Aquaparamyxovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=254, label = "Respirovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=92, label = "Ferlavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=91, label = "Hippocavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=238, label = "Henipavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=233, label = "Narmovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=230, label = "Paramorbillivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=166, label = "Jeilongvirus/Parajeilongvirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=292, label = "Orthorubulavirus", align = T, offset = 0.6, color = "#FF69B4", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=220, label = "Morbillivirus", align = T, offset = 0.6, color = "#FF69B4", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=299, label = "Orthorubulavirus mammalis", align = T, offset = 0.3, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  geom_cladelabel(node=227, label = "Morbillivirus canis", align = T, offset = 0.3, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Paramyxoviridae")+
  hexpand(0.3)
p_Paramyxoviridae





tree_Picornaviridae <- read.newick("Picornaviridae_rep.nwk", node.lab="support")
tree_Picornaviridae@data$support <- ifelse(tree_Picornaviridae@data$support<90,NA,tree_Picornaviridae@data$support)



highlight_Picornaviridae_Healthy <- c("UN_7D7_2A-Picornaviridae-1", "UN_MBA191024-Picornaviridae-1","UN_MBO191002-Picornaviridae-1","UN_MBO191053-Picornaviridae-1")
highlight_Picornaviridae_Unhealthy <- c("MJ_Mix5-Picornaviridae-1")

Picornaviridae_collapse <- c(1115,1113,1111,1108,1106,544,1105,1101,1099,532,1092,1073,1063,1062,
                             1060,496,495,1047,480,479,1030,466,465,1029,1027,460,1026,1019,449,448,
                             1012,444,1011,441,1009,1007,999,976,965,963,961,936,920,911,907,881,
                             869,860,857,850,567)

p_Picornaviridae <- ggtree(tree_Picornaviridae, size = 1, layout = "rectangular", color = "black", ladderize = FALSE) 
for (node in Picornaviridae_collapse) {
  p_Picornaviridae <- collapse(p_Picornaviridae, node = node)
}
p_Picornaviridae <- p_Picornaviridae +
  geom_point(data = filter(p_Picornaviridae$data, node %in% Picornaviridae_collapse), 
             aes(x = x, y = y), shape = 17, size = 4, color = "grey") + 

  geom_tiplab(align=FALSE,size=3) +
  geom_treescale(x = 0, y = 0, offset = 0.5) 

p_Picornaviridae <- p_Picornaviridae +
  geom_point(data = filter(p_Picornaviridae$data, label %in% highlight_Picornaviridae_Healthy), color = "#6BAED6", size = 4) + 
  geom_point(data = filter(p_Picornaviridae$data, label %in% highlight_Picornaviridae_Unhealthy), color = "#E34A33", size = 4) + 
  geom_cladelabel(node=1115, label = "Aquamavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1113, label = "Kunsagivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1111, label = "Potamipivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1108, label = "Limnipivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1106, label = "Orivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1105, label = "Avisivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1101, label = "Grusopivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1099, label = "Avihepatovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=532, label = "Crohivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1092, label = "Pasivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1073, label = "Parechovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1063, label = "Fipivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1062, label = "Rohelivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1060, label = "Tremovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1047, label = "Hepatovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1030, label = "Megrivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1029, label = "Tropivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1027, label = "Pemapivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1026, label = "Dicipivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1019, label = "Rosavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1012, label = "Rafivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1011, label = "Oscivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1009, label = "Gallivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1007, label = "Passerivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=999, label = "Sicinivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=976, label = "Kobuvirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=965, label = "Mosavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=963, label = "Erbovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=961, label = "Malagasivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=936, label = "Teschovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=920, label = "Aphthovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=911, label = "Cosavirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=907, label = "Mischivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=881, label = "Cardiovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=869, label = "Sapelovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=860, label = "Rabovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=857, label = "Boosepivirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=850, label = "Parabovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=567, label = "Enterovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=558, label = "unclassified Picornaviridae", align = T, offset = 0.1, color = "#3182bd", barsize = 3, fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=1095, label = "Shanbavirus", align = T, offset = 0.1, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=953, label = "Hunnivirus", align = T, offset = 0.9, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=953, label = "Hunnivirus amagyari", align = T, offset = -0.7, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Picornaviridae")+
  hexpand(0.3)
p_Picornaviridae






tree_Parvoviridae <- read.newick("Parvoviridae_NS1.nwk", node.lab="support")
tree_Parvoviridae@data$support <- ifelse(tree_Parvoviridae@data$support<90,NA,tree_Parvoviridae@data$support)



highlight_Parvoviridae_Healthy <- c("UN_MBO191000-Parvoviridae-1",
                                    "UN_MBO191001-Parvoviridae-1",
                                    "UN_MBA191002-Parvoviridae-1",
                                    "UN_MBO191003-Parvoviridae-1",
                                    "UN_MBO191004-Parvoviridae-1",
                                    "UN_MBO191007-Parvoviridae-1",
                                    "UN_MBA191010-Parvoviridae-1",
                                    "UN_MBA191013-Parvoviridae-1",
                                    "UN_MBA191015-Parvoviridae-1",
                                    "UN_MBA191016-Parvoviridae-1",
                                    "UN_MBO191017-Parvoviridae-1",
                                    "UN_MBA191019-Parvoviridae-1",
                                    "UN_MBO191020-Parvoviridae-1",
                                    "UN_MBA191021-Parvoviridae-1",
                                    "UN_MBO191024-Parvoviridae-1",
                                    "UN_MBO191025-Parvoviridae-1",
                                    "UN_MBO191026-Parvoviridae-1",
                                    "UN_MBA191027-Parvoviridae-1",
                                    "UN_MBO191028-Parvoviridae-1",
                                    "UN_MBO191031-Parvoviridae-1",
                                    "UN_MBO191033-Parvoviridae-1",
                                    "UN_MBA191037-Parvoviridae-1",
                                    "UN_MBA191038-Parvoviridae-1",
                                    "UN_MBO191040-Parvoviridae-1",
                                    "UN_MBO191042-Parvoviridae-1",
                                    "UN_MBO191043-Parvoviridae-1",
                                    "UN_MBO191045-Parvoviridae-1",
                                    "UN_MBO191048-Parvoviridae-1",
                                    "UN_MBO191051-Parvoviridae-1",
                                    "UN_MBO191052-Parvoviridae-1",
                                    "UN_MBO191053-Parvoviridae-1",
                                    "UN_MBO191055-Parvoviridae-1",
                                    "UN_MBO191061-Parvoviridae-1",
                                    "UN_MBO191062-Parvoviridae-1",
                                    "UN_MBO191064-Parvoviridae-1",
                                    "UN_MBO191065-Parvoviridae-1",
                                    "UN_MBO191068-Parvoviridae-1",
                                    "UN_MBO191073-Parvoviridae-1",
                                    "UN_MBA191074-Parvoviridae-1",
                                    "UN_MBA191079-Parvoviridae-1")
highlight_Parvoviridae_Unhealthy <- c("MJ_Lung27-Parvoviridae-1",
                                      "MJ_Lung8-Parvoviridae-1",
                                      "MJ_Lymph33-Parvoviridae-1",
                                      "MJ_Lymph33-Parvoviridae-2",
                                      "MP_Mix4-Parvoviridae-1",
                                      "MP_Mix5-Parvoviridae-1",
                                      "UN_7D11_4O-Parvoviridae-2",
                                      "UN_7D11_4O-Parvoviridae-1",
                                      "UN_7D7_2A-Parvoviridae-1",
                                      "UN_7D7_2A-Parvoviridae-3",
                                      "UN_MBA191005-Parvoviridae-1",
                                      "MJ_Spleen27-Parvoviridae-1",
                                      "UN_7D7_2A-Parvoviridae-2")

Parvoviridae_collapse <- c(198,372,358,350,347,319,309,78,77,250,381,301)

p_Parvoviridae <- ggtree(tree_Parvoviridae, size = 1, layout = "rectangular", color = "black", ladderize = FALSE) 
for (node in Parvoviridae_collapse) {
  p_Parvoviridae <- collapse(p_Parvoviridae, node = node)
  }
p_Parvoviridae <- p_Parvoviridae +
  geom_point(data = filter(p_Parvoviridae$data, node %in% Parvoviridae_collapse), 
             aes(x = x, y = y), shape = 17, size = 4, color = "grey") +   

  geom_tiplab(align=FALSE,size=3) +
  geom_treescale(x = 0, y = 0, offset = 0.5) 
p_Parvoviridae <- p_Parvoviridae +
  geom_point(data = filter(p_Parvoviridae$data, label %in% highlight_Parvoviridae_Healthy), color = "#6BAED6", size = 4) + 
  geom_point(data = filter(p_Parvoviridae$data, label %in% highlight_Parvoviridae_Unhealthy), color = "#E34A33", size = 4) + 
  geom_cladelabel(node=372, label = "Tetraparvovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=358, label = "Dependoparvovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=350, label = "Erythroparvovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=347, label = "Aveparvovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=319, label = "Bocaparvovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=309, label = "Amdoparvovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=250, label = "Chaphamaparvovirus", align = T, offset = 0, color = "black", barsize = 0,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=400, label = "Blattambidensovirus", align = T, offset = 2, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=211, label = "Unclassified Parvoviridae", align = T, offset = 2, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=286, label = "Protoparvovirus", align = T, offset = 0.3, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=379, label = "Copiparvovirus", align = T, offset = -0.5, color = "#3182bd", barsize = 3,fontsize = 5,fontface = "italic") +
  geom_cladelabel(node=201, label = "Blattambidensovirus incertum1", align = T, offset = 0, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  geom_cladelabel(node=294, label = "Protoparvovirus carnivoran1", align = T, offset = -2, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  geom_cladelabel(node=390, label = "Copiparvovirus_P223T", align = T, offset = -2, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
  geom_cladelabel(node=1, label = "Zophobas morio black wasting virus", align = T, offset = -1, color = "black", barsize = 0,fontsize = 4,fontface = "italic") +
    theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  ) +
  ggtitle("Parvoviridae")+
  hexpand(0.3)
p_Parvoviridae

p_Circoviridae
p_Papillomaviridae
p_Parvoviridae


p_Flaviviridae
p_Paramyxoviridae
p_Picornaviridae

p1 <- grid.arrange(p_Circoviridae, p_Papillomaviridae, p_Parvoviridae, ncol = 3)
p2 <- grid.arrange(p_Flaviviridae, p_Paramyxoviridae, p_Picornaviridae, ncol = 3)
p3 <- grid.arrange(p1, p2, ncol = 1, heights = c(1,1))


ggsave("RdRp_treefile.pdf", plot = p3, width = 15, height = 15)








