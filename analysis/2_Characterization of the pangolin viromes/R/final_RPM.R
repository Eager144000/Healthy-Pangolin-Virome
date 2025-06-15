library(tidyverse)
library(ggplot2)
library(tibble)
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)
library(ggsci)
library(scales)


rm(list=ls())
d1 <- read.csv("Pangolin_virus_rpms.csv",sep = ',',header = T)
d2<- data.frame(d1)
d3_1 <-d2[,c(1,2,3,4)]
d3 <- d2[,c(2,5,6,7,10)]
d4 <- spread(d3,key = "sample", value = "lg_rpm_1")
d4 <- d4[-1,]
family_order <- c("Anelloviridae",
                  "Circoviridae",
                  "Papillomaviridae",
                  "Parvoviridae",
                  "Flaviviridae",
                  "Paramyxoviridae",
                  "Pneumoviridae",
                  "Picornaviridae",
                  "unclassified Picornavirales"
)
d4$Family <- factor(d4$Family, levels = family_order)
sorted_d4 <- d4[order(d4$Family),]

d3_1 <- unique(d3_1)
d3_1<- data.frame(d3_1)
Type_order <- c("Anal","Oral","Fecal","Lung","Lymph","MMM","Muscle","Skin","Spleen")
d3_1$Type <- factor(d3_1$Type, levels = Type_order)
Host_order <- c("Manis javanica","Manis pentadactyla")
d3_1$Host <- factor(d3_1$Host, levels = Host_order)
sorted_d3_1 <- d3_1[order(d3_1$Type, d3_1$Host),]


sorted_d4[is.na(sorted_d4)] <- 0
rownames(sorted_d4) <- sorted_d4$Species
d5 <- sorted_d4[,(-1:-3)]
d5 <- d5[,sorted_d3_1$sample]
d6 <- as.matrix(d5)


Family <- sorted_d4$Family
Genus <- sorted_d4$Genus
family_colors <- c(
  "Circoviridae" = "#FFB6C1",
  "Parvoviridae" = "#FFA07A",
  "Picornaviridae" = "#20B2AA",
  "Papillomaviridae" = "#778899",
  "Paramyxoviridae" = "#9932CC",
  "Flaviviridae" = "#8FBC8F",
  "Pneumoviridae" = "#BDB76B",
  "Anelloviridae" = "#FFD700",
  "unclassified Picornavirales" = "#FF6347",
)

genus_colors <- c(
  "Orthopneumovirus" = "#BDB76B",
  "Pestivirus" = "#8FBC8F",
  "Tettorquevirus" = "#9932CC",
  "Orthorubulavirus" = "#9932CC",
  "Respirovirus" = "#9932CC",
  "unclassified Parvoviridae genus" = "#FFA07A",
  "unclassified Pneumoviridae genus" = "#BDB76B",
  "Copiparvovirus" = "#FFA07A",
  "Hunnivirus" = "#20B2AA",
  "Circovirus" = "#FFB6C1",
  "Cyclovirus" = "#FFB6C1",
  "Senecavirus" = "#20B2AA",
  "Protoparvovirus" = "#FFA07A",
  "unclassified Circoviridae genus" = "#FFB6C1",
  "unclassified Papillomaviridae genus" = "#778899",
  "Morbillivirus" = "#9932CC",
  "Shanbavirus" = "#9932CC",
  "unclassified Picornavirales genus" = "#FF6347"
)
row_ha = rowAnnotation(
  Family = sorted_d4$Family,
  Genus =  sorted_d4$Genus, 
  col = list(Family = family_colors, Genus = genus_colors),
  width=unit(0.8, "mm"),
  gap = unit(2, "mm")
  )

Health<- sorted_d3_1$Health
Type<- sorted_d3_1$Type
Host <- sorted_d3_1$Host
Health_colors <- c("Healthy" = "#6baed6", "Unhealthy" = "#fd8d3c")
Type_colors <- c("Oral" = "#74c476", "Anal" = "#DAA520", "Lung" = "#a1d99b", "Lymph" = "#c7e9c0",
                     "MMM" = "#756bb1", "Skin" = "#9e9ac8", "Spleen" = "#bcbddc", "Fecal" = "#fdae6b",
                     "Muscle" = "#fdbe85")
host_colors <- c("Manis javanica" = "#3182bd", "Manis pentadactyla" = "#FF69B4")
col_ha = columnAnnotation(
  Health  = sorted_d3_1$Health,
  Type= sorted_d3_1$Type,
  Host = sorted_d3_1$Host,
  col = list(Health=Health_colors, Type=Type_colors,Host=host_colors)
)

col_fun = colorRamp2(c(0,6),c('#EDEADF','#a50026'))
pdf("Pangolin_Virus_RPM.pdf",width = 30,height = 20)
Heatmap(d6,
        cluster_columns = FALSE,
        cluster_rows = FALSE,
        left_annotation = row_ha,
        top_annotation = col_ha,
        row_split = factor(sorted_d4$Family),
        column_split = factor(sorted_d3_1$Health),
        row_names_side = "right",
        col = col_fun, 
        width = ncol(sorted_d4) * unit(1.5, "mm"),
        height = nrow(sorted_d4) * unit(6, "mm"),
        border_gp = gpar(col = "#EDEADF", lty = 0),
        rect_gp = gpar(col = "transparent", lwd = 0.3),
        column_names_gp = gpar(fontsize = 5),
        column_names_rot = 45,
        bottom_annotation = NULL,
        show_column_names = FALSE
        )

dev.off()

write.csv(d6, "Virus_rpm.csv", row.names = TRUE)
