library(corrplot)
library(ggplot2)
library(ggpubr)
rm(list = ls())


cor_data <- read.table("healthy_pangolin_virus_rpm.csv", header = TRUE,sep = ",")
#pearson：
cor_pearson <- cor (cor_data, method="pearson")
cor_pearson_Res = cor.mtest(cor_data, method="pearson", conf.level = 0.95)
write.csv(cor_pearson, "cor_pearson.csv",row.names = TRUE)
write.csv(cor_pearson_Res$p, "pearson_P.csv",row.names = TRUE)


addcol <- colorRampPalette(c("red", "white", "blue"))
pdf("correlation-pearson-RPM.pdf", width = 15, height = 15)
corrplot(cor_pearson, method = "circle",col = addcol(100),
         tl.col = "black", tl.cex = 1, tl.srt = 45,tl.pos = "lt", 
         p.mat=cor_pearson_Res$p, diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.2,
         insig = 'label_sig', pch.col = 'grey30',
         order = 'AOE', addgrid.col = "grey70", cl.cex = 1.2)
corrplot(cor_pearson, method = "number", diag = FALSE,type = "lower",col = addcol(100), 
         tl.col = "n", tl.cex = 1, tl.pos = "n", order = 'AOE',
         add = T, number.cex = 0.8,cl.pos = "n")
dev.off()
#kendall；
cor_kendall <- cor (cor_data, method="kendall")
cor_kendall_Res = cor.mtest(cor_data, method="kendall", conf.level = 0.95)

addcol <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_kendall, method = "color",col = addcol(100),
         tl.col = "black", tl.cex = 1, tl.srt = 45,tl.pos = "lt", 
         p.mat=cor_kendall_Res$p, diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.2,
         insig = 'label_sig', pch.col = 'grey30',
         order = 'AOE', addgrid.col = "grey70", cl.cex = 1.2)

corrplot(cor_kendall, method = "number", diag = FALSE, type = "lower",col = addcol(100), 
         tl.col = "n", tl.cex = 1, tl.pos = "n", order = 'AOE',
         add = T, number.cex = 0.5)


 #spearman：
cor_spearman <- cor (cor_data, method="spearman")
cor_spearman_Res = cor.mtest(cor_data, method="spearman", conf.level = 0.95)

addcol <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor_spearman, method = "color",col = addcol(100),
         tl.col = "black", tl.cex = 1, tl.srt = 45,tl.pos = "lt", 
         p.mat=cor_spearman_Res$p, diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.2,
         insig = 'label_sig', pch.col = 'grey30',
         order = 'AOE', addgrid.col = "grey70", cl.cex = 1.2)

corrplot(cor_spearman, method = "number",  diag = FALSE, type = "lower",col = addcol(100), 
         tl.col = "n", tl.cex = 1, tl.pos = "n", order = 'AOE',
         add = T, number.cex = 0.5)
