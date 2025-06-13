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
d2_1<- data.frame(d1)
d2 <- d2_1 %>% 
  filter(Health == "Healthy")
d3 <-d2[,c(2,3,7,8)]

d3_clean <- d3 %>% filter(Species != "")


d3_rarefaction <- d3_clean %>%
  arrange(Type, sample) %>%
  group_by(Type) %>%
  mutate(Cumulative_Samples = cumsum(!duplicated(sample)),
         Cumulative_Unique_Species = cumsum(!duplicated(Species)))

p <- ggplot(d3_rarefaction, aes(x = Cumulative_Samples, y = Cumulative_Unique_Species, color = Type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Cumulative_Unique_Species - sqrt(Cumulative_Unique_Species),
                  ymax = Cumulative_Unique_Species + sqrt(Cumulative_Unique_Species),
                  fill = Type), alpha = 0.2) +
  labs(title = "Rarefaction Curve for Oral and Anal Samples",
       x = "Number of Samples",
       y = "Cumulative Unique Virus Species") +
  theme_minimal() +
  scale_color_manual(values = c("#DAA520", "#74c476")) +
  scale_fill_manual(values = c("#DAA520"， "#74c476"))+
  theme(text = element_text(size = 14),
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.border = element_rect(colour = "black", linewidth = 1, fill = NA),
      axis.line = element_line(color = "black", linewidth = 1))
p
ggsave( "Rarefaction Curve for Oral and Anal Samples.pdf", width = 8, height = 5)




