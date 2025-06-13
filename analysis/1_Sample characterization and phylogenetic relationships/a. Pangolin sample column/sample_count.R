library(ggplot2)
library(dplyr)

rm(list=ls())


sample<-read.csv("Pangolin_sample.csv",fill=TRUE,sep = ",")

summary_sample <- sample %>%
  group_by(Health_condition, Host, Organism) %>%
  summarise(Count = n(), .groups = 'drop')

Host_colors <- c("Manis javanica" = "#3182bd", "Manis pentadactyla" = "#FF69B4")
Organism_order <- c("Anal","Oral","Lung","MMM","Spleen","Fecal","Lymph","Muscle","Skin")
summary_sample$Organism <- factor(summary_sample$Organism,levels = Organism_order)


pdf("Pangolin_samples.pdf", width = 8, height = 8)
ggplot(summary_sample, aes(x = Organism, y = Count, fill = Host)) +
  geom_bar(stat = "identity", position = 'stack', width=0.8) +
  scale_fill_manual(values = Host_colors) +
  theme_minimal() +
  theme(
    axis.text = element_text(size=10),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black"),
    panel.background = element_rect(fill = "white", colour = "black")
  ) +
  labs(title = "Sample Count by Organism and Host",
       x = "Organism",
       y = "Count",
       fill = "Host")

dev.off()
