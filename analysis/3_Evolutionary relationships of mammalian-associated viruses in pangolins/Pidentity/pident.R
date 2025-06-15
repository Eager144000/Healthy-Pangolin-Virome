library(ggplot2)
library(dplyr)

rm(list=ls())

data <-read.csv("Pidentity.csv",header=TRUE)
summary <- data %>%
  group_by(nt_qseqid, nt_saccver, Family, Genus, Species) %>%
  summarise(
    total_nt_length = sum(nt_length),
    total_nt_nident = sum(nt_nident),
    pident = (sum(nt_nident) / sum(nt_length)) * 100
  ) %>%
  ungroup()

p <- ggplot(summary, aes(y = Family, x = pident, fill = Family)) +
  geom_boxplot() +
  scale_fill_manual(values = rep("lightblue", length(unique(summary$Family)))) + 
  scale_x_continuous(breaks = seq(70, 100, by = 5)) +
  theme_minimal() +
  labs(title = "Pident of representative genomes in each viral Family", 
       x = "Pident", 
       y = "Family") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "none"
  )

print(p)
ggsave("Pident of representative genomes in each viral Family.pdf", width = 8, height = 6)

