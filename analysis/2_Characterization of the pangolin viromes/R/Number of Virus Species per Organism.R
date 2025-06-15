library(ggplot2)
library(dplyr)

rm(list=ls())
data <- read.csv("virus_prevalance.csv",header =TRUE)


data <- data %>%
  mutate(Species = ifelse(is.na(Species) | Species == "", NA, Species))


sample_virus_count <- data %>%
  group_by(sample, Type) %>%
  summarise(Virus_Count = sum(!is.na(Species)), .groups = 'drop')

sample_virus_count <- sample_virus_count %>%
  mutate(Type = factor(Type, levels = c( "Anal","Oral")))


ggplot(sample_virus_count, aes(y = Type, x = Virus_Count, fill = Type)) +
  geom_boxplot(width = 0.5, alpha = 0.6, outlier.color = "red", outlier.shape = 16) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 18, 
    size = 3, 
    color = "black"
  ) +
  labs(
    x = "Number of Virus Species",
    y = "",
    title = "Number of Virus Species per Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "italic", size = 12),
    axis.line = element_line(color = "black", linewidth = 0.8),
    panel.border = element_blank()
  ) +
  scale_fill_manual(values = c( "#DAA520", "#74C476"))
ggsave( "Number of Virus Species per Type.pdf", width = 7, height = 5)

