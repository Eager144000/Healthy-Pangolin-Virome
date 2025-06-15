
library(ggplot2)
library(dplyr)

rm(list=ls())
df <- read.csv("2_result.predictions.csv", header = TRUE)

df <- df %>%
  arrange(Family, Genus, calibrated_score_mean)

df$Species_vOUT_nums <- factor(df$Species_vOUT_nums, levels = df$Species_vOUT_nums)
df$Genus <- factor(df$Genus, levels = unique(df$Genus))

priority_colors <- c("Low" = "#45a9e0", "Medium" = "#efea3a", 
                     "High" = "#f4ba19", "Very High" = "#e71f18")

p <- ggplot(df, aes(y = Species_vOUT_nums, x = calibrated_score_mean)) +
  geom_errorbar(aes(xmin = calibrated_score_lower, xmax = calibrated_score_upper), 
                width = 0.3, color = "black") + 
  geom_point(aes(fill = priority_category), 
             shape = 21, size = 5, color = "black") + 
  scale_fill_manual(values = priority_colors) + 
  scale_x_continuous(limits = c(0, 1.25), expand = c(0, 0)) + 
  labs(x = "Calibrated Score", y = NULL, fill = "Priority") +
  geom_vline(xintercept = 0.293, linetype = "dashed", color = "grey50", size = 0.8) +
  geom_text(aes(x = 1.1, label = Genus), 
            size = 5, hjust = 0, vjust = 0.5) + 
  expand_limits(x = 2) + 

  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 5, hjust = 1),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 5),
    

    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.background = element_rect(color = "black", fill = NA, size = 1)
  )

p
