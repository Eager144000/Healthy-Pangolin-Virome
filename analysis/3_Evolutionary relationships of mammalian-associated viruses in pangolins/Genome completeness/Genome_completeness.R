library(ggplot2)
library(dplyr)

rm(list=ls())

completeness_data <- read.csv("completeness.csv",header = TRUE)

Family_counts <- completeness_data %>%
  group_by(Family,aai_confidence) %>%
  summarise(Count =n(), .groups = 'drop')

p <- ggplot(Family_counts, aes(x = Count, y = Family, fill = aai_confidence)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  scale_fill_manual(values = c("High" = "#567B9E", "Medium" = "#E8A02A", "Not determined" = "#AF77A3"))+
  labs(title = "Distribution of Virus Families",
       x = "Number of viral strain", y = "", fill = "Genome completeness") +
  theme_minimal() + 
  theme(
  panel.border = element_rect(colour = "black", fill = NA, size = 1), 
  axis.text.x = element_text(size = 12), 
  axis.text.y = element_text(size = 12), 
  axis.title.x = element_text(size = 14), 
  axis.title.y = element_text(size = 14), 
  legend.title = element_text(size = 12),  
  legend.text = element_text(size = 10)  
)
p
ggsave("Completeness of representative genomes in each viral family.pdf", width = 8, height = 6)

