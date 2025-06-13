library(ggplot2)
library(dplyr)

rm(list=ls())
data <- read.csv("Virues_Species_prevalence_table.csv",header =TRUE)

data1 <- group_by(data, Type, Species)%>%
  summarise(Species_Count = sum(Species != "None"), .groups = "drop" )

data2 <- filter(data1, Species != "None")

data3 <- data2 %>%
  mutate(Percentage = if_else(
    Type == "Anal",
    -round((Species_Count / 82) * 100, 2),
    round((Species_Count / 82) * 100, 2)
  ))
data3 <- data3[order(data3$Percentage), ] 
Species_order<-unique(rev(data3$Species))
data3$Species <-factor(data3$Species, levels = Species_order)
Type_colors <- c("Oral" = "#74c476", "Anal" = "#DAA520")

p <- ggplot(data3, aes(x = Species, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0), width = 0.7) +
  scale_fill_manual(values = Type_colors) +
  theme_minimal() +
  labs(title = "Virus Prevalence by Organism Type",
       x = "Species",
       y = "Prevalence") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0,size=5),
        panel.border = element_rect(colour = "black", linewidth = 1, fill = NA),
        axis.line = element_line(color = "black", linewidth = 1))

p
ggsave( "Virus Speices Prevalence by healthy pangolin Organims Type.pdf", width = 12, height = 5)


rm(list=ls())
data <- read.csv("Virues_Family_prevalence.csv",header =TRUE)

data1 <- group_by(data, Type, Family)%>%
  summarise(Family_Count = sum(Family != "None"), .groups = "drop" )

data2 <- filter(data1, Family != "None")

data3 <- data2 %>%
  mutate(Percentage = if_else(
    Type == "Anal",
    -round((Family_Count / 82) * 100, 2),
    round((Family_Count / 82) * 100, 2)
  ))
data3 <- data3[order(data3$Percentage), ] 
Family_order<-unique(rev(data3$Family))
data3$Family <-factor(data3$Family, levels = Family_order)
write.csv(data3, file = "Virues_Family_prevelence_table.csv", row.names = FALSE)
Type_colors <- c("Oral" = "#74c476", "Anal" = "#DAA520")


#画图
p <- ggplot(data3, aes(x = Family, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0), width = 0.7) +
  scale_fill_manual(values = Type_colors) +
  theme_minimal() +
  labs(title = "Virus Prevalence by Organism Type",
       x = "Family",
       y = "Prevalence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", linewidth = 1, fill = NA),
        axis.line = element_line(color = "black", linewidth = 1))

p
ggsave( "Virus Family Prevalence by healthy Pangolin Organims Type.pdf", width = 5, height = 5)
