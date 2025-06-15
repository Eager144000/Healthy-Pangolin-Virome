library(circlize)
library(dplyr)

rm(list = ls())

recomb_data <- read.csv("recombination_data.csv", stringsAsFactors = FALSE)

seq_lengths <- read.csv("sequence_lengths.csv", stringsAsFactors = FALSE)

seq_colors <- c(
  "Recombinant" = "#df857a", 
  "Major" = "#c53867", 
  "Minor" = "#8b669e" 
)

link_colors <- c(
  "Major" = "#df857a", 
  "Minor" = "#8b669e" 
)

sequence_types <- setNames(rep("Recombinant", nrow(seq_lengths)), seq_lengths$Sequence) 

sequence_types[seq_lengths$Sequence %in% recomb_data$Parent[recomb_data$Parent_type == "Major"]] <- "Major"
sequence_types[seq_lengths$Sequence %in% recomb_data$Parent[recomb_data$Parent_type == "Minor"]] <- "Minor"


seq_lengths <- seq_lengths %>%
  mutate(Type = sequence_types[Sequence],
         Color = seq_colors[Type])

recomb_data <- merge(recomb_data, seq_lengths, by.x = "Recombinant", by.y = "Sequence", all.x = TRUE)
recomb_data <- merge(recomb_data, seq_lengths, by.x = "Parent", by.y = "Sequence", all.x = TRUE, suffixes = c("_recombinant", "_parent"))


circos.clear()
circos.par(
  track.height = 0.08, 
  gap.degree = 2, 
  start.degree = 90 
)


sequence_colors <- setNames(seq_lengths$Color, seq_lengths$Sequence)

recomb_data$Color <- link_colors[recomb_data$Parent_type]


all_sequences <- unique(c(recomb_data$Recombinant, recomb_data$Parent))

new_order <- c("UN_MBA191074-Parvoviridae-1","UN_7D7_2A-Parvoviridae-1", "UN_MBA191037-Parvoviridae-1",
                "OQ363507-Pangolin", 
               "OQ363501-Pangolin", "OQ363502-Pangolin", "MJ_Lymph33-Parvoviridae-1",
               "JN896331-dog","MF926599-Dog", 
               "UN_MBA191024-Paramyxoviridae-1")

all_sequences <- factor(new_order, levels = new_order)

sequence_types <- rep("Recombinant", length(all_sequences))
sequence_types[all_sequences %in% recomb_data$Parent[recomb_data$Parent_type == "Major"]] <- "Major"
sequence_types[all_sequences %in% recomb_data$Parent[recomb_data$Parent_type == "Minor"]] <- "Minor"
sequence_colors <- seq_colors[sequence_types]


circos.initialize(
  factors = all_sequences,
  xlim = cbind(rep(0, length(all_sequences)), seq_lengths$Length[match(all_sequences, seq_lengths$Sequence)])
)


circos.track(
  ylim = c(0, 1), 
  panel.fun = function(x, y) {
    seq_name <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    
    circos.text(
      mean(xlim), 1.2, seq_name, 
      facing = "clockwise", 
      niceFacing = TRUE, 
      adj = c(0, 0.5),
      cex = 0.8
    )
    
   
    circos.axis(
      h = "top",
      labels.cex = 0.5,
      major.at = seq(0, max(xlim), by = 2000),
      minor.ticks = 2,
      labels.niceFacing = TRUE
    )
  },

 bg.col = sequence_colors 
)


for (i in 1:nrow(recomb_data)) {
  circos.link(
    sector.index1 = recomb_data$Recombinant[i], 
    point1 = c(recomb_data$Recombinant_start[i], recomb_data$Recombinant_end[i]),
    sector.index2 = recomb_data$Parent[i], 
    point2 = c(recomb_data$Parent_start[i], recomb_data$Parent_end[i]),
    col = recomb_data$Color[i], 
    border = ifelse(recomb_data$Parent_type[i] == "Major", "#b5b5b6", NA),
    lwd = 2
  )
}

