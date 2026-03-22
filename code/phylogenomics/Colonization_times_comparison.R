# Compare colonization times across the dated phylogenies 

# Load required libraries
library(readxl)  # For reading Excel files
library(ggplot2) # For plotting
library(dplyr)   # For data manipulation
library(stringr) # For string operations
library(patchwork) # For combining plots

# Step 1: Read the Excel file
# Ignoring the "Notes" column
file_path <- "Colonization_times_comparison.xlsx"  # Replace with the correct file path
data <- read_excel(file_path, col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "skip"))

# Step 2: Clean dataset
data_clean <- data %>%
  filter(!is.na(Colonization_time) & !is.na(CI_Max) & !is.na(CI_Min)) %>%  # Remove rows with NA in critical columns
  mutate(
    Dataset = str_replace_all(Dataset, "_", " "),  # Replace underscores with spaces
    Lineage = str_replace_all(Lineage, "_", " ")  # Replace underscores with spaces
  )

# Step 3: Add jitter effect for datasets to spread them vertically within their lineages
data_clean <- data_clean %>%
  group_by(Lineage) %>%
  mutate(Dataset_position = as.numeric(factor(Dataset))) %>% # Assign unique positions to datasets
  ungroup()

# Step 4: Define custom color palette
custom_colors <- c(
  "AsteraceaeWide Hybpiper" = "#c1d9ea", 
  "AsteraceaeWide ParalogFree" = "#84b4d6", 
  "AsteraceaeWide ParagoneMO" = "#468ec1", 
  "IslandMainland Hybpiper" = "#3aac5a", 
  "IslandMainland ParalogFree"  = "#94dd68", 
  "IslandMainland ParagoneMO" = "#bfeba4"
)

# Step 5: Set Dataset order
dataset_order <- c(
  "IslandMainland ParagoneMO",
  "IslandMainland ParalogFree",
  "IslandMainland Hybpiper", 
  "AsteraceaeWide ParagoneMO",
  "AsteraceaeWide ParalogFree",
  "AsteraceaeWide Hybpiper"
)

# Step 6: Modify the Dataset column to factor with the set order
data_clean <- data_clean %>%
  mutate(Dataset = factor(Dataset, levels = dataset_order, ordered = TRUE))

# Step 7: Function to create a dot-and-whisker plot
create_plot <- function(data_subset, title) {
  ggplot(data_subset, aes(x = Colonization_time, 
                          y = factor(Lineage, levels = rev(unique(Lineage))), 
                          color = Dataset, 
                          group = interaction(Lineage, Dataset))) +  # Changed Dataset_position to Dataset
    geom_point(size = 3, position = position_dodge(width = 0.8)) +
    geom_errorbarh(aes(xmin = CI_Min, xmax = CI_Max), height = 0.2, position = position_dodge(width = 0.8)) +
    scale_y_discrete(name = "Lineage") +
    scale_x_continuous(name = "Colonization Time") +
    scale_color_manual(values = custom_colors, breaks = dataset_order, drop = FALSE) +  # Added drop = FALSE
    labs(
      title = title,
      color = "Dataset"
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom",  # Keep the legend at the bottom
      legend.box.margin = margin(0, 0, 0, -20),  # Adjust the legend's position
      legend.text = element_text(size = 8),  # Decrease legend text size
      legend.title = element_text(size = 9),  # Decrease legend title size
      panel.grid.major.x = element_line(color = "grey80"),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.75)  # Adding the box around the plot
    )
}

# Step 8: Generate separate datasets for Hawaii and Mascarenes
data_hawaii <- data_clean %>% filter(Archipelago == "Hawaii")
data_mascarenes <- data_clean %>% filter(Archipelago == "Mascarenes")

# Step 9: Create plots for Hawaii and Mascarenes
plot_hawaii <- create_plot(data_hawaii, "Colonization Times - Hawaii")
plot_mascarenes <- create_plot(data_mascarenes, "Colonization Times - Mascarenes")

# Step 10: Display the plots
print(plot_mascarenes)
print(plot_hawaii)

# Step 11. Save the plots

# Remove individual legends
plot_hawaii_nolegend <- plot_hawaii + theme(legend.position = "none")
plot_mascarenes_nolegend <- plot_mascarenes + theme(legend.position = "none")

# Extract the legend from one of the plots
legend <- cowplot::get_legend(plot_hawaii + theme(legend.position = "bottom"))

# Combine plots vertically and add the shared legend
combined_plot <- plot_hawaii_nolegend / plot_mascarenes_nolegend / cowplot::plot_grid(legend, ncol = 1, rel_heights = c(1, 3, 0.2))
combined_plot

combined_plot <- (plot_hawaii_nolegend + plot_mascarenes_nolegend + 
                    plot_layout(heights = c(1, 2))) / legend + 
  plot_layout(heights = c(10, 1))

ggsave("Colonization_time_comparison_raw.pdf", 
       plot = combined_plot, 
       device = "pdf", 
       width = 8.27,  # A4 width
       height = 11.69,  # A4 height
       units = "in")

