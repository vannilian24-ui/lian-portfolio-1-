# Portfolio 1 R Script
# Author: Lian

# Project: Soil Analysis 
printf("The study of Arylsulfatae activity in different soil types")


# Plot the numbers
# Uncomment the next line if running in an environment that supports plotting
# plot(numbers, type='b', col='blue', main='Sample Plot', xlab='Index', ylab='Value')
install.packages("data.table")
install.packages("tidyverse")
install.packages("modeldata")
install.packages("plotly")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("multcompView")
install.packages("dplyr")
install.packages("datasets")


library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)
library(readxl)
library(modeldata)
library(data.table)
library(ggpubr)
library(rlang)
library(cowplot)
library(multcompView)
library(dplyr)
library(datasets)


data <- read_excel("C:/Users/vanni/OneDrive/Desktop/ARS file/ARS.xlsx")
ARS <- copy(data)
str(ARS)
View(ARS)
glimpse(ARS)


# Rename 'Soil Type' to 'Soil_Type'
colnames(ARS) <- gsub(" ", "_", colnames(ARS))
colnames(ARS)

# Check the new column names
colnames(ARS)

# Now you can refer to 'Soil_Type' without backticks
ARS$Soil_Type

glimpse(ARS)
# Ensure 'Soil Type' is a factor
ARS$Soil_Type <- as.factor(ARS$Soil_Type)
################################################################################
yellow_colors <- c("#FFFF00", "#FFD700", "#FFFFE0", "#FFFACD", "#F0E68C", "#BDB76B", "#DAA520", "#EEDD82", "#EEE8AA")

colors <- c("#008000", "#006400", "#228B22", "#2E8B57", "#32CD32", "#00FF7F", "#90EE90", "#98FB98", "#7FFF00")

red_colors <- c("#FF0000", "#8B0000", "#B22222", "#CD5C5C", "#FF4500", "#FF6347", "#FF7F50", "#FA8072", "#F08080")

blue_colors <- c("#0000FF", "#00008B", "#000080", "#4169E1", "#87CEEB", "#4682B4", "#00BFFF", "#1E90FF", "#ADD8E6")
################################################################################
#Kruskal-Wallis Test
# Custom colors for the boxplot
custom_plot_colors <- c("AEG" =  "#CD5C5C" , "HEG" =   "#BDB76B" , "SEG" =   "#4682B4")

# 1. Kruskal-Wallis test (non-parametric alternative to one-way ANOVA)
kruskal_res <- kruskal.test(ARS_Activity ~ Plot, data = ARS)
print(kruskal_res)

# 2. Dunn's post hoc test for pairwise comparisons
if (!require(dunn.test)) {
  install.packages("dunn.test")
  library(dunn.test)
}
dunn_res <- dunn.test::dunn.test(ARS$ARS_Activity, ARS$Plot, method = "bh")  # Using Benjamini-Hochberg adjustment
print(dunn_res)

# Optional: Creating a compact letter display for Dunn's test
if (!require(multcompView)) {
  install.packages("multcompView")
  library(multcompView)
}

# Create a p-value matrix from Dunn's test results.
# Note: dunn_res returns a list with a component 'P.adjusted'
comparisons <- dunn_res$comparisons  # Comparisons are provided as "Group1 - Group2"
p_values <- dunn_res$P.adjusted
p_values
# Create a matrix for the groups
groups <- unique(ARS$Plot)
pval_matrix <- matrix(1, nrow = length(groups), ncol = length(groups),
                      dimnames = list(groups, groups))
for(i in seq_along(p_values)) {
  # Extract group names from the comparison string
  comp <- unlist(strsplit(comparisons[i], " - "))
  g1 <- comp[1]
  g2 <- comp[2]
  pval_matrix[g1, g2] <- p_values[i]
  pval_matrix[g2, g1] <- p_values[i]
}

# Generate compact letter display from the p-value matrix
cld_letters <- multcompView::multcompLetters(pval_matrix)$Letters

# Create a data frame with the letters
letters_plot <- data.frame(
  Plot = names(cld_letters),
  Letters = as.character(cld_letters)
)

# Merge letters with the ARS dataset (requires dplyr)
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
ARS_plot <- left_join(ARS, letters_plot, by = "Plot")
ARS_plot

# Compute maximum ARS_Activity per plot for text placement on the plot
plot_ARS_max <- ARS_plot %>%
  group_by(Plot) %>%
  summarise(max_y_value = max(ARS_Activity, na.rm = TRUE) * 1.05)

# Merge the maximum values with letters_plot
letters_plot <- left_join(letters_plot, plot_ARS_max, by = "Plot")
letters_plot
# Boxplot with compact letter display annotations from Dunn's test
ARS_plot_fig <- ggplot(ARS_plot, aes(x = Plot, y = ARS_Activity, fill = Plot)) + 
  geom_boxplot(outlier.shape = NA, color = "black") + 
  labs(
    title = "Arylsulfatase Activity Across Three Study Regions",
    x = "Region",
    y = expression("Arylsulfatase Enzyme Activity (" * mu * g ~ g^-1 ~ h^-1 * ")")
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 28, face = "bold", margin = margin(t = 30)),
    axis.title.y = element_text(size = 28, face = "bold", margin = margin(r = 14)),
    axis.text.x = element_text(angle = 24, hjust = 1, size = 24, face = "bold"),
    axis.text.y = element_text(size = 24, face = "bold"),
    legend.position = "none",
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
  ) + 
  scale_fill_manual(values = custom_plot_colors) + 
  geom_text(
    data = letters_plot,
    aes(x = Plot, y = max_y_value, label = Letters),  # Dynamically calculated Y position
    size = 11,
    vjust = 1.0
  )

# Display the boxplot
print(ARS_plot_fig)

# Save the plot as a PNG file with specified dimensions and resolution
ggsave(filename = "ARS_plot_fig.png", plot = ARS_plot_fig, width = 12, height = 10, dpi = 500)

#For smaller size
ggsave(filename = "figure.png", plot = ARS_plot_fig, width = 7, height = 5, dpi = 300)

################################################################################
##Non-parametric Analysis: Kruskal-Wallis & Dunn's Test ------------------

# 1. Overall test using Kruskal-Wallis (does not assume normality)
kruskal_res <- kruskal.test(ARS_Activity ~ Soil_Type, data = ARS)
print(kruskal_res)

# 2. Post hoc pairwise comparisons using Dunn's test with Benjamini-Hochberg adjustment
if (!require(dunn.test)) {
  install.packages("dunn.test")
  library(dunn.test)
}
dunn_res <- dunn.test::dunn.test(ARS$ARS_Activity, ARS$Soil_Type, method = "bh")
print(dunn_res)

# 3. Create a p-value matrix from Dunn's test results for compact letter display
#    Extract comparisons (e.g., "Cambisol - Gleysol") and adjusted p-values.
comparisons <- dunn_res$comparisons  
p_values   <- dunn_res$P.adjusted

# Get all unique groups
groups <- unique(ARS$Soil_Type)
# Create a square matrix of p-values (initialize with 1's)
pval_matrix <- matrix(1, nrow = length(groups), ncol = length(groups),
                      dimnames = list(groups, groups))
# Fill the matrix with the adjusted p-values from Dunn's test
for(i in seq_along(p_values)) {
  comp <- unlist(strsplit(comparisons[i], " - "))
  g1 <- comp[1]
  g2 <- comp[2]
  pval_matrix[g1, g2] <- p_values[i]
  pval_matrix[g2, g1] <- p_values[i]
}

# Generate compact letter display using multcompView
if (!require(multcompView)) {
  install.packages("multcompView")
  library(multcompView)
}
letters_compact <- multcompView::multcompLetters(pval_matrix)$Letters

# Create a data frame for the letters
letters_soil <- data.frame(
  Soil_Type = names(letters_compact),
  Letters = as.character(letters_compact)
)
print(letters_soil)

#--- Merge Letters with Data & Summarise for Plot Annotation ------------------

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Merge the compact letters with the ARS dataset by Soil_Type
ARS <- left_join(ARS, letters_soil, by = "Soil_Type")

# Compute the maximum ARS_Activity per Soil_Type for text placement
max_vals <- ARS %>%
  group_by(Soil_Type) %>%
  summarise(max_ARS = max(ARS_Activity, na.rm = TRUE))

# Merge these max values with the letters data frame
letters_soil <- left_join(letters_soil, max_vals, by = "Soil_Type")
# Adjust the maximum for display (e.g., 5% above the max value)
letters_soil <- letters_soil %>% 
  mutate(max_ARS = max_ARS * 1.05)

#--- Boxplot with Dunn's Letters ------------------------------------------------

library(ggplot2)
# Define custom colors for the soil types
custom_colors <- c(
  "Albeluvisol" = "#A6D854",
  "Leptosol"    = "#8B4513",
  "Stagnosol"   = "#56B4E9",
  "Cambisol"    = "#E69F00",
  "Gleysol"     = "#0000FF",
  "Histosol"    = "#000000",
  "Luvisol"     = "#D55E00",
  "Vertisol"    = "#006400"
)

soil_plot <- ggplot(ARS, aes(x = Soil_Type, y = ARS_Activity, fill = Soil_Type)) + 
  geom_boxplot(outlier.shape = NA, color = "red") + 
  labs(
    title = "Arylsulfatase Activity Across Soil Types",
    x = "Soil Type",
    y = expression("Arylsulfatase Enzyme Activity (" * mu * g ~ g^-1 ~ h^-1 * ")")
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5, vjust = 1.0),
    axis.title.x = element_text(size = 26, face = "bold", margin = margin(t = 24)),
    axis.title.y = element_text(size = 26, face = "bold", margin = margin(r = 14)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, face = "bold"),
    axis.text.y = element_text(size = 22),
    legend.position = "none",
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
  ) + 
  scale_fill_manual(values = custom_colors) + 
  geom_text(
    data = letters_soil,
    mapping = aes(x = Soil_Type, y = max_ARS, label = Letters),
    size = 11,
    vjust = 0.7
  )

# Display the boxplot
print(soil_plot)

# Save the plot as a PNG file with specified dimensions and resolution
ggsave(filename = "ARS_soil_type.png", plot = soil_plot, width = 12, height = 10, dpi = 500)
#################################################################################

################################################################################
#AEG SOil 
# Load necessary packages
library(ggplot2)
library(dplyr)
library(multcompView)
library(dunn.test)

#Mann–Whitney U test
library(ggplot2)
library(dplyr)
library(multcompView)  # For consistent letter display, if needed later

# Filter only AEG plot
ARS_AEG <- ARS %>% filter(Plot == "AEG")
print(ARS_AEG)

# Check number of unique soil types
unique_soils <- unique(ARS_AEG$Soil_Type)
num_groups <- length(unique_soils)
cat("Number of Soil_Type groups in AEG:", num_groups, "\n")

if(num_groups == 2) {
  # For two groups, use the Wilcoxon rank-sum test
  wilcox_result <- wilcox.test(ARS_Activity ~ Soil_Type, data = ARS_AEG)
  print(wilcox_result)
  
  # Compute the medians for each group
  group_medians <- ARS_AEG %>%
    group_by(Soil_Type) %>%
    summarise(median_val = median(ARS_Activity, na.rm = TRUE)) %>%
    arrange(median_val)
  print(group_medians)
  
  # Create a simple letter assignment: if significant, assign "a" to the group with the lower median and "b" to the other.
  if(wilcox_result$p.value < 0.05) {
    letter_df <- data.frame(
      Soil_Type = group_medians$Soil_Type,
      Letters = c("a", "b")
    )
  } else {
    # If not significant, assign the same letter to both groups
    letter_df <- data.frame(
      Soil_Type = group_medians$Soil_Type,
      Letters = c("a", "a")
    )
  }
  
} else if(num_groups > 2) {
  # For three or more groups, use the Kruskal-Wallis test
  kw_result <- kruskal.test(ARS_Activity ~ Soil_Type, data = ARS_AEG)
  print(kw_result)
  
  # And use Dunn's test with BH adjustment for pairwise comparisons
  dunn_result <- dunn.test::dunn.test(ARS_AEG$ARS_Activity, ARS_AEG$Soil_Type, method = "bh")
  print(dunn_result)
  
  # Build a p-value matrix from Dunn's test results:
  groups <- unique(ARS_AEG$Soil_Type)
  pval_matrix <- matrix(1, nrow = length(groups), ncol = length(groups),
                        dimnames = list(groups, groups))
  
  comp_vec <- if (is.matrix(dunn_result$comparisons)) {
    as.vector(dunn_result$comparisons)
  } else {
    dunn_result$comparisons
  }
  
  for (i in seq_along(dunn_result$P.adjusted)) {
    comp <- unlist(strsplit(comp_vec[i], " - "))
    g1 <- comp[1]
    g2 <- comp[2]
    pval_matrix[g1, g2] <- dunn_result$P.adjusted[i]
    pval_matrix[g2, g1] <- dunn_result$P.adjusted[i]
  }
  
  # Generate the compact letter display using the p-value matrix
  letters_compact <- multcompView::multcompLetters(as.matrix(pval_matrix))$Letters
  
  # Create a data frame for the letters (only for the soil types in AEG)
  letter_df <- data.frame(
    Soil_Type = names(letters_compact),
    Letters = as.character(letters_compact)
  ) %>% filter(Soil_Type %in% unique(ARS_AEG$Soil_Type))
}

print(letter_df)

# Merge the letters with the ARS_AEG dataset
ARS_AEG <- left_join(ARS_AEG, letter_df, by = "Soil_Type")

# Compute the maximum ARS_Activity for text placement (adjust upward by 5%)
max_y_value <- max(ARS_AEG$ARS_Activity, na.rm = TRUE) * 1.05

# Define custom colors (adjust as needed)
custom_colors <- c(
  "Albeluvisol" = "#A6D854",
  "Leptosol"    = "#8B4513",
  "Stagnosol"   = "#56B4E9",
  "Cambisol"    = "#E69F00",
  "Gleysol"     = "#0000FF",
  "Histosol"    = "#000000",
  "Luvisol"     = "#D55E00",
  "Vertisol"    = "#006400"
)

# Create the boxplot and annotate with the letter display
AEGsoil_plot <- ggplot(ARS_AEG, aes(x = Soil_Type, y = ARS_Activity, fill = Soil_Type)) + 
  geom_boxplot(outlier.shape = NA, color = "red") + 
  labs(
    title = "Arylsulfatase Activity by Soil Type in AEG",
    x = "Soil Type",
    y = expression("Arylsulfatase Enzyme Activity (" * mu * g ~ g^-1 ~ h^-1 * ")")
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 3.0),
    axis.title.x = element_text(size = 25, face = "bold", margin = margin(t = 14)),
    axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(r = 25)),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 25, face = "bold"),
    axis.text.y = element_text(size = 20),
    legend.position = "none",
    plot.margin = unit(c(2.0, 2.0, 2.0, 2.0), "cm")
  ) + 
  scale_fill_manual(values = custom_colors) + 
  geom_text(
    data = letter_df,
    aes(x = Soil_Type, y = max_y_value, label = Letters),
    size = 11,
    vjust = 1.0
  )

# Display the plot
print(AEGsoil_plot)

# Save the plot as a PNG file with specified dimensions and resolution
ggsave(filename = "AEGsoil_plot.png", plot = AEGsoil_plot, width = 12, height = 10, dpi = 500)
################################################################################
#HEG Soil Type
# Filter only HEG plot
ARS_HEG <- ARS %>% filter(Plot == "HEG")
print(ARS_HEG)

# 1. Overall test using the Kruskal-Wallis test (non-parametric alternative)
kw_result_HEG <- kruskal.test(ARS_Activity ~ Soil_Type, data = ARS_HEG)
print(kw_result_HEG)

# 2. Post hoc pairwise comparisons using Dunn's test with Benjamini-Hochberg adjustment
# Use Dunn's test by providing the response and grouping directly
dunn_result_HEG <- dunn.test::dunn.test(ARS_HEG$ARS_Activity, ARS_HEG$Soil_Type, method = "bh")
print(dunn_result_HEG)

# Build a p-value matrix from Dunn's test results:
groups <- unique(ARS_HEG$Soil_Type)
pval_matrix <- matrix(1, nrow = length(groups), ncol = length(groups),
                      dimnames = list(groups, groups))

# Ensure the 'comparisons' element is a vector:
comp_vec <- if (is.matrix(dunn_result_HEG$comparisons)) {
  as.vector(dunn_result_HEG$comparisons)
} else {
  dunn_result_HEG$comparisons
}

# Fill the p-value matrix using the converted vector
for (i in seq_along(dunn_result_HEG$P.adjusted)) {
  comp <- unlist(strsplit(comp_vec[i], " - "))
  g1 <- comp[1]
  g2 <- comp[2]
  pval_matrix[g1, g2] <- dunn_result_HEG$P.adjusted[i]
  pval_matrix[g2, g1] <- dunn_result_HEG$P.adjusted[i]
}

# Generate the compact letter display ensuring pval_matrix is a matrix:
letters_compact <- multcompView::multcompLetters(as.matrix(pval_matrix))$Letters
print(letters_compact)

# Create a data frame for the letters (for HEG soil types only)
letters_HEGsoil <- data.frame(
  Soil_Type = names(letters_compact),
  Letters = as.character(letters_compact)
) %>% filter(Soil_Type %in% unique(ARS_HEG$Soil_Type))

# Merge the compact letters with the ARS_HEG dataset
ARS_HEG <- left_join(ARS_HEG, letters_HEGsoil, by = "Soil_Type")

# Compute the maximum ARS_Activity for text placement (adjusted upward by 5%)
HEGmax_y_value <- max(ARS_HEG$ARS_Activity, na.rm = TRUE) * 1.05

# Define custom colors (adjust as needed)
custom_colors <- c(
  "Albeluvisol" = "#A6D854",
  "Leptosol"    = "#8B4513",
  "Stagnosol"   = "#56B4E9",
  "Cambisol"    = "#E69F00",
  "Gleysol"     = "#0000FF",
  "Histosol"    = "#000000",
  "Luvisol"     = "#D55E00",
  "Vertisol"    = "#006400"
)

# 3. Create the boxplot and annotate with the compact letter display
HEGsoil_plot <- ggplot(ARS_HEG, aes(x = Soil_Type, y = ARS_Activity, fill = Soil_Type)) + 
  geom_boxplot(outlier.shape = NA, color = "red") + 
  labs(
    title = "Arylsulfatase Activity by Soil Type in HEG",
    x = "Soil Type",
    y = expression("Arylsulfatase Enzyme Activity (" * mu * g ~ g^-1 ~ h^-1 * ")")
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 1.5),
    axis.title.x = element_text(size = 25, face = "bold", margin = margin(t = 18)),
    axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 24, face = "bold"),
    axis.text.y = element_text(size = 22),
    legend.position = "none",
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
  ) + 
  scale_fill_manual(values = custom_colors) + 
  geom_text(
    data = letters_HEGsoil,
    aes(x = Soil_Type, y = HEGmax_y_value, label = Letters),
    size = 11,
    vjust = 2.0
  )

# Print the plot
print(HEGsoil_plot)

# Save the plot as a PNG file with specified dimensions and resolution
ggsave(filename = "HEGsoil_plot.png", plot = HEGsoil_plot, width = 12, height = 10, dpi = 500)
####################################################################################
#SEG Soil 
# Filter only SEG plot
ARS_SEG <- ARS %>% filter(Plot == "SEG")
print(ARS_SEG)

# 1. Overall test using the Kruskal-Wallis test (non-parametric alternative)
kw_result_SEG <- kruskal.test(ARS_Activity ~ Soil_Type, data = ARS_SEG)
print(kw_result_SEG)

# 2. Post hoc pairwise comparisons using Dunn's test with Benjamini-Hochberg adjustment
dunn_result_SEG <- dunn.test::dunn.test(ARS_SEG$ARS_Activity, ARS_SEG$Soil_Type, method = "bh")
print(dunn_result_SEG)

# Build a p-value matrix from Dunn's test results:
# Get the unique soil groups from the SEG subset
groups <- unique(ARS_SEG$Soil_Type)

# Initialize a square matrix with 1's (assuming no significant difference by default)
pval_matrix <- matrix(1, nrow = length(groups), ncol = length(groups),
                      dimnames = list(groups, groups))

# Ensure the 'comparisons' element is a vector:
comp_vec <- if (is.matrix(dunn_result_SEG$comparisons)) {
  as.vector(dunn_result_SEG$comparisons)
} else {
  dunn_result_SEG$comparisons
}

# Fill the p-value matrix using the converted vector
for (i in seq_along(dunn_result_SEG$P.adjusted)) {
  comp <- unlist(strsplit(comp_vec[i], " - "))
  g1 <- comp[1]
  g2 <- comp[2]
  pval_matrix[g1, g2] <- dunn_result_SEG$P.adjusted[i]
  pval_matrix[g2, g1] <- dunn_result_SEG$P.adjusted[i]
}

# Generate the compact letter display ensuring pval_matrix is a matrix:
letters_compact <- multcompView::multcompLetters(as.matrix(pval_matrix))$Letters
print(letters_compact)

# Create a data frame for the letters (for SEG soil types only)
letters_SEGsoil <- data.frame(
  Soil_Type = names(letters_compact),
  Letters = as.character(letters_compact)
) %>% filter(Soil_Type %in% unique(ARS_SEG$Soil_Type))

# Merge the compact letters with the ARS_SEG dataset
ARS_SEG <- left_join(ARS_SEG, letters_SEGsoil, by = "Soil_Type")

# Compute the maximum ARS_Activity for text placement (adjusted upward by 5%)
SEGmax_y_value <- max(ARS_SEG$ARS_Activity, na.rm = TRUE) * 1.05

# Define custom colors (ensure 'custom_colors' is defined as needed)
custom_colors <- c(
  "Albeluvisol" = "#A6D854",
  "Leptosol"    = "#8B4513",
  "Stagnosol"   = "#56B4E9",
  "Cambisol"    = "#E69F00",
  "Gleysol"     = "#0000FF",
  "Histosol"    = "#000000",
  "Luvisol"     = "#D55E00",
  "Vertisol"    = "#006400"
)

# 3. Create the boxplot and annotate with the compact letter display
SEGsoil_plot <- ggplot(ARS_SEG, aes(x = Soil_Type, y = ARS_Activity, fill = Soil_Type)) + 
  geom_boxplot(outlier.shape = NA, color = "red") + 
  labs(
    title = "Arylsulfatase Activity by Soil Type in SEG",
    x = "Soil Type",
    y = expression("Arylsulfatase Enzyme Activity (" * mu * g ~ g^-1 ~ h^-1 * ")")
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 1.5),
    axis.title.x = element_text(size = 25, face = "bold", margin = margin(t = 18)),
    axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 24, face = "bold"),
    axis.text.y = element_text(size = 22),
    legend.position = "none",
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
  ) + 
  scale_fill_manual(values = custom_colors) + 
  geom_text(
    data = letters_SEGsoil,
    aes(x = Soil_Type, y = SEGmax_y_value, label = Letters),
    size = 11,
    vjust = 1.0
  )

# Print the plot
print(SEGsoil_plot)

# Save the plot as a PNG file with specified dimensions and resolution
ggsave(filename = "SEGsoil_plot.png", plot = SEGsoil_plot, width = 12, height = 10, dpi = 500)

################################################################################
##RECheck
#SEG Soil 
# Filter only SEG plot
ARS_SEG <- ARS %>% filter(Plot == "SEG")
print(ARS_SEG)

# 1. Overall test using the Kruskal-Wallis test (non-parametric alternative)
kw_result_SEG <- kruskal.test(ARS_Activity ~ Soil_Type, data = ARS_SEG)
print(kw_result_SEG)

# 2. Post hoc pairwise comparisons using Dunn's test with Benjamini-Hochberg adjustment
dunn_result_SEG <- dunn.test::dunn.test(ARS_SEG$ARS_Activity, ARS_SEG$Soil_Type, method = "bh")
print(dunn_result_SEG)

# Build a p-value matrix from Dunn's test results:
# Get the unique soil groups from the SEG subset
groups <- unique(ARS_SEG$Soil_Type)

# Initialize a square matrix with 1's (assuming no significant difference by default)
pval_matrix <- matrix(1, nrow = length(groups), ncol = length(groups),
                      dimnames = list(groups, groups))

# Ensure the 'comparisons' element is a vector:
comp_vec <- if (is.matrix(dunn_result_SEG$comparisons)) {
  as.vector(dunn_result_SEG$comparisons)
} else {
  dunn_result_SEG$comparisons
}

# Fill the p-value matrix using the converted vector
for (i in seq_along(dunn_result_SEG$P.adjusted)) {
  comp <- unlist(strsplit(comp_vec[i], " - "))
  g1 <- comp[1]
  g2 <- comp[2]
  pval_matrix[g1, g2] <- dunn_result_SEG$P.adjusted[i]
  pval_matrix[g2, g1] <- dunn_result_SEG$P.adjusted[i]
}

# Generate the compact letter display ensuring pval_matrix is a matrix:
letters_compact <- multcompView::multcompLetters(as.matrix(pval_matrix))$Letters
print(letters_compact)

# Create a data frame for the letters (for SEG soil types only)
letters_SEGsoil <- data.frame(
  Soil_Type = names(letters_compact),
  Letters = as.character(letters_compact)
) %>% filter(Soil_Type %in% unique(ARS_SEG$Soil_Type))

# Merge the compact letters with the ARS_SEG dataset
ARS_SEG <- left_join(ARS_SEG, letters_SEGsoil, by = "Soil_Type")

# Compute the maximum ARS_Activity for text placement (adjusted upward by 5%)
SEGmax_y_value <- max(ARS_SEG$ARS_Activity, na.rm = TRUE) * 1.05

# Define custom colors (ensure 'custom_colors' is defined as needed)
custom_colors <- c(
  "Albeluvisol" = "#A6D854",
  "Leptosol"    = "#8B4513",
  "Stagnosol"   = "#56B4E9",
  "Cambisol"    = "#E69F00",
  "Gleysol"     = "#0000FF",
  "Histosol"    = "#000000",
  "Luvisol"     = "#D55E00",
  "Vertisol"    = "#006400"
)

# 3. Create the boxplot and annotate with the compact letter display
SEGsoil_plot <- ggplot(ARS_SEG, aes(x = Soil_Type, y = ARS_Activity, fill = Soil_Type)) + 
  geom_boxplot(outlier.shape = NA, color = "red") + 
  labs(
    title = "Arylsulfatase Activity by Soil Type in SEG",
    x = "Soil Type",
    y = expression("Arylsulfatase Enzyme Activity (" * mu * g ~ g^-1 ~ h^-1 * ")")
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = 1.5),
    axis.title.x = element_text(size = 25, face = "bold", margin = margin(t = 18)),
    axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 24, face = "bold"),
    axis.text.y = element_text(size = 22),
    legend.position = "none",
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
  ) + 
  scale_fill_manual(values = custom_colors) + 
  geom_text(
    data = letters_SEGsoil,
    aes(x = Soil_Type, y = SEGmax_y_value, label = Letters),
    size = 11,
    vjust = 2.0
  )

# Print the plot
print(SEGsoil_plot)
