# Load libraries
library(tidyverse)
library(ggplot2)
library(ggrepel)  # For non-overlapping labels

# Read in the distance matrix
dist_matrix <- read.csv("~/BIOL7210/snp_distance_matrix.csv", row.names = 1)

# Convert to a numeric matrix
dist_matrix <- as.matrix(dist_matrix)

# Run classical multidimensional scaling (like PCA but on distances)
pca_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)  # k = number of dimensions

# Create a data frame for plotting
pca_df <- as.data.frame(pca_result$points)
colnames(pca_df) <- c("PC1", "PC2")
pca_df$Sample <- rownames(pca_df)

# Set a seed for reproducibility
set.seed(123)

### Step 1: Determine optimal k using Elbow Method ###
# Compute and plot WCSS for different k values
wcss <- numeric(10)  # Test up to k=10 clusters
for (k in 1:10) {
  wcss[k] <- kmeans(pca_df[, c("PC1", "PC2")], centers = k, nstart = 10)$tot.withinss
}

# Plot the Elbow Method
elbow_plot <- data.frame(k = 1:10, WCSS = wcss)
ggplot(elbow_plot, aes(x = k, y = WCSS)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:10)

### Step 2: After visually inspecting the elbow plot, choose optimal k ###

optimal_k <- 2  # Example - adjust based on your elbow plot

### Step 3: Run K-means with optimal k ###
km_res <- kmeans(pca_df[, c("PC1", "PC2")], centers = optimal_k, nstart = 10)

# Add cluster assignments to the data
pca_df$Cluster <- as.factor(km_res$cluster)

### Step 4: Visualize PCA with clusters ###
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  # Add non-overlapping labels using ggrepel
  geom_text_repel(
    aes(label = Sample),
    size = 3,
    max.overlaps = 20,       # Increase if you have many overlapping points
    box.padding = 0.5,       # Adjust padding around labels
    point.padding = 0.3,     # Space between points and labels
    min.segment.length = 0.2, # Shorten the connecting lines
    show.legend = FALSE
  ) +
  stat_ellipse(type = "norm", level = 0.95, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = paste("PCA with K-means Clustering (k =", optimal_k, ")"),
    x = "PC1",
    y = "PC2"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
