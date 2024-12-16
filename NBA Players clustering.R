# Load necessary libraries
library(readr)
library(dplyr)

# Load the CSV file
nba_data <- read_csv("Book1.csv")

# Remove the Rk, From, To, and Age columns
nba_data <- nba_data %>%
  select(-Rk, -From, -To)

# Display the first few rows of the dataset to understand its structure
head(nba_data)

# Check for missing values
missing_values <- sapply(nba_data, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values)

# Data types of each column
data_types <- sapply(nba_data, class)
print("Data types of each column:")
print(data_types)

# Descriptive statistics for numeric columns
descriptive_stats <- summary(nba_data)
print("Descriptive statistics:")
print(descriptive_stats)

#------------------------------------------------------------------------------#
#Visualisation
# Load necessary libraries for visualization
library(ggplot2)
install.packages("GGally")
install.packages("ggcorrplot")
library(GGally)
library(ggcorrplot)

# Histogram for key metrics
ggplot(nba_data, aes(x = `PTS`)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = 'Distribution of Points Scored', x = 'Points', y = 'Frequency')

#------------------------------------------------------------------------------#
#Correlation Analysis

# Select a subset of relevant features
relevant_features <- nba_data %>%
  select(G, MP, `FG%`, `3P%`, `FT%`, `TS%`, `eFG%`, PF, PTS, WS, ORtg, DRtg, OWS, DWS, `WS/48`, OBPM, DBPM, VORP, PER, `ORB%`, `DRB%`, `AST%`,`STL%`, `BLK%`, `TOV%`, `USG%`)

# Calculate correlation matrix for relevant features
cor_matrix_relevant <- cor(relevant_features, use = "complete.obs")

# Plot correlation matrix for relevant features using ggcorrplot
ggcorrplot(cor_matrix_relevant, lab = TRUE, lab_size = 3, method = "circle", 
           colors = c("blue", "white", "red"), title = "Correlation Matrix of Relevant NBA Metrics")

# Load necessary libraries
library(corrplot)

# Perform hierarchical clustering on the correlation matrix
hc <- hclust(as.dist(1 - cor_matrix_relevant))

# Plot the clustered correlation matrix
corrplot(cor_matrix_relevant, method = "circle", order = "hclust", 
         addrect = 3, # Draw rectangles around the clusters
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, title = "Clustered Correlation Matrix of Relevant NBA Metrics")

# Load necessary libraries
library(GGally)

# Pairwise correlation plots for key performance metrics
key_metrics <- nba_data %>%
  select(PER, PTS, `MP`, `WS`)

# Pairwise correlation plot
ggpairs(key_metrics)

#------------------------------------------------------------------------------#
#Visualisation

# Scatter plot of Points vs. Minutes Played
ggplot(nba_data, aes(x = `MP`, y = `PTS`)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = 'Points vs. Minutes Played', x = 'Minutes Played', y = 'Points')

#------------------------------------------------------------------------------#
#Analyse specific player metric

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Select specific players for analysis
selected_players <- nba_data %>%
  filter(Player %in% c('Shareef Abdur-Rahim', 'Álex Abrines', 'Precious Achiuwa', 'Quincy Acy', 'Steven Adams'))

# Display the data for selected players
print(selected_players)

# Plot Points for selected players
ggplot(selected_players, aes(x = Player, y = PTS, fill = Player)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = 'Total Points by Selected Players', x = 'Player', y = 'Points') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Assists for selected players
ggplot(selected_players, aes(x = Player, y = AST, fill = Player)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = 'Total Assists by Selected Players', x = 'Player', y = 'Assists') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Rebounds for selected players
ggplot(selected_players, aes(x = Player, y = TRB, fill = Player)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = 'Total Rebounds by Selected Players', x = 'Player', y = 'Rebounds') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Player Efficiency Rating (PER) for selected players
ggplot(selected_players, aes(x = Player, y = PER, fill = Player)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = 'Player Efficiency Rating (PER) by Selected Players', x = 'Player', y = 'PER') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Win Shares for selected players
ggplot(selected_players, aes(x = Player, y = `WS`, fill = Player)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = 'Win Shares by Selected Players', x = 'Player', y = 'Win Shares') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Offensive Rating (ORtg) for selected players
ggplot(selected_players, aes(x = Player, y = ORtg, fill = Player)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = 'Offensive Rating (ORtg) by Selected Players', x = 'Player', y = 'Offensive Rating') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Defensive Rating (DRtg) for selected players
ggplot(selected_players, aes(x = Player, y = DRtg, fill = Player)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = 'Defensive Rating (DRtg) by Selected Players', x = 'Player', y = 'Defensive Rating') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load necessary libraries
install.packages("fmsb")
library(fmsb)

# Prepare data for radar chart
radar_data <- selected_players %>%
  select(Player, PTS, AST, TRB, PER, `Win Shares`, ORtg, DRtg) %>%
  column_to_rownames(var = "Player")

# Normalize data for radar chart
radar_data <- as.data.frame(lapply(radar_data, function(x) (x - min(x)) / (max(x) - min(x))))

# Add max and min rows for radar chart scaling
radar_data <- rbind(rep(1, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)

# Plot radar chart
radarchart(radar_data, axistype = 1, pcol = rainbow(5), pfcol = alpha(rainbow(5), 0.5), 
           plwd = 2, plty = 1, title = "Comparison of Selected Players", 
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 1, 0.2))
legend(x = 1.2, y = 1, legend = rownames(radar_data)[-c(1, 2)], bty = "n", pch = 20, 
       col = rainbow(5), text.col = "black", cex = 1.2, pt.cex = 3)

#------------------------------------------------------------------------------#

# Handle missing values by imputing with mean
nba_data <- nba_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Calculate career averages
nba_data <- nba_data %>%
  mutate(
    Career_Avg_PTS = PTS / `G`,
    Career_Avg_AST = AST / `G`,
    Career_Avg_TRB = TRB / `G`
  )

# Ensure that nba_data is a data frame
nba_data <- as.data.frame(nba_data)

# Select relevant features for clustering, including career averages and other specified metrics
features_for_clustering <- nba_data %>%
  select(
    Career_Avg_PTS, Career_Avg_AST, Career_Avg_TRB, PER, `WS`, ORtg, DRtg, `USG%`, 
    `STL%`, `AST%`, `TRB%`, `BLK%`, OBPM, BPM, DBPM, VORP, `WS/48`, OWS, DWS, `FG%`, `3P%`, `FT%`
  )

# Scale the data and ensure it remains a data frame
features_for_clustering_scaled <- as.data.frame(scale(features_for_clustering))

# Display the first few rows of the selected features
head(features_for_clustering_scaled)

# Determine the optimal number of clusters using the Elbow method
set.seed(123)
wss <- sapply(1:15, function(k) {
  kmeans(features_for_clustering_scaled, centers = k, nstart = 10)$tot.withinss
})

library(cluster)
library(factoextra)

fviz_nbclust(features_for_clustering_scaled, kmeans, method = "wss")
fviz_nbclust(features_for_clustering_scaled, kmeans, method = "silhouette")

# Plot the Elbow plot
plot(1:15, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", 
     ylab = "Total within-clusters sum of squares")

# Perform K-Means clustering with the chosen number of clusters (e.g., 5)
set.seed(123)
kmeans_result <- kmeans(features_for_clustering_scaled, centers = 3, nstart = 50, iter.max = 100)

# Add the cluster assignments to the original data
nba_data$cluster <- kmeans_result$cluster

# Display the first few rows of the data with cluster assignments
head(nba_data)

# Perform PCA for visualization
pca_result <- prcomp(features_for_clustering_scaled, scale. = TRUE)

# Create a data frame with PCA results and cluster assignments
pca_data <- data.frame(pca_result$x[, 1:2], cluster = as.factor(nba_data$cluster), Player = nba_data$Player)

# Plot the clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster, label = Player)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text(size = 2, vjust = 1.5) +
  theme_minimal() +
  labs(title = 'Player Clusters', x = 'Principal Component 1', y = 'Principal Component 2')

# Display the player names along with their cluster assignments
player_clusters <- nba_data %>%
  select(Player, cluster) %>%
  arrange(cluster)

# Display the player clusters
print(player_clusters)

# Calculate summary statistics for each cluster
cluster_summary <- nba_data %>%
  group_by(cluster) %>%
  summarize(
    avg_Career_Avg_PTS = mean(Career_Avg_PTS, na.rm = TRUE),
    avg_Career_Avg_AST = mean(Career_Avg_AST, na.rm = TRUE),
    avg_Career_Avg_TRB = mean(Career_Avg_TRB, na.rm = TRUE),
    avg_PER = mean(PER, na.rm = TRUE),
    avg_WS = mean(`WS`, na.rm = TRUE),
    avg_ORtg = mean(ORtg, na.rm = TRUE),
    avg_DRtg = mean(DRtg, na.rm = TRUE),
    avg_USG = mean(`USG%`, na.rm = TRUE),
    avg_STL = mean(`STL%`, na.rm = TRUE),
    avg_AST = mean(`AST%`, na.rm = TRUE),
    avg_TRB = mean(`TRB%`, na.rm = TRUE),
    avg_BLK = mean(`BLK%`, na.rm = TRUE),
    avg_OBPM = mean(OBPM, na.rm = TRUE),
    avg_BPM = mean(BPM, na.rm = TRUE),
    avg_DBPM = mean(DBPM, na.rm = TRUE),
    avg_VORP = mean(VORP, na.rm = TRUE),
    avg_WS48 = mean(`WS/48`, na.rm = TRUE),
    avg_OWS = mean(OWS, na.rm = TRUE),
    avg_DWS = mean(DWS, na.rm = TRUE),
    avg_FG = mean(`FG%`, na.rm = TRUE),
    avg_3P = mean(`3P%`, na.rm = TRUE),
    avg_FT = mean(`FT%`, na.rm = TRUE)
  )

# Display the summary statistics for each cluster
print(cluster_summary)

# Examine cluster centers
cluster_centers <- kmeans_result$centers
print(cluster_centers)

# Silhouette analysis
silhouette_score <- silhouette(kmeans_result$cluster, dist(features_for_clustering_scaled))
fviz_silhouette(silhouette_score)

# Assess cluster stability with multiple runs
set.seed(123)
n_runs <- 10
cluster_assignments <- replicate(n_runs, {
  kmeans(features_for_clustering_scaled, centers = 5, nstart = 10)$cluster
})

# Calculate the adjusted Rand index to measure the similarity between clusterings
library(mclust)
adj_rand_index <- adjustedRandIndex(cluster_assignments[,1], cluster_assignments[,2])
print(adj_rand_index)

# Hierarchical clustering
hc_result <- hclust(dist(features_for_clustering_scaled))
cutree_result <- cutree(hc_result, k = 4)
nba_data$hc_cluster <- cutree_result

# Compare K-means and hierarchical clustering results
table(nba_data$cluster, nba_data$hc_cluster)

# Perform and visualize DBSCAN clustering
library(dbscan)
dbscan_result <- dbscan(features_for_clustering_scaled, eps = 0.5, minPts = 5)
nba_data$dbscan_cluster <- dbscan_result$cluster

# Compare K-means and DBSCAN clustering results
table(nba_data$cluster, nba_data$dbscan_cluster)


# Load necessary library for Random Forest
library(randomForest)

# Select features for prediction
features_for_prediction <- nba_data %>%
  select(Career_Avg_PTS, Career_Avg_AST, Career_Avg_TRB, PER, `WS`, ORtg, DRtg, `USG%`, 
         `STL%`, `AST%`, `TRB%`, `BLK%`, OBPM, BPM, DBPM, VORP, `WS/48`, OWS, DWS, `FG%`, `3P%`, `FT%`)

# Assume 'pick' is the target variable representing the draft pick position
# Randomly splitting data into training and test sets
set.seed(123)
train_indices <- sample(seq_len(nrow(features_for_prediction)), size = 0.7 * nrow(features_for_prediction))
train_data <- features_for_prediction[train_indices, ]
test_data <- features_for_prediction[-train_indices, ]
train_labels <- nba_data$pick[train_indices]
test_labels <- nba_data$pick[-train_indices]

# Train a Random Forest model
rf_model <- randomForest(train_data, y = train_labels, ntree = 100)

# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
print(rf_model)
print(cor(rf_predictions, test_labels))

