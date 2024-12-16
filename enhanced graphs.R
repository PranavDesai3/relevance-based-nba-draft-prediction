# Load necessary libraries
library(ggplot2)
library(scales)  # For better scaling of numbers

# Create data frame
df <- data.frame(
  Model = rep(c("Random Forest", "Neural Network", "Bayesian Regression"), 
              times=c(5*3, 4*3, 6*3)),
  Configuration = c(
    rep(c("n=500", "n=250", "n=100", "n=50", "n=10"), each=3),
    rep(c("size=5, iterations=1000", "size=6, iterations=2000", 
          "size=10, iterations=5000", "size=15, iterations=10000"), each=3),
    rep(c("chain=4, iter=2000, normal (0,1)", "chain=4, iter=2000, normal (0,2)", 
          "chain=4, iter=200, normal (0,3)", "chain=8. iter=400, normal(0,1)", 
          "chain=8. iter=400, normal(0,2)", "chain=8. iter=400, normal(0,3)"), each=3)
  ),
  Runtime = c(
    # Random Forest Runtimes for PER, WS, VORP
    6270, 6233, 6064, 2973, 3326, 2836, 1230, 1262, 945, 
    619, 580, 543, 240, 228, 231,
    # Neural Networks Runtimes for PER, WS, VORP
    45, 84, 73, 177, 256, 226, 711, 874, 696, 
    1130, 1357, 1078,
    # Bayesian Regressions Runtimes for PER, WS, VORP
    287, 356, 288, 633, 721, 617, 1374, 1583, 1420, 
    544, 698, 604, 1470, 1362, 1317, 2657, 3331, 2798
  ),
  Metric = rep(c("PER", "WS", "VORP"), times = 15 + 12 + 18)
)

# Generate bar graph using ggplot2 with customized tooltip font
ggplot(df, aes(x=Configuration, y=Runtime, fill=Metric)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
  geom_text(aes(label=Runtime), 
            position=position_dodge(width=0.7), 
            vjust=-0.5, 
            size=2.0,
            color="black",      # Text color
            family="Arial") +   # Font family
  facet_wrap(~ Model, scales = "free_x") +
  labs(title="Runtime Comparison of Different Model Configurations", 
       subtitle="Evaluating performance across Random Forest, Neural Network, and Bayesian Regression",
       x="Configurations", y="Runtime (seconds)", fill="Metric") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +  # Custom color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face="bold", size=16),
        plot.subtitle = element_text(hjust = 0.5, size=12))

#-------------------------------------------------------------------------------#

# Create a dataframe with the results for each model and each metric
df_performance <- data.frame(
  Metric = rep(c("RMSE", "MAE", "R-Squared"), times = 3),
  Model = c(rep("Random Forest", 3), rep("Neural Network", 3), rep("Bayesian Regression", 3)),
  PER = c(1.94, 1.51, 0.95, 2.06, 1.6, 0.94, 2.55, 1.7, 0.89),
  WS = c(0.52, 0.36, 0.85, 0.56, 0.42, 0.83, 0.7, 0.55, 0.69),
  VORP = c(3.02, 2.34, 0.78, 3.13, 2.46, 0.76, 3.45, 2.75, 0.71)
)

# Melt the dataframe to long format for ggplot2
library(reshape2)
df_melted <- melt(df_performance, id.vars = c("Metric", "Model"))
colnames(df_melted) <- c("Metric", "Model", "Performance_Metric", "Value")


# Create the clustered column graph
ggplot(df_melted, aes(x=Performance_Metric, y=Value, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title="Comparison of Model Performance Across Metrics", 
       subtitle="Evaluating RMSE, MAE, and R-Squared across Random Forest, Neural Network, and Bayesian Regression",
       x="Performance Metrics", y="Value", fill="Model") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +  # Custom color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major = element_line(color = "gray80", size = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face="bold", size=16),
        plot.subtitle = element_text(hjust = 0.5, size=12),
        strip.text = element_text(size=12, face="bold")) +
  geom_text(aes(label=sprintf("%.2f", Value)), 
            position=position_dodge(width=0.7), 
            vjust=-0.5, size=3.5, fontface="bold", color="black", family="Arial")
#-------------------------------------------------------------------------------#
# Create the dataframe with configurations, R-Squared values, and Runtime
df_heatmap <- data.frame(
  Configuration = c("RF_n=500", "RF_n=250", "RF_n=100", "RF_n=50", "RF_n=10",
                    "NN_size=5_iter=1000", "NN_size=6_iter=2000", "NN_size=10_iter=5000", "NN_size=15_iter=10000",
                    "BR_chain=4_iter=2000_norm(0,1)", "BR_chain=4_iter=2000_norm(0,2)", "BR_chain=4_iter=200_norm(0,3)",
                    "BR_chain=8_iter=400_norm(0,1)", "BR_chain=8_iter=400_norm(0,2)", "BR_chain=8_iter=400_norm(0,3)"),
  Model = c(rep("Random Forest", 5), rep("Neural Networks", 4), rep("Bayesian Regressions", 6)),
  R_Squared = c(0.95, 0.94, 0.92, 0.91, 0.89,
                0.9, 0.92, 0.94, 0.93,
                0.89, 0.89, 0.89, 0.89, 0.89, 0.89),
  Runtime = c(6270, 2973, 1230, 619, 240,
              45, 177, 711, 1130,
              287, 633, 1374, 544, 1470, 2657)
)

# Create the heatmap
ggplot(df_heatmap, aes(x=Configuration, y=R_Squared, fill=Runtime)) +
  geom_tile(color = "white", size = 0.2) +
  scale_fill_gradient(low = "#00AFBB", high = "#FC4E07", name = "Runtime (sec)") +
  labs(title="Heatmap of Model Accuracy vs. Runtime",
       subtitle="Visualizing the Trade-Off Between R-Squared and Runtime Across Configurations",
       x="Model Configurations", y="R-Squared Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face="bold"),
        axis.text.y = element_text(size = 10, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=16),
        plot.subtitle = element_text(hjust = 0.5, size=12),
        legend.position = "right") +
  geom_text(aes(label=sprintf("%.2f", R_Squared)), color="black", size=3, fontface="bold")
#-------------------------------------------------------------------------------#

# Create the dataframe
df_players <- data.frame(
  Player_Name = c("Reed Sheppard", "Donovan Clingan", "Stephon Castle", "Rob Dillingham", "Zach Edey", 
                  "Ron Holland II", "Devin Carter", "Cody Williams", "Kel’el Ware", "Tristan da Silva", 
                  "Jared McCain", "Bub Carrington", "Isaiah Collier", "Dalton Knecht", "Jaylon Tyson", 
                  "DaRon Holmes II", "Kyle Filipowski", "AJ Johnson", "Kyshawn George", "Terrence Shannon Jr.", 
                  "Ja’Kobe Walter", "Jonathan Mogbo", "Ryan Dunn", "Dillon Jones", "Tyler Kolek", 
                  "Antonio Reeves", "Johnny Furphy", "Adem Bona", "Matas Buzelis", "Bobi Klintman", 
                  "Baylor Scheierman", "Harrison Ingram", "Justin Edwards", "KJ Simpson", "Trey Alexander", 
                  "Tristen Newton", "Judah Mintz", "Cam Christie", "Anton Watson", "Joel Scott", 
                  "Jamal Shead", "Jaylen Wells", "Pelle Larsson", "Ajay Mitchell", "Kevin McCullar Jr.", 
                  "Oso Ighodaro", "Cam Spencer", "Enrique Freeman", "Armel Traoré", "David Jones", 
                  "PJ Hall", "Isaiah Crawford", "Jalen Lewis", "Melvin Ajinca", "T.J. Bickerstaff", 
                  "Quinten Post", "Drew Pember", "Trentyn Flowers"),
  Player_Potential = c(131.20, 145.00, 132.75, 122.70, 130.38, 100.72, 94.13, 99.44, 92.78, 92.33, 
                       90.82, 89.76, 89.15, 91.22, 84.90, 84.54, 88.42, 88.20, 87.00, 84.15, 
                       93.00, 94.08, 85.10, 81.98, 88.20, 87.00, 87.75, 78.30, 75.48, 75.42, 
                       79.68, 83.18, 83.48, 81.42, 79.35, 68.42, 72.92, 72.12, 69.72, 74.04, 
                       69.48, 72.66, 68.52, 69.42, 64.79, 62.61, 55.11, 53.31, 60.65, 59.16, 
                       54.74, 56.14, 55.77, 56.90, 52.80, 53.25, 43.91, 40.91),
  Predicted_PER = c(28.58, 26.04, 27.86, 28.77, 25.75, 27.21, 28.79, 26.20, 28.68, 28.70, 
                    28.74, 28.18, 27.04, 28.11, 27.66, 27.32, 26.24, 25.50, 25.61, 26.31, 
                    27.23, 25.95, 26.41, 26.11, 25.76, 26.84, 25.48, 27.53, 28.63, 27.41, 
                    26.80, 25.20, 26.53, 27.44, 24.64, 28.58, 27.17, 26.79, 25.37, 25.14, 
                    27.00, 25.19, 26.15, 24.02, 27.17, 25.32, 28.80, 28.53, 25.57, 26.78, 
                    27.81, 28.39, 26.74, 27.01, 27.89, 27.59, 28.13, 28.08)
)

# Create the double line chart
ggplot(df_players, aes(x=Player_Name)) + 
  geom_line(aes(y=Player_Potential, color="Player Potential"), size=1.2) + 
  geom_line(aes(y=Predicted_PER, color="Predicted PER"), size=1.2) + 
  labs(title="Player Potential vs. Predicted PER", 
       x="Player Name", 
       y="Value") + 
  scale_color_manual(values=c("Player Potential"="#1f77b4", "Predicted PER"="#ff7f0e")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        plot.title = element_text(hjust = 0.5, size=16, face="bold"),
        legend.title = element_blank(),
        legend.position = "top") +
  geom_text(aes(y=Player_Potential, label=round(Player_Potential, 1)), size=2.5, vjust=-0.5, angle=45) +
  geom_text(aes(y=Predicted_PER, label=round(Predicted_PER, 1)), size=2.5, vjust=1.5, angle=45)

#-------------------------------------------------------------------------------#

# Assuming your data is stored in a dataframe called 'data'
data_boxplot <- read_xlsx("final players data.xlsx")
# Melt the dataframe to long format for ggplot
data_long <- reshape2::melt(data_boxplot, measure.vars = c("PTS", "PER", "eFG%", "BPM"), 
                            variable.name = "Metric", value.name = "Value")

# Assuming your data is stored in a dataframe called 'data'

# Boxplot for PTS
p1 <- ggplot(data_boxplot, aes(y = PTS, x = "", fill = "PTS")) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of PTS",
       x = "",
       y = "PTS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )

# Convert to interactive plot
interactive_p1 <- ggplotly(p1)

# Boxplot for PER
p2 <- ggplot(data_boxplot, aes(y = PER, x = "", fill = "PER")) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of PER",
       x = "",
       y = "PER") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )

# Convert to interactive plot
interactive_p2 <- ggplotly(p2)

# Boxplot for eFG
p3 <- ggplot(data_boxplot, aes(y = `eFG%`, x = "", fill = "eFG")) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of eFG",
       x = "",
       y = "eFG") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )

# Convert to interactive plot
interactive_p3 <- ggplotly(p3)

# Boxplot for BPM
p4 <- ggplot(data_boxplot, aes(y = BPM, x = "", fill = "BPM")) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of BPM",
       x = "",
       y = "BPM") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )

# Convert to interactive plot
interactive_p4 <- ggplotly(p4)

# Show the plots
interactive_p1
interactive_p2
interactive_p3
interactive_p4
