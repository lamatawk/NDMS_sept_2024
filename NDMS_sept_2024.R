# # getwd()
# # setwd("C:/Users/lamat/OneDrive/Documents/GitHub/NDMS_sept_2024")
# 
# # Install necessary packages if not already installed
# if (!requireNamespace("vegan", quietly = TRUE)) {
#   install.packages("vegan")
# }
# if (!requireNamespace("ggplot2", quietly = TRUE)) {
#   install.packages("ggplot2")
# }
# if (!requireNamespace("ggrepel", quietly = TRUE)) {
#   install.packages("ggrepel")
# }
# 
# # Load the necessary libraries
# library(vegan)
# library(ggplot2)
# library(ggrepel)
# 
# # Load the data file
# data <- read.csv("data_ALL_geocoded_stats_sept2024_for_nmds.csv")
# 
# 
# # NMDS for Group 1: Urban and Economic Metrics
# urban_economic_variables <- data[, c("POPULATION", "BUILDVALUE", "AGRIVALUE", "AREA")]
# nmds_urban_economic <- metaMDS(urban_economic_variables, distance = "euclidean", k = 2, trymax = 100)
# 
# envfit_urban_economic <- envfit(nmds_urban_economic, urban_economic_variables, perm = 999)
# nmds_scores_urban_economic <- as.data.frame(scores(nmds_urban_economic, "sites"))
# env_scores_urban_economic <- as.data.frame(scores(envfit_urban_economic, "vectors"))
# env_scores_urban_economic$Variable <- rownames(env_scores_urban_economic)
# 
# # Plot NMDS for Urban and Economic Metrics
# ggplot() +
#   geom_point(data = nmds_scores_urban_economic, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
#   geom_text_repel(data = nmds_scores_urban_economic, aes(x = NMDS1, y = NMDS2, label = rownames(nmds_scores_urban_economic)), size = 3, color = "darkgray", box.padding = 0.5) +
#   geom_segment(data = env_scores_urban_economic, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
#                arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
#   geom_text_repel(data = env_scores_urban_economic, aes(x = NMDS1, y = NMDS2, label = Variable), 
#                   color = "blue", size = 4, box.padding = 0.5) +
#   theme_minimal() +
#   labs(title = "NMDS Plot for Urban and Economic Metrics",
#        x = "NMDS1",
#        y = "NMDS2") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # NMDS for Group 2: Environmental and Geographical Metrics
# env_geo_variables <- data[, c("EAL_SCORE", "elevation", "latitude", "longitude", "usda_zones_dec")]
# nmds_env_geo <- metaMDS(env_geo_variables, distance = "euclidean", k = 2, trymax = 100)
# 
# envfit_env_geo <- envfit(nmds_env_geo, env_geo_variables, perm = 999)
# nmds_scores_env_geo <- as.data.frame(scores(nmds_env_geo, "sites"))
# env_scores_env_geo <- as.data.frame(scores(envfit_env_geo, "vectors"))
# env_scores_env_geo$Variable <- rownames(env_scores_env_geo)
# 
# # Plot NMDS for Environmental and Geographical Metrics
# ggplot() +
#   geom_point(data = nmds_scores_env_geo, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
#   geom_text_repel(data = nmds_scores_env_geo, aes(x = NMDS1, y = NMDS2, label = rownames(nmds_scores_env_geo)), size = 3, color = "darkgray", box.padding = 0.5) +
#   geom_segment(data = env_scores_env_geo, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
#                arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
#   geom_text_repel(data = env_scores_env_geo, aes(x = NMDS1, y = NMDS2, label = Variable), 
#                   color = "blue", size = 4, box.padding = 0.5) +
#   theme_minimal() +
#   labs(title = "NMDS Plot for Environmental and Geographical Metrics",
#        x = "NMDS1",
#        y = "NMDS2") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # NMDS for Group 3: Textual and Statistical Metrics
# text_stat_variables <- data[, c("lit_score", "total_words", "skewness", "R_squared")]
# nmds_text_stat <- metaMDS(text_stat_variables, distance = "euclidean", k = 2, trymax = 100)
# 
# envfit_text_stat <- envfit(nmds_text_stat, text_stat_variables, perm = 999)
# nmds_scores_text_stat <- as.data.frame(scores(nmds_text_stat, "sites"))
# env_scores_text_stat <- as.data.frame(scores(envfit_text_stat, "vectors"))
# env_scores_text_stat$Variable <- rownames(env_scores_text_stat)
# 
# # Plot NMDS for Textual and Statistical Metrics
# ggplot() +
#   geom_point(data = nmds_scores_text_stat, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
#   geom_text_repel(data = nmds_scores_text_stat, aes(x = NMDS1, y = NMDS2, label = rownames(nmds_scores_text_stat)), size = 3, color = "darkgray", box.padding = 0.5) +
#   geom_segment(data = env_scores_text_stat, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
#                arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
#   geom_text_repel(data = env_scores_text_stat, aes(x = NMDS1, y = NMDS2, label = Variable), 
#                   color = "blue", size = 4, box.padding = 0.5) +
#   theme_minimal() +
#   labs(title = "NMDS Plot for Textual and Statistical Metrics",
#        x = "NMDS1",
#        y = "NMDS2") +
#   theme(plot.title = element_text(hjust = 0.5))
# Load the necessary libraries
# Load the necessary libraries
# Load the necessary libraries
# Load the necessary libraries
library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)

# Set the working directory to where your data file is located
setwd("C:/Users/lamat/OneDrive/Documents/GitHub/NDMS_sept_2024")

# Load the data file
data <- read.csv("data_ALL_geocoded_stats_sept2024_for_nmds.csv")

# Select and standardize the variables for NMDS
selected_variables <- data[, c("elevation", "latitude", "longitude", "usda_zones_dec", 
                               "EAL_SCORE", "lit_score", "R_squared", "skewness", "total_words")]

selected_variables_standardized <- scale(selected_variables)  # Standardize the variables

# Perform NMDS
nmds_selected <- metaMDS(selected_variables_standardized, distance = "euclidean", k = 2, trymax = 100)

# Fit environmental vectors to the NMDS
envfit_selected <- envfit(nmds_selected, selected_variables_standardized, perm = 999)

# Extract NMDS scores and environmental scores
nmds_scores_selected <- as.data.frame(scores(nmds_selected, "sites"))
env_scores_selected <- as.data.frame(scores(envfit_selected, "vectors"))
env_scores_selected$Variable <- rownames(env_scores_selected)

# Add city names to the NMDS scores dataframe
nmds_scores_selected$City <- data$City

# Assign colors to each response variable for plotting
variable_colors <- c("lit_score" = "red", "R_squared" = "green", 
                     "skewness" = "cyan", "total_words" = "purple")

# Plot the NMDS with color-coded arrows
ggplot() +
  geom_point(data = nmds_scores_selected, aes(x = NMDS1, y = NMDS2), size = 2, color = "black") +
  geom_text_repel(data = nmds_scores_selected, aes(x = NMDS1, y = NMDS2, label = as.numeric(rownames(nmds_scores_selected))), 
                  size = 3, color = "darkgray", box.padding = 0.5, max.overlaps = 100) +
  geom_segment(data = env_scores_selected, aes(x = 0, xend = NMDS1 * 10, y = 0, yend = NMDS2 * 10, color = Variable), 
               arrow = arrow(length = unit(0.3, "cm")), size = 1.5) +  # Increase arrow size
  geom_text_repel(data = env_scores_selected, aes(x = NMDS1 * 10, y = NMDS2 * 10, label = Variable, color = Variable), 
                  size = 5, box.padding = 0.5) +  # Increase text size
  scale_color_manual(values = variable_colors) +
  labs(title = "Variation 1: Color-coded Response Variables",
       x = "NMDS1", y = "NMDS2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())

##############
#######Attempting with legend now city-state DIDNT WORK YET


# View the R² and p-values from the envfit object
summary(envfit_selected)

# Extract R² values and p-values
envfit_results <- data.frame(envfit_selected$vectors$r, envfit_selected$vectors$pvals)
colnames(envfit_results) <- c("R2", "p_value")

# Order by importance
envfit_results <- envfit_results[order(-envfit_results$R2), ]
print(envfit_results)



##########DREDGE

# Install and load necessary packages
install.packages("MuMIn")
library(MuMIn)

# Load your data (if not already loaded)
data <- read.csv("C:/Users/lamat/OneDrive/Documents/GitHub/NDMS_sept_2024/data_ALL_geocoded_stats_sept2024_for_nmds.csv")

# Create a global model with na.action set to na.fail
global_model <- lm(lit_score ~ EAL_SCORE + usda_zones_dec + latitude + longitude + R_squared + skewness + total_words + elevation + AGRIVALUE + AREA + BUILDVALUE + POPULATION, 
                   data = data, na.action = na.fail)

# Run dredge to generate all possible models
model_selection <- dredge(global_model)

# Print the model selection table
print(model_selection)

# Summarize the Akaike weights for each variable
akaike_weights <- model.avg(model_selection, subset = delta < 2)
summary(akaike_weights)

# To see the importance of each variable (Akaike weights)
importance(akaike_weights)
