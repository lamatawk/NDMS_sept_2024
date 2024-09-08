#install.packages("ggrepel")
# Load necessary libraries
library(vegan)   # For NMDS and envfit
library(ggplot2) # For enhanced plotting
library(ggrepel) # For better text label repulsion

# Load the data file
data <- read.csv("data_ALL_geocoded_stats_sept2024_for_nmds.csv")

# Subset the data to include only the variables of interest
variables <- data[, c("lit_score", "R_squared", "skewness", "total_words")]

# Select only the environmental variables for envfit
environmental_variables <- data[, c("elevation", "usda_zones_dec", "latitude", "longitude")]

# Run NMDS on the response variables
nmds <- metaMDS(variables, distance = "euclidean", k = 2, trymax = 100)

# Fit only the environmental gradients to the NMDS ordination
envfit_result <- envfit(nmds, environmental_variables, perm = 999)

# Extract NMDS site scores for plotting
nmds_scores <- as.data.frame(scores(nmds, "sites"))
nmds_scores$ID <- 1:nrow(nmds_scores)  # Add an ID column to label points with numbers

# Extract environmental vector scores for plotting
env_scores <- as.data.frame(scores(envfit_result, "vectors"))
env_scores$Variable <- rownames(env_scores)

# Manually calculate vectors for the response variables
response_vectors <- envfit(nmds, variables, perm = 999)
response_scores <- as.data.frame(scores(response_vectors, "vectors"))
response_scores$Variable <- rownames(response_scores)

# Plot NMDS with environmental vectors using ggplot2
ggplot() +
  # Plot NMDS site scores
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = ID), size = 3, color = "red", box.padding = 0.5) +
  
  # Plot environmental vectors (in blue)
  geom_segment(data = env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  geom_text_repel(data = env_scores, aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "blue", size = 4, box.padding = 0.5) +
  
  # Plot response vectors (in red)
  geom_segment(data = response_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "red") +
  geom_text_repel(data = response_scores, aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "red", size = 4, box.padding = 0.5) +
  
  # Customize the plot appearance
  theme_minimal() +
  labs(title = "NMDS with Environmental and Response Vectors",
       x = "NMDS1",
       y = "NMDS2") +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title = element_text(size = 12),    # Increase axis title size
        axis.text = element_text(size = 10))     # Increase axis text size


##### PLOT VARIATIONS

# Load necessary libraries
library(vegan)
library(ggplot2)
library(ggrepel)

# Load your data (assuming the data has already been loaded and NMDS has been computed as in the previous script)
# data <- read.csv("data_ALL_geocoded_stats_aug2024_for_nmds.csv")
# nmds and envfit have already been calculated

# Extract NMDS site scores and environmental vector scores if not already done
# (Assuming the following are done in the previous script)
# nmds_scores <- as.data.frame(scores(nmds, "sites"))
# nmds_scores$ID <- 1:nrow(nmds_scores)
# env_scores <- as.data.frame(scores(envfit_result, "vectors"))
# env_scores$Variable <- rownames(env_scores)
# response_scores <- as.data.frame(scores(response_vectors, "vectors"))
# response_scores$Variable <- rownames(response_scores)

# 1. Variation 1: Color-coded response variable vectors
ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = ID), size = 3, color = "darkgray", box.padding = 0.5) +
  geom_segment(data = env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  geom_text_repel(data = env_scores, aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "blue", size = 4, box.padding = 0.5) +
  geom_segment(data = response_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2, color = Variable), 
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text_repel(data = response_scores, aes(x = NMDS1, y = NMDS2, label = Variable, color = Variable), 
                  size = 4, box.padding = 0.5) +
  theme_minimal() +
  labs(title = "Variation 1: Color-coded Response Variables",
       x = "NMDS1",
       y = "NMDS2") +
  theme(plot.title = element_text(hjust = 0.5))

# 2. Variation 2: Faceting by environmental variable
ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = ID), size = 3, color = "darkgray", box.padding = 0.5) +
  geom_segment(data = env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  geom_text_repel(data = env_scores, aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "blue", size = 4, box.padding = 0.5) +
  facet_wrap(~Variable, scales = "free") +  # Faceting by environmental variable
  theme_minimal() +
  labs(title = "Variation 2: Faceted by Environmental Variable",
       x = "NMDS1",
       y = "NMDS2") +
  theme(plot.title = element_text(hjust = 0.5))

# 3. Variation 3: Using point size to represent total_words
ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2, size = data$total_words), color = "black") +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = ID), size = 3, color = "darkgray", box.padding = 0.5) +
  geom_segment(data = env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  geom_text_repel(data = env_scores, aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "blue", size = 4, box.padding = 0.5) +
  geom_segment(data = response_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2, color = Variable), 
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text_repel(data = response_scores, aes(x = NMDS1, y = NMDS2, label = Variable, color = Variable), 
                  size = 4, box.padding = 0.5) +
  scale_size_continuous(range = c(1, 5)) +  # Adjust point size based on total_words
  theme_minimal() +
  labs(title = "Variation 3: Point Size by Total Words",
       x = "NMDS1",
       y = "NMDS2") +
  theme(plot.title = element_text(hjust = 0.5))

# 4. Variation 4: Highlighting specific environmental gradients
highlight_vars <- c("latitude", "elevation")  # Variables to highlight

ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = ID), size = 3, color = "darkgray", box.padding = 0.5) +
  geom_segment(data = subset(env_scores, Variable %in% highlight_vars), 
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue", size = 1.2) +
  geom_text_repel(data = subset(env_scores, Variable %in% highlight_vars), 
                  aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "blue", size = 4, box.padding = 0.5) +
  geom_segment(data = subset(env_scores, !(Variable %in% highlight_vars)), 
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "lightgray", size = 0.8) +
  geom_text_repel(data = subset(env_scores, !(Variable %in% highlight_vars)), 
                  aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "lightgray", size = 3, box.padding = 0.5) +
  theme_minimal() +
  labs(title = "Variation 4: Highlighted Environmental Gradients",
       x = "NMDS1",
       y = "NMDS2") +
  theme(plot.title = element_text(hjust = 0.5))

# 5. Variation 5: Adding ellipses around groups (optional if you have groups)
# If your data has a grouping factor (e.g., City groups), you can add ellipses around these groups
ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = ID), size = 3, color = "darkgray", box.padding = 0.5) +
  geom_segment(data = env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  geom_text_repel(data = env_scores, aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "blue", size = 4, box.padding = 0.5) +
  stat_ellipse(data = nmds_scores, aes(x = NMDS1, y = NMDS2, group = data$City), 
               linetype = 2, color = "red", level = 0.95) +  # 95% confidence ellipses
  theme_minimal() +
  labs(title = "Variation 5: Ellipses Around Groups",
       x = "NMDS1",
       y = "NMDS2") +
  theme(plot.title = element_text(hjust = 0.5))







###########################################
###########################################
###########################################


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


##############################
##############################
##############################

# Load necessary libraries
library(vegan)
library(ggplot2)
library(ggrepel)
library(MuMIn)

# Load your data
data <- read.csv("data_ALL_geocoded_stats_sept2024_for_nmds.csv")

# Select the statistical metrics and environmental gradients
selected_variables <- data[, c("lit_score", "R_squared", "skewness", "total_words")]
environmental_variables <- data[, c("elevation", "usda_zones_dec", "latitude", "longitude", "EAL_SCORE")]

# Standardize the variables for NMDS
selected_variables_standardized <- scale(selected_variables)
environmental_variables_standardized <- scale(environmental_variables)

# Step 1: Perform NMDS on the standardized statistical variables
nmds <- metaMDS(selected_variables_standardized, distance = "euclidean", k = 2, trymax = 100)

# Step 2: Fit environmental gradients to the NMDS ordination
envfit_result <- envfit(nmds, environmental_variables_standardized, perm = 999)

# Step 3: Plot NMDS with environmental vectors and response variable vectors
nmds_scores <- as.data.frame(scores(nmds, "sites"))
nmds_scores$ID <- 1:nrow(nmds_scores)  # Add an ID for labeling points

env_scores <- as.data.frame(scores(envfit_result, "vectors"))
env_scores$Variable <- rownames(env_scores)

# Manually calculate vectors for the response variables
response_vectors <- envfit(nmds, selected_variables_standardized, perm = 999)
response_scores <- as.data.frame(scores(response_vectors, "vectors"))
response_scores$Variable <- rownames(response_scores)

# Step 3: Plot NMDS with environmental and response vectors
ggplot() +
  geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2), size = 3, color = "black") +
  geom_text_repel(data = nmds_scores, aes(x = NMDS1, y = NMDS2, label = ID), size = 3, color = "red") +
  
  # Environmental vectors (blue)
  geom_segment(data = env_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  geom_text_repel(data = env_scores, aes(x = NMDS1, y = NMDS2, label = Variable), 
                  color = "blue", size = 4) +
  
  # Response variable vectors (color-coded)
  geom_segment(data = response_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2, color = Variable), 
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text_repel(data = response_scores, aes(x = NMDS1, y = NMDS2, label = Variable, color = Variable), 
                  size = 4) +
  theme_minimal() +
  labs(title = "NMDS with Environmental and Response Vectors",
       x = "NMDS1", y = "NMDS2")

# Step 4: Conduct model selection using dredge (for lit_score as an example)
global_model <- lm(lit_score ~ elevation + usda_zones_dec + latitude + longitude + EAL_SCORE, 
                   data = data, na.action = na.fail)

# Run dredge to generate all possible models
model_selection <- dredge(global_model)

# Summarize Akaike weights for each variable
akaike_weights <- model.avg(model_selection, subset = delta < 2)
summary(akaike_weights)

# To see the importance of each variable (Akaike weights)
importance(akaike_weights)

# Repeat the dredge analysis for other response variables (e.g., skewness, R_squared)
global_model_skewness <- lm(skewness ~ elevation + usda_zones_dec + latitude + longitude + EAL_SCORE, 
                            data = data, na.action = na.fail)

model_selection_skewness <- dredge(global_model_skewness)
akaike_weights_skewness <- model.avg(model_selection_skewness, subset = delta < 2)
importance(akaike_weights_skewness)

############################
############################
############################

# Load necessary libraries
library(MuMIn)  # For dredge and model selection
library(vegan)  # For NMDS if needed
library(ggplot2)
library(ggrepel)

# Load your data
data <- read.csv("data_ALL_geocoded_stats_sept2024_for_nmds.csv")

# Step 1: Build a global linear model for lit_score with environmental variables
global_model_lit_score <- lm(lit_score ~ elevation + usda_zones_dec + latitude + longitude + EAL_SCORE, 
                             data = data, na.action = na.fail)

# Step 2: Use dredge for model selection and extract the best models
model_selection_lit_score <- dredge(global_model_lit_score)

# Step 3: Model averaging based on Akaike weights (models with delta < 2)
akaike_weights_lit_score <- model.avg(model_selection_lit_score, subset = delta < 2)

# Summarize the model-averaged coefficients
summary(akaike_weights_lit_score)

# View the importance of each variable based on Akaike weights
importance(akaike_weights_lit_score)

# Step 4: Interpret the results (elevation, USDA zones, latitude, EAL_SCORE)
# You can print or visualize the coefficients and significance levels from the model averaging

# Optional: Visualize NMDS (if needed for exploring environmental gradients)
# nmds and envfit can still be used to visualize the relationship between lit_score and environmental variables

# Step 4: Summarize and Interpret the Results from the Model Selection

# Summarize the model-averaged coefficients for lit_score
summary(akaike_weights_lit_score)

# View the importance of each environmental variable based on Akaike weights
# Use sw() to view the importance (Akaike weights) of each environmental variable
sw(akaike_weights_lit_score)


# Optional: Visualize the model-averaged coefficients (interpretation)
# You can create a data frame of the results for easier interpretation or visualization

# Extract coefficients and significance levels
coefficients_lit_score <- coef(akaike_weights_lit_score, full = TRUE)  # Full model-averaged coefficients
summary_table <- summary(akaike_weights_lit_score)$coefmat.full  # Extract full summary table
print(summary_table)

# Interpretation of Results
# You can now interpret which environmental variables have significant effects on lit_score
# For example, if elevation has a significant p-value, you might interpret that higher elevations have less biodiversity-related content in the ordinances.

# Optional: Visualizing the effect of environmental variables on lit_score
# Example using ggplot to visualize the relationship between significant variables and lit_score
library(ggplot2)

# Example: Plot lit_score against a significant environmental variable like elevation
ggplot(data, aes(x = elevation, y = lit_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Effect of Elevation on Lit_Score",
       x = "Elevation", y = "Lit_Score") +
  theme_minimal()

# Repeat for other environmental variables (e.g., USDA zones, latitude)


