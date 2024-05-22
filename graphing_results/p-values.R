# Load necessary library
remove(list = ls())

library(ComplexHeatmap)
library(readxl)
library(ggplot2)
library(tidyr)
library(ggbump)
library(dplyr)
library(tidyverse)


# Bank Data ---------------------------------------------------------------

# Read data from Excel
data = read_excel("G:/Monash_University/Year5/Honours_Final/Bank p-values.xlsx")
# Assuming the first column is the row names (features)
data

data =as.data.frame(data)
rownames(data) <- data$Feature
data <- data[,-1]  # Remove the first column as it's now row names
data = data[-1,] # Remove sensitive attribute
# Convert data to numeric, in case it's not

data = data.frame(lapply(data, function(x) as.numeric(as.character(x))),
           check.names=F, row.names = rownames(data))
data = abs(data) # convert to absolute values

data
# Use heatmap function to create the heatmap
ComplexHeatmap::pheatmap(as.matrix(data), cluster_rows = T, cluster_cols = F,
                         show_row_dend =F,
                         display_numbers = T,
                         color = hcl.colors(10, "sunset", rev=T), 
                         border_color = "black", number_color = "black",
                         fontsize_number = 13, column_names_side = c("top"),
                         angle_col = c("0"),
                         name = "correlation")



# Census Data -------------------------------------------------------------

# Read data from Excel
data = read_excel("G:/Monash_University/Year5/Honours_Final/Census p-values.xlsx")
# Assuming the first column is the row names (features)
data

data =as.data.frame(data)
rownames(data) <- data$Feature
data <- data[,-1]  # Remove the first column as it's now row names
data = data[c(-8,-9),] # Remove sensitive attribute

# Convert data to numeric, in case it's not

data = data.frame(lapply(data, function(x) as.numeric(as.character(x))),
                  check.names=F, row.names = rownames(data))
data = abs(data) # convert to absolute values

data
# Use heatmap function to create the heatmap
ComplexHeatmap::pheatmap(as.matrix(data), cluster_rows = T, cluster_cols = F,
                         show_row_dend =F,
                         display_numbers = T,
                         color = hcl.colors(10, "sunset", rev=T), 
                         border_color = "black", number_color = "black",
                         fontsize_number = 13, column_names_side = c("top"),
                         angle_col = c("0"),
                         name = "correlation")


# convert to ranking -------------------------------------------------------

rank_df <- data.frame(
  RF = rowMeans(data[, 1:3]),
  MLPC = rowMeans(data[, 4:6]),
  DT = rowMeans(data[, 7:9])
)
rank_df

# Rank each column with the highest value as rank 1
ranked_data <- as.data.frame(lapply(rank_df, function(x) {
  length(x) + 1 - rank(x, ties.method = "min")
}))
ranked_data$Attribute = rownames(rank_df)


long_data <- pivot_longer(ranked_data, cols = c(RF, MLPC, DT), names_to = 'ML Model', values_to = "Rank")
long_data = as.data.frame(long_data)

ranked_data

# Creating a bump chart
ggplot(long_data, aes(x = `ML Model`, y = Rank, group = Attribute, color = Attribute)) +
  geom_line(size = 1.2) +  # Draw lines between points
  geom_point(size = 4) +   # Add points for each rank
  scale_y_reverse(limits = c(11, 1)) +  # Reverse the Y axis so that rank 1 is at the top
  labs(title = "Ranking of Teams Over Time",
       x = "ML Model",
       y = "Rank") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),  # Adjust the text angle for x-axis labels
        legend.title = element_blank())  # Remove the legend title 

long_data


ggplot(long_data, aes(x = `ML Model`, y = Rank, group = Attribute, color = Attribute)) +
  geom_bump(size = 1) +
  geom_point(size = 6) +
  scale_y_reverse(limits = c(11, 1)) +  # Reverse the Y axis so that rank 1 is at the top
  geom_text(data = long_data %>% filter(`ML Model` == min(`ML Model`)),
            aes(x=rep(0.9,11), label = Attribute),
            size = 5, hjust = 1) +
  geom_text(data = long_data %>% filter(`ML Model` == max(`ML Model`)),
            aes(x=rep(3.1,11), label = Attribute),
            size = 5, hjust = 0) +
  theme(legend.position = "none")



# Bump chart for bank -----------------------------------------------------

rank_df <- data.frame(
  RF = rowMeans(data[, 1:3]),
  MLPC = rowMeans(data[, 4:6]),
  DT = rowMeans(data[, 7:9])
)
rank_df

# Rank each column with the highest value as rank 1
ranked_data <- as.data.frame(lapply(rank_df, function(x) {
  length(x) + 1 - rank(x, ties.method = "min")
}))
ranked_data$Attribute = rownames(rank_df)


long_data <- pivot_longer(ranked_data, cols = c(RF, MLPC, DT), names_to = 'ML Model', values_to = "Rank")
long_data = as.data.frame(long_data)



# Creating a bump chart
ggplot(long_data, aes(x = `ML Model`, y = Rank, group = Attribute, color = Attribute)) +
  geom_line(size = 1.2) +  # Draw lines between points
  geom_point(size = 4) +   # Add points for each rank
  scale_y_reverse(limits = c(15, 1)) +  # Reverse the Y axis so that rank 1 is at the top
  labs(title = "Ranking of Teams Over Time",
       x = "ML Model",
       y = "Rank") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),  # Adjust the text angle for x-axis labels
        legend.title = element_blank())  # Remove the legend title 

long_data


ggplot(long_data, aes(x = `ML Model`, y = Rank, group = Attribute, color = Attribute)) +
  geom_bump(size = 1) +
  geom_point(size = 6) +
  scale_y_reverse(limits = c(15, 1)) +  # Reverse the Y axis so that rank 1 is at the top
  geom_text(data = long_data %>% filter(`ML Model` == min(`ML Model`)),
            aes(x=rep(0.9,15), label = Attribute),
            size = 5, hjust = 1) +
  geom_text(data = long_data %>% filter(`ML Model` == max(`ML Model`)),
            aes(x=rep(3.1,15), label = Attribute),
            size = 5, hjust = 0) +
  theme(legend.position = "none")
