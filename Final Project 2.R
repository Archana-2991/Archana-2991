getwd()
setwd("C:/Users/archa/OneDrive/Desktop/Data Visualization")
dir()
housing_data <-read.csv("housing +school merged file.csv")
# Load necessary libraries
library(dplyr)
library(ggplot2)
install.packages("mice")
library(mice)

# Mean Imputation
housing_data_imputed <- housing_data
for(col in names(housing_data)){
  if(is.numeric(housing_data[[col]])){
    housing_data_imputed[[col]][is.na(housing_data[[col]])] <- mean(housing_data[[col]], na.rm = TRUE)
  }
}
write.csv(housing_data_imputed, "housing_data_imputed.csv", row.names = F)
# Calculate summary statistics
summary_stats <- housing_data_imputed %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(., na.rm = TRUE), 
    sd = ~sd(., na.rm = TRUE), 
    median = ~median(., na.rm = TRUE), 
    IQR = ~IQR(., na.rm = TRUE)
  )))

# Print summary statistics
print(summary_stats)

# Identify outliers using the IQR method
outliers <- housing_data_imputed %>%
  summarise(across(where(is.numeric), ~list(
    Outliers = .[. < (quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE)) | 
                   . > (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE))]
  )))

# Print outliers
print(outliers)

# Visualize outliers using boxplots
numeric_columns <- names(housing_data_imputed)[sapply(housing_data_imputed, is.numeric)]
par(mfrow=c(2,2)) # Adjust the grid size based on the number of numeric columns
for(i in numeric_columns) {
  boxplot(housing_data[[i]], main=i)
}

# Addressing outliers
# This is an example of capping the outliers to a maximum threshold
capped_data <- housing_data_imputed %>%
  mutate(across(where(is.numeric), ~ifelse(. > (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE)), 
                                           (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE)), 
                                           .)))

# Replace outliers with NA and then impute with median
imputed_data1 <- housing_data_imputed %>%
  mutate(across(where(is.numeric), ~ifelse(. < (quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE)) | 
                                             . > (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE)), 
                                           NA, 
                                           .))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Print the first few rows of the capped data
print(head(capped_data))

# Print the first few rows of the imputed data
print(head(imputed_data1))
write.csv(imputed_data1, "ImputedDataset1.csv", row.names = F)

# Histogram of Sold Prices
ggplot(housing_data_imputed, aes(x=soldprice)) +
  geom_histogram(binwidth=50000, fill="blue", color="black") +
  labs(title="Histogram of Sold Prices", x="Sold Price ($)", y="Count")

# Bar Plot of Property Types
ggplot(housing_data_imputed, aes(x=type)) +
  geom_bar(fill="coral", color="black") +
  labs(title="Bar Plot of Property Types", x="Property Type", y="Count")

# Box Plot of Square Footage
ggplot(housing_data_imputed, aes(y=sqft)) +
  geom_boxplot(fill="lightgreen", color="black") +
  labs(title="Box Plot of Square Footage", x="", y="Square Footage (Sqft)")

## Scatter Plot of Sold Price vs. Square Footage
ggplot(housing_data_imputed, aes(x=sqft, y=soldprice)) +
  geom_point(aes(color=type), alpha=0.6) +
  labs(title="Scatter Plot of Sold Price vs. Square Footage", x="Square Footage (Sqft)", y="Sold Price ($)")


# High Density Plot of Sold Price vs. Square Footage
ggplot(housing_data_imputed, aes(x=sqft, y=soldprice)) +
  geom_density_2d_filled(aes(fill=..level..)) +
  labs(title="High Density Plot of Sold Price vs. Square Footage", x="Square Footage (Sqft)", y="Sold Price ($)")
par(mfrow=c(1,1))
# Linear Regression Model to predict Sold Price
linear_model <- lm(soldprice ~ sqft , data = housing_data_imputed)
summary(linear_model)
# Plot soldprice against sqft (you can choose other predictors if needed)
plot(housing_data_imputed$sqft, housing_data_imputed$soldprice, xlab = "sqft", ylab = "soldprice", main = "Regression Line")
abline(linear_model, col = "red")  # Add regression line to the plot

# K-means Clustering
set.seed(123) # For reproducibility
kmeans_result <- kmeans(housing_data_imputed[, c("sqft", "soldprice")], centers=3)
housing_data_imputed$cluster <- kmeans_result$cluster

# Summarize cluster characteristics
aggregate(housing_data_imputed[, c("sqft", "soldprice")], by=list(housing_data_imputed$cluster), mean)

# Summarize cluster characteristics
cluster_means <- aggregate(cbind(sqft, soldprice) ~ cluster, data = housing_data_imputed, FUN = mean)
# Plot clusters
library(ggplot2)
ggplot(housing_data_imputed, aes(x = sqft, y = soldprice, color = factor(cluster))) +
  geom_point() +
  geom_point(data = cluster_means, aes(x = sqft, y = soldprice), color = "black", size = 3, shape = 17) +
  labs(title = "K-means Clustering", x = "sqft", y = "soldprice") +
  scale_color_discrete(name = "Cluster") +
  theme_minimal()



