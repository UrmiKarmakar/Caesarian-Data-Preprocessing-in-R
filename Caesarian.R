install.packages("ggplot2")
install.packages("dplyr")
install.packages("visdat")
install.packages("ROSE")

library(ggplot2)
library(dplyr)
library(visdat)
library(ROSE)

patientData<- read.csv("E:/caesarian/caesarian.csv",header=TRUE,sep=",")
patientData

caesarian_data <- patientData
str(caesarian_data)
summary(caesarian_data)
head(caesarian_data)

# Check unique values for the catagorical entities to find invalid entries
unique(caesarian_data$Gender)
unique(caesarian_data$Blood)
unique(caesarian_data$Delivery_number)

# Define valid values
valid_genders <- c("male", "female")
valid_delivery_number <- c("1","2","3","4")

# Replace invalid values
# Create a function to replace invalid values
replace_invalid <- function(value, valid_values, replacement = NA) {
  if (value %in% valid_values) {
    return(value)
  } else {
    return(replacement)
  }
}

# Apply the function to the Gender column
caesarian_data$Gender <- sapply(caesarian_data$Gender, replace_invalid, valid_genders)
caesarian_data$Delivery_number <- sapply(caesarian_data$Delivery_number, replace_invalid, valid_delivery_number)

# Optional - Inspect the Data Again
unique(caesarian_data$Gender)
unique(caesarian_data$Delivery_number)

# Type covertion of Delivery_number
caesarian_data$Delivery_number <- as.integer(caesarian_data$Delivery_number)

# Convert 'gender' to numerical: male = 0, female = 1
caesarian_data$Gender <- factor(caesarian_data$Gender, 
                             levels = c("male", "female"), 
                             labels = c(0, 1))

# Convert 'blood' to numerical: low = 0, normal = 1, high = 2
caesarian_data$Blood <- factor(caesarian_data$Blood, 
                            levels = c("low", "normal", "high"), 
                            labels = c(0, 1, 2))


# Display the structure of the dataset after conversion
str(caesarian_data)

# Calculate Q1, Q3, and IQR for Age
Q1_age <- quantile(caesarian_data$Age, 0.25, na.rm = TRUE)
Q3_age <- quantile(caesarian_data$Age, 0.75, na.rm = TRUE)
IQR_age <- Q3_age - Q1_age

# Calculate Q1, Q3, and IQR for Weight
Q1_weight <- quantile(caesarian_data$weight.kg., 0.25, na.rm = TRUE)
Q3_weight <- quantile(caesarian_data$weight.kg., 0.75, na.rm = TRUE)
IQR_weight <- Q3_weight - Q1_weight

# Display calculated values
Q1_age
Q3_age
IQR_age

Q1_weight
Q3_weight
IQR_weight

# Function to identify outliers
detect_outliers <- function(x, Q1, Q3, IQR) {
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Identify outliers for Age
age_outliers <- detect_outliers(caesarian_data$Age, Q1_age, Q3_age, IQR_age)

# Identify outliers for Weight
weight_outliers <- detect_outliers(caesarian_data$weight.kg., Q1_weight, Q3_weight, IQR_weight)

# Plot boxplot for Age with outliers
ggplot(caesarian_data, aes(y = "", x = Age)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot of Age with Outliers") +
  theme_minimal() +
  labs(x = "Age", y = "")

# Plot boxplot for Weight with outliers
ggplot(caesarian_data, aes(y = "", x = weight.kg.)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplot of Weight (kg) with Outliers") +
  theme_minimal() +
  labs(x = "Weight (kg)", y = "")

# Capping function to handle outliers
cap_outliers <- function(x, Q1, Q3, IQR) {
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# Apply capping to Age and weight.kg.
caesarian_data$Age <- cap_outliers(caesarian_data$Age, Q1_age, Q3_age, IQR_age)
caesarian_data$weight.kg. <- cap_outliers(caesarian_data$weight.kg., Q1_weight, Q3_weight, IQR_weight)

# Plot boxplot for Age after handling outliers
ggplot(caesarian_data, aes(y = "", x = Age)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot of Age After Handling Outliers") +
  theme_minimal() +
  labs(x = "Age", y = "")

# Plot boxplot for Weight after handling outliers
ggplot(caesarian_data, aes(y = "", x = weight.kg.)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplot of Weight (kg) After Handling Outliers") +
  theme_minimal() +
  labs(x = "Weight (kg)", y = "")

# Check for missing values
missing_values <- colSums(is.na(caesarian_data))
print(missing_values)

# Visualize missing values
vis_miss(caesarian_data)

#For Numerical Columns:
# Impute missing values in Age with median
caesarian_data$Age[is.na(caesarian_data$Age)] <- round(median(caesarian_data$Age, na.rm = TRUE))

# Impute missing values in weight.kg. with mean
caesarian_data$weight.kg.[is.na(caesarian_data$weight.kg.)] <- mean(caesarian_data$weight.kg., na.rm = TRUE)

#For Categorical Columns:
# Mode imputation function
mode_impute <- function(x) {
  uniq_x <- na.omit(unique(x))
  freq_x <- table(x)
  return(names(freq_x)[which.max(freq_x)])
}

# Impute missing values for categorical columns
caesarian_data$Gender[is.na(caesarian_data$Gender)] <- mode_impute(caesarian_data$Gender)
caesarian_data$Delivery_number[is.na(caesarian_data$Delivery_number)] <- mode_impute(caesarian_data$Delivery_number)
caesarian_data$Delivery_time[is.na(caesarian_data$Delivery_time)] <- mode_impute(caesarian_data$Delivery_time)
caesarian_data$Blood[is.na(caesarian_data$Blood)] <- mode_impute(caesarian_data$Blood)

# Remove rows with any remaining missing values
caesarian_data <- na.omit(caesarian_data)

vis_miss(caesarian_data)

# Removing duplicates in rows
caesarian_data<-distinct(caesarian_data)
caesarian_data<-distinct(caesarian_data,Patient_id, .keep_all = TRUE) 

caesarian_data

# Check if the dataframe is loaded correctly
str(caesarian_data)

# Perform oversampling
oversampled_data <- ovun.sample(Caesarian ~ ., data = caesarian_data, method = "over", p = 0.5, seed = 1)$data

# Check the balance of the Caesarian column in the oversampled data
table(oversampled_data$Caesarian)

# Perform undersampling
undersampled_data <- ovun.sample(Caesarian ~ ., data = caesarian_data, method = "under", p = 0.5, seed = 1)$data

# Check the balance of the Caesarian column in the undersampled data
table(undersampled_data$Caesarian)


# Balance the dataset
balanced_data <- ovun.sample(Caesarian ~ ., data = caesarian_data, method = "both", p = 0.5, seed = 1)$data

# Check the balance of the Caesarian column
table(balanced_data$Caesarian)

# Filter:
filtered_data <- caesarian_data %>% filter(Gender == 1)
filtered_data

# Calculate mean, median, and mode
mean_age <- mean(caesarian_data$Age, na.rm = TRUE)
median_age <- median(caesarian_data$Age, na.rm = TRUE)

# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_age <- get_mode(caesarian_data$Age)

# Create a data frame for plotting
stats_df <- data.frame(
  Statistic = c("Mean", "Median", "Mode"),
  Value = c(mean_age, median_age, mode_age)
)

# Plot using ggplot2
ggplot(stats_df, aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.6) +
  labs(title = "Mean, Median, and Mode of Age",
       x = "Statistic",
       y = "Value") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green", "red"))

# Plot the Age distribution with mean
plot_mean <- ggplot(caesarian_data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "solid", size = 1) +
  ggtitle("Age Distribution with Mean") +
  theme_minimal() +
  labs(x = "Age", y = "Frequency") +
  annotate("text", x = mean_age, y = Inf, label = paste("Mean:", round(mean_age, 2)), color = "red", vjust = 2)

# Plot the Age distribution with median
plot_median <- ggplot(caesarian_data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = median_age), color = "green", linetype = "solid", size = 1) +
  ggtitle("Age Distribution with Median") +
  theme_minimal() +
  labs(x = "Age", y = "Frequency") +
  annotate("text", x = median_age, y = Inf, label = paste("Median:", round(median_age, 2)), color = "green", vjust = 2)

# Plot the Age distribution with mode
plot_mode <- ggplot(caesarian_data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = mode_age), color = "purple", linetype = "solid", size = 1) +
  ggtitle("Age Distribution with Mode") +
  theme_minimal() +
  labs(x = "Age", y = "Frequency") +
  annotate("text", x = mode_age, y = Inf, label = paste("Mode:", round(mode_age, 2)), color = "purple", vjust = 2)

# Display the plots
plot_mean
plot_median
plot_mode

# Summary statistics for Age
summary(caesarian_data$Age)

# Display the structure of the cleaned dataset
str(caesarian_data)

# Min-Max Normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to Age
caesarian_data$Age_normalized <- normalize(caesarian_data$Age)

# Display the first few rows to check the normalized values
head(caesarian_data)

caesarian_data
