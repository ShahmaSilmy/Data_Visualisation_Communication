#Installing the tidyverse package
install.packages("tidyverse")

#getting the tidyverse library
library(tidyverse)
library(dplyr)

#Loading the dataset
customer_data <- read.csv("ecommerce_customer_data.csv")

#preview the dataset
glimpse(customer_data)

summary(customer_data)

#Removing the duplicate rows
customer_data <- customer_data %>%
  distinct()

View(customer_data)

# Remove rows with missing or blank Customer IDs
customer_data <- customer_data %>%
  filter(!is.na(CustomerID) & CustomerID != "")

#Convert the 'Date' column to proper Date format
customer_data$RegistrationDate <- as.Date(customer_data$RegistrationDate, format = "%Y-%m-%d")

# Calculate the median date
median_date <- median(customer_data$RegistrationDate, na.rm = TRUE)

# Replace missing or blank dates with the median date
customer_data$RegistrationDate[is.na(customer_data$RegistrationDate) | customer_data$RegistrationDate == ""] <- as.Date(median_date)

#Confirm changes
head(customer_data$RegistrationDate)

#Editing the ages 
customer_data <- customer_data %>%
  mutate(Age = ifelse(Age < 15, 15, Age))

#check for missing values and viewing 
missing_values_df <- customer_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))  # Count missing values per column
View(missing_values_df)

#visualising missing values
customer_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MissingCount") %>%
  ggplot(aes(x = Variable, y = MissingCount)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Missing Values per Variable", x = "Variable", y = "Count") +
  theme_minimal()

#since there are negative values in the Customer life time value column making them into positive
positive_cltv_mean <- mean(customer_data$CustomerLifetimeValue[customer_data$CustomerLifetimeValue >= 0], na.rm = TRUE)
   customer_data$CustomerLifetimeValue[customer_data$CustomerLifetimeValue < 0] <- positive_cltv_mean

#replacing the missing values with mean for numerical columns
columns_to_fill_mean <- c("AverageOrderValue", "CustomerLifetimeValue", 
                          "EmailEngagementRate", "SocialMediaEngagementRate", 
                          "AverageSatisfactionScore", 
                          "EmailConversionRate", "SocialMediaConversionRate", 
                          "SearchEngineConversionRate")

# Replace missing values with mean
customer_data <- customer_data %>%
  mutate(across(all_of(columns_to_fill_mean), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

View(customer_data)

#replace values with median
columns_to_fill_median <- c("Age","TotalPurchases","CustomerServiceInteractions")
customer_data <- customer_data %>%
  mutate(across(all_of(columns_to_fill_median), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


#categorical columns to fill with mode
categorical_columns <- c("Gender", "IncomeLevel", "City", "Country", "FavoriteCategory", 
                         "SecondFavoriteCategory", "MobileAppUsage", "RepeatCustomer", 
                         "PremiumMember", "HasReturnedItems")

# Function to calculate the mode 
get_mode <- function(x) {
  x <- na.omit(x)  # Remove NAs
  if (length(x) == 0) return(NA)  # If there are no valid values, return NA
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]  # Return most frequent value
}

# Check if categorical columns exist 
cat("Checking column names:\n")
print(names(customer_data))

# Check for missing values before imputation
cat("Missing values before imputation:\n")
print(colSums(is.na(customer_data[categorical_columns])))

# Replace non-standard missing values ("", "nan") with NA
customer_data[categorical_columns] <- lapply(customer_data[categorical_columns], function(x) {
  ifelse(x == "" | x == "nan", NA, x)
})

# Check if the replacement worked
cat("Missing values after replacing non-standard missing values:\n")
print(colSums(is.na(customer_data[categorical_columns])))

# Replace missing values in categorical columns with mode
for (col in categorical_columns) {
  mode_val <- get_mode(customer_data[[col]])  # Get mode for each column
  customer_data[[col]] <- ifelse(is.na(customer_data[[col]]), mode_val, customer_data[[col]])  
}

# Check missing values after imputation
cat("Missing values after imputation:\n")
print(colSums(is.na(customer_data[categorical_columns])))

# Print updated dataset 
cat("Updated dataset (first few rows):\n")
print(head(customer_data))  # Print the first few rows

#Handle outliers using IQR method
#handle_outliers <- function(x) {
#  Q1 <- quantile(x, 0.25, na.rm = TRUE)  # First quartile
 # Q3 <- quantile(x, 0.75, na.rm = TRUE)  # Third quartile
  #IQR <- Q3 - Q1                         # Interquartile range
  #lower_bound <- Q1 - 1.5 * IQR          # Lower bound
  #upper_bound <- Q3 + 1.5 * IQR          # Upper bound
  
  # Replace values outside bounds with the bounds
  #x <- ifelse(x < lower_bound, lower_bound, x)
  #x <- ifelse(x > upper_bound, upper_bound, x)
  #return(x)
#}

# Apply outlier handling to all numeric columns
#customer_data <- customer_data %>%
 # mutate(across(where(is.numeric), handle_outliers))
# Function to identify rows with outliers

# Function to remove outliers iteratively for all numeric columns
remove_outliers_iterative <- function(data, threshold = 1.5) {
  repeat {
    # Store the number of rows before filtering
    rows_before <- nrow(data)
    
    # Apply outlier removal to the numeric column
    data <- data %>%
      filter(!if_any(where(is.numeric), function(x) {
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - threshold * IQR
        upper_bound <- Q3 + threshold * IQR
        x < lower_bound | x > upper_bound
      }))
    
    # Stop if no rows were removed in the last iteration
    if (nrow(data) == rows_before) break
  }
  return(data)
}

# Visualize boxplot before cleaning the outliers
boxplot(customer_data[, sapply(customer_data, is.numeric)], 
        main = "Boxplot Before Removing Outliers", 
        las = 2, col = "pink3")

# Apply iterative outlier removal for all numeric columns
customer_data_cleaned <- remove_outliers_iterative(customer_data, threshold = 1.5)

# Visualize boxplot after cleaning the outliers from the dataset
boxplot(customer_data_cleaned[, sapply(customer_data_cleaned, is.numeric)], 
        main = "Boxplot After Removing Outliers", 
        las = 2, col = "green")

#Getting the cleaned dataset into csv file
write.csv(customer_data_cleaned, "cleaned_ecommerce_customer_data.csv", row.names = FALSE)
