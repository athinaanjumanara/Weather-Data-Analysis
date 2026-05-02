# Data Preparation
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(caret)

# Load Dataset
data <- read.csv("C:/Users/athin/Downloads/Project-20250118/06_Weather.csv")

cat("Inspecting LocalTimestamp column:\n")
head(data$LocalTimestamp)

# Parse LocalTimestamp
data <- data %>%
  mutate(
    LocalTimestamp = parse_date_time(LocalTimestamp, orders = c("mdy HMS", "mdy HM", "mdyHMS"))
  )
failed_local <- sum(is.na(data$LocalTimestamp))
cat("Failed to parse LocalTimestamp:", failed_local, "rows\n")

data_clean <- data %>% filter(!is.na(LocalTimestamp))
# Handle Missing Values
data_clean <- data_clean %>% mutate(
  AirTemperature = ifelse(is.na(AirTemperature), mean(AirTemperature, na.rm = TRUE), AirTemperature),
  BarometricPressure = ifelse(is.na(BarometricPressure), mean(BarometricPressure, na.rm = TRUE), BarometricPressure),
  RelativeHumidity = ifelse(is.na(RelativeHumidity), mean(RelativeHumidity, na.rm = TRUE), RelativeHumidity)
)
# Extract Hour for Temporal Analysis
data_clean <- data_clean %>% mutate(Hour = hour(LocalTimestamp))

cat("First few rows of data after extracting Hour:\n")
head(data_clean)


# Plot Trend of Air Temperature Over Time
plt_temp_time <- ggplot(data_clean, aes(x = LocalTimestamp, y = AirTemperature)) +
  geom_line(color = "blue", size = 1) +  
  geom_point(color = "darkblue", size = 0.5, alpha = 0.6) +  
  labs(
    title = "Trend of Air Temperature Over Time",
    x = "Timestamp (Local Time)",
    y = "Air Temperature (°C)"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 10),  
    axis.title = element_text(size = 12)
  ) +
  scale_x_datetime(
    date_breaks = "2 days",  
    date_labels = "%d %b"  
  ) +
  scale_y_continuous(breaks = seq(
    min(data_clean$AirTemperature, na.rm = TRUE), 
    max(data_clean$AirTemperature, na.rm = TRUE), 
    by = 2
  ))  

# Display the enhanced plot
print(plt_temp_time)



# Plot Relationship Between Barometric Pressure and Relative Humidity
plt_pressure_humidity <- ggplot(data_clean, aes(x = BarometricPressure, y = RelativeHumidity)) +
  geom_point(alpha = 0.6, color = "red", size = 2) +  
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed", size = 1) +
  labs(
    title = "Relationship Between Barometric Pressure and Relative Humidity",
    subtitle = "Scatter plot with regression line for visualizing correlation",
    x = "Barometric Pressure (hPa)",
    y = "Relative Humidity (%)"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),  
    axis.text = element_text(size = 10),  
    axis.title = element_text(size = 12, face = "bold"),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted")
  ) +
  scale_x_continuous(breaks = seq(
    floor(min(data_clean$BarometricPressure, na.rm = TRUE)), 
    ceiling(max(data_clean$BarometricPressure, na.rm = TRUE)), 
    by = 5
  )) +  
  scale_y_continuous(breaks = seq(
    floor(min(data_clean$RelativeHumidity, na.rm = TRUE)), 
    ceiling(max(data_clean$RelativeHumidity, na.rm = TRUE)), 
    by = 10
  ))  

# Display the enhanced plot
print(plt_pressure_humidity)



#Plot Air Temperature Distribution by Sensor
data_clean$DeviceID <- as.factor(data_clean$DeviceID)  
plt_temp_sensor <- ggplot(data_clean, aes(x = DeviceID, y = AirTemperature, fill = DeviceID)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 2) +  
  scale_fill_brewer(palette = "Set3") +  
  labs(
    title = "Air Temperature Distribution Across Sensors",
    subtitle = "Comparing temperature ranges recorded by each sensor",
    x = "Sensor ID",
    y = "Air Temperature (°C)"
  ) +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 10),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),  
    legend.position = "none"  
  ) +
  scale_y_continuous(breaks = seq(
    floor(min(data_clean$AirTemperature, na.rm = TRUE)), 
    ceiling(max(data_clean$AirTemperature, na.rm = TRUE)), 
    by = 5
  ))  

# Display the enhanced plot
print(plt_temp_sensor)


# Step 3: Statistical Analysis

# Testing H1: Variance in Air Temperature Throughout the Day
result_temp_time_lm <- lm(AirTemperature ~ Hour, data = data_clean)
summary(result_temp_time_lm)

# Testing H2: Correlation Between Barometric Pressure and Relative Humidity
correlation <- cor(data_clean$BarometricPressure, data_clean$RelativeHumidity)
cat("Correlation between Barometric Pressure and Relative Humidity:", correlation, "\n")

# Testing H3: Sensor-Level Variation in Air Temperature
result_temp_sensor <- aov(AirTemperature ~ DeviceID, data = data_clean)
summary_temp_sensor <- summary(result_temp_sensor)
cat("ANOVA Result for Air Temperature by Device ID:\n")
print(summary_temp_sensor)


# Summary and Interpretation
cat("Summary of Findings:\n")
cat("1. Significant variation in air temperature was observed throughout the day.\n")
cat("2. A moderate negative correlation exists between barometric pressure and relative humidity.\n")
cat("3. Significant location-based variations were observed in air temperature.\n")


# Model Planning and Development
# Fit Linear Regression Model: Predicting AirTemperature based on BarometricPressure and RelativeHumidity
linear_model <- lm(AirTemperature ~ BarometricPressure + RelativeHumidity, data = data_clean)

model_summary <- summary(linear_model)
cat("Linear Model Summary:\n")
print(model_summary)

cat("\nCoefficients of the Linear Model:\n")
print(coef(linear_model))

data_clean$PredictedTemperature <- predict(linear_model, newdata = data_clean)


data_clean$Residuals <- data_clean$AirTemperature - data_clean$PredictedTemperature


cat("\nModel Evaluation Metrics:\n")
cat("Mean Absolute Error (MAE):", mean(abs(data_clean$Residuals)), "\n")
cat("Mean Squared Error (MSE):", mean(data_clean$Residuals^2), "\n")
cat("Root Mean Squared Error (RMSE):", sqrt(mean(data_clean$Residuals^2)), "\n")


saveRDS(linear_model, "linear_model.rds")
write.csv(data_clean, "predicted_data.csv", row.names = FALSE)

cat("\nLinear Regression Model and Predictions saved successfully.\n")




