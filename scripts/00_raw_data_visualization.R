#install.packages("knitr")
library(knitr)
#knitr::opts_knit$set(root.dir = "/Users/yurikim/Desktop/ForcastingProject/forecasting-project-2026")
knitr::opts_knit$set(root.dir = "forecasting-project-2026.Rproj")
knitr::opts_chunk$set(message = FALSE)

rm(list = ls())

# Load necessary libraries
install.packages("tseries")  
install.packages("xts") 
install.packages("forecast") 
install.packages("tsbox")    
install.packages("seasonal")
install.packages("mFilter")
library(tseries)
library(xts)
library(forecast) 
library(tsbox)    
library(seasonal)
library(mFilter)
library(ggplot2)

# Load the data
#rawData <- read.csv("data_raw/Filtered_data.csv", header = TRUE)
rawData <- read.csv("scripts/Filtered_data.csv", header = TRUE)
ncol(rawData)
nrow(rawData)
colnames(rawData)
data <- ts(rawData$OBS_VALUE,start = c(2005,1), frequency = 12)

frequency(data)
tsp(data)

# Understanding the Data -----------------------------------------------------------

# Visualize the data
g <- autoplot(data) +
  ggtitle("Swiss EV Registration") +
  ylab("No. of EV") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  # Remove legend title
  theme(axis.title.y = element_blank())  # Remove vertical axis label
g
ggsave("graphs/EV_registration_visualization.png", plot = g, width = 8, height = 5, dpi = 300)
# Observation 1
# The graph shows a clear upward trend in the number of EV registrations after around year 2020. 
# It means that the data is not stationary (non stationary mean), and we will need to apply transformations such as differencing to make it suitable for modeling.

# Observation 2
# Another feature is the graph fluctuates more in the later years, which suggests that the variance is not constant over time (non stationary variance). 
# 'Heteroscedasticity' is the term used to describe this phenomenon where the variance of the errors is not constant across all levels of an independent variable.
# This means that we may need to apply a transformation such as a log transformation to stabilize the variance before modeling.

# Observation 3
# Lastly, the structure changes after 2020, which may indicate a structural break in the data. Thus it may be hard to model that fit the whole data.


# seasonal graph to visualize the seasonal pattern in the data
season_plot <- ggseasonplot(data)
season_plot
ggsave("graphs/seasonal_EV_registration_visualization.png", plot = season_plot, width = 8, height = 5, dpi = 300)

# Observation 4
# From the seasonal graph, we can see that there is a clear strong seasonal pattern in the data, with peaks in June, Sep, Dec repeatly. 
# This means that we will need to account for seasonality in our modeling, such as using seasonal differencing or including seasonal dummy variables.

# Observation 5
# Also, the seasonal fluctuations are more pronounced in the later years, which further supports the observation of increasing variance over time.
# This means that we may need to apply a log transformation to stabilize the variance before modeling.

# Stationary transformations

# Applying log transformation to stabilize the variance
log_data <- log(data)

# Visualizing the log-series
g_log <- autoplot(log_data) +
  ggtitle("Log-Transformed Swiss EV Registrations") +
  theme_minimal()
g_log

# Test for stationarity on the log-level (Null Hypothesis (H0): The series has a unit root (is non-stationary))
adf_log <- adf.test(log_data, alternative = "stationary")
print(adf_log)
# Interpretation: If p-value > 0.05, we need to difference the data.

# Applying first difference to remove the trend (This represents the monthly growth rate in log terms)
diff_log_data <- diff(log_data)

# Final Stationarity Check
adf_final <- adf.test(diff_log_data, alternative = "stationary")
print(adf_final)
# "p-value smaller than printed p-value"; correct

# Visualizing the stationary series
g_stationary <- autoplot(diff_log_data) +
  ggtitle("Stationary Series (Monthly Change in Log-Registrations)") +
  theme_minimal()
g_stationary

# Saving the diagnostic plots
ggsave("graphs/stationary_series.png", plot = g_stationary, width = 8, height = 5)

# Commit: Added log transformation and first-differencing to ensure stationarity (validated with ADF test).
