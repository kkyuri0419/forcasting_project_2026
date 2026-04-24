install.packages("knitr")
library(knitr)
knitr::opts_knit$set(root.dir = "/Users/yurikim/Desktop/ForcastingProject/forecasting-project-2026")
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
rawData <- read.csv("data_raw/Filtered_data.csv", header = TRUE)
ncol(rawData)
nrow(rawData)
colnames(rawData)
data <- ts(rawData$OBS_VALUE,start = c(2005,1), frequency = 12)

frequency(data)
tsp(data)

# Section1 - Understanding the Data -----------------------------------------------------------

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



# Section2 - Preprocessing -----------------------------------------------------------

# differentiate the data to remove the trend
diff_data <- diff(data)
autoplot(diff_data)

#Although the series has been differenced, the variance is still increasing over time, 
#so it is not suitable for modeling yet — a log transformation is needed
log_data <- log(data)
diff_log <- diff(log_data)
autoplot(diff_log)
