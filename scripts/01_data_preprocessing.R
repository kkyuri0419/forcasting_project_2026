source("scripts/setup.R")


# 00_Load the Data -----------------------------------------------------------

rawData <- read.csv("data_raw/Filtered_data.csv", header = TRUE)
#ncol(rawData)
#nrow(rawData)
#colnames(rawData)
data <- ts(rawData$OBS_VALUE,start = c(2005,1), frequency = 12)


# 01_Differentiate the data --------------------------------------------------


# differentiate the data to remove the trend


# Apply log transformation to stabilize the variance
log_data <- log(data)
log_g <- autoplot(log_data)
log_g
ggsave("graphs/data_log_transformed_visualization.png", plot = log_g, width = 8, height = 5, dpi = 300)

# Apply differencing to remove the trend
diff_log_data <- diff(log_data)
df_g <- autoplot(diff_log_data)
df_g
ggsave("graphs/data_diff_log_visualization.png", plot = df_g, width = 8, height = 5, dpi = 300)

# Apply seasonal differencing to remove the seasonality
diff_seasonal <- diff(diff_log_data, lag=12)
df_s_g <- autoplot(diff_seasonal)
df_s_g
ggsave("graphs/data_diff_seasonal_visualization.png", plot = df_s_g, width = 8, height = 5, dpi = 300)
