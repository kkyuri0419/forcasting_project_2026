source("scripts/setup.R")


# 00_Load the Data -----------------------------------------------------------

rawData <- read.csv("data_raw/Filtered_data.csv", header = TRUE)
#ncol(rawData)
#nrow(rawData)
#colnames(rawData)
data <- ts(rawData$OBS_VALUE,start = c(2005,1), frequency = 12)


# 01_Differentiate the data --------------------------------------------------


# differentiate the data to remove the trend
diff_data <- diff(data)
df_g <- autoplot(diff_data)
df_g
ggsave("graphs/data_differentiation_visualization.png", plot = df_g, width = 8, height = 5, dpi = 300)

#Although the series has been differenced, the variance is still increasing over time, 
#so it is not suitable for modeling yet — a log transformation is needed
log_data <- log(data)
log_g <- autoplot(log_data)
log_g
ggsave("graphs/data_log_transformed_visualization.png", plot = log_g, width = 8, height = 5, dpi = 300)
