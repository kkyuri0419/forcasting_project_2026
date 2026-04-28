# .R -----------------------------------------------------
# Goal: Test if Oil Prices help predict EV Registrations (Lecture 2)

library(tidyquant)
library(tidyverse)
library(forecast)

# 1. Cargar los datos que Yuri procesó
load("data_preprocessed/processed_data.RData") 

# 2. Descargar el precio del Brent (Petróleo de referencia en Europa)
# 'DCOILBRENTEU' es el código de la Reserva Federal (FRED)
oil_raw <- tq_get("DCOILBRENTEU", get = "economic.data", 
                  from = "2005-01-01", to = "2024-03-31")

# Limpiar y alinear a meses (sacamos el promedio mensual)
oil_monthly <- oil_raw %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarize(oil_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# Crear el objeto de serie de tiempo para el petróleo
# Nota: debe coincidir exactamente con el inicio de tus datos de EVs
oil_ts <- ts(oil_monthly$oil_price, start = c(2005, 1), frequency = 12)

# 3. Transformación: Log y Diferencia (Igual que hicimos con los EVs)
log_oil <- log(oil_ts)
diff_log_oil <- diff(log_oil)