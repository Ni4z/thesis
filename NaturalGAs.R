###############################################################################
# Load necessary libraries
###############################################################################
library(ggplot2)
library(readxl)
library(forecast)
library(smoots)     # tsmoothlm(), tsmooth()
library(fracdiff)   # For estimating d, etc.
library(zoo)
library(deseats)
library(TSA)
library(esemifar)
library(scales)     # alpha() in ggplot

###############################################################################
# Step 1: Read and Prepare the Data
###############################################################################
# Import Data
new_data <- read_xlsx("FuelEthanolConsumption.xlsx")

# Convert date column to Date class
new_data$observation_date <- as.Date(new_data$observation_date, format = "%Y-%m-%d")

# Filter the data: only keep observations from 1990-01-01 to 2019-12-31
new_data <- new_data[
  new_data$observation_date >= as.Date("2000-01-01") &
    new_data$observation_date <= as.Date("2019-12-31"), 
]

# Extract start year and month from the earliest date
start_year  <- as.numeric(format(min(new_data$observation_date), "%Y"))
start_month <- as.numeric(format(min(new_data$observation_date), "%m"))

# Create a monthly time series (frequency = 12)
Xt_ts <- ts(new_data$data, start = c(start_year, start_month), frequency = 12)

###############################################################################
# Step 1.5: Seasonal Adjustment via STL
###############################################################################
# Decompose the series with STL (Seasonal and Trend decomposition using Loess)
# s.window = "periodic" is standard for strongly seasonal monthly data
stl_decomp  <- stl(Xt_ts, s.window = "periodic")
# Extract the seasonally adjusted series (original minus seasonal component)
Xt_sa <- seasadj(stl_decomp)

# Convert to zoo for easy plotting
Xt_sa_zoo <- as.zoo(Xt_sa)

###############################################################################
# Step 2: Plot the Seasonally Adjusted Series
###############################################################################
autoplot.zoo(Xt_sa_zoo) +
  theme_bw() +
  xlab("Year") + 
  ylab("Data (Seasonally Adjusted)") +
  ggtitle("Seasonally Adjusted Monthly Data from 1990 to 2019")

###############################################################################
# (Optional) Periodogram of the Seasonally Adjusted Series
###############################################################################
TSA::periodogram(Xt_sa, main = "Periodogram of Seasonally Adjusted Series")

###############################################################################
# Step 3: Fit tsmoothlm (LM) and tsmooth (SM) to the Seasonally Adjusted Data
###############################################################################
est_lm <- tsmoothlm(Xt_sa, pmax = 3, qmax = 3)
est_sm <- tsmooth(Xt_sa)

cat("LM intercept =", est_lm$b0, "\n")
cat("SM intercept =", est_sm$b0, "\n")

###############################################################################
# Step 4: Extract Trend and Residual Series from the Seasonally Adjusted Data
###############################################################################
ye_lm <- ts(est_lm$ye, start = start(Xt_sa), frequency = 12)
ye_sm <- ts(est_sm$ye, start = start(Xt_sa), frequency = 12)

res_lm <- ts(est_lm$res, start = start(Xt_sa), frequency = 12)
res_sm <- ts(est_sm$res, start = start(Xt_sa), frequency = 12)

###############################################################################
# Step 5: Plot the Seasonally Adjusted Data + LM and SM Trends
###############################################################################
# Combine them in a zoo object for autoplot
sa_data_zoo <- as.zoo(cbind(Xt_sa, ye_lm, ye_sm))

# We customize colors: black = data, red = LM trend, blue = SM trend
autoplot.zoo(sa_data_zoo, facets = NULL) +
  theme_bw() +
  xlab("Year") + 
  ylab("Data (SA)") +
  ggtitle("Seasonally Adjusted Data with LM (red) & SM (blue) Trends") +
  scale_color_manual(
    name   = "Series",
    values = c("black", "red", "blue"),  # match the order of cbind
    labels = c("SA Data", "LM Trend", "SM Trend")
  )

###############################################################################
# Step 6: ACF and Periodogram of LM Residuals (Optional)
###############################################################################
acf(res_lm, main = "ACF of LM Residuals (SA)")
TSA::periodogram(res_lm, main = "Periodogram of LM Residuals (SA)")

###############################################################################
# Step 7: FARIMA Components from tsmoothlm
###############################################################################
cat("\nFARIMA components (LM on SA data):\n")
print(est_lm$FARMA.BIC[c("d", "ar", "ma")])

###############################################################################
# Step 8: FARIMA Residuals
###############################################################################
farima_res <- ts(est_lm$FARMA.BIC$residuals, start = start(Xt_sa), frequency = 12)
autoplot.zoo(farima_res) +
  theme_bw() +
  xlab("Year") + 
  ylab("Residuals") +
  ggtitle("FARIMA Residuals (LM Approach on SA Data)")

TSA::periodogram(farima_res, main = "Periodogram of FARIMA Residuals (SA)")

###############################################################################
# Step 9: Forecasting (FARIMA ONLY, Long Memory)
###############################################################################
# Re-estimate with fracdiff to confirm AR/MA orders, using the LM residuals
farima_fit <- fracdiff(res_lm, nar = est_lm$p.BIC, nma = est_lm$q.BIC, h = 1)

# Forecast 10 steps ahead with the FARIMA model
farima_fc <- forecast(farima_fit, h = 10)

# Extract point + intervals
fc_point <- farima_fc$mean
fc_lower <- farima_fc$lower[, 2]  # 95% lower
fc_upper <- farima_fc$upper[, 2]  # 95% upper

###############################################################################
# Step 10: Combine FARIMA Residual Forecast with the LM Trend
###############################################################################
trend_vals <- est_lm$ye  # LM trend from the seasonally adjusted data
delta <- diff(tail(trend_vals, 2))
trend_start <- tail(trend_vals, 1)

h <- 1:10
trend_fc <- trend_start + h * delta

# Final forecast = (LM Trend) + (FARIMA residual forecast)
point_fc <- trend_fc + fc_point
lower_fc <- trend_fc + fc_lower
upper_fc <- trend_fc + fc_upper

###############################################################################
# Step 11: Build a Time Series for the Forecast (Next Month Start)
###############################################################################
end_obs <- end(Xt_sa)  # end of the seasonally adjusted series
year_end  <- end_obs[1]
month_end <- end_obs[2]

start_year_fc  <- year_end
start_month_fc <- month_end + 1
if (start_month_fc > 12) {
  start_year_fc  <- start_year_fc + 1
  start_month_fc <- start_month_fc - 12
}

fc_ts <- ts(
  point_fc,
  start = c(start_year_fc, start_month_fc),
  frequency = 12
)

###############################################################################
# Step 12: Plot the Final Forecast
###############################################################################
fc_df <- data.frame(
  Time  = time(fc_ts),
  Point = as.numeric(point_fc),
  Lower = as.numeric(lower_fc),
  Upper = as.numeric(upper_fc)
)

# We'll overlay the seasonally adjusted data (black) plus forecast
autoplot.zoo(Xt_sa, facets = NULL) +
  theme_bw() +
  xlab("Year") +
  ylab("Data (SA)") +
  ggtitle("Seasonally Adjusted Data + FARIMA Forecast (Long Memory)") +
  geom_ribbon(
    data = fc_df,
    aes(x = Time, ymin = Lower, ymax = Upper),
    fill = alpha("blue", 0.3),
    inherit.aes = FALSE
  ) +
  geom_line(
    data = fc_df,
    aes(x = Time, y = Point),
    color = "blue",
    size = 1,
    inherit.aes = FALSE
  )

