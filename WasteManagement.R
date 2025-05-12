###############################################################################
# Load necessary libraries
###############################################################################
library(ggplot2)
library(readxl)
library(forecast)
library(smoots)    # For tsmoothlm(), tsmooth()
library(fracdiff)  # For estimating d
library(zoo)
library(deseats)
library(TSA)
library(esemifar)
library(scales)    # for alpha() in ggplot (ribbon transparency)

###############################################################################
# Step 1: Read and Prepare the Data
###############################################################################
# Import Data
new_data <- read_xls("WasteManagement.xls")

# Convert date column to Date class
new_data$observation_date <- as.Date(new_data$observation_date, format = "%Y-%m-%d")

# ----------------------------------------------------------------------
# FILTER the data: only keep observations between 1990-01-01 and 2019-12-31
# ----------------------------------------------------------------------
new_data <- new_data[
  new_data$observation_date >= as.Date("1990-01-01") &
    new_data$observation_date <= as.Date("2019-12-31"), 
]

# Extract start year and month
start_year  <- as.numeric(format(min(new_data$observation_date), "%Y"))
start_month <- as.numeric(format(min(new_data$observation_date), "%m"))

# Create a monthly time series (frequency=12)
Xt_ts <- ts(new_data$data, start = c(start_year, start_month), frequency = 12)

# Convert to zoo for autoplot
Xt_zoo <- as.zoo(Xt_ts)

###############################################################################
# Step 2: Plot the Series
###############################################################################
autoplot.zoo(Xt_zoo) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Data") +
  ggtitle("Monthly Data of Waste Manegement in USD/Tons")

###############################################################################
# Step 3: Periodogram of the Original Series
###############################################################################
periodogram(Xt_ts, main = "Periodogram of the Data")

###############################################################################
# Step 4: Apply tsmoothlm (LM) and tsmooth (SM) to Estimate Trends
###############################################################################
est_lm <- tsmoothlm(Xt_ts, pmax = 3, qmax = 3)
est_sm <- tsmooth(Xt_ts)

est_lm$b0
est_sm$b0

length(Xt_ts)

###############################################################################
# Step 5: Extract Trend and Residual Series
###############################################################################
# Extract the estimated trends
ye_lm <- ts(est_lm$ye, start = start(Xt_ts), frequency = 12)
ye_sm <- ts(est_sm$ye, start = start(Xt_ts), frequency = 12)

# Extract residuals
res_lm <- ts(est_lm$res, start = start(Xt_ts), frequency = 12)
res_sm <- ts(est_sm$res, start = start(Xt_ts), frequency = 12)

# Plot the Observations and Trends
autoplot.zoo(cbind(Xt_ts, ye_sm, ye_lm), facets = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Data") +
  ggtitle("Observed Data with SM and LM Trends") +
  scale_color_manual(
    name = "Series",
    labels = c("Observations", "SM trend", "LM trend"),
    values = c("grey70", "blue", "red")
  )

# Plot the residuals
.df <- data.frame(
  Residuals = c(res_sm, res_lm),
  Year = rep(time(res_sm), 2),
  Model = rep(c("SM", "LM"), each = length(Xt_ts))
)

ggplot(.df, aes(x = Year, y = Residuals)) +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylab("Residuals") +
  ggtitle("Residual Series (SM vs LM)") +
  facet_grid(Model ~ .)

###############################################################################
# Step 6: ACF and Periodogram of Residuals
###############################################################################
acf(c(res_lm), main = "ACF of the residuals")
periodogram(res_lm, main = "Periodogram of the residuals")

###############################################################################
# Step 7: Examine the FARIMA Components from tsmoothlm
###############################################################################
est_lm$FARMA.BIC[c("d", "ar", "ma")]

###############################################################################
# Step 8: FARIMA Residuals
###############################################################################
farima_res <- ts(est_lm$FARMA.BIC$residuals, start = start(Xt_ts), frequency = 12)
autoplot.zoo(farima_res) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Residuals") +
  ggtitle("Residuals of the Fitted FARIMA model")

periodogram(farima_res, main = "Periodogram of FARIMA residuals")

###############################################################################
# Step 9: Forecasting
###############################################################################
# (A) Fractional differencing estimation on residuals to confirm parameters
farima_fit <- fracdiff(est_lm$res, nar = est_lm$p.BIC, nma = est_lm$q.BIC)

d_value  <- farima_fit$d
d_stderr <- farima_fit$stderror.dpq[1]  # standard error of d
ci_lower <- d_value - 1.96 * d_stderr
ci_upper <- d_value + 1.96 * d_stderr

cat("\nEstimated d =", d_value, 
    "\nStd error of d =", d_stderr,
    "\n95% CI for d =", paste0("(", round(ci_lower,4), ", ", round(ci_upper,4), ")\n"))

# Decide if short memory or long memory
if (ci_lower <= 0 && ci_upper >= 0) {
  cat("=> 0 is within the CI => likely SHORT MEMORY\n\n")
  
  # (B1) If short memory, let's do an ARIMA forecast on the residuals:
  cat("Fitting ARIMA to the residuals, ignoring fractional differencing...\n")
  
  fit_arima <- auto.arima(est_lm$res) 
  # Forecast 20 steps ahead:
  arima_fc <- forecast(fit_arima, h = 10)
  fc_point <- arima_fc$mean
  fc_lower <- arima_fc$lower[, 2]  # 95% lower
  fc_upper <- arima_fc$upper[, 2]  # 95% upper
  
} else {
  cat("=> 0 is outside the CI => potential LONG MEMORY => use FARIMA forecast\n\n")
  
  # (B2) If truly long memory, proceed with FARIMA forecast from fracdiff object
  farima_fc <- forecast(farima_fit, h = 10)  # Forecast 20 steps ahead
  fc_point <- farima_fc$mean
  fc_lower <- farima_fc$lower[, 2]
  fc_upper <- farima_fc$upper[, 2]
}

# (C) Construct a naive trend forecast 
#     (switch to 'est_lm$ye' if you prefer the LM trend)
trend_vals <- est_lm$ye

delta <- as.numeric(diff(tail(trend_vals, 2)))  # slope of last two points
trend_start <- as.numeric(tail(trend_vals, 1))

h <- 1:10
trend_fc <- trend_start + h * delta

# Combine trend + residual forecast
point_fc <- trend_fc + fc_point
lower_fc <- trend_fc + fc_lower
upper_fc <- trend_fc + fc_upper

###############################################################################
# (D) Ensure Forecast Starts Immediately After the Last Obs
###############################################################################
end_obs <- end(Xt_ts)   
year_end  <- end_obs[1]
month_end <- end_obs[2]

# Start the forecast at the *next* month
start_year_fc  <- year_end
start_month_fc <- month_end +1

# If it goes beyond December, roll over
if (start_month_fc > 12) {
  start_year_fc  <- start_year_fc + 1
  start_month_fc <- start_month_fc - 12
}
# Convert the point forecast to a time series
fc_ts <- ts(
  point_fc,
  start = c(start_year_fc, start_month_fc),
  frequency = 12
)

###############################################################################
# (E) Build a Data Frame for ggplot
###############################################################################
fc_df <- data.frame(
  Time  = time(fc_ts),
  Point = as.numeric(point_fc),
  Lower = as.numeric(lower_fc),
  Upper = as.numeric(upper_fc)
)

###############################################################################
# (F) Plot the Final Forecast
###############################################################################
autoplot.zoo(Xt_ts, facets = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + 
  ylab("Data") +
  ggtitle("Observed Data with Point and Interval Forecasts") +
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

