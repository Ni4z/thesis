#------------------------------------------------------------
# Thesis: Modeling and forecasting trend- or scale-stationary time
# series under short or long memory applied to economic,
# financial and environmental data
#------------------------------------------------------------

#------------------------------------------------------------
# Loading necessary libraries
#------------------------------------------------------------
library(ggplot2)
library(fracdiff)
library(grid)
library(gridExtra)
library(esemifar)
library(longmemo)
library(zoo)
library(readxl)

packageVersion("esemifar")

#------------------------------------------------------------
# Load the Dataset
#------------------------------------------------------------

# Loading the dataset
producerprice_us <- read_xls("ProducerPriceIndex.xls")
consumerprice_us <- read_xls("ConsumerPriceIndex.xls")
employment_us <- read_xls("employmentLevelUSA.xls")

# Change Column names for the dataset
names(producerprice_us) <- c("DATE", "USD")
names(consumerprice_us) <- c("DATE", "USD")
names(employment_us) <- c("DATE", "Index")

# Assign DATE format for the dataset
producerprice_us$DATE <- as.Date(producerprice_us$DATE, format = "%Y-%m-%d")
consumerprice_us$DATE <- as.Date(consumerprice_us$DATE, format = "%Y-%m-%d")
employment_us$DATE <- as.Date(employment_us$DATE, format = "%Y-%m-%d")

# Extract the year and month from the first date entry
start_year_pp <- as.numeric(format(min(producerprice_us$DATE), "%Y"))
start_month_pp <- as.numeric(format(min(producerprice_us$DATE), "%m"))

start_year_cp <- as.numeric(format(min(consumerprice_us$DATE), "%Y"))
start_month_cp <- as.numeric(format(min(consumerprice_us$DATE), "%m"))

start_year_emp <- as.numeric(format(min(employment_us$DATE), "%Y"))
start_month_emp <- as.numeric(format(min(employment_us$DATE), "%m"))

#------------------------------------------------------------
# Filter Data to Set Range
#------------------------------------------------------------

producerprice_us <- producerprice_us[producerprice_us$DATE >= as.Date("1974-01-01") 
                                     & producerprice_us$DATE <= as.Date("2019-12-31"), ]

consumerprice_us <- consumerprice_us[consumerprice_us$DATE >= as.Date("1947-01-01") 
                                     & consumerprice_us$DATE <= as.Date("2019-12-31"), ]

employment_us <- employment_us[employment_us$DATE >= as.Date("1948-01-01") 
                               & employment_us$DATE <= as.Date("2019-12-31"), ]

#------------------------------------------------------------
# Create Time Series Objects 
#------------------------------------------------------------

producerprice_us_ts <- ts(producerprice_us$USD, start=c(start_year_pp, start_month_pp), frequency=12)
consumerprice_us_ts <- ts(consumerprice_us$USD, start=c(start_year_cp, start_month_cp), frequency=12)
employment_us_ts <- ts(employment_us$Index, start=c(start_year_emp, start_month_emp), frequency=12)

# Log-transforming Time Series Objects 
log_producerprice_us_ts <- log(producerprice_us_ts)
log_consumerprice_us_ts <- log(consumerprice_us_ts)
log_employment_us_ts <- log(employment_us_ts)

#------------------------------------------------------------
# Plot the Time Series Objects
#------------------------------------------------------------

plot_pp <- autoplot.zoo(log_producerprice_us_ts, frequency=12) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Log-USD") +
  ggtitle("Monthly Log US Producer Price, Jan 1974 to December 2019")

plot_cp <- autoplot.zoo(log_consumerprice_us_ts, frequency=12) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Log-USD") +
  ggtitle("Monthly Log US Consumer Price, Jan 1947 to December 2019")

plot_emp <- autoplot.zoo(log_employment_us_ts, frequency=12) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Log-Index") +
  ggtitle("Monthly Log US Employment, Jan 1948 to December 2019")

# Combining all log-transformed time series plots into one
combined_plot_ts <- grid.arrange(
  plot_pp,
  plot_cp,
  plot_emp,
  ncol = 1  # Arrange plots in a single column
)

#------------------------------------------------------------
# Generating LM and SM Trends for Producer Price Data
#------------------------------------------------------------

est_lm_pp <- tsmoothlm(log_producerprice_us_ts, p=1, pmax = 3, qmax = 3)
est_lm_pp$b0

length(log_producerprice_us_ts)

### Plot the LM trends for Producer Price Data 

ye_lm_pp <- ts(est_lm_pp$ye, start = c(1974, 1), frequency = 12)

res_lm_pp <- ts(est_lm_pp$res, start = c(1974, 1), frequency = 12)

plot_trends_pp <- autoplot.zoo(cbind(log_producerprice_us_ts, ye_lm_pp), facets = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Log-USD") +
  ggtitle("Log of US Producer Price & Estimated Trend") +
  scale_color_manual(name = "Series", labels = c("Observations", "LM Trend"), values = c("grey70", "red"))

plot_trends_pp

#------------------------------------------------------------
# Generating LM Trend for Consumer Price Data
#------------------------------------------------------------

est_lm_cp <- tsmoothlm(log_consumerprice_us_ts, p=1, pmax = 3, qmax = 3)

est_lm_cp$b0

length(log_consumerprice_us_ts)

### Plot the LM trends for Consumer Price Data 

ye_lm_cp <- ts(est_lm_cp$ye, start = c(1947, 1), frequency = 12)

res_lm_cp <- ts(est_lm_cp$res, start = c(1947, 1), frequency = 12)

plot_trends_cp <- autoplot.zoo(cbind(log_consumerprice_us_ts, ye_lm_cp), facets = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Log-USD") +
  ggtitle("Log of US Consumer Price and Estimated Trend") +
  scale_color_manual(name = "Series", labels = c("Observations",  "LM Trend"), values = c("grey70", "red"))

plot_trends_cp

#------------------------------------------------------------
# Generating LM Trends for Employment Data
#------------------------------------------------------------

est_lm_emp <- tsmoothlm(log_employment_us_ts, p=1, pmax = 3, qmax = 3)

est_lm_emp$b0

length(log_employment_us_ts)

### Plot the LM for Employment Data 

ye_lm_emp <- ts(est_lm_emp$ye, start = c(1948, 1), frequency = 12)

res_lm_emp <- ts(est_lm_emp$res, start = c(1948, 1), frequency = 12)

plot_trends_emp <- autoplot.zoo(cbind(log_employment_us_ts, ye_lm_emp), facets = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Log-Index") +
  ggtitle("Log of US Employment & Estimated Trend") +
  scale_color_manual(name = "Series", labels = c("Observations", "LM Trend"), values = c("grey70", "red"))

plot_trends_emp

#------------------------------------------------------------
# Combined LM Trends and Residuals of Fitted FARIMA for All Data
#------------------------------------------------------------

# Residuals of the fitted FARIMA model for Producer Price Data
# Extract the components of FARIMA for Producer Price Data
farima_components_pp <- est_lm_pp$FARMA.BIC[c("d", "ar", "ma")]
farima_components_pp

# Determine p and q for Producer Price Data
p_pp <- length(farima_components_pp$ar)
q_pp <- length(farima_components_pp$ma)

print(p_pp)
print(q_pp)

# Extract standard errors for FARIMA parameters for Producer Price Data
stderror_pp <- est_lm_pp$FARMA.BIC$stderror.dpq

print(stderror_pp)

# Plot FARIMA residuals for Producer Price Data
farima_res_pp <- ts(est_lm_pp$FARMA.BIC$residuals, start = c(1974, 1), frequency = 12)
farima_plot_pp <- autoplot.zoo(farima_res_pp) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Residuals") +
  ggtitle("(b) Trend-adjusted Residuals of Log-US Producer Price")



# Residuals of the fitted FARIMA model for Consumer Price Data
# Extract the components
farima_components_cp <- est_lm_cp$FARMA.BIC[c("d", "ar", "ma")]
farima_components_cp

# Determine p and q for Consumer Price Data
p_cp <- length(farima_components_cp$ar)
q_cp <- length(farima_components_cp$ma)

print(p_cp)
print(q_cp)

# Extract standard errors for FARIMA parameters for Consumer Price Data
stderror_cp <- est_lm_cp$FARMA.BIC$stderror.dpq

print(stderror_cp)

# Plot FARIMA residuals for Consumer Price Data
farima_res_cp <- ts(est_lm_cp$FARMA.BIC$residuals, start = c(1947, 1), frequency = 12)
farima_plot_cp <- autoplot.zoo(farima_res_cp) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Residuals") +
  ggtitle("(d) Trend-adjusted Residuals of Log-US Consumer Price")

# Residuals of the fitted FARIMA model for Employment Data
# Extract the FARIMA components for Employment Data
farima_components_emp <- est_lm_emp$FARMA.BIC[c("d", "ar", "ma")]
farima_components_emp

# Determine p and q
p_emp <- length(farima_components_emp$ar)
q_emp <- length(farima_components_emp$ma)

print(p_emp)
print(q_emp)

# Extract standard errors for Employment Data
stderror_emp <- est_lm_emp$FARMA.BIC$stderror.dpq
print(stderror_emp)

# Plot the FARIMA residuals for Employment Data
farima_res_emp <- ts(est_lm_emp$FARMA.BIC$residuals, start = c(1948, 1), frequency = 12)
farima_plot_emp <- autoplot.zoo(farima_res_emp) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") + ylab("Residuals") +
  ggtitle("(f) Trend-adjusted Residuals of Log-US Emploment")

# Combined FARIMA Residuals Plots for All Data
all_combined_plot_ts <- grid.arrange(
  farima_plot_pp,
  farima_plot_cp,
  farima_plot_emp,
  ncol = 1  # Arrange plots in a single column
)

#------------------------------------------------------------
# Predictions for Producer Price Data
#------------------------------------------------------------

fc_pp <- predict(est_lm_pp, n.ahead = 20, method = "norm", expo = TRUE)

plot_fc_pp <- plot(fc_pp)

fc_pp$mean
fc_pp$lower
fc_pp$upper

#------------------------------------------------------------
# Predictions for Consumer Price Data
#------------------------------------------------------------

fc_cp <- predict(est_lm_cp, n.ahead = 20, method = "norm", expo = TRUE)

plot_fc_cp <- plot(fc_cp)

fc_cp$mean
fc_cp$lower
fc_cp$upper

#------------------------------------------------------------
# Predictions for Employment Data
#------------------------------------------------------------

fc_emp <- predict(est_lm_emp, n.ahead = 20, method = "norm", expo = TRUE)

plot_fc_emp <- plot(fc_emp)

fc_emp$mean
fc_emp$lower
fc_emp$upper

#------------------------------------------------------------
# Plot Forecasts for All Data
#------------------------------------------------------------

# Set up the plotting area for three plots stacked vertically
par(mfrow = c(3, 1))

# Generate the first plot with custom title and axis labels
plot(fc_pp, 
     main = "(a) Interval and Point Forecasts for Log-US Producer Price Data", 
     xlab = "Observations", 
     ylab = "Log-USD")

# Generate the second plot with custom title and axis labels
plot(fc_cp, 
     main = "(b) Interval and Point Forecasts for Log-US Consumer Price Data", 
     xlab = "Observations", 
     ylab = "Log-USD")

# Generate the third plot with custom title and axis labels
plot(fc_emp, 
     main = "(c) Interval and Point Forecasts for Log-US Employment Data", 
     xlab = "Observations", 
     ylab = "Log-Index")

# Reset the plotting area back to the default
par(mfrow = c(1, 1))

