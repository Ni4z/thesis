require(fracdiff)
require(esemifar)
require(forecast)
require(readxl)
# Load the data
nasdaq_data <- read_xlsx("Nikkei225_ETFs_Volume_Data.xlsx")

# Assuming the volume data is in a column named 'Volume'
Vol <- nasdaq_data$Volume[nasdaq_data$Volume > 0]

#Log-transform the Volume data
xt <- log(Vol)
n <- length(xt)

# Adjust the time range to reflect 01.01.2000 to 31.12.2019
Year <- (0:(n-1)) / n * 20 + 2000  # Adjust the scaling factor to match the 20-year period

###Trend treated by esemifar
results <- tsmoothlm(xt, p = 1, qmax = 1, pmax = 1, InfR = "Opt")

# Access the main estimation results
b.opt <- results$b0  # The optimal bandwidth
xte <- results$ye    # The trend estimates  
yt <- results$res    # The residuals

#FARIMA outcomes
results$FARMA.BIC[c("d", "ar", "ma")]
#Extract model parameters
p.BIC <- results$p.BIC
q.BIC <- results$q.BIC
d.BIC <- results$d.BIC

print(p.BIC)
print(q.BIC)

FARMA <- results$FARMA

# Extract standard errors
stderror <- results$FARMA.BIC$stderror.dpq
d_se <- stderror[1]
ar_se <- stderror[2]
ma_se <- stderror[3]

print(stderror)

#Display the results
b.opt
p.BIC
q.BIC
d.BIC
summary(FARMA)

coef(FARMA)
sqrt(diag(vcov(FARMA)))

#Fit Farima models to the residuals
yt.fit <- fracdiff(yt, nar = p.BIC, nma = q.BIC)
yt.fit

# Estimate d and its 95% confidence interval
d_hat <- yt.fit$d
se_d <- yt.fit$stderror.dpq[1]  # Correctly extracting the standard error for d
ci_lower <- d_hat - 1.96 * se_d
ci_upper <- d_hat + 1.96 * se_d

cat("Estimated d:", d_hat, "\n")
cat("95% Confidence Interval for d:", ci_lower, "-", ci_upper, "\n")

# Estimate phi_1 and its 95% confidence interval
phi_hat <- yt.fit$ar # Extract the AR coefficients
se_phi <- yt.fit$stderror.dpq[2]  # Standard error for phi_1

# Manually calculate 95% CI for phi_1
ci_lower_phi <- phi_hat - 1.96 * se_phi
ci_upper_phi <- phi_hat + 1.96 * se_phi

cat("Estimated phi_1:", phi_hat, "\n")
cat("95% Confidence Interval for phi_1:", ci_lower_phi, "-", ci_upper_phi, "\n")

#Forecast Future values
K <- 50
delta <- xte[n] - xte[n-1]
xte.fc <- xte[n] + (1:K) * delta
Vol.e.fc <- exp(xte.fc)

yt.fc <- forecast(yt.fit, h = K)

yt.fc.PFC <- yt.fc$mean
yt.fc.U80 <- yt.fc$upper[,1]
yt.fc.U95 <- yt.fc$upper[,2]
yt.fc.L80 <- yt.fc$lower[,1]
yt.fc.L95 <- yt.fc$lower[,2]

#### design of the figures with six windows, 3 for orignal and 3 log-data
TS.NA <- (1:n) * NA  # Helps to display the vanishing part of a series in a figure

TS.LT <- c(xt, yt.fc.PFC + xte.fc)  # L for log, T for total series
U80.LT <- c(TS.NA, yt.fc.U80 + xte.fc)
U95.LT <- c(TS.NA, yt.fc.U95 + xte.fc)
L80.LT <- c(TS.NA, yt.fc.L80 + xte.fc)
L95.LT <- c(TS.NA, yt.fc.L95 + xte.fc)

Kn <- 150  # Last 150 observations will be selected

TS.LTP <- c(xt[(n-Kn+1):n], yt.fc.PFC + xte.fc)  # P for selected piece/part
U80.LTP <- c(TS.NA[(n-Kn+1):n], yt.fc.U80 + xte.fc)
U95.LTP <- c(TS.NA[(n-Kn+1):n], yt.fc.U95 + xte.fc)
L80.LTP <- c(TS.NA[(n-Kn+1):n], yt.fc.L80 + xte.fc)
L95.LTP <- c(TS.NA[(n-Kn+1):n], yt.fc.L95 + xte.fc)

TS.LSP <- c(yt[(n-Kn+1):n], yt.fc.PFC)  # S for stationary component
U80.LSP <- c(TS.NA[(n-Kn+1):n], yt.fc.U80)
U95.LSP <- c(TS.NA[(n-Kn+1):n], yt.fc.U95)
L80.LSP <- c(TS.NA[(n-Kn+1):n], yt.fc.L80)
L95.LSP <- c(TS.NA[(n-Kn+1):n], yt.fc.L95)

# Plot the results
par(mfrow = c(3, 2))
Year <- (0:(n-1+K)) / n * (20 + K/250) + 2000

matplot(Year, exp(cbind(TS.LT, U80.LT, U95.LT, L80.LT, L95.LT)), type = "lllll", col = c(1, 2, 3, 2, 3), ylab = "")
lines(Year[1:n], exp(xte), col = "red", lwd = 2)
title("(a) Nikkei225 Volumes, point & 80%/95% FI")

matplot(Year, cbind(TS.LT, U80.LT, U95.LT, L80.LT, L95.LT), type = "lllll", col = c(1, 2, 3, 2, 3), ylab = "")
lines(Year[1:n], xte, col = "red", lwd = 2)  
title("(b) Log-Nikkei225 Volumes, point & 80%/95% FI")

Year <- Year[(n-Kn+1):(n+K)]

matplot(Year, exp(cbind(TS.LTP, U80.LTP, U95.LTP, L80.LTP, L95.LTP)), type = "lllll", col = c(1, 2, 3, 2, 3), ylab = "")
title("(c) Nikkei225 Volumes, point & 80%/95% FI, sel")

matplot(Year, cbind(TS.LTP, U80.LTP, U95.LTP, L80.LTP, L95.LTP), type = "lllll", col = c(1, 2, 3, 2, 3), ylab = "")
title("(d) Log-Nikkei225 Volumes, point & 80%/95% FI, sel")

matplot(Year, exp(cbind(TS.LSP, U80.LSP, U95.LSP, L80.LSP, L95.LSP)), type = "lllll", col = c(1, 2, 3, 2, 3), ylab = "")
title("(e) Stationary res, point & 80%/95% FI, orig")

matplot(Year, cbind(TS.LSP, U80.LSP, U95.LSP, L80.LSP, L95.LSP), type="lllll", col=c(1, 2, 3, 2, 3), ylab="")
title("(f) Stationary res, point & 80%/95% FI, log")


