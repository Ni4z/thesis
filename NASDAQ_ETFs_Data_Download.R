# ---------------------------
# Script: Download Trade Volume Data for NASDAQ ETFs
# Description: Downloads historical trade volume data for specified ETFs tracking the NASDAQ index.
# Author: [Your Name]
# Date: [Today's Date]
# ---------------------------

# ---------------------------
# 1. Install and Load Required Packages
# ---------------------------

# List of required packages
required_packages <- c("tidyquant", "writexl", "dplyr", "tidyr", "ggplot2", "purrr")

# Install any missing packages
installed_packages <- rownames(installed.packages())
for(pkg in required_packages){
  if(!(pkg %in% installed_packages)){
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load the packages
library(tidyquant)  # For financial data retrieval
library(writexl)     # For writing data to Excel
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(ggplot2)     # For data visualization
library(purrr)       # For functional programming (includes 'reduce')

# ---------------------------
# 2. Define NASDAQ ETF Symbols
# ---------------------------

# ETFs tracking the NASDAQ Index
nasdaq_etfs <- c(
  "QQQ",   # Invesco QQQ Trust
  "ONEQ",  # Fidelity Nasdaq Composite Index ETF
  "TQQQ",  # ProShares UltraPro QQQ
  "SQQQ",  # ProShares UltraShort QQQ
  "PSQ"    # ProShares Short QQQ
)

# **Note:**
# - **`QQQ`** and **`ONEQ`** are the primary ETFs tracking the NASDAQ Composite Index.
# - **Leverage ETFs:** **`TQQQ`** (3x long) and **`SQQQ`** (3x short) provide leveraged exposure.
# - **`PSQ`** offers inverse exposure to the NASDAQ 100 Index.
# - **Ensure Symbols Are Correct:** Verify each symbol on [Yahoo Finance](https://finance.yahoo.com/) to confirm their validity and availability.

# Combine all ETF symbols
all_etfs <- nasdaq_etfs

# ---------------------------
# 3. Set Date Range for Data Retrieval
# ---------------------------

# Define the start and end dates for the historical data
start_date <- "2000-01-01"        # Start Date (YYYY-MM-DD)
end_date <- "2019-12-31"             # End Date: Current Date

# ---------------------------
# 4. Download Volume Data Using tidyquant
# ---------------------------

# Function to download volume data for a given ETF symbol
download_volume <- function(symbol, from, to){
  tryCatch({
    # Fetch historical stock prices
    data <- tq_get(symbol,
                   from = from,
                   to = to,
                   get = "stock.prices")
    
    # Check if data is returned
    if(nrow(data) == 0){
      message(paste("No data found for:", symbol))
      return(NULL)
    }
    
    # Select relevant columns: Date and Volume
    volume_data <- data %>%
      select(date, volume) %>%
      rename(Date = date,
             !!paste0("Volume_", symbol) := volume)
    
    return(volume_data)
  }, error = function(e){
    message(paste("Error downloading data for:", symbol, "-", e$message))
    return(NULL)
  })
}

# Initialize an empty list to store volume data
volume_list <- list()

# Loop through each ETF symbol and download volume data
for(etf in all_etfs){
  cat("Downloading data for:", etf, "\n")
  vol_data <- download_volume(etf, start_date, end_date)
  
  if(!is.null(vol_data)){
    volume_list[[etf]] <- vol_data
  }
}

# ---------------------------
# 5. Combine Volume Data into a Single Dataframe
# ---------------------------

# Check if any volume data was downloaded
if(length(volume_list) == 0){
  stop("No volume data was downloaded. Please check the ETF symbols and internet connection.")
}

# Merge all volume data by Date using purrr::reduce and dplyr::full_join
combined_volume <- volume_list %>%
  reduce(full_join, by = "Date") %>%
  arrange(Date)

# Optionally, replace NA values with 0
# Uncomment the next line if you prefer to treat missing volumes as zero
# combined_volume[is.na(combined_volume)] <- 0

# View the first few rows of the combined volume data
print("First Few Rows of Combined Volume Data:")
print(head(combined_volume))

# ---------------------------
# 6. Save the Combined Volume Data to an Excel File
# ---------------------------

# Define the output Excel file path
output_excel <- "NASDAQ_ETFs_Volume_Data.xlsx"

# Write the combined volume data to Excel
write_xlsx(combined_volume, path = output_excel)

# Confirmation message
cat("Trade volume data successfully saved to", output_excel, "\n")

# ---------------------------
# 7. Optional: Visualize the Volume Data
# ---------------------------

# Uncomment the following block to generate volume plots

# # Gather the data for plotting
# volume_long <- combined_volume %>%
#   pivot_longer(cols = starts_with("Volume_"),
#                names_to = "ETF",
#                values_to = "Volume")
# 
# # Plot Volume Over Time for Each ETF
# ggplot(volume_long, aes(x = Date, y = Volume, color = ETF)) +
#   geom_line() +
#   labs(title = "Trade Volume Over Time for NASDAQ ETFs",
#        x = "Date",
#        y = "Volume",
#        color = "ETF Symbol") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
