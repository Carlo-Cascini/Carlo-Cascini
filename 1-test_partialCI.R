# Load necessary libraries
library(devtools)
library(quantmod)
library(xts)
library(zoo)
library(partialAR)
library(partialCI)
library(TTR)
library(readxl)
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)

# Read the Excel file
data <- read_excel("Desktop/tesi master/partial_CI/67_cleaned_etfs.xlsx")

# Convert the dataframe to xts object
data$Dates <- as.Date(data$Dates)
data_xts <- xts(data[, -1], order.by = data$Dates)

# Define a list of stock tickers
stock_tickers <- c("EWA", "EWK", "EWO", "EWC", "EWQ", "EWG", "EWH", "EWI", "EWJ", "EWM", "EWW", "EWN", "EWS", "EWP", "EWD", 
                   "EWL", "EWY", "EZU", "EWU", "EWZ", "EWT", "SPY", "EZA", "EPI", "RSX", "TUR", "EIS", "THD", "PIN", "NORW", 
                   "EEM", "VWO", "AAXJ", "ILF", "AFK", "FEZ", "XLF", "XLK", "XLE", "XLV", "XLY", "XLI", "XLB", "XLU", "IYR", 
                   "SMH", "XBI", "VTI", "IVV", "QQQ", "IWV", "GLD", "SLV", "USO", "UNG", "DBO", "DBC", "UGA", "DBA", "GSG", 
                   "SOXX", "FDN", "TAN", "ICLN", "PBW", "IBB", "PNQI")

# Generate unique pairs of stock tickers
stock_pairs <- combn(stock_tickers, 2, simplify = FALSE)

# Function to calculate rsq_MR and check conditions for a given time window
calculate_rsq_MR <- function(stock_a, stock_b, start_date, end_date) {
  tryCatch({
    # Extract the data for the given stocks
    a_prices <- data_xts[index(data_xts) >= start_date & index(data_xts) <= end_date, stock_a, drop = FALSE]
    b_prices <- data_xts[index(data_xts) >= start_date & index(data_xts) <= end_date, stock_b, drop = FALSE]
    
    # Merge the adjusted closing prices
    prices <- merge(a_prices, b_prices, all = FALSE)
    
    # Fit the PCI model
    fit <- fit.pci(prices[, 1], prices[, 2])
    
    # Calculate rsq_MR
    sigmak <- fit$sigma_M
    sigmaz <- fit$sigma_R
    rho <- fit$rho
    beta<-fit$beta
    rsq_MR <- fit$pvmr
    
    # Return result including negloglik
    return(list(stock_a = stock_a, stock_b = stock_b, rsq_MR = rsq_MR, rho = rho, negloglik = fit$negloglik, beta=beta))
  }, error = function(e) {
    message(paste("Error with", stock_a, "and", stock_b, ":", e$message))
    return(NULL)  # Handle errors gracefully
  })
}

# Define the starting and ending years for the analysis
start_year <- 2010
end_year <- 2023
window_length <- 5

# Define the directory to save results
save_dir <- "/Users/carlocascini/Desktop/tesi master/partial_CI/test_1/"

process_window <- function(start) {
  # Capture the start time
  start_time <- Sys.time()
  
  # Define time windows for analysis and trading
  analysis_start_date <- as.Date(paste0(start, "-01-01"))
  analysis_end_date <- as.Date(paste0(start + window_length - 1, "-12-31"))
  trading_start_date <- as.Date(paste0(start + window_length, "-01-01"))
  trading_end_date <- as.Date(paste0(start + window_length + 1, "-12-31"))
  
  # Print the period being processed for debugging
  message(paste("Processing period:", start, "to", start + window_length))
  
  # Iterate over each stock pair and calculate the results
  results <- lapply(stock_pairs, function(pair) {
    calculate_rsq_MR(pair[1], pair[2], analysis_start_date, analysis_end_date)
  })
  
  # Filter out NULL results
  results <- Filter(function(x) !is.null(x), results)
  
  # Filter by conditions on rho and rsq_MR
  final_results <- Filter(function(x) abs(x$rho) > 0.5 && abs(x$rho) < 1 && x$rsq_MR > 0.5, results)
  
  # Dynamically assign the results to a variable with the name corresponding to the period
  variable_name <- paste0("results_", start + window_length, "-", start + window_length + 1)
  assign(variable_name, final_results, envir = .GlobalEnv)
  
  # Save results to a file in the specified directory
  file_name <- paste0(save_dir, "results_", start + window_length, "-", start + window_length + 1, ".RData")
  save(list = variable_name, file = file_name)
  
  # Capture the end time and calculate the elapsed time
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  
  # Print the number of results and time taken
  num_results <- length(final_results)
  message(paste("Number of results:", num_results))
  message(paste("Time taken for period", start, "to", start + window_length, ":", format(elapsed_time, digits = 2)))
  
  # Return path to the saved file
  return(file_name)
}

# Process each time window and save results incrementally
for (start in start_year:(end_year - window_length)) {
  result_file <- process_window(start)
  message(paste("Results saved to", result_file))
    Sys.sleep(1) #interrompi esecuzione 
}