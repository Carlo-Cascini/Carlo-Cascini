# Function to calculate bottom 5% threshold for negative log likelihood
get_bottom_5_percent_threshold <- function(results_variable_name) {
  # Retrieve the results list from the global environment
  results <- get(results_variable_name, envir = .GlobalEnv)
  
  # Check if results are not empty
  if (length(results) == 0) {
    cat("No results available in", results_variable_name, "\n")
    return(NULL)
  } else {
    # Extract negative log likelihood values
    negloglik_values <- sapply(results, function(x) x$negloglik)
    
    # Calculate the threshold for the bottom 5%
    threshold <- quantile(negloglik_values, 0.05)
    
    return(threshold)
  }
}

# Function to filter results based on negative log likelihood threshold
filter_by_negloglik <- function(results_variable_name, threshold) {
  # Retrieve the results list from the global environment
  results <- get(results_variable_name, envir = .GlobalEnv)
  
  # Check if results are not empty
  if (length(results) == 0) {
    cat("No results available in", results_variable_name, "\n")
    return(NULL)
  } else {
    # Filter results to get those with negative log likelihood <= threshold
    bottom_5_percent_results <- Filter(function(x) x$negloglik <= threshold, results)
    return(bottom_5_percent_results)
  }
}

# Function to filter results by additional conditions
filter_by_conditions <- function(results) {
  filtered_results <- Filter(function(x) x$rho > 0.5 && x$rho < 1 && x$rsq_MR > 0.5, results)
  return(filtered_results)
}

# Function to create a list of pairs from filtered results
create_pairs_list <- function(filtered_results) {
  pairs <- lapply(filtered_results, function(x) {
    c(x$stock_a, x$stock_b)
  })
  return(pairs)
}

# Loop through each period from 2015-2016 to 2023-2024
for (start_year in 2015:2023) {
  end_year <- start_year + 1
  results_variable_name <- paste0("results_", start_year, "-", end_year)
  
  # Get bottom 5% threshold for negative log likelihood
  threshold <- get_bottom_5_percent_threshold(results_variable_name)
  
  if (!is.null(threshold)) {
    # Filter results by negative log likelihood threshold
    bottom_5_percent_results <- filter_by_negloglik(results_variable_name, threshold)
    
    # Filter results by additional conditions
    final_filtered_results <- filter_by_conditions(bottom_5_percent_results)
    
    # Assign the filtered results to a variable with the name corresponding to the period
    assign(paste0("bottom_5_percent_", start_year), final_filtered_results, envir = .GlobalEnv)
    
    # Create pairs list from final filtered results
    pairs <- create_pairs_list(final_filtered_results)
    
    # Assign the pairs list to a variable with the name corresponding to the period
    assign(paste0("pairs_", start_year), pairs, envir = .GlobalEnv)
    
    # Print the pairs list
    cat("Pairs List for", start_year, "-", end_year, ":\n")
    print(pairs)
  }
}

# Print the complete list of pairs for all periods
cat("Complete List of Pairs for All Periods:\n")
for (start_year in 2015:2023) {
  cat(paste0("Pairs List for ", start_year, ": "), "\n")
  print(get(paste0("pairs_", start_year), envir = .GlobalEnv))
}


