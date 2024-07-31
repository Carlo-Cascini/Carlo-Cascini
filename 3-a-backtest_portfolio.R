# Load the libraries
library(readxl)
library(xts)
library(quantmod)
library(zoo)
library(TTR)
library(KFAS)
library(partialAR)
library(partialCI)

data <- read_excel("~/Desktop/tesi master/partial_CI/67_cleaned_etfs.xlsx")

# Convert the dataframe to xts object
data$Dates <- as.Date(data$Dates)
data_xts <- xts(data[, -1], order.by = data$Dates)
Y <- log(data_xts)

# we define a function for convenience and future use
generate_signal <- function(Z_score, threshold_long, threshold_short) {
  signal <- Z_score
  colnames(signal) <- "signal"
  signal[] <- NA
  
  #initial position
  signal[1] <- 0
  if (Z_score[1] <= threshold_long[1]) {
    signal[1] <- 1
  } else if (Z_score[1] >= threshold_short[1])
    signal[1] <- -1
  
  # loop
  for (t in 2:nrow(Z_score)) {
    if (signal[t-1] == 0) {  #if we were in no position
      if (Z_score[t] <= threshold_long[t]) {
        signal[t] <- 1
      } else if(Z_score[t] >= threshold_short[t]) {
        signal[t] <- -1
      } else signal[t] <- 0
    } else if (signal[t-1] == 1) {  #if we were in a long position
      if (Z_score[t] >= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    } else {  #if we were in a short position
      if (Z_score[t] <= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    }
  }
  return(signal)
}


generate_Z_score_EMA <- function(spread, n = 120) {
  ## traditional rolling windowed mean and variance
  # first, the mean
  spread.mean <- EMA(spread, n)
  spread.mean <- na.locf(spread.mean, fromLast = TRUE)
  spread.demeaned <- spread - spread.mean
  # second, the variance
  spread.var <- EMA(spread.demeaned^2, n)
  spread.var <- na.locf(spread.var, fromLast = TRUE)
  # finally compute Z-score
  Z.score <- spread.demeaned/sqrt(spread.var)
  return(Z.score)
}

pairs_trading <- function(Y, gamma, mu, name = NULL, threshold = 0.5, plot = FALSE) {
  # spread and spread portfolio
  w_spread <- cbind(1, -gamma)/cbind(1+gamma, 1+gamma)
  spread <- rowSums(Y * w_spread) - mu/(1+gamma)
  
  # Z-score
  Z_score <- generate_Z_score_EMA(spread)
  threshold_long <- threshold_short <- Z_score
  threshold_short[] <- threshold
  threshold_long[] <- -threshold
  
  # trading signal
  signal <- generate_signal(Z_score, threshold_long, threshold_short)
  
  # combine the ref portfolio with trading signal
  w_portf <- w_spread * lag(cbind(signal, signal))   # NOTE THE LAG!!
  
  # # fix the portfolio (gamma and mu) during a trade
  # lag_signal <- as.numeric(lag(signal))
  # for (t in 2:nrow(w_portf)) {
  #   if (lag_signal[t] != 0 && lag_signal[t] == lag_signal[t-1])
  #     w_portf[t, ] <- w_portf[t-1, ]
  # }
  
  # now compute the PnL from the log-prices and the portfolio
  X <- diff(Y)  #compute log-returns from log-prices
  portf_return <- xts(rowSums(X * w_portf), index(X))
  portf_return[is.na(portf_return)] <- 0
  colnames(portf_return) <- name
  
  # plots
  if (plot) {
    tmp <- cbind(Z_score, signal)
    colnames(tmp) <- c("Z-score", "signal")
    par(mfrow = c(2, 1))
    { plot(tmp, legend.loc = "topleft",
           main = paste("Z-score and trading on spread based on", name))
      lines(threshold_short, lty = 2)
      print(lines(threshold_long, lty = 2)) }
    print(plot(cumprod(1 + portf_return), main = paste("Cum P&L for spread based on", name)))
  }
  
  return(portf_return)
}
calculate_performance <- function(portf_return, risk_free_rate = 0) {
  # Ensure that portf_return is numeric and remove NA values
  portf_return <- na.omit(as.numeric(portf_return))
  
  if (length(portf_return) == 0) {
    return(list(
      total_return = NA,
      annualized_return = NA,
      sharpe_ratio = NA,
      num_days = NA
    ))
  }
  
  # Compute cumulative returns
  cum_return <- cumprod(1 + portf_return) - 1
  
  # Compute total return
  total_return <- cum_return[length(cum_return)]
  
  # Compute average return and standard deviation of returns
  avg_return <- mean(portf_return, na.rm = TRUE)
  sd_return <- sd(portf_return, na.rm = TRUE)
  
  # Compute annualized return
  num_days <- length(portf_return)
  annualized_return <- (1 + total_return)^(252 / num_days)
  
  # Compute Sharpe ratio
  sharpe_ratio <- (avg_return - risk_free_rate) / sd_return * sqrt(252)
  
  return(list(
    total_return = total_return,
    annualized_return = annualized_return,
    sharpe_ratio = sharpe_ratio,
    num_days = num_days,
    sd_return=sd_return
  ))
}
# Define the equity curve plotting function
plot_equity_curve <- function(portfolio_returns, title) {
  # Ensure portfolio_returns is numeric and handle NA values
  portfolio_returns <- na.omit(as.numeric(portfolio_returns))
  
  # Calculate cumulative returns
  equity_curve <- cumprod(1 + portfolio_returns) - 1
  
  # Plot the equity curve
  plot(index(portfolio_returns), equity_curve, type = 'l', col = 'blue',
       main = title, xlab = 'Date', ylab = 'Cumulative Return',
       ylim = range(c(0, equity_curve), na.rm = TRUE))
  grid()
}
estimate_mu_gamma_LS <- function(Y, pct_training = 0.2) {
  T <- nrow(Y)
  T_trn <- round(pct_training*T)
  # LS regression
  ls_coeffs <- coef(lm(Y[1:T_trn, 1] ~ Y[1:T_trn, 2]))
  mu <- xts(rep(ls_coeffs[1], T), index(Y))
  colnames(mu) <- "mu-LS"
  gamma <- xts(rep(ls_coeffs[2], T), index(Y))
  colnames(gamma) <- "gamma-LS"
  return(list(mu = mu, gamma = gamma))
}

estimate_mu_gamma_rolling_LS <- function(Y, pct_training = 0.2) {
  T <- nrow(Y)
  T_start <- round(pct_training*T)
  T_lookback <- 50  # lookback window length
  T_shift <- 10  # how often is refreshed
  # init empty variables
  gamma_rolling_LS <- mu_rolling_LS <- xts(rep(NA, T), index(Y))
  colnames(mu_rolling_LS) <- "mu-rolling-LS"
  colnames(gamma_rolling_LS) <- "gamma-rolling-LS"
  # loop
  t0_update <- seq(from = min(T_start, T_lookback), to = T-T_shift, by = T_shift)
  for (t0 in t0_update) {
    T_lookback_ <- ifelse(t0-T_lookback+1 >= 1, T_lookback, T_start)
    ls_coeffs <- coef(lm(Y[(t0-T_lookback_+1):t0, 1] ~ Y[(t0-T_lookback_+1):t0, 2],
                         weights = last(1:T_lookback, T_lookback_)))
    mu_rolling_LS[t0+1] <- ls_coeffs[1]
    gamma_rolling_LS[t0+1] <- ls_coeffs[2]
  }
  # complete values
  mu_rolling_LS <- na.locf(mu_rolling_LS)
  mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  # smoothing
  L <- 15
  mu_rolling_LS[] <- filter(mu_rolling_LS, rep(1, L)/L, sides = 1)
  mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  gamma_rolling_LS[] <- filter(gamma_rolling_LS, rep(1, L)/L, sides = 1)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  return(list(mu = mu_rolling_LS, gamma = gamma_rolling_LS))
}

estimate_mu_gamma_Kalman <- function(Y) {
  T <- nrow(Y)
  # init empty variables
  gamma_Kalman_filtering <- mu_Kalman_filtering <- xts(rep(NA, T), index(Y))
  colnames(mu_Kalman_filtering) <- "mu-Kalman"
  colnames(gamma_Kalman_filtering) <- "gamma-Kalman"
  # Kalman parameters
  Tt <- diag(2)
  Rt <- diag(2)
  Qt <- 1e-3*diag(2)  # state transition variance very small
  Zt <- array(as.vector(t(cbind(1, as.matrix(Y[, 2])))), dim = c(1, 2, T))  # time-varying
  Ht <- matrix(1e-3)  # observation variance
  # the prior in the code: P1cov = kappa*P1Inf + P1, kappa = 1e7
  init <- estimate_mu_gamma_LS(Y)
  a1 <- matrix(c(init$mu[1], init$gamma[1]), 2, 1)
  P1 <- 1e-5*diag(2)  # variance of initial point
  P1inf <- 0*diag(2)
  # create Kalman model
  model <- SSModel(as.matrix(Y[, 1]) ~ 0 + SSMcustom(Z=Zt, T=Tt, R=Rt, Q=Qt, a1=a1, P1=P1, P1inf=P1inf), H=Ht)
  # run Kalman filtering
  out <- KFS(model)
  mu_Kalman_filtering[] <- out$a[-1, 1]  # a is Kalman filtering (alphahat is Kalman smoothing) (a(T+1)=alphahat(T))
  gamma_Kalman_filtering[] <- out$a[-1, 2]
  # smoothing
  L <- 30
  mu_Kalman_filtering[] <- filter(mu_Kalman_filtering, rep(1, L)/L, sides = 1)
  mu_Kalman_filtering <- na.locf(mu_Kalman_filtering, fromLast = TRUE)
  gamma_Kalman_filtering[] <- filter(gamma_Kalman_filtering, rep(1, L)/L, sides = 1)
  gamma_Kalman_filtering <- na.locf(gamma_Kalman_filtering, fromLast = TRUE)
  return(list(mu = mu_Kalman_filtering, gamma = gamma_Kalman_filtering))
}


estimate_mu_gamma_rolling_PCI <- function(Y, pct_training = 0.2) {
  T <- nrow(Y)
  T_start <- round(pct_training * T)
  T_lookback <- 50  # lookback window length
  T_shift <- 10  # how often is refreshed
  
  # Init empty variables
  gamma_rolling_PCI <- mu_rolling_PCI <- xts(rep(NA, T), index(Y))
  colnames(mu_rolling_PCI) <- "mu-rolling-PCI"
  colnames(gamma_rolling_PCI) <- "gamma-rolling-PCI"
  
  # Loop
  t0_update <- seq(from = min(T_start, T_lookback), to = T - T_shift, by = T_shift)
  for (t0 in t0_update) {
    T_lookback_ <- ifelse(t0 - T_lookback + 1 >= 1, T_lookback, T_start)
    fit <- fit.pci(Y[(t0 - T_lookback_ + 1):t0, ], include_alpha = FALSE)
    mu_rolling_PCI[t0 + 1] <- 0
    gamma_rolling_PCI[t0 + 1] <- fit$beta
  }
  
  # Complete values
  mu_rolling_PCI <- na.locf(mu_rolling_PCI)
  mu_rolling_PCI <- na.locf(mu_rolling_PCI, fromLast = TRUE)
  gamma_rolling_PCI <- na.locf(gamma_rolling_PCI)
  gamma_rolling_PCI <- na.locf(gamma_rolling_PCI, fromLast = TRUE)
  
  return(list(mu = mu_rolling_PCI, gamma = gamma_rolling_PCI))
}






analyze_and_plot_strategies <- function(Y, pairs, start_date, end_date, performance_start_date) {
  
  # Define the analysis function
  analyze_pair <- function(Y, stock1, stock2, start_date, end_date) {
    Y_pair <- Y[paste0(start_date, "::", end_date), c(stock1, stock2)]
    if (anyNA(Y_pair)) Y_pair <- na.approx(Y_pair)
    
    LS <- estimate_mu_gamma_LS(Y_pair[])
    rolling_LS <- estimate_mu_gamma_rolling_LS(Y_pair)
    Kalman <- estimate_mu_gamma_Kalman(Y_pair)
    PCI <- estimate_mu_gamma_rolling_PCI(Y_pair)
    
    return_LS <- pairs_trading(Y_pair, LS$gamma, LS$mu, "LS", plot = FALSE)
    return_rolling_LS <- pairs_trading(Y_pair, rolling_LS$gamma, rolling_LS$mu, "rolling-LS", plot = FALSE)
    return_Kalman <- pairs_trading(Y_pair, Kalman$gamma, Kalman$mu, "Kalman", plot = FALSE)
    return_PCI <- pairs_trading(Y_pair, PCI$gamma, PCI$mu, "PCI", plot = FALSE)
    
    return(list(
      return_LS = return_LS,
      return_rolling_LS = return_rolling_LS,
      return_Kalman = return_Kalman,
      return_PCI = return_PCI
    ))
  }
  
  # Initialize lists to store portfolio returns for each strategy
  kalman_returns <- list()
  ls_returns <- list()
  rolling_ls_returns <- list()
  pci_returns <- list()
  
  # Analyze each pair and store returns
  for (pair in pairs) {
    stock1 <- pair[1]
    stock2 <- pair[2]
    
    cat("Analyzing pair:", stock1, "and", stock2, "\n")
    
    result <- analyze_pair(Y, stock1, stock2, start_date, end_date)
    
    kalman_returns[[paste(stock1, stock2, sep = "_")]] <- result$return_Kalman
    ls_returns[[paste(stock1, stock2, sep = "_")]] <- result$return_LS
    rolling_ls_returns[[paste(stock1, stock2, sep = "_")]] <- result$return_rolling_LS
    pci_returns[[paste(stock1, stock2, sep = "_")]] <- result$return_PCI
  }
  
  # Combine returns for each strategy
  combine_returns <- function(returns_list) {
    combined_returns <- do.call(merge, returns_list)
    portfolio_returns <- rowMeans(combined_returns, na.rm = TRUE)
    portfolio_returns <- xts(portfolio_returns, order.by = index(combined_returns))
    colnames(portfolio_returns) <- "Portfolio_Return"
    return(portfolio_returns)
  }
  
  # Calculate portfolio returns for each strategy
  portfolio_returns_kalman <- combine_returns(kalman_returns)
  portfolio_returns_ls <- combine_returns(ls_returns)
  portfolio_returns_rolling_ls <- combine_returns(rolling_ls_returns)
  portfolio_returns_pci <- combine_returns(pci_returns)
  
  # Extract returns from March 1, 2015 onwards
  portfolio_returns_kalman_march <- portfolio_returns_kalman[paste0(performance_start_date, "::")]
  portfolio_returns_ls_march <- portfolio_returns_ls[paste0(performance_start_date, "::")]
  portfolio_returns_rolling_ls_march <- portfolio_returns_rolling_ls[paste0(performance_start_date, "::")]
  portfolio_returns_pci_march <- portfolio_returns_pci[paste0(performance_start_date, "::")]
  
  # Calculate performance metrics from March 1, 2015 onwards
  calculate_performance_march <- function(portfolio_returns) {
    if (nrow(portfolio_returns) == 0) return(NULL)
    portfolio_returns <- na.omit(portfolio_returns)
    return(calculate_performance(portfolio_returns))
  }
  
  overall_performance_kalman_march <- calculate_performance_march(portfolio_returns_kalman_march)
  overall_performance_ls_march <- calculate_performance_march(portfolio_returns_ls_march)
  overall_performance_rolling_ls_march <- calculate_performance_march(portfolio_returns_rolling_ls_march)
  overall_performance_pci_march <- calculate_performance_march(portfolio_returns_pci_march)
  
  # Print performance metrics from March 1, 2015 onwards
  print_performance_metrics <- function(performance, strategy_name) {
    cat("Overall Portfolio Performance from", performance_start_date, "(", strategy_name, "Strategy):\n")
    cat("  Overall Return: ", performance$total_return, "\n")
    cat("  Annualized Return: ", performance$annualized_return, "\n")
    cat("  Standard_deviation: ", performance$sd_return, "\n")
    cat("  Sharpe Ratio: ", performance$sharpe_ratio, "\n")
    cat("  Number of Days: ", performance$num_days, "\n\n")
  }
  
  print_performance_metrics(overall_performance_ls_march, "LS")
  print_performance_metrics(overall_performance_rolling_ls_march, "Rolling LS")
  print_performance_metrics(overall_performance_kalman_march, "Kalman Filter")
  print_performance_metrics(overall_performance_pci_march, "PCI")
  
  # Plot cumulative returns
  par(mar = c(5, 4, 4, 2) + 0.1) # Set plot margins
  
  combined_returns_march <- cbind(portfolio_returns_ls_march, portfolio_returns_rolling_ls_march, portfolio_returns_kalman_march, portfolio_returns_pci_march)
  cum_returns_march <- cumprod(1 + combined_returns_march)
  
  # Calculate y-axis limits to ensure lines are contained within borders
  ylims <- range(cum_returns_march, na.rm = TRUE)
  
  # Plot cumulative returns with correct colors and legend
  plot(index(cum_returns_march), cum_returns_march[,1], type = "l", col = "blue", 
       ylim = ylims, xlab = "Date", ylab = "Cumulative Return", main = "Cumulative P&L", 
       lwd = 2)
  
  lines(index(cum_returns_march), cum_returns_march[,2], col = "red", lwd = 2)
  lines(index(cum_returns_march), cum_returns_march[,3], col = "green", lwd = 2)
  lines(index(cum_returns_march), cum_returns_march[,4], col = "purple", lwd = 2)
  
  # Add grid lines
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  
  # Add legend
  legend("topleft", legend = c("LS Strategy", "Rolling LS Strategy", "Kalman Filter Strategy", "PCI Strategy"),
         col = c("blue", "red", "green", "purple"), lty = 1, lwd = 2)
}


# i want to start computing parameter in : 2015-01-01, close the trades in: 2015-12-31, 
# and start the trades in: 2015-03-12 it is 50 days later (0.2*252) than 2015-01-01 to avoid look-ahead bias

analyze_and_plot_strategies(Y, pairs_2015, "2015-01-01", "2015-12-31", "2015-03-12")
analyze_and_plot_strategies(Y, pairs_2016, "2016-01-01", "2016-12-31", "2016-03-12")
analyze_and_plot_strategies(Y, pairs_2017, "2017-01-01", "2017-12-31", "2017-03-12")
analyze_and_plot_strategies(Y, pairs_2018, "2018-01-01", "2018-12-31", "2018-03-12")
analyze_and_plot_strategies(Y, pairs_2019, "2019-01-01", "2019-12-31", "2019-03-12")
analyze_and_plot_strategies(Y, pairs_2020, "2020-01-01", "2020-12-31", "2020-03-12")
analyze_and_plot_strategies(Y, pairs_2021, "2021-01-01", "2021-12-31", "2021-03-12")
analyze_and_plot_strategies(Y, pairs_2022, "2022-01-01", "2022-12-31", "2022-03-12")
analyze_and_plot_strategies(Y, pairs_2023, "2023-01-01", "2023-12-31", "2023-03-12")





