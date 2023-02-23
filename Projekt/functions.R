library(slider)

rolling_mean <- function(dataset,periods, ignore) (
  dataset %>% ungroup() %>% 
    group_by(isin) %>% 
    mutate(across(!c(date,ignore),
                  ~ rollmean(.x, periods, fill = NA, align = "right"))) %>% na.omit()
  
)


assign_portfolio <- function(data, var, n_portfolios) {
  breakpoints <- data %>%
    summarize(breakpoint = quantile({{ var }}, 
                                    #quantile() produces - in this case - 10 quantiles to split the data into, by a sequences from 0 to 1 by the number of portfolios. Thus creating a breakpoint for which we can split the portfolios into. 
                                    probs = seq(0, 1, length.out = n_portfolios + 1),
                                    na.rm = TRUE #Removes all NA's
    )) %>%
    pull(breakpoint) %>%
    as.numeric()
  
  data %>%
    mutate(portfolio = findInterval({{ var }}, #Given a vector of breakpoints, we find the interval containing each element of breakpoints
                                    breakpoints,
                                    all.inside = TRUE 
    )) %>%
    pull(portfolio) #Returns the portfolio number for each security
}

assign_portfolio_bi <- function(data, var, n_portfolios) {
  breakpoints <- data %>%
    summarize(breakpoint = quantile(
      {{ var }},
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE
    )) %>%
    pull(breakpoint) %>%
    as.numeric()
  
  assigned_portfolios <- data %>%
    mutate(portfolio = findInterval({{ var }},
                                    breakpoints,
                                    all.inside = TRUE
    )) %>%
    pull(portfolio)
  
  return(assigned_portfolios)
}

estimate_capm_beta <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(W_RETURN ~ mkt_excess, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}

estimate_capm_alpha <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    alpha <- as.numeric(NA)
  } else {
    fit <- lm(W_RETURN ~ mkt_excess, data = data)
    alpha <- as.numeric(coefficients(fit)[1])
  }
  return(alpha)
}


roll_capm_estimation <- function(data, weeks, min_obs) {
  data <- data %>%
    arrange(date)
  
  betas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = "week",
    .f = ~ estimate_capm_beta(., min_obs),
    .before = weeks - 1,
    .complete = FALSE
  )
  
  alphas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = "week",
    .f = ~ estimate_capm_alpha(., min_obs),
    .before = weeks - 1,
    .complete = FALSE
  )

  return(tibble(
    date = unique(data$date),
    beta = betas,
    alpha = alphas
  ))
}


roll_capm_estimation2 <- function(data, months, min_obs) {
  data <- data |>
    arrange(date)
  
  betas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = "week",
    .f = ~ estimate_capm_beta(., min_obs),
    .before = months - 1,
    .complete = FALSE
  )
  
  return(tibble(
    month = unique(data$date),
    beta = betas
  ))
}
