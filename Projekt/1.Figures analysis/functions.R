library(slider)

rolling_mean <- function(dataset,periods, ignore) (
  dataset %>% ungroup() %>% 
    group_by(isin) %>% 
    mutate(across(!c(date,ignore),
                  ~ rollmean(.x, k = 5, fill = NA, align = "right"))) %>% na.omit()
  
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

estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(W_RETURN ~ mkt_excess, data = data, na.action=na.omit)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}

estimate_capm_alpha <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    alpha <- as.numeric(NA)
  } else {
    fit <- lm(W_RETURN ~ mkt_excess, data = data, na.action=na.omit)
    alpha <- as.numeric(coefficients(fit)[1])
  }
  return(alpha)
}


roll_capm_estimation <- function(data, window, min_obs, period = "day" ) {
  data <- data %>% 
    arrange(date) 
  
  betas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = period,
    .f = ~ estimate_capm(data = ., min_obs),
    .before = window - 1,
    .complete = FALSE
  )
  
  alphas <- slide_period_vec(
    .x = data,
    .i = data$date,
    .period = period,
    .f = ~ estimate_capm_alpha(., min_obs),
    .before = window - 1,
    .complete = FALSE
  )

  return(tibble(
    date = unique(data$date),
    beta = betas,
    alpha = alphas
  ))
}

winsorize <- function(x, cut) {
  x <- replace(
    x,
    x > quantile(x, 1 - cut, na.rm = T),
    quantile(x, 1 - cut, na.rm = T)
  )
  x <- replace(
    x,
    x < quantile(x, cut, na.rm = T),
    quantile(x, cut, na.rm = T)
  )
  return(x)
}


compute_portfolio_weights <- function(theta,
                                      data,
                                      value_weighting = TRUE,
                                      allow_short_selling = TRUE) {
  data |>
    group_by(month) |>
    bind_cols(
      characteristic_tilt = data |>
        transmute(across(contains("lag"), ~ . / n)) |>
        as.matrix() %*% theta |> as.numeric()
    ) |>
    mutate(
      # Definition of benchmark weight
      weight_benchmark = case_when(
        value_weighting == TRUE ~ relative_mktcap,
        value_weighting == FALSE ~ 1 / n
      ),
      # Parametric portfolio weights
      weight_tilt = weight_benchmark + characteristic_tilt,
      # Short-sell constraint
      weight_tilt = case_when(
        allow_short_selling == TRUE ~ weight_tilt,
        allow_short_selling == FALSE ~ pmax(0, weight_tilt)
      ),
      # Weights sum up to 1
      weight_tilt = weight_tilt / sum(weight_tilt)
    ) |>
    ungroup()
}




evaluate_portfolio <- function(weights_crsp,
                               full_evaluation = TRUE) {
  evaluation <- weights_crsp |>
    group_by(month) |>
    summarize(
      return_tilt = weighted.mean(ret_excess, weight_tilt),
      return_benchmark = weighted.mean(ret_excess, weight_benchmark)
    ) |>
    pivot_longer(-month,
                 values_to = "portfolio_return",
                 names_to = "model"
    ) |>
    group_by(model) |>
    left_join(factors_ff_monthly, by = "month") |>
    summarize(tibble(
      "Expected utility" = mean(power_utility(portfolio_return)),
      "Average return" = 100 * mean(12 * portfolio_return),
      "SD return" = 100 * sqrt(12) * sd(portfolio_return),
      "Sharpe ratio" = sqrt(12) * mean(portfolio_return) / sd(portfolio_return),
      "CAPM alpha" = coefficients(lm(portfolio_return ~ mkt_excess))[1],
      "Market beta" = coefficients(lm(portfolio_return ~ mkt_excess))[2]
    )) |>
    mutate(model = str_remove(model, "return_")) |>
    pivot_longer(-model, names_to = "measure") |>
    pivot_wider(names_from = model, values_from = value)
  
  if (full_evaluation) {
    weight_evaluation <- weights_crsp |>
      select(month, contains("weight")) |>
      pivot_longer(-month, values_to = "weight", names_to = "model") |>
      group_by(model, month) |>
      transmute(tibble(
        "Absolute weight" = abs(weight),
        "Max. weight" = max(weight),
        "Min. weight" = min(weight),
        "Avg. sum of negative weights" = -sum(weight[weight < 0]),
        "Avg. fraction of negative weights" = sum(weight < 0) / n()
      )) |>
      group_by(model) |>
      summarize(across(-month, ~ 100 * mean(.))) |>
      mutate(model = str_remove(model, "weight_")) |>
      pivot_longer(-model, names_to = "measure") |>
      pivot_wider(names_from = model, values_from = value)
    evaluation <- bind_rows(evaluation, weight_evaluation)
  }
  return(evaluation)
}


compute_objective_function <- function(theta,
                                       data,
                                       objective_measure = "Expected utility",
                                       value_weighting = TRUE,
                                       allow_short_selling = TRUE) {
  processed_data <- compute_portfolio_weights(
    theta,
    data,
    value_weighting,
    allow_short_selling
  )
  
  objective_function <- evaluate_portfolio(processed_data,
                                           full_evaluation = FALSE
  ) |>
    filter(measure == objective_measure) |>
    pull(tilt)
  
  return(-objective_function)
}