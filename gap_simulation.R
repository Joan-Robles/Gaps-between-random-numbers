set.seed(123)

get_larger_gap <- function(x = max_x) {
  numbers <- runif(n = x / log(x), min = 1, max = x)
  sorted <- sort(numbers)
  gaps <- diff(sorted)
  return(max(gaps))
}

get_gap <- function(x = x, n_sim = n_sim) {
  .simulations <- sapply(seq_len(n_sim), function(i) {
    get_larger_gap(x)
  })
  .mean <- mean(.simulations)
  .sd <- sd(.simulations)
  return(c(.mean, .sd))
}

get_prove_numbers <- function(max_x, power = 1.05) {
  prove_numbers <- sapply(seq_len(max_x), function(x) ceiling(power^x)) # Polynomial increasing values
  prove_numbers <- unique(prove_numbers)
  return(prove_numbers)
}

get_results <- function(max_x, n_sim) {
  # prove_numbers <- seq(from = 2, to = max_x, by = 10)
  prove_numbers <- get_prove_numbers(max_x)
  numbers <- sapply(prove_numbers, function(x) get_gap(x = x, n_sim = n_sim))
  return(numbers)
}

get_nice_plot <- function(max_x, results) {
  y <- get_prove_numbers(max_x)
  mean_values <- results[1, ]
  sd_values <- results[2, ]

  # Calculate the upper bound (for example, using log(n))
  upper_bound <- log(y)^2

  # Plot the mean values
  plot(y, mean_values,
    type = "l",
    col = "blue",
    lwd = 2,
    xlab = "X",
    ylab = "Brecha máxima media",
    main = "Primos lejanos",
    cex.lab = 1.2,
    cex.main = 1.4,
    log = "x"
  )
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

  # Add the fan chart for standard deviation
  upper <- mean_values + sd_values
  lower <- mean_values - sd_values
  x_polygon <- c(y, rev(y))
  y_polygon <- c(upper, rev(lower))
  polygon(x_polygon, y_polygon, col = rgb(0, 0, 1, 0.25), border = NA)

  # Redraw the mean line for clarity
  lines(y, mean_values, type = "l", col = "blue", lwd = 2)

  # Add the upper bound line
  lines(
    y, upper_bound,
    type = "l", col = "red", lwd = 2, lty = 2
  ) # Dashed red line for the upper bound

  # Add a legend
  legend("topleft",
    legend = c("Brecha máxima media", "Límite superior teórico"),
    col = c("blue", "red"),
    lwd = 2,
    lty = c(1, 1, 2),
    cex = 0.6
  ) # Smaller text size
}

# Results -----------------------------------------------------------------
max_x <- 320 # around 2000000000
n_sim <- 100
begin <- Sys.time()
results <- get_results(max_x = max_x, n_sim = n_sim)
get_nice_plot(max_x = max_x, results = results)
end <- Sys.time()
end - begin # how long it took

# max(get_prove_numbers(400,1.05))
