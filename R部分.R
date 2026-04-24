# =========================================================
# War memory dynamics pipeline
# =========================================================
# Scope:
#   1. Load and screen war-level daily view series
#   2. Aggregate normalized pre/post-anniversary trajectories
#   3. Fit shifted-power decomposition for main figure
#   4. Fit multiple alternative decay models for model comparison
#   5. Generate publication-style base R figures
# =========================================================

rm(list = ls())

# ---------------------------------------------------------
# Configuration
# ---------------------------------------------------------
setwd("D:/战争实验/全部战争")

library(ggplot2)
library(dplyr)

memory_table <- read.csv(
  "D:/战争实验/新建 Microsoft Excel 工作表 (2).csv",
  header = TRUE,
  sep = ",",
  fileEncoding = "gb18030",
  fill = TRUE,
  comment.char = ""
)

file_list <- list.files(pattern = "\\.csv$")

YEAR_RANGE  <- 2015:2023
WINDOW_SIZE <- 150

# ---------------------------------------------------------
# Color palette
# ---------------------------------------------------------
COL <- list(
  NEWS = rgb(0.9, 0.6, 0),
  TWITTER = rgb(0, 0.45, 0.7)
)

COL_LIGHT <- list(
  NEWS = rgb(0.9, 0.6, 0, 0.3),
  TWITTER = rgb(0, 0.45, 0.7, 0.3)
)

COL_XLIGHT <- list(
  NEWS = rgb(0.9, 0.6, 0, 0.03),
  TWITTER = rgb(0, 0.45, 0.7, 0.03)
)

COL_LIGHTBLUE <- "#56B4E9"
COL_YELLOW    <- "#F0E442"
COL_DARKBLUE  <- "#0072B2"
COL_RED       <- "#D55E00"
COL_MAGENTA   <- "#CC79A7"
COL_GRAY      <- "#999999"
COL_ORANGE    <- "#E69F00"
COL_GREEN     <- "#009E73"

# ---------------------------------------------------------
# Utilities
# ---------------------------------------------------------
safe_log10 <- function(x) {
  log10(pmax(x, 1e-10))
}

safe_predict <- function(model, newdata = NULL) {
  out <- tryCatch(
    {
      if (is.null(newdata)) {
        predict(model)
      } else {
        predict(model, newdata = newdata)
      }
    },
    error = function(e) rep(NA_real_, if (is.null(newdata)) length(fitted(model)) else nrow(newdata))
  )
  as.numeric(out)
}

safe_fit <- function(expr) {
  tryCatch(expr, error = function(e) NULL, warning = function(w) invokeRestart("muffleWarning"))
}

calc_r2 <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  
  if (length(y) < 2) return(NA_real_)
  
  sst <- sum((y - mean(y))^2)
  if (sst == 0) return(NA_real_)
  
  ssr <- sum((yhat - mean(y))^2)
  ssr / sst
}

calc_aicc <- function(model, n) {
  k <- length(coef(model))
  aic <- AIC(model)
  if ((n - k - 1) <= 0) return(NA_real_)
  aic + 2 * k * (k + 1) / (n - k - 1)
}

# ---------------------------------------------------------
# Data loading helpers
# ---------------------------------------------------------
read_war_file <- function(file) {
  data <- read.csv(file)
  data <- data[-nrow(data), , drop = FALSE]
  data[, 1] <- gsub("/", "-", data[, 1])
  data[, 1] <- as.Date(data[, 1], format = "%Y-%m-%d")
  data
}

get_memory_date <- function(file, memory_table) {
  file_name <- tools::file_path_sans_ext(file)
  date_value <- memory_table$date[memory_table$name == file_name]
  
  if (length(date_value) == 0 || is.na(date_value[1])) {
    return(NA)
  }
  
  as.Date(date_value[1], format = "%m月%d日")
}

extract_year_window <- function(data, memory_date, year, window_size = 150) {
  event_date <- as.Date(format(memory_date, paste0(year, "-%m-%d")))
  
  before_start <- event_date - (window_size - 1)
  before_seq <- seq(before_start, event_date, by = "day")
  
  after_end <- event_date + (window_size - 1)
  after_seq <- seq(event_date, after_end, by = "day")
  
  before_data <- data[data$Date %in% before_seq, , drop = FALSE]
  after_data  <- data[data$Date %in% after_seq, , drop = FALSE]
  
  if (nrow(before_data) == window_size && nrow(after_data) == window_size) {
    list(before = before_data, after = after_data)
  } else {
    NULL
  }
}

# ---------------------------------------------------------
# Screening: first pass
# ---------------------------------------------------------
process_single_file_first_pass <- function(file,
                                           memory_table,
                                           year_range = 2015:2023,
                                           window_size = 150) {
  data <- read_war_file(file)
  file_name <- tools::file_path_sans_ext(file)
  memory_date <- get_memory_date(file, memory_table)
  
  if (is.na(memory_date)) {
    return(NULL)
  }
  
  mean_views <- mean(data[, 2], na.rm = TRUE)
  
  before_sum <- numeric(window_size)
  after_sum  <- numeric(window_size)
  
  valid_years <- 0
  high_peak_years <- 0
  
  for (yr in year_range) {
    year_window <- extract_year_window(data, memory_date, yr, window_size)
    
    if (!is.null(year_window)) {
      before_vec <- year_window$before[, 2]
      after_vec  <- year_window$after[, 2]
      
      before_sum <- before_sum + before_vec
      after_sum  <- after_sum + after_vec
      
      valid_years <- valid_years + 1
      
      if (after_vec[1] > 2 * mean_views) {
        high_peak_years <- high_peak_years + 1
      }
    }
  }
  
  if (valid_years == 0) {
    return(list(
      file = file,
      file_name = file_name,
      keep = FALSE,
      reason = "no_valid_year"
    ))
  }
  
  before_mean <- before_sum / valid_years
  after_mean  <- after_sum / valid_years
  
  if (before_mean[window_size] == 0 || after_mean[1] == 0) {
    return(list(
      file = file,
      file_name = file_name,
      keep = FALSE,
      reason = "zero_division"
    ))
  }
  
  before_norm <- before_mean / before_mean[window_size]
  after_norm  <- after_mean / after_mean[1]
  
  peak_ratio <- high_peak_years / valid_years
  
  list(
    file = file,
    file_name = file_name,
    keep = peak_ratio > 0.7,
    reason = ifelse(peak_ratio > 0.7, "ok", "peak_ratio_low"),
    before_norm = before_norm,
    after_norm = after_norm,
    peak_ratio = peak_ratio,
    valid_years = valid_years
  )
}

# ---------------------------------------------------------
# Screening: second pass
# ---------------------------------------------------------
process_single_file_second_pass <- function(file,
                                            memory_table,
                                            year_range = 2015:2023,
                                            window_size = 150) {
  data <- read_war_file(file)
  file_name <- tools::file_path_sans_ext(file)
  memory_date <- get_memory_date(file, memory_table)
  
  if (is.na(memory_date)) {
    return(NULL)
  }
  
  mean_views <- mean(data[, 2], na.rm = TRUE)
  
  before_sum <- numeric(window_size)
  after_sum  <- numeric(window_size)
  valid_years <- 0
  
  for (yr in year_range) {
    year_window <- extract_year_window(data, memory_date, yr, window_size)
    
    if (!is.null(year_window)) {
      before_vec <- year_window$before[, 2]
      after_vec  <- year_window$after[, 2]
      
      if (after_vec[1] > 2 * mean_views) {
        if (max(after_vec) < 1.0001 * after_vec[1] &&
            max(before_vec) < 1.0001 * after_vec[1]) {
          before_sum <- before_sum + before_vec
          after_sum  <- after_sum + after_vec
          valid_years <- valid_years + 1
        }
      }
    }
  }
  
  if (valid_years == 0) {
    return(list(
      file = file,
      file_name = file_name,
      keep = FALSE,
      reason = "no_valid_year_after_refine"
    ))
  }
  
  before_mean <- before_sum / valid_years
  after_mean  <- after_sum / valid_years
  
  if (before_mean[window_size] == 0 || after_mean[1] == 0) {
    return(list(
      file = file,
      file_name = file_name,
      keep = FALSE,
      reason = "zero_division"
    ))
  }
  
  before_norm <- before_mean / before_mean[window_size]
  after_norm  <- after_mean / after_mean[1]
  
  abnormal_shape <- (max(after_norm[10:150]) > 0.7) || (max(before_norm[1:140]) > 0.7)
  
  list(
    file = file,
    file_name = file_name,
    keep = !abnormal_shape,
    reason = ifelse(abnormal_shape, "shape_abnormal", "ok"),
    before_norm = before_norm,
    after_norm = after_norm,
    valid_years = valid_years
  )
}

# ---------------------------------------------------------
# Screening execution
# ---------------------------------------------------------
first_pass_results <- lapply(
  file_list,
  process_single_file_first_pass,
  memory_table = memory_table,
  year_range = YEAR_RANGE,
  window_size = WINDOW_SIZE
)

first_pass_results <- first_pass_results[!sapply(first_pass_results, is.null)]
first_pass_results <- first_pass_results[sapply(first_pass_results, function(x) isTRUE(x$keep))]

filtered_files <- sapply(first_pass_results, function(x) x$file)

cat("First-pass retained files:", length(filtered_files), "\n")

second_pass_results <- lapply(
  filtered_files,
  process_single_file_second_pass,
  memory_table = memory_table,
  year_range = YEAR_RANGE,
  window_size = WINDOW_SIZE
)

second_pass_results <- second_pass_results[!sapply(second_pass_results, is.null)]
final_results <- second_pass_results[sapply(second_pass_results, function(x) isTRUE(x$keep))]
abnormal_results <- second_pass_results[!sapply(second_pass_results, function(x) isTRUE(x$keep))]

cat("Second-pass retained files:", length(final_results), "\n")

if (length(abnormal_results) > 0) {
  cat("Excluded files:\n")
  print(sapply(abnormal_results, function(x) x$file_name))
}

if (length(final_results) == 0) {
  stop("No valid files remained after screening.")
}

# ---------------------------------------------------------
# Aggregate normalized trajectories
# ---------------------------------------------------------
up_sum <- Reduce(`+`, lapply(final_results, function(x) x$before_norm))
down_sum <- Reduce(`+`, lapply(final_results, function(x) x$after_norm))

up <- up_sum / length(final_results)
down <- down_sum / length(final_results)

df_up <- data.frame(
  x = 1:150,
  down_mean = up[150:1]
)

df_down <- data.frame(
  x = 1:150,
  down_mean = down
)

df_result <- data.frame(
  x = 1:150,
  x1 = -150:-1,
  up_mean = up,
  down_mean = down
)

# ---------------------------------------------------------
# Shifted-power decomposition for main figure
# ---------------------------------------------------------
fit_shifted_power_decomposition <- function(df, y_col = "down_mean") {
  x <- df$x
  y <- df[[y_col]]
  
  model0 <- lm(log(y) ~ log(x))
  N0 <- exp(coef(model0)[1])
  p0 <- -coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log(N * x^(-p) + r),
    start = list(p = p0, N = N0, r = 0),
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  coefs1 <- coef(model1)
  p_start <- coefs1["p"] * coefs1["N"]
  r_start <- coefs1["r"] * coefs1["p"]
  
  model2 <- nls(
    y ~ (p / (p + r)) * x^(-(p + r)) + (r / (p + r)),
    start = list(p = p_start, r = r_start),
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  coefs2 <- coef(model2)
  p <- coefs2["p"]
  r <- coefs2["r"]
  
  mem_comm <- (p / (p + r)) * x^(-(p + r))
  mem_cult <- rep(r / (p + r), length(x))
  mem_total <- mem_comm + mem_cult
  
  y_hat <- predict(model2, newdata = df)
  
  list(
    model = model2,
    p = p,
    r = r,
    c = r / (p + r),
    mem_comm = mem_comm,
    mem_cult = mem_cult,
    mem_total = mem_total,
    AIC = AIC(model2),
    AICc = calc_aicc(model2, length(y)),
    R2 = calc_r2(y, y_hat)
  )
}

fit_down <- fit_shifted_power_decomposition(df_down, "down_mean")
fit_up <- fit_shifted_power_decomposition(df_up, "down_mean")

df_result$mem_commdown <- fit_down$mem_comm
df_result$mem_cultdown <- fit_down$mem_cult
df_result$memdown <- fit_down$mem_total

df_result$mem_commup_power <- fit_up$mem_comm[150:1]
df_result$mem_cultup_power <- fit_up$mem_cult[150:1]
df_result$memup_power <- fit_up$mem_total[150:1]

# ---------------------------------------------------------
# Main decomposition figure
# ---------------------------------------------------------
plot_memory_decomposition <- function(df_result,
                                      COL,
                                      COL_GREEN = "#009E73",
                                      COL_RED = "#D55E00") {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(mfrow = c(1, 2))
  par(family = "sans")
  
  y_up_log <- safe_log10(df_result$up_mean)
  t_before <- df_result$x1
  t_before_log <- safe_log10(-t_before)
  
  plot(
    df_result$x1, df_result$up_mean,
    type = "p",
    bty = "n",
    lwd = 2,
    xlab = "",
    ylab = "",
    panel.first = abline(v = 0, col = "gray", lwd = 1, lty = 2),
    col = COL[["NEWS"]],
    las = 0
  )
  
  mtext(expression(paste("Day ", italic(t), " before memory day")), side = 1, line = 2)
  mtext("Frequency", side = 2, line = 2)
  
  legend_before <- c(
    expression(paste("War memory ", s(-t) == u(-t) + v(-t))),
    expression(paste("Communicative memory ", u(-t) == p[1]/(p[1] + r[1]) * (-t)^-(p[1] + r[1]))),
    expression(paste("Cultural memory ", v(-t) == r[1]/(p[1] + r[1])))
  )
  
  lines(df_result$x1, df_result$mem_commup_power, lty = 1, lwd = 2, col = COL_GREEN)
  lines(df_result$x1, df_result$mem_cultup_power, lty = 1, lwd = 2, col = COL_RED)
  lines(df_result$x1, df_result$memup_power, lty = 1, lwd = 2, col = "black")
  
  legend(
    "topleft",
    bty = "n",
    legend = c("Daily average", legend_before),
    seg.len = 3,
    pch = c(1, NA, NA, NA),
    lty = c(0, 1, 1, 1),
    col = c(COL[["NEWS"]], "black", COL_GREEN, COL_RED),
    lwd = c(2, 2, 2, 2),
    cex = 0.9
  )
  
  y_down_log <- safe_log10(df_result$down_mean)
  t_after <- df_result$x
  t_after_log <- safe_log10(t_after)
  
  plot(
    df_result$x, df_result$down_mean,
    type = "p",
    bty = "n",
    lwd = 2,
    xlab = "",
    ylab = "",
    panel.first = abline(v = 0, col = "gray", lwd = 1, lty = 2),
    col = COL[["NEWS"]],
    las = 0
  )
  
  mtext(expression(paste("Day ", italic(t), " after memory day")), side = 1, line = 2)
  mtext("Frequency", side = 2, line = 2)
  
  legend_after <- c(
    expression(paste("War memory ", s(t) == u(t) + v(t))),
    expression(paste("Communicative memory ", u(t) == p/(p + r) * t^-(p + r))),
    expression(paste("Cultural memory ", v(t) == r/(p + r)))
  )
  
  lines(df_result$x, df_result$mem_commdown, lty = 1, lwd = 2, col = COL_GREEN)
  lines(df_result$x, df_result$mem_cultdown, lty = 1, lwd = 2, col = COL_RED)
  lines(df_result$x, df_result$memdown, lty = 1, lwd = 2, col = "black")
  
  legend(
    "topright",
    bty = "n",
    legend = c("Daily average", legend_after),
    seg.len = 3,
    pch = c(1, NA, NA, NA),
    lty = c(0, 1, 1, 1),
    col = c(COL[["NEWS"]], "black", COL_GREEN, COL_RED),
    lwd = c(2, 2, 2, 2),
    cex = 0.9
  )
  
  par(fig = c(0.07, 0.40, 0.20, 0.74), new = TRUE, family = "sans")
  plot(
    -t_before_log, y_up_log,
    type = "p",
    bty = "n",
    xlab = "",
    ylab = "",
    lwd = 2,
    col = COL[["NEWS"]],
    las = 0
  )
  lines(-t_before_log, safe_log10(df_result$mem_commup_power), lty = 1, lwd = 2, col = COL_GREEN)
  lines(-t_before_log, safe_log10(df_result$mem_cultup_power), lty = 1, lwd = 2, col = COL_RED)
  lines(-t_before_log, safe_log10(df_result$memup_power), lty = 1, lwd = 2, col = "black")
  legend("topright", bty = "n", legend = expression(paste("Log ", "-log")), inset = c(0.4, 0.1))
  
  par(fig = c(0.60, 0.93, 0.20, 0.74), new = TRUE, family = "sans")
  plot(
    t_after_log, y_down_log,
    type = "p",
    bty = "n",
    xlab = "",
    ylab = "",
    lwd = 2,
    col = COL[["NEWS"]],
    las = 0
  )
  lines(t_after_log, safe_log10(df_result$mem_commdown), lty = 1, lwd = 2, col = COL_GREEN)
  lines(t_after_log, safe_log10(df_result$mem_cultdown), lty = 1, lwd = 2, col = COL_RED)
  lines(t_after_log, safe_log10(df_result$memdown), lty = 1, lwd = 2, col = "black")
  legend("topright", bty = "n", legend = expression(paste("Log ", "-log")), inset = c(0.25, 0.1))
}

# ---------------------------------------------------------
# Alternative models for comparison
# ---------------------------------------------------------
biexp_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(log(y) ~ x)
  N0 <- exp(coef(model0)[1])
  p0 <- -coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log(N / (p + r) * (p * exp(-(p + r) * x) + r)),
    start = list(p = p0, N = N0, r = 0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  N1 <- coef(model1)["N"]
  p1 <- coef(model1)["p"]
  r1 <- coef(model1)["r"]
  
  model2 <- nls(
    log(y) ~ log(N / (p - q + r) * ((p - q) * exp(-(p + r) * x) + r * exp(-q * x))),
    start = list(p = p1, N = N1, r = r1, q = 0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port",
    lower = c(q = 0)
  )
  
  N2 <- coef(model2)["N"]
  p2 <- coef(model2)["p"]
  r2 <- coef(model2)["r"]
  q2 <- coef(model2)["q"]
  
  nls(
    y ~ N / (p - q + r) * ((p - q) * exp(-(p + r) * x) + r * exp(-q * x)),
    start = list(p = p2, N = N2, r = r2, q = q2),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port",
    lower = c(q = 0)
  )
}

exp_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(log(y) ~ x)
  a0 <- exp(coef(model0)[1])
  b0 <- -coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log(a * exp(-b * x)),
    start = list(a = a0, b = b0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  a1 <- coef(model1)["a"]
  b1 <- coef(model1)["b"]
  
  nls(
    y ~ a * exp(-b * x),
    start = list(a = a1, b = b1),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
}

exp_power_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  c0 <- -1
  model0 <- lm(log(y) ~ I(x^c0))
  log_a0 <- coef(model0)[1]
  b0 <- coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log_a + b * x^c,
    start = list(log_a = log_a0, b = b0, c = c0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  a1 <- exp(coef(model1)["log_a"])
  b1 <- coef(model1)["b"]
  c1 <- coef(model1)["c"]
  
  nls(
    y ~ a * exp(b * x^c),
    start = list(a = a1, b = b1, c = c1),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
}

lognorm_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  lm(log(y) ~ log(x) + I(log(x)^2), data = df)
}

lognorm_constrained_fit <- function(df) {
  df2 <- df
  df2$combo <- log(df2$x)^2 - 2 * log(max(df2$x)) * log(df2$x)
  lm(log(down_mean) ~ combo, data = df2)
}

bipower_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(log(y) ~ log(x))
  N0 <- exp(coef(model0)[1])
  p0 <- -coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log(N / (p + r) * (p * x^-(p + r) + r)),
    start = list(p = p0, N = N0, r = 0.01),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  N1 <- coef(model1)["N"]
  p1 <- coef(model1)["p"]
  r1 <- coef(model1)["r"]
  
  model2 <- nls(
    log(y) ~ log(N / (p - q + r) * ((p - q) * x^-(p + r) + r * x^-q)),
    start = list(p = p1, N = N1, r = r1, q = 0.01),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port",
    lower = c(q = 0)
  )
  
  N2 <- coef(model2)["N"]
  p2 <- coef(model2)["p"]
  r2 <- coef(model2)["r"]
  q2 <- coef(model2)["q"]
  
  nls(
    y ~ N / (p - q + r) * ((p - q) * x^-(p + r) + r * x^-q),
    start = list(p = p2, N = N2, r = r2, q = q2),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port",
    lower = c(q = 0)
  )
}

bipower_fit_1 <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(log(y) ~ log(x))
  p0 <- -coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log((1 - c) * x^-p + c * x^-q),
    start = list(p = p0, c = 0.2, q = 0.01),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port",
    lower = c(q = 0)
  )
  
  coefs <- coef(model1)
  p_start <- coefs["p"] * (1 - coefs["c"])
  r_start <- coefs["c"] * coefs["p"]
  q_start <- coefs["q"]
  
  nls(
    y ~ (p / (p + r)) * x^-(p + r) + (r / (p + r)) * x^-q,
    start = list(p = p_start, r = r_start, q = q_start),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port",
    lower = c(q = 0)
  )
}

shifted_power_fit_1 <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(log(y) ~ log(x))
  N0 <- exp(coef(model0)[1])
  p0 <- -coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log(N * x^-p + r),
    start = list(p = p0, N = N0, r = 0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  coefs <- coef(model1)
  p_start <- coefs["p"] * (1 - coefs["N"])
  r_start <- coefs["r"] * coefs["p"]
  
  nls(
    y ~ (p / (p + r)) * x^-(p + r) + r / (p + r),
    start = list(p = p_start, r = r_start),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
}

shifted_power_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(log(y) ~ log(x))
  N0 <- exp(coef(model0)[1])
  p0 <- -coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ log(N * x^-p + r),
    start = list(p = p0, N = N0, r = 0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  p1 <- coef(model1)["p"]
  N1 <- coef(model1)["N"]
  r1 <- coef(model1)["r"]
  
  nls(
    y ~ N * x^-p + r,
    start = list(N = N1, p = p1, r = r1),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
}

power_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(log(y) ~ log(x))
  a0 <- exp(coef(model0)[1])
  b0 <- -coef(model0)[2]
  
  nls(
    y ~ a * x^(-b),
    start = list(a = a0, b = b0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
}

log_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(y ~ log(x))
  a0 <- coef(model0)[1]
  b0 <- coef(model0)[2]
  
  if (a0 + b0 * log(max(x)) <= 0) {
    a0 <- abs(a0) + 1
  }
  
  model1 <- nls(
    log(y) ~ log(a + b * log(x)),
    start = list(a = a0 / 2, b = b0 / 2),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  a1 <- coef(model1)["a"]
  b1 <- coef(model1)["b"]
  
  nls(
    y ~ a + b * log(x),
    start = list(a = a1, b = b1),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
}

hyperbolic_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  model0 <- lm(1 / y ~ x)
  a0 <- coef(model0)[1]
  b0 <- coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ -log(a + b * x),
    start = list(a = a0, b = b0),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
  
  a1 <- coef(model1)["a"]
  b1 <- coef(model1)["b"]
  
  nls(
    y ~ (a + b * x)^(-1),
    start = list(a = a1, b = b1),
    data = df,
    control = list(maxiter = 1e5),
    algorithm = "port"
  )
}

hyperbolic_power_fit <- function(df) {
  x <- df$x
  y <- df$down_mean
  
  c0 <- -1
  model0 <- lm(1 / y ~ I(x^c0))
  a0 <- coef(model0)[1]
  b0 <- coef(model0)[2]
  
  model1 <- nls(
    log(y) ~ -log(a + b * x^c),
    start = list(a = a0, b = b0, c = c0),
    data = df,
    control = list(maxiter = 1e5, warnOnly = TRUE),
    algorithm = "port",
    trace = FALSE,
    lower = c(a = 0, b = -Inf, c = -Inf),
    upper = c(a = Inf, b = 0, c = 0)
  )
  
  a1 <- coef(model1)["a"]
  b1 <- coef(model1)["b"]
  c1 <- coef(model1)["c"]
  
  nls(
    y ~ (a + b * x^c)^(-1),
    start = list(a = a1, b = b1, c = c1),
    data = df,
    control = list(maxiter = 1e5, warnOnly = TRUE),
    algorithm = "port",
    trace = FALSE,
    lower = c(a = 0, b = -Inf, c = -Inf),
    upper = c(a = Inf, b = 0, c = 0)
  )
}

# ---------------------------------------------------------
# Model registry
# ---------------------------------------------------------
fit_all_models <- function(df) {
  list(
    "Exponential"          = safe_fit(exp_fit(df)),
    "Hyperbolic"           = safe_fit(hyperbolic_fit(df)),
    "Logarithmic"          = safe_fit(log_fit(df)),
    "Power"                = safe_fit(power_fit(df)),
    "Hyperbolic-\npower"   = safe_fit(hyperbolic_power_fit(df)),
    "Log-normal-\nbased"   = safe_fit(lognorm_constrained_fit(df)),
    "Exponential-\npower"  = safe_fit(exp_power_fit(df)),
    "Biexponential"        = safe_fit(biexp_fit(df)),
    "Bipower"              = safe_fit(bipower_fit(df)),
    "Bipower_1"            = safe_fit(bipower_fit_1(df)),
    "Shifted power"        = safe_fit(shifted_power_fit(df)),
    "Shifted power_1"      = safe_fit(shifted_power_fit_1(df))
  )
}

summarize_models <- function(model_list, df, panel = c("down", "up")) {
  panel <- match.arg(panel)
  y <- df$down_mean
  n <- length(y)
  
  out <- lapply(names(model_list), function(name) {
    model <- model_list[[name]]
    
    if (is.null(model)) {
      return(data.frame(
        model = name,
        AIC = NA_real_,
        AICc = NA_real_,
        R2 = NA_real_,
        converged = FALSE
      ))
    }
    
    yhat <- safe_predict(model, df)
    
    data.frame(
      model = name,
      AIC = AIC(model),
      AICc = calc_aicc(model, n),
      R2 = calc_r2(y, yhat),
      converged = TRUE
    )
  })
  
  do.call(rbind, out) |>
    dplyr::arrange(AICc)
}

# ---------------------------------------------------------
# Model comparison plots
# ---------------------------------------------------------
plot_model_fit <- function(df, title_text, model, mar = c(3, 3, 1, 1)) {
  t <- df$x
  y <- df$down_mean
  yhat <- safe_predict(model, df)
  
  par(mar = mar)
  plot(t, y, type = "p", bty = "n", lwd = 2, xlab = "", ylab = "", las = 0)
  mtext("Days", side = 1, line = 2, cex = 0.6)
  mtext("freq", side = 2, line = 2, cex = 0.6)
  
  if (all(is.finite(yhat))) {
    lines(t, yhat, lwd = 2)
  }
  
  n <- length(y)
  AICc <- if (is.null(model)) NA_real_ else calc_aicc(model, n)
  r2 <- calc_r2(y, yhat)
  
  text(max(t) * 0.80, max(y) - 0.4,
       sprintf("R² = %.4f\nAICc = %.4f", r2, AICc),
       adj = c(1, 1), cex = 1)
  
  text(max(t) * 0.65, max(y), title_text, adj = c(0.5, 1), cex = 1.1)
}

plot_model_fit_up <- function(df, title_text, model, mar = c(3, 3, 1, 1)) {
  t <- -150:-1
  y <- df$down_mean[150:1]
  yhat <- rev(safe_predict(model, df))
  
  par(mar = mar)
  plot(t, y, type = "p", bty = "n", lwd = 2, xlab = "", ylab = "", las = 0)
  mtext("Days", side = 1, line = 2, cex = 0.6)
  mtext("freq", side = 2, line = 2, cex = 0.6)
  
  if (all(is.finite(yhat))) {
    lines(t, yhat, lwd = 2)
  }
  
  n <- length(y)
  AICc <- if (is.null(model)) NA_real_ else calc_aicc(model, n)
  r2 <- calc_r2(y, yhat)
  
  text(-80, max(y) - 0.4,
       sprintf("R² = %.4f\nAICc = %.4f", r2, AICc),
       adj = c(1, 1), cex = 1)
  
  text(-100, max(y), title_text, adj = c(0.5, 1), cex = 1.1)
}

plot_model_grid <- function(model_list, df, up_panel = FALSE) {
  valid_names <- names(model_list)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(mfcol = c(4, 3))
  
  for (i in seq_along(valid_names)) {
    name <- valid_names[i]
    model <- model_list[[name]]
    title_text <- sprintf("(%d) %s", i, name)
    
    if (is.null(model)) {
      par(mar = c(3, 4, 1.5, 1))
      plot.new()
      text(0.5, 0.65, title_text, cex = 1)
      text(0.5, 0.45, "fit failed", cex = 0.9, col = "red")
    } else if (up_panel) {
      plot_model_fit_up(df, title_text, model, mar = c(3, 4, 1.5, 1))
    } else {
      plot_model_fit(df, title_text, model, mar = c(3, 4, 1.5, 1))
    }
  }
}

# ---------------------------------------------------------
# Fit model sets
# ---------------------------------------------------------
models_down <- fit_all_models(df_down)
models_up   <- fit_all_models(df_up)

summary_down <- summarize_models(models_down, df_down, panel = "down")
summary_up   <- summarize_models(models_up, df_up, panel = "up")

print(summary_down)
print(summary_up)

# ---------------------------------------------------------
# Output figures
# ---------------------------------------------------------
plot_memory_decomposition(
  df_result = df_result,
  COL = COL,
  COL_GREEN = COL_GREEN,
  COL_RED = COL_RED
)

plot_model_grid(models_down, df_down, up_panel = FALSE)
plot_model_grid(models_up, df_up, up_panel = TRUE)