library(tidyverse)
library(animation)

# The function to animate the rotating line
lm_fig <- function(i) {

  # tester...
  # i <- 0.075628

  # Set the slope
  slope <- i

  # Create random data
  set.seed(666)
  random_dat <- data.frame(Y = rnorm(50, 10, 1),
                           X = rnorm(50, 10, 1),
                           row_seq = seq(1, 50))
  random_dat <- random_dat %>%
    mutate(Y = Y * row_seq,
           X = X * row_seq)

  # Calculate fitted values
  XY_pred <- random_dat %>%
    mutate(Y_pred = X * slope,
           Y_fit = Y_pred + mean(Y) - mean(Y_pred),
           Y_error = Y_fit - Y,
           alpha_range = 1 - (abs(Y_error) / max(abs(Y_error))),
           error_fix = if_else((X >= mean(X)), -(Y_error), Y_error))

  # Set intercept value
  intercept <- round(mean(XY_pred$Y) - mean(XY_pred$Y_pred), 0)

  # Sum of Squares (SS)
  total_ss_val <- round(sum((XY_pred$Y - mean(XY_pred$Y))^2), 0)
  residual_ss_val <- round(sum(XY_pred$Y_error^2), 0)
  regression_ss_val <- round(sum((XY_pred$Y_fit - mean(XY_pred$Y))^2), 0)

  # Mean square (MS)
  regression_ms_val <- regression_ss_val / 1
  residual_ms_val <- residual_ss_val / (nrow(XY_pred) - 2)

  # Final stats
  f_val <- round(regression_ms_val / residual_ms_val, 0)
  # f_test <- qf(0.95, 1, 270)
  p_val <- 1-pf(f_val, 1, (nrow(XY_pred) - 2))
  r2_val <- round(regression_ss_val / total_ss_val, 2)

  # Create p value for printing
  if(p_val < 0.001) {
    p_val_print <- " < 0.001"
  } else {
    p_val_print <- paste0(" == ", round(p_val, 3))
  }

  # Create square polygon dataframes
  poly_1 <- XY_pred %>%
    filter(X >= min(X))
  poly_2 <- XY_pred %>%
    filter(X <= min(X))

  # Create the figure
  lmr <- ggplot(data = XY_pred, aes(x = X, y = Y)) +
    geom_rect(aes(xmin = X, xmax = X + abs(Y_error),
                  ymin = Y, ymax = Y_fit,
                  fill = abs(Y_error), alpha = alpha_range)) +
    geom_segment(size = 0.2, alpha = 0.7, aes(xend = X, y = Y_fit, yend = Y)) +
    geom_point(shape = 21) +
    geom_line(aes(y = Y_fit)) +
    annotate("text", x = -20, y = 625, label = paste0("slope == ", slope, "~(Y/X)"), parse = TRUE, hjust = 0) +
    annotate("text", x = -20, y = 600, label = paste0("intercept == ", intercept), parse = TRUE, hjust = 0) +
    annotate("text", x = -20, y = 575, label = paste0("italic(r)^2 == ", r2_val), parse = TRUE, hjust = 0) +
    annotate("text", x = -20, y = 550, label = paste0("residual~SS == ", residual_ss_val), parse = TRUE, hjust = 0) +
    annotate("text", x = -20, y = 525, label = paste0("regression~SS == ", regression_ss_val), parse = TRUE, hjust = 0) +
    annotate("text", x = -20, y = 500, label = paste0("d.f. == ", nrow(XY_pred)-2), parse = TRUE, hjust = 0) +
    annotate("text", x = -20, y = 475, label = paste0("F == ", f_val), parse = TRUE, hjust = 0) +
    annotate("text", x = -20, y = 450, label = paste0("italic(p)", p_val_print), parse = TRUE, hjust = 0) +
    scale_fill_gradient(low = "white", high = "salmon", guide = FALSE) +
    scale_alpha(guide = FALSE) +
    coord_equal(xlim = c(-50, 650), ylim = c(-50, 650)) +
    labs(title = "Rotating linear fit",
         x = "X",
         y = "Y") +
    theme_dark()
  print(lmr)
}

# Create animation of the histogram
animate_lm <- function() {
  lapply(rep(c(seq(0.000, 1.50, by = 0.001),
               seq(1.499, 0.001, by = -0.001)), 3), function(i) {
                 lm_fig(i)
               })
}

# Note that this spits out in the main directory and is manually moved to the figures folder
# system.time(saveGIF(animate_lm(), interval = 1, ani.width = 600, movie.name = "lm_rotate.gif")) ## ~4 seconds
system.time(saveVideo(animate_lm(), interval = 0.01, ani.width = 600, video.name = "figures/lm_rotate.mp4")) #~553 seconds
