# Now that we've seen how to perform a single factor ANOVA, let's watch some animations that highlight how certain aspects of our data may affect our results. See


# ![Changing sample size](../../mov/aov_n_slide.mp4){#fig-anim1 width=70%}

# ![Changing means](../../mov/aov_mean_slide.mp4){#fig-anim2 width=70%}

# ![Changing variance](../../mov/aov_sd_slide.mp4){#fig-anim3 width=70%}

library(animation)

# Function that serves as the basis for the sliding ANOVA figures
slide_fig <- function(df) {

  # Calculate basic stats
  sub_stats <- df %>%
    group_by(title, sample) %>%
    summarise(sub_mean = round(mean(dat), 2),
              sub_sd = round(sd(dat), 2),
              sub_n = round(n(), 0))

  # Crerate fancy dataframe for use in ggplot2 text printing
  sub_stats_print <- sub_stats %>%
    gather(key = stat, value = value, -sample, -title) %>%
    mutate(y = c(rep(18.9, 3), rep(17.9, 3), rep(16.9, 3)),
           stat_word = c(rep("mean = ", 3), rep("sd = ", 3), rep("n = ", 3)),
           stat_print = paste0(stat_word, value))

  # Run ANOVA
  sub_aov <- summary(aov(dat ~ sample, data = df))

  # Create final stats for printing
  df_val_print <- paste0("d.f. = ", sub_aov[[1]]$Df[1])
  ss_val_print <- paste0("SS = ", round(sub_aov[[1]]$`Sum Sq`[1], 2))
  f_val_print <-  paste0("F = ", round(sub_aov[[1]]$`F value`[1], 2))
  if(sub_aov[[1]]$`Pr(>F)`[1] < 0.001) {
    p_val_print <- "p < 0.001"
  } else {
    p_val_print <- paste0("p = ", round(sub_aov[[1]]$`Pr(>F)`[1], 3))
  }

  # Create the figure
  aovb <- ggplot(data = df, aes(x = sample, y = dat, colour = sample)) +
    geom_hline(aes(yintercept = mean(df$dat)), size = 1, linetype = "dashed") +
    geom_violin(fill = "grey70", alpha = 0.8) +
    geom_boxplot(width = 0.1, colour = "black", fill = "white", notch = T) +
    geom_jitter(shape = 16, width = 0.01, colour = "red", alpha = 0.2) +
    geom_label(data = sub_stats_print, aes(label = stat_print, y = y), size = 4,
               label.padding = unit(0.25, "lines"), fontface = "bold") +
    scale_y_continuous(limits = c(0, 19)) +
    labs(title = sub_stats_print$title,
         subtitle = paste(df_val_print, ss_val_print,
                          f_val_print, p_val_print, sep = ", "),
         x = "Sample",
         y = "value") +
    theme_pubclean() +
    theme(legend.position = "none")
  print(aovb)
}

# Create random data
set.seed(666)
random_n_dat <- data.frame(A = rnorm(100, 10.0, 2),
                           B = rnorm(100, 10.2, 2),
                           C = rnorm(100, 10.4, 2),
                           title = "ANOVA: increasing n")

# tester...
# i <- 20

# The function to animate increasing n
aov_n_fig <- function(i) {
  # Create animated dataframe
  sub_n_dat <- random_n_dat %>%
    slice(1:i) %>%
    gather(key = "sample", value = "dat", -title)
  # Calculate stats and output figure
  slide_fig(sub_n_dat)
}

# Create animation of the histogram
animate_aov_n <- function() {
  lapply(c(seq(30, 99, by = 1), rep(100, 30),
           seq(99, 31, by = -1), rep(30, 30)),
         function(i) {
           aov_n_fig(i)
         })
}

# system.time(saveGIF(animate_lm(), interval = 1, ani.width = 600, movie.name = "lm_rotate.gif")) ## ~4 seconds
system.time(saveVideo(animate_aov_n(), interval = 0.1, ani.width = 600, video.name = "Resources/aov_n_slide.mp4", other.opts = "-pix_fmt yuv420p -b 300k")) # ~73 seconds

# Create random data
set.seed(666)
random_mean_dat <- data.frame(A = rnorm(100, 10.1, 2),
                              B = rnorm(100, 10.2, 2),
                              C = rnorm(100, 10.3, 2),
                              title = "ANOVA: increasing mean")

# tester...
# i <- 1

# The function to animate increasing mean
aov_mean_fig <- function(i) {
  # Create animated dataframe
  sub_mean_dat <- random_mean_dat %>%
    mutate(B = B-i) %>%
    gather(key = "sample", value = "dat", -title)
  # Calculate stats and output figure
  slide_fig(sub_mean_dat)
}

# Create animation of the histogram
animate_aov_mean <- function() {
  lapply(c(seq(0, -0.99, by = -0.01), rep(-1, 30),
           seq(-0.99, -0.01, by = 0.01), rep(0, 30)),
         function(i) {
           aov_mean_fig(i)
         })
}

# system.time(saveGIF(animate_lm(), interval = 1, ani.width = 600, movie.name = "lm_rotate.gif")) ## ~4 seconds
system.time(saveVideo(animate_aov_mean(), interval = 0.1, ani.width = 600, video.name = "Resources/aov_mean_slide.mp4", other.opts = "-pix_fmt yuv420p -b 300k")) # ~105 seconds


# Create random data
set.seed(666)
random_sd_dat <- data.frame(A = rnorm(100, 9.8, 1.5),
                            B = rnorm(100, 10.4, 1.5),
                            C = rnorm(100, 10.2, 1.5),
                            title = "ANOVA: increasing SD")

# tester...
# i <- 2.0

# The function to animate increasing SD
aov_sd_fig <- function(i) {
  # Create dataframe for sliding figure
  sub_sd_dat <- random_sd_dat %>%
    mutate(C = (C*i)-10.2*(i-1)) %>%
    gather(key = "sample", value = "dat", -title) %>%
    mutate(sample = as.factor(sample))
  # Calculate stats and output figure
  slide_fig(sub_sd_dat)
}

# Create animation of the histogram
animate_aov_sd <- function() {
  lapply(c(seq(1.00, 1.98, by = 0.02), rep(2.00, 30),
           seq(1.98, 1.02, by = -0.02), rep(1.00, 30)),
         function(i) {
           aov_sd_fig(i)
         })
}

# system.time(saveGIF(animate_lm(), interval = 1, ani.width = 600, movie.name = "lm_rotate.gif")) ## ~4 seconds
system.time(saveVideo(animate_aov_sd(), interval = 0.1, ani.width = 600, video.name = "Resources/aov_sd_slide.mp4", other.opts = "-pix_fmt yuv420p -b 300k")) # ~89 seconds

