# Regenerate the lecture figures from the dated source snapshots, without network access.
# Run from the repository root: Rscript BDC223/scripts/update_climate_figures.R
library(ggplot2)
library(svglite)
library(jsonlite)

script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
module <- normalizePath(file.path(dirname(script), ".."))
data_dir <- file.path(module, "data", "climate")
figure_dir <- file.path(module, "images", "climate")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
sources <- fromJSON(file.path(data_dir, "sources.json"))
cutoff <- as.Date(sources$retrieved)
complete_year <- as.integer(format(cutoff, "%Y")) - 1L
snapshot <- paste("Data retrieved", sources$retrieved)
theme_set(theme_minimal(base_size = 13, base_family = "Arial") +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        plot.title.position = "plot", plot.caption = element_text(hjust = 0, size = 9),
        legend.position = "bottom", legend.title = element_blank(),
        axis.title = element_text(size = 11), plot.margin = margin(14, 20, 12, 12)))
save_figure <- function(name, plot, width = 9, height = 4.8) {
  ggsave(file.path(figure_dir, paste0(name, ".svg")), plot,
         width = width, height = height, device = svglite, bg = "white")
}
read_noaa <- function(name) read.table(file.path(data_dir, name), comment.char = "#")
mlo <- read_noaa("co2_mm_mlo.txt")
names(mlo) <- c("year", "month", "decimal", "mean", "adjusted", "days", "sd", "uncertainty")
mlo$date <- as.Date(sprintf("%04d-%02d-15", mlo$year, mlo$month))
mlo <- subset(mlo, date <= cutoff & mean > 0 & adjusted > 0)
latest_mlo <- tail(mlo, 1)
p <- ggplot(mlo, aes(date, mean)) +
  geom_line(aes(colour = "Monthly mean"), linewidth = .55) +
  geom_line(aes(y = adjusted, colour = "Seasonally adjusted"), linewidth = .8) +
  scale_colour_manual(values = c("Monthly mean" = "#BC5A29", "Seasonally adjusted" = "#23565A")) +
  labs(title = "Atmospheric CO2 at Mauna Loa",
       subtitle = sprintf("March 1958 to %s | latest monthly mean %.2f ppm",
                          format(latest_mlo$date, "%B %Y"), latest_mlo$mean),
       x = NULL, y = "CO2 mole fraction (ppm)",
       caption = paste("NOAA GML and Scripps |", snapshot, "| Recent values may be revised."))
save_figure("l05-co2-mauna-loa", p)

season <- subset(mlo, year >= complete_year - 4 & year <= complete_year)
stopifnot(nrow(season) == 60)
season$residual <- season$mean - season$adjusted
cycle <- aggregate(residual ~ month, season, mean)
p <- ggplot(season, aes(month, residual, group = year)) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = .4) +
  geom_line(colour = "#7B9596", linewidth = .65, alpha = .7) +
  geom_line(data = cycle, aes(group = 1), colour = "#214F53", linewidth = 1.2) +
  geom_point(data = cycle, aes(group = 1), colour = "#214F53", size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "The seasonal CO2 cycle at Mauna Loa",
       subtitle = sprintf("Thin lines: %d-%d | thick line: five-year mean", complete_year - 4, complete_year),
       x = NULL, y = "Monthly mean minus seasonally adjusted CO2 (ppm)",
       caption = paste("NOAA GML | Seasonal component after removing the long-term trend |", snapshot))
save_figure("l05-co2-seasonal-cycle", p)

global <- read_noaa("co2_annmean_gl.txt")
names(global) <- c("year", "mean", "uncertainty")
global <- subset(global, year <= complete_year)
growth <- read_noaa("co2_gr_gl.txt")
names(growth) <- c("year", "increase", "uncertainty")
growth <- subset(growth, year >= 1980 & year <= complete_year)
global_latest <- tail(global, 1)
growth_latest <- tail(growth, 1)
p <- ggplot(growth, aes(year, increase)) +
  geom_col(fill = "#3F7477", width = .75) +
  geom_errorbar(aes(ymin = increase - uncertainty, ymax = increase + uncertainty), width = .3) +
  labs(title = "Annual growth of global atmospheric CO2",
       subtitle = sprintf("%d: %.2f +/- %.2f ppm per year", growth_latest$year,
                          growth_latest$increase, growth_latest$uncertainty),
       x = NULL, y = "CO2 growth (ppm per year)",
       caption = paste("NOAA marine boundary layer network | January-to-December trend change |", snapshot))
save_figure("l05-co2-global-growth", p)

ice <- read.table(file.path(data_dir, "antarctica2015co2composite.txt"),
                  header = TRUE, comment.char = "#", fileEncoding = "UTF-8-BOM")
ice$age_ka <- ice$age_gas_calBP / 1000
for (span in c(800, 10)) {
  paleodata <- subset(ice, age_ka >= 0 & age_ka <= span)
  p <- ggplot(paleodata, aes(age_ka, co2_ppm)) +
    geom_line(colour = "#23565A", linewidth = .65) +
    geom_point(aes(x = (1950 - year) / 1000, y = mean), inherit.aes = FALSE,
               data = global_latest, colour = "#B34827", size = 3) +
    annotate("text", x = span * .03, y = global_latest$mean - 12,
             label = sprintf("%d global mean: %.2f ppm", global_latest$year, global_latest$mean),
             hjust = 1, colour = "#953D23", size = 3.8) +
    scale_x_reverse(limits = c(span, -.1), expand = expansion(mult = c(.01, .03))) +
    coord_cartesian(ylim = c(160, 445)) +
    labs(title = if (span == 800) "CO2 across glacial cycles and today" else "Holocene CO2 and the modern atmosphere",
         subtitle = "Antarctic ice-core composite with a separate modern observation",
         x = "Ice-core gas age (thousands of years before 1950)", y = "CO2 concentration (ppm)",
         caption = paste("Bereiter et al. (2015), NOAA NCEI; modern point: NOAA GML |", snapshot))
  save_figure(paste0("l05-co2-", if (span == 800) "800k" else "holocene"), p)
}

temp <- read.csv(file.path(data_dir, "GLB.Ts+dSST.csv"), skip = 1, check.names = FALSE, na.strings = "***")
annual <- subset(temp, Year <= complete_year & !is.na(`J-D`))
recent <- subset(annual, Year >= complete_year - 49)
fit <- lm(`J-D` ~ Year, recent)
recent$fitted <- predict(fit)
rate <- unname(coef(fit)[[2]]) * 10
p <- ggplot(annual, aes(Year, `J-D`)) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = .4) +
  geom_line(colour = "grey50", linewidth = .6) +
  geom_point(colour = "#23565A", size = 1) +
  geom_line(data = recent, aes(y = fitted), colour = "#B34827", linewidth = 1) +
  labs(title = "Global annual surface temperature",
       subtitle = sprintf("1880-%d | %d-%d linear trend: %.3f °C per decade", complete_year,
                          min(recent$Year), max(recent$Year), rate),
       x = NULL, y = "Temperature anomaly (°C relative to 1951-1980)",
       caption = paste("NASA GISTEMP v4 land-ocean index | Trend is an OLS fit to 50 annual means |", snapshot))
save_figure("l05-global-temperature", p)

monthly <- data.frame(year = rep(temp$Year, each = 12), month = rep(1:12, nrow(temp)),
                      anomaly = as.vector(t(as.matrix(temp[, month.abb]))))
monthly$date <- as.Date(sprintf("%04d-%02d-15", monthly$year, monthly$month))
monthly <- subset(monthly, !is.na(anomaly) & date <= cutoff)
highlights <- subset(monthly, year >= complete_year - 1)
p <- ggplot(subset(monthly, year < complete_year - 1), aes(month, anomaly, group = year)) +
  geom_line(colour = "grey75", linewidth = .35, alpha = .6) +
  geom_line(data = highlights, aes(colour = factor(year), linetype = factor(year)), linewidth = .9) +
  scale_colour_manual(values = c("#23565A", "#B34827", "#91642C")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Monthly temperature anomalies in historical context",
       subtitle = "Grey: earlier years from 1880 | dashed: incomplete current year",
       x = NULL, y = "Temperature anomaly (°C)",
       caption = paste("NASA GISTEMP v4 | Reference: each calendar month's 1951-1980 mean |", snapshot))
save_figure("l05-temperature-monthly", p)

timeline <- read.csv(file.path(data_dir, "african-timeline.csv"), stringsAsFactors = FALSE)
timeline$y <- rev(seq_len(nrow(timeline)))
wrap <- function(x, width) vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"), "")
p <- ggplot(timeline) +
  geom_rect(aes(xmin = 0, xmax = 11, ymin = y - .45, ymax = y + .45, fill = factor(y %% 2)), colour = NA) +
  scale_fill_manual(values = c("#F0F4F1", "#FFFFFF"), guide = "none") +
  geom_text(aes(x = .15, y = y, label = wrap(period, 17)), hjust = 0, size = 3.9, fontface = "bold") +
  geom_text(aes(x = 2.1, y = y, label = wrap(environment, 46)), hjust = 0, size = 3.7, lineheight = 1.15) +
  geom_text(aes(x = 6.7, y = y, label = wrap(people_plants, 43)), hjust = 0, size = 3.7, lineheight = 1.15) +
  annotate("text", x = c(.15, 2.1, 6.7), y = max(timeline$y) + .85,
           label = c("Approximate period", "Climate and environments", "People, plants and livelihoods"),
           hjust = 0, size = 3.9, fontface = "bold") +
  scale_x_continuous(limits = c(0, 11), expand = c(0, 0)) +
  scale_y_continuous(limits = c(.4, max(timeline$y) + 1.15), expand = c(0, 0)) +
  labs(title = "An African companion to the long climate timeline",
       subtitle = "Selected regional examples | Rows are not spaced to scale; African hydroclimates differ",
       caption = "Schematic summary of Lecture 4; periods overlap and do not imply one continent-wide response.") +
  theme_void(base_family = "Arial") + theme(plot.title = element_text(face = "bold", size = 17),
     plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
     plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 12)),
     plot.margin = margin(16, 12, 14, 12))
save_figure("l04-african-timeline", p, width = 12, height = 10.8)

write_json(list(retrieved = sources$retrieved, latest_mlo = latest_mlo[c("year", "month", "mean")],
                global_annual = global_latest, global_growth = growth_latest,
                nasa_50_year_period = range(recent$Year), nasa_50_year_trend_C_per_decade = rate,
                latest_temperature_month = format(max(monthly$date), "%Y-%m")),
           file.path(data_dir, "figure_values.json"), pretty = TRUE, auto_unbox = TRUE)
