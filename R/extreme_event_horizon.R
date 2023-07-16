# Miscellaneous functions for MHW/MCS threshold calculations and plotting.
# Author: AJ Smit
# Date: 30 Mar 2023

# Set line and fill colours -----------------------------------------------

lineColCat <- c(
  "Daily" = "grey60",
  "Climatology" = "darkseagreen2",
  "Threshold (90)" = "red3",
  "Threshold (10)" = "blue3"
)

# Set category fill colours
fillColCat <- c(
  "+ Extreme" = "#2d0000",
  "+ Severe" = "#9e0000",
  "+ Strong" = "#ff6900",
  "+ Moderate" = "#ffc866",
  "- Moderate" = "#C7ECF2",
  "- Strong" = "#85B7CC",
  "- Severe" = "#4A6A94",
  "- Extreme" = "#111433"
)

# Function to calculate thresholds ----------------------------------------

thresh_fun <-
  function(df, ...) {

    df <- data.frame(t = df[[1]],
                     daily = df[[2]])

    clm_90 <-
      ts2clm(df, y = daily, ...)
    hi_ev <- detect_event(clm_90, y = daily, ...)$climatology |>
      mutate(type = "high")

    # coldspells
    clm_10 <-
      ts2clm(df, pctile = 10, y = daily, ...)
    lo_ev <- detect_event(clm_10, y = daily,
                          coldSpells = TRUE, ...)$climatology |>
      mutate(type = "low")

    ev <- rbind(hi_ev, lo_ev)

    # create a function for calculating thresholds
    thresh_calc <- function(df) {
      res1 <- df |>
        dplyr::mutate(
          diff = thresh - seas,
          thresh_2x = thresh + diff,
          thresh_3x = thresh_2x + diff,
          thresh_4x = thresh_3x + diff
        )
      return(res1)
    }

    # apply the function
    res2 <- thresh_calc(ev) |>
      select(-threshCriterion, -durationCriterion) |>
      pivot_wider(
        names_from = type,
        values_from = c(thresh, event, event_no, diff,
                        thresh_2x, thresh_3x, thresh_4x)
      )
    return(res2)
  }

# Function for horizon plots ----------------------------------------------

horizon_plot <-
  function(df, title = NULL) {
    ggplot(data = df, aes(x = doy, y = daily)) +
      # ribbon
      geom_ribbon(
        aes(ymin = thresh_low, ymax = thresh_high),
        fill = "grey97",
        colour = "NA"
      ) +
      # high
      geom_flame(aes(y2 = thresh_high, fill = "+ Moderate")) +
      geom_flame(aes(y2 = thresh_2x_high, fill = "+ Strong")) +
      geom_flame(aes(y2 = thresh_3x_high, fill = "+ Severe")) +
      geom_flame(aes(y2 = thresh_4x_high, fill = "+ Extreme")) +
      geom_line(
        aes(y = thresh_high, col = "Threshold (90)"),
        linewidth = 0.3,
        linetype = "dotted"
      ) +
      # low
      geom_flame(aes(y = thresh_low, y2 = daily, fill = "- Moderate")) +
      geom_flame(aes(y = thresh_2x_low, y2 = daily, fill = "- Strong")) +
      geom_flame(aes(y = thresh_3x_low, y2 = daily, fill = "- Severe")) +
      geom_flame(aes(y = thresh_4x_low, y2 = daily, fill = "- Extreme")) +
      geom_line(
        aes(y = thresh_low, col = "Threshold (10)"),
        linewidth = 0.3,
        linetype = "dotted"
      ) +
      # climatology and temperature
      geom_line(aes(y = seas, col = "Climatology"), linewidth = 0.4) +
      geom_line(aes(y = daily, col = "Daily"), linewidth = 0.4) +
      # scales, guides, theme, etc.
      scale_colour_manual(name = NULL,
                          values = lineColCat,
                          breaks = names(lineColCat)) +
      scale_fill_manual(name = NULL,
                        values = fillColCat,
                        breaks = names(fillColCat)) +
      guides(colour = guide_legend(),
             fill = guide_legend()) +
      labs(y = NULL, x = NULL) +
      facet_grid(year(t) ~ .) +
      theme_few() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.spacing.y = unit(-2.0, "lines"),
        strip.background = element_blank(),
        strip.text.y = element_text(
          vjust = 0.75,
          size = 9,
          angle = 0,
          hjust = 0
        ),
      ) +
      labs(x = "Doy of the year",
           title = title)
  }

# Function for horizon/Hovmöller plots ------------------------------------

hovmoller_horizon_plot <-
  function(df, title = NULL, subtitle = NULL) {
    ggplot(data = df, aes(x = t, y = daily)) +
      # ribbon
      geom_ribbon(
        aes(ymin = thresh_low, ymax = thresh_high),
        fill = "grey97",
        colour = "NA"
      ) +
      # high
      geom_flame(aes(y2 = thresh_high, fill = "+ Moderate")) +
      geom_flame(aes(y2 = thresh_2x_high, fill = "+ Strong")) +
      geom_flame(aes(y2 = thresh_3x_high, fill = "+ Severe")) +
      geom_flame(aes(y2 = thresh_4x_high, fill = "+ Extreme")) +
      geom_line(
        aes(y = thresh_high, col = "Threshold (90)"),
        linewidth = 0.3,
        linetype = "dotted"
      ) +
      # low
      geom_flame(aes(y = thresh_low, y2 = daily, fill = "- Moderate")) +
      geom_flame(aes(y = thresh_2x_low, y2 = daily, fill = "- Strong")) +
      geom_flame(aes(y = thresh_3x_low, y2 = daily, fill = "- Severe")) +
      geom_flame(aes(y = thresh_4x_low, y2 = daily, fill = "- Extreme")) +
      geom_line(
        aes(y = thresh_low, col = "Threshold (10)"),
        linewidth = 0.3,
        linetype = "dotted"
      ) +
      # climatology and temperature
      geom_line(aes(y = seas, col = "Climatology"), linewidth = 0.4) +
      geom_line(aes(y = daily, col = "Daily"), linewidth = 0.4) +
      # scales, guides, theme, etc.
      scale_colour_manual(name = NULL,
                          values = lineColCat,
                          breaks = names(lineColCat)) +
      scale_fill_manual(name = NULL,
                        values = fillColCat,
                        breaks = names(fillColCat)) +
      guides(colour = guide_legend(),
             fill = guide_legend()) +
      facet_grid(fct_rev(lat) ~ .) +
      theme_few() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing.y = unit(-2.0, "lines"),
        strip.background = element_blank(),
        strip.text.y = element_text(
          vjust = 0.75,
          size = 9,
          angle = 0,
          hjust = 0
        ),
      ) +
      labs(x = "Date",
           title = title,
           subtitle = subtitle)
  }

