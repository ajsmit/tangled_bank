# generating new theme
theme_map2 <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170
) {
  theme_linedraw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      axis.line = element_line(colour = "black", size = rel(0.55)),
      axis.text = element_text(color = "navy", size = rel(0.5)),
      axis.ticks = element_line(colour = "navy"),
      axis.title = element_text(color = "black", size = rel(0.55)),
      # legend.key.width = unit(0.35, "cm"),
      legend.position = "bottom",
      legend.text = element_text(colour = "navy", size = rel(0.5)),
      legend.title = element_text(colour = "navy", size = rel(0.55)),
      panel.background = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_line("black", linetype = "dotted"),
      panel.grid.minor = element_line("black", linetype = "dotted"),
      panel.ontop = TRUE,
      plot.title = element_text(colour = "black", size = rel(0.55), hjust = 0),
      plot.subtitle = element_text(colour = "black", size = rel(0.5), hjust = 0),
      complete = TRUE
    )
}

# generating new theme
theme_map <- function(
  base_size = 9,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  theme_linedraw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      axis.line = element_line(colour = "black", size = rel(0.55)),
      axis.text = element_text(color = "grey20"),
      axis.ticks = element_line(colour = "grey20"),
      axis.title = element_text(color = "grey20"),
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.position = "right",
      legend.text = element_text(colour = "grey20", size = rel(0.7)),
      legend.title = element_text(colour = "grey20", size = rel(0.8)),
      panel.background = element_rect(colour = "grey90", fill = NA),
      panel.grid.major = element_line("grey50", linetype = "dotted", size = rel(0.7)),
      panel.grid.minor = element_line("grey50", linetype = "dotted", size = rel(0.5)),
      panel.ontop = TRUE,
      plot.title = element_text(colour = "grey20", hjust = 0),
      plot.subtitle = element_text(colour = "grey20", hjust = 0),
      strip.background = element_rect(colour = "grey80", fill = "grey95"),
      strip.text = element_text(colour = "grey20"),
      complete = TRUE
    )
}
