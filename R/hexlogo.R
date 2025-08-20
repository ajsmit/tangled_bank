# Tree Hex Sticker with Grid Overlay
# Required packages installation (run if needed):
# install.packages(c("hexSticker", "ggplot2", "magick", "png", "grid"))

library(hexSticker)
library(ggplot2)
library(magick)
library(png)
library(grid)

# Function to create gridded overlay on image
create_gridded_tree <- function(image_path, grid_size = 20) {
  # Read the image
  img <- magick::image_read("images/Richtersveld_lone_tree_L.jpg")
  img_info <- magick::image_info(img)

  # Convert to raster for ggplot
  img_raster <- as.raster(img)

  # Get image dimensions
  img_width <- img_info$width
  img_height <- img_info$height

  # Create base plot with image
  p <- ggplot() +
    annotation_raster(img_raster,
      xmin = 0, xmax = img_width,
      ymin = 0, ymax = img_height
    ) +
    coord_fixed(ratio = 1) +
    xlim(0, img_width) +
    ylim(0, img_height)

  # Calculate grid dimensions
  x_tiles <- ceiling(img_width / grid_size)
  y_tiles <- ceiling(img_height / grid_size)

  # Create grid data
  grid_data <- expand.grid(
    x = seq(0, img_width - grid_size, by = grid_size),
    y = seq(0, img_height - grid_size, by = grid_size)
  )

  # Add random colors for visual interest (optional)
  # You can modify this to use specific colors or patterns
  set.seed(123) # For reproducibility
  grid_data$fill_color <- sample(c("#4ECDC4", "#95E1D3", "#A8E6CF", "#C7CEEA", "#B2E1D4"),
    nrow(grid_data),
    replace = TRUE
  )

  # Add grid overlay with semi-transparent tiles
  p <- p +
    geom_tile(
      data = grid_data,
      aes(
        x = x + grid_size / 2,
        y = y + grid_size / 2
      ),
      fill = grid_data$fill_color,
      alpha = 0.2, # As requested
      width = grid_size,
      height = grid_size,
      color = "white", # Tile borders
      size = 0.05
    ) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0, "pt"))

  return(p)
}

# Hex logo
create_custom_hex <- function(image_path,
                              package_name = "TreeGrid",
                              grid_size = 25,
                              tile_alpha = 0.2,
                              hex_fill = "#CD5C5C",
                              output_name = "custom_hex.png") {
  # Create gridded plot
  plot_with_grid <- create_gridded_tree(image_path, grid_size)

  # Generate hex sticker with custom parameters
  sticker(
    subplot = plot_with_grid,
    package = package_name,
    p_size = 18,
    p_color = "white",
    p_y = 1.6,
    p_x = 1,
    p_family = "sans",
    s_x = 0.95,
    s_y = 1,
    s_width = 3.25,
    s_height = 3.25,
    h_fill = hex_fill,
    h_color = "#78C2AD",
    h_size = 1.0,
    filename = output_name,
    dpi = 300,
    white_around_sticker = TRUE,
    spotlight = FALSE, # Add spotlight effect
    l_x = 1, # Spotlight x position
    l_y = 0.8, # Spotlight y position
    l_alpha = 0.3, # Spotlight transparency
    l_width = 3 # Spotlight width
  )

  cat(paste("Custom hex sticker saved as:", output_name, "\n"))
}

# Run the function
create_custom_hex("tree_image.jpg",
  package_name = "R@BCB",
  grid_size = 30,
  tile_alpha = 0.2,
  hex_fill = "#CD5C5C",
  output_name = "my_tree_hex.png"
)
