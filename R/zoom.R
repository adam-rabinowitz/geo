# Function to calculate tile width
calc_tile_width <- function(latitude, zoom) {
  circumfrence <- units::as_units(40075016.686, 'm')
  tile_width <- (circumfrence * base::cos(latitude)) / (2 ^ zoom)
  return(tile_width)
}

# Function to calculate zoom width
calc_width_per_pixel <- function(latitude, zoom, tile_pixels) {
  tile_width <- calc_tile_width(latitude = latitude, zoom = zoom)
  width_per_pixel <- tile_width / tile_pixels
  return(width_per_pixel)
}

# Calculate zoom
calculate_zoom <- function(
  plot_height, plot_width, plot_data
) {
  # Extract long and lat metrics
  lat_range <- base::range(plot_data$lat)
  lat_center <- base::mean(lat_range)
  long_range <- base::range(plot_data$long)
  long_center <- base::mean(long_range)
  # Calculate vertical range of plot data
  vertical_meters <- sf::st_linestring(
    cbind(
      rep(long_center, 2),
      rev(lat_range)
    )
  ) |>
    sf::st_sfc(crs=4326) |>
    sf::st_length()
  # Calculate horizontal range of plot data
  horizontal_meters <- sf::st_linestring(
    cbind(
      rev(long_range),
      rep(lat_center, 2)
    )
  ) |>
    sf::st_sfc(crs=4326) |>
    sf::st_length()
  # Calculate minimum pixels per meter
  meters_per_pixel = base::max(
    vertical_meters / plot_height,
    horizontal_meters / plot_width
  ) |>
    units::drop_units()
  # Calculate zoom
  zoom = (
    (
      log((40075016.686 * cos(lat_center * pi/180))/meters_per_pixel)
    ) / log(2)
  ) - 8
  return(floor(zoom))
}
