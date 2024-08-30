create_polygon_from_str <- function(
  coordinate_str, in_crs, out_crs = 4326
) {
  # Check arguments
  stopifnot(grepl('^\\[\\[-{0,1}\\d', coordinate_str))
  stopifnot(grepl('\\d\\]\\]$', coordinate_str))
  # Convert coordinates to a vector of strings
  coordinate_str_vector <- coordinate_str |>
    gsub(pattern = '\\s+', replacement = '') |>
    gsub(pattern = '^\\[+', replacement = '') |>
    gsub(pattern = '\\]+$', replacement = '') |>
    strsplit(split = '\\],\\[')
  # Check first and last coordinates are identical
  stopifnot(
    identical(
      head(coordinate_str_vector[[1]], 1),
      tail(coordinate_str_vector[[1]], 1)
    )
  )
  # Convert character vector of coordiantes to polygon
  polygon <- coordinate_str_vector[[1]] |>
    base::strsplit(split = ',') |>
    base::lapply(as.numeric) |>
    base::lapply(sf::st_point) |>
    sf::st_sfc(crs = crs) |>
    sf::st_combine() |>
    sf::st_cast('LINESTRING') |>
    sf::st_cast('POLYGON') |>
    sf::st_transform(crs = in_crs) |>
    sf::st_make_valid()
  # Convert polygon crs to 4326
  if (in_crs != out_crs) {
    polygon <- sf::st_transform(
      polygon, crs = out_crs
    )
  }
  return(polygon)
}

create_str_from_polygon <- function(
  polygon
) {
  # Get coordinate str
  point_matrix <- sf::st_coordinates(polygon)[,c('X', 'Y')]
  coordinate_str <- paste0(
    '[[',
    paste(apply(point_matrix, 1, paste, collapse=','), collapse='],['),
    ']]'
  )
  # Get crs
  crs <- as.integer(
    gsub('EPSG:', '', sf::st_crs(polygon)$input)
  )
  # Create and return string
  output <- list(
    'coordinates' = coordinate_str, 
    'crs' = crs
  )
  return(output)
}
