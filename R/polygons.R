#' Create polygon from str
#' 
#' Creates a polygon from a string listing polgon points
#' 
#' @param coordinate_str A string of the polygon points
#' @param in_crs CRS of the input points
#' @param out_crs CRS of the output polygon
#' @returns A SFC object containing the polygon
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
    sf::st_sfc(crs = in_crs) |>
    sf::st_combine() |>
    sf::st_cast('LINESTRING') |>
    sf::st_cast('POLYGON') |>
    sf::st_make_valid()
  # Convert polygon crs to 4326
  if (in_crs != out_crs) {
    polygon <- sf::st_transform(
      polygon, crs = out_crs
    )
  }
  return(polygon)
}

#' Create str from polygon
#' 
#' Creates a string of coordinates from an SFC polygon
#' 
#' @param polygon A SFC object containing the polygon
#' @returns A list containing the coordinate string and the CRS
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
    'coordinate_str' = coordinate_str, 
    'crs' = crs
  )
  return(output)
}
