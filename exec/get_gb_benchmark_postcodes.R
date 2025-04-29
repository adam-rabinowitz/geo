library(sf)
###############################################################################
## Set path, variables and read in data
###############################################################################
# Set variables
start_date <- as.Date('2022-01-01')
end_date <- as.Date('2025-01-01')
pull_id <- paste(
  'SixtyTwoCities', format(start_date, '%b%y'), format(end_date, '%b%y'), sep = '_'
)
catchment_distance <- units::as_units(10, 'km')
# Set paths
city_path <- 'data/sixty_two_cities.yaml'
postcode_path <- '~/beauclair/data/ONS/ONSPD/ONSPD_NOV_2024/processed_postcodes/jan22_nov24_postcode_points_4326.rds'
output_dir <- '~/beauclair/monthly_data/formatted_input/2025_01/queries'
# Read in city and postcode data
city_definitions <- yaml::read_yaml(city_path)
stopifnot(all(sapply(city_definitions, '[[', 'crs') == 4326))
postcodes <- readRDS(postcode_path)
stopifnot(sf::st_crs(postcodes)$input == 'EPSG:4326')
# Get polygons for each city
city_polygons <- lapply(
  city_definitions,
  function(definition) {
    geo:::create_polygon_from_str(
      coordinate_str = definition$coordinates,
      in_crs = definition$crs,
      out_crs = 4326
    ) |>
      sf::st_make_valid()
  }
)
# Create sfc for retail areas
retail_sfc <- sf::st_sf(
  city = names(city_polygons),
  geometry = do.call(c, city_polygons)
)
stopifnot(all(sf::st_is_valid(retail_sfc$geometry)))
# Create sfc for customer catchment areas
customer_sfc <- sf::st_buffer(
  retail_sfc,
  dist = catchment_distance
)

###############################################################################
## Generate months
###############################################################################
# Find start and end of months
months_start <- seq.Date(
  from = start_date, to = end_date, by = 'month'
)
stopifnot(all(lubridate::day(months_start) == 1))
months_end <- lubridate::ceiling_date(months_start, unit = 'month') - 1
# Create months table
dates_table <- tibble::tibble(
  id = pull_id,
  start_date = months_start,
  end_date = months_end,
  group = months_start
)
# Save dates
dates_path <- file.path(
  output_dir,
  paste0(pull_id, '_dates.csv')
)
readr::write_csv(
  dates_table, dates_path, progress = FALSE
)

###############################################################################
## Generate retail postcodes
###############################################################################
# Check retail overlaps
retail_intersects <- sf::st_intersects(
  retail_sfc, sparse = T, remove_self = T
)
stopifnot(sum(sapply(retail_intersects, length)) == 0)
# Find retail overlaps
retail_postcodes <- postcodes[
  sf::st_intersects(
    postcodes,
    sf::st_union(retail_sfc),
    sparse = FALSE
  )[,1],
]
retail_postcode_intersect <- sf::st_intersects(
  retail_sfc,
  retail_postcodes,
  sparse = TRUE
)
# Create retail postcode table
retail_postcode_table <- purrr::imap(
  retail_postcode_intersect,
  function(postcode_indices, city_index) {
    dplyr::tibble(
      id = pull_id,
      project = retail_sfc$city[city_index],
      retail_area = retail_sfc$city[city_index],
      postcode = retail_postcodes$pcds[postcode_indices]
    )
  }
) |>
  dplyr::bind_rows()
# Save retail postcodes
retail_path <- file.path(
  output_dir,
  paste0(pull_id, '_retail_postcodes.csv')
)
readr::write_csv(
  retail_postcode_table, retail_path, progress = FALSE
)

###############################################################################
## Generate customer postcodes
###############################################################################
# Find customer overlaps
customer_postcodes <- postcodes[
  sf::st_intersects(
    postcodes,
    sf::st_union(customer_sfc),
    sparse = FALSE
  )[,1],
]
customer_postcode_intersect <- sf::st_intersects(
  customer_sfc,
  customer_postcodes,
  sparse = TRUE
)
# Create retail postcode table
customer_postcode_table <- purrr::imap(
  customer_postcode_intersect,
  function(postcode_indices, city_index) {
    dplyr::tibble(
      id = pull_id,
      project = customer_sfc$city[city_index],
      customer_area = customer_sfc$city[city_index],
      postcode = customer_postcodes$pcds[postcode_indices]
    )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::reframe(
    postcode = geo::truncate_postcodes(
      postcode, level = 'sector', unique = TRUE, sort = TRUE 
    ),
    .by = c(id, project, customer_area)
  )
# Save retail postcodes
customer_path <- file.path(
  output_dir,
  paste0(pull_id, '_customer_postcodes.csv')
)
readr::write_csv(
  customer_postcode_table, customer_path, progress = FALSE
)



