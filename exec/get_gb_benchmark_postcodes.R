library(sf)

###############################################################################
# Functions
###############################################################################
which_unique_max <- function(values) {
  max_value <- max(values)
  max_indices <- which(values == max_value)
  if (length(max_indices) != 1) {
    stop('failure to find max value')
  }
  return(max_indices)
}

###############################################################################
## Read in data
###############################################################################
# Set variables
city_path <- '~/Desktop/cfc.yaml'
postcode_path <- '~/beauclair/data/ONSPD/ONSPD_MAY_2024/Data/ONSPD_MAY_2024_UK.csv.gz'
earliest_termination <- as.Date('2022-01-01')
# Read in city data
city_definitions <- yaml::read_yaml(
  '~/Desktop/cfc.yaml'
)
stopifnot(all(sapply(city_definitions, '[[', 'crs') == 4326))
# Read in postcode data
postcodes <- readr::read_csv(
  '~/beauclair/data/ONSPD/ONSPD_MAY_2024/Data/ONSPD_MAY_2024_UK.csv.gz',
  progress=F, show_col_types = FALSE,
  col_select=c(pcds, doterm, osgrdind, lat, long)
) |>
  dplyr::mutate(
    doterm = as.Date(paste0(doterm, '01'), '%Y%m%d')
  ) |>
  dplyr::filter(
    osgrdind < 9 &
    lat < 99.9 &
    (is.na(doterm) | (doterm >= earliest_termination))
  ) |>
  sf::st_as_sf(
    coords = c('long', 'lat'), remove = TRUE, crs = 4326
  )
# Create city sfc
city_polygons <- lapply(
  city_definitions,
  function(definition) {
    geo:::create_polygon_from_str(
      coordinate_str = definition$coordinates,
      crs = definition$crs
    ) |>
      sf::st_make_valid()
  }
)
city_sfc <- sf::st_sf(
  city = names(city_polygons),
  geometry = do.call(c, city_polygons)
)
stopifnot(all(sf::st_is_valid(city_sfc$geometry)))

###############################################################################
## Get city postcodes
###############################################################################
# Check overlaps
city_intersects <- sf::st_intersects(
  city_sfc, sparse = T, remove_self = T, 
)
stopifnot(sum(sapply(city_intersects, length)) == 0)
# Find city postcodes
city_postcodes <- postcodes[
  sf::st_intersects(
    postcodes,
    sf::st_union(city_sfc),
    sparse = FALSE
  )[,1],
]
# Create postcode table
city_postcode_assignment <- sf::st_nearest_feature(
  city_postcodes, city_sfc
)
city_postcode_tb <- dplyr::tibble(
  city = city_sfc$city[city_postcode_assignment],
  postcode = city_postcodes$pcds
)
stopifnot(!any(duplicated(city_postcode_tb$postcode)))

###############################################################################
## Get customer postcodes
###############################################################################
# Create customer data
customer_sfc <- sf::st_buffer(
  city_sfc, dist = units::as_units(5, 'km')
)
# Find city postcodes
customer_postcodes <- postcodes[
  sf::st_intersects(
    postcodes,
    sf::st_union(customer_sfc),
    sparse = FALSE
  )[,1],
]
# Find nearest
customer_postcode_assignment <- sf::st_nearest_feature(
  customer_postcodes, city_sfc
)
# Create postcode table
customer_postcode_tb <- dplyr::tibble(
  city = city_sfc$city[customer_postcode_assignment],
  postcode = customer_postcodes$pcds
)
# Create sector table
customer_sector_tb <- customer_postcode_tb |>
  dplyr::mutate(
    sector = geo::truncate_postcodes(
      postcode, level = 'sector', unique = F, sort = F)
  ) |>
  dplyr::summarise(
    postcode_count = dplyr::n(),
    .by = c(city, sector)
  ) |>
  dplyr::summarise(
    city = city[which_unique_max(postcode_count)],
    .by = sector
  ) |>
  dplyr::select(city, sector)
