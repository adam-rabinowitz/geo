# Set variables
var <- list(
  postcodes = '~/beauclair/data/ONSPD/ONSPD_AUG_2023_UK/Data/ONSPD_AUG_2023_UK.csv.gz',
  lat = 52.9206241505552,
  long = -1.47605299150281,
  crs = 4326L,
  distance = 50,
  units = 'km'
)
mosaic_values = c(
  'A - City Prosperity' = 1.20,
  'B - Prestige Positions' = 1.25,
  'C - Country Living' = 0.90,
  'D - Rural Reality' = 0.85,
  'E - Senior Security' = 0.90,
  'F - Suburban Stability' = 0.90,
  'G - Domestic Success' = 1.10,
  'H - Aspiring Homemakers' = 1.00,
  'I - Family Basics' = 0.90,
  'J - Transient Renters' = 0.80,  
  'K - Municipal Tenants' = 0.70,
  'L - Vintage Value' = 0.70,
  'M - Modest Traditions' = 0.75,
  'N - Urban Cohesion' = 0.75,
  'O - Rental Hubs' = 0.80
)
# Read postcodes
all_postcodes <- geo::read_ons_postcodes(
 var$postcodes, rm_terminated = TRUE, rm_nogrid = TRUE
)
# Get centroid of all postal sectors
all_sectors <- all_postcodes |>
  dplyr::mutate(
    sector = geo::truncate_postcodes(
      pcds, level = 'sector', unique = F, sort = F
    )
  ) |>
  dplyr::summarise(
    geometry = sf::st_centroid(sf::st_combine(geometry)),
    .by = sector
  )
# Find postcodes within 50km of Derby
proximal_sectors <- geo:::get_point_postcodes(
  all_postcodes, lat = var$lat, long = var$long,
  crs = var$crs, distance = var$distance, units = var$units 
) |>
  dplyr::pull(pcds) |>
  geo::truncate_postcodes(level = 'sector', unique = TRUE, sort = TRUE)
# Set distances
proximal_sector_distances <- tibble::tibble(
  'postal_sector' = proximal_sectors,
  'distance' = sf::st_distance(
    all_sectors[
      match(proximal_sectors, all_sectors$sector),
    ],
    sf::st_sfc(
      sf::st_point(c(var$long, var$lat)),
      crs = var$crs
    )
  )[,1]
) |>
  dplyr::arrange(
    distance
  ) |>
  dplyr::pull(
    distance, name = postal_sector
  )
# Generate empty tibble
empty_data <- tidyr::expand_grid(
  postal_sector = proximal_sectors,
  retail_area = c('in_area', 'out_of_area'),
  mosaic = names(mosaic_values)
)
# Complete empty values
base::set.seed(42)
empty_data[['sales']] <- (
  1000 *
  (mosaic_values[empty_data$mosaic]) *
  ifelse(
    empty_data$retail_area == 'in_area',
    0.6 - units::drop_units(
      proximal_sector_distances[empty_data$postal_sector] * 0.00001
    ),
    0.4 + units::drop_units(
      proximal_sector_distances[empty_data$postal_sector] * 0.00001
    )
  ) *
  (base::abs(stats::rnorm(nrow(empty_data), mean = 1, sd = 0.2)))
)
empty_data[['transactions']] <- 2
empty_data[['customers']] <- 1
# Set background frequencies
library(ggplot2)
x <- empty_data |>
  dplyr::summarise(
    sales = sum(sales),
    .by = c(postal_sector, retail_area)
  ) |>
  dplyr::mutate(
    sales = sales / sum(sales),
    distance = units::drop_units(
      proximal_sector_distances[postal_sector]
    ),
    .by = postal_sector
  ) |>
  dplyr::filter(
    retail_area == 'in_area'
  )
ggplot(
  x,
  aes(x = distance, y = sales)
) +
  geom_point()
y <- empty_data |>
  dplyr::summarise(
    sales = sum(sales),
    .by = c(mosaic, postal_sector)
  )
ggplot(
  y,
  aes(x = mosaic, y = sales)
) +
  geom_boxplot()

plot(x$distance, x$sales)



