## Script to generate voronoi from postcodes. All generation of voronoi is done
## using British National Grid coordinates (CRS: 27700) and converted to WGS
## 1984 coordinates (CRS: 4326) afterwards. There can be inconsistencies between
## the British National Grid coordinates and WGS 1984 coordinates within a 
## postcode file. Only postcodes from England, Wales and Scotland are generated.

###############################################################################
## Functions for trimming voronoi
###############################################################################
# Trim voronoi using multicore approach
multicore_trim_voronoi <- function(
  voronoi, polygon, chunk = 1000L, cores = 8L  
) {
  # Check arguments
  stopifnot(is.integer(chunk))
  stopifnot(chunk > 0)
  stopifnot(is.integer(cores))
  stopifnot(cores > 1)
  # Perform trimming on chunks
  trimmed_voronoi <- parallel::mclapply(
    split(voronoi, ceiling((1:nrow(voronoi)) / chunk)),
    function(voronoi, polygon) {sf::st_intersection(voronoi, polygon)},
    polygon = polygon,
    mc.cores = cores
  ) |>
    dplyr::bind_rows()
  return(trimmed_voronoi)
}

###############################################################################
## Set variables
###############################################################################
country_file <- '~/beauclair/data/ONS/Countries/Countries_December_2023_Boundaries_UK_BGC_6982568813931010125/CTRY_DEC_2023_UK_BGC.shp'
postcode_file <- '~/beauclair/data/ONS/ONSPD/ONSPD_NOV_2024/Data/ONSPD_NOV_2024_UK.csv.gz'
start_date <- as.Date('2022-01-01')
out_prefix <- '~/beauclair/data/ONS/ONSPD/ONSPD_NOV_2024/processed_postcodes/jan22_nov24'

###############################################################################
## Read and save initial data
###############################################################################
# Create output data
if (!dir.exists(dirname(out_prefix))) {
  dir.create(dirname(out_prefix), showWarnings = F, recursive = T)
}
# Save parameters to yaml
list(
  country_file = list(
    path = country_file,
    md5sum = tools::md5sum(country_file)
  ),
  postcode_file = list(
    path = postcode_file,
    md5sum = tools::md5sum(postcode_file)
  ),
  start_date = as.character(start_date)
) |>
  yaml::write_yaml(paste0(out_prefix, '_parameters.yaml'))
# Get polygons for GB
gb_polygons <- sf::read_sf(
  country_file  
) |>
  dplyr::filter(
    grepl('^(E|S|W)', CTRY23NM)
  ) |>
  sf::st_geometry() |>
  sf::st_cast(to = 'POLYGON') |>
  sf::st_union()
# Read gb postcodes
gb_postcodes <- readr::read_csv(
  postcode_file,
  progress = FALSE,
  col_types = readr::cols_only(
    pcds = readr::col_character(),
    dointr = readr::col_character(),
    doterm = readr::col_character(),
    osgrdind = readr::col_integer(),
    oscty = readr::col_character(),
    oslaua = readr::col_character(),
    osward = readr::col_character(),
    oseast1m = readr::col_integer(),
    osnrth1m = readr::col_integer()
  )
) |>
  dplyr::mutate(
    dointr = lubridate::as_date(dointr, format = '%Y%m'),
    doterm = lubridate::as_date(doterm, format = '%Y%m')
  ) |>
  dplyr::filter(
    (is.na(doterm) | doterm >= start_date) &
    grepl('^(E|S|W)', oscty) &
    osgrdind < 9
  ) |>
  sf::st_as_sf(
    coords = c('oseast1m', 'osnrth1m'), remove = TRUE, crs = 27700
  )
# Save input to file
saveRDS(
  gb_polygons,
  paste0(out_prefix, '_gb_polygons_27700.rds')
)
saveRDS(
  sf::st_transform(gb_polygons, crs = 4326),
  paste0(out_prefix, '_gb_polygons_4326.rds')
)
saveRDS(
  gb_postcodes,
  paste0(out_prefix, '_postcode_points_27700.rds')
)
saveRDS(
  sf::st_transform(gb_postcodes, crs = 4326),
  paste0(out_prefix, '_postcode_points_4326.rds')
)
# Create gb voronoi
gb_voronoi <- sf::st_voronoi(
  sf::st_union(gb_postcodes),
  envelope = gb_polygons
) |>
  sf::st_collection_extract()
# Determine the intersection between voronoi and postcodes
intersect_index_list <- sf::st_intersects(gb_postcodes$geometry, gb_voronoi)
stopifnot(all(sapply(intersect_index_list, length) == 1))
intersect_index <- unlist(intersect_index_list, use.names = FALSE)
stopifnot(length(intersect_index) == nrow(gb_postcodes))
# Create voronoi sfc
postcode_voronoi = sf::st_sf(
  pcds = gb_postcodes$pcds,
  geometry = gb_voronoi[intersect_index]
)
rm(gb_voronoi, intersect_index, intersect_index_list)
gc()
# Check distances
voronoi_distances <- parallel::mcmapply(
  sf::st_distance,
  x = gb_postcodes$geometry,
  y = postcode_voronoi$geometry,
  mc.cores = 8L
)
stopifnot(all(voronoi_distances == 0))
rm(voronoi_distances)
gc()
# Trim voronoi
start.time <- Sys.time()
trimmed_postcode_voronoi <- multicore_trim_voronoi(
  postcode_voronoi, gb_polygons, chunk = 1000L, cores = 8L
)
print(Sys.time() - start.time)
# Create complete sfc with trimmed voronoi
postcode_index <- match(
  gb_postcodes$pcds,
  trimmed_postcode_voronoi$pcds
)
complete_postcode_voronoi <- gb_postcodes
complete_postcode_voronoi$geometry <- trimmed_postcode_voronoi$geometry[
  postcode_index
]
# Save to file
saveRDS(
  complete_postcode_voronoi,
  paste0(out_prefix, '_postcode_voronoi_27700.rds')
)
saveRDS(
  sf::st_transform(complete_postcode_voronoi, crs = 4326),
  paste0(out_prefix, '_postcode_voronoi_4326.rds')
)

