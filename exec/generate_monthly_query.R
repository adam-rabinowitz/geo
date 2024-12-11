###############################################################################
## Set paths and variables
###############################################################################
# Set paths and variables
yaml_files <- list(
  'Blaenau Gwent' = '~/beauclair/city_data/region_definitions/yaml_definitions/blaenau_gwent.yaml',
  'Cardiff' = '~/beauclair/city_data/region_definitions/yaml_definitions/cardiff_regions.yaml',
  'Chester' = '~/beauclair/city_data/region_definitions/yaml_definitions/chester_regions.yaml',
  'Doncaster' = '~/beauclair/city_data/region_definitions/yaml_definitions/doncaster_regions.yaml',
  'Glasgow' = '~/beauclair/city_data/region_definitions/yaml_definitions/glasgow_regions.yaml',
  'Kensington' = '~/beauclair/city_data/region_definitions/yaml_definitions/kensington_regions.yaml',
  'West Central' = '~/beauclair/city_data/region_definitions/yaml_definitions/west_central_regions.yaml',
  'Liverpool' = '~/beauclair/city_data/region_definitions/yaml_definitions/liverpool_regions.yaml',
  'Manchester' = '~/beauclair/city_data/region_definitions/yaml_definitions/manchester_regions.yaml',
  'Milton Keynes' = '~/beauclair/city_data/region_definitions/yaml_definitions/milton_keynes_regions.yaml',
  'North Notts' = '~/beauclair/city_data/region_definitions/yaml_definitions/north_notts.yaml',
  'Reading' = '~/beauclair/city_data/region_definitions/yaml_definitions/reading_regions.yaml',
  'Salisbury' = '~/beauclair/city_data/region_definitions/yaml_definitions/salisbury_regions.yaml',
  'Sheffield' = '~/beauclair/city_data/region_definitions/yaml_definitions/sheffield_regions.yaml',
  'Shrewsbury' = '~/beauclair/city_data/region_definitions/yaml_definitions/shrewsbury_constituency_regions.yaml',
  'South West' = '~/beauclair/city_data/region_definitions/yaml_definitions/south_west.yaml',
  'Southampton' = '~/beauclair/city_data/region_definitions/yaml_definitions/southampton_regions.yaml',
  'Southport' = '~/beauclair/city_data/region_definitions/yaml_definitions/southport.yaml',
  'Sunderland' = '~/beauclair/city_data/region_definitions/yaml_definitions/sunderland_regions.yaml',
  'Watford' = '~/beauclair/city_data/region_definitions/yaml_definitions/watford_regions.yaml',
  'York' = '~/beauclair/city_data/region_definitions/yaml_definitions/york_regions.yaml'
)
postcode_points_path <- '~/beauclair/data/ONSPD/ONSPD_NOV_2024/processed_postcodes/jan22_nov24_postcode_points_4326.yaml'
postcode_voronoi_path <- '~/beauclair/data/ONSPD/ONSPD_NOV_2024/processed_postcodes/jan22_nov24_postcode_voronoi_4326.yaml'
month_range <- as.Date(c('2022-01-01', '2024-11-01'))
pull_id <- paste(c('BID', format(month_range, '%b%y')), collapse = '_')
months_start <- seq.Date(
  from = month_range[1], to = month_range[2], by = 'month'
)
months_end <- lubridate::ceiling_date(months_start, unit = 'month') - 1
if (!dir.exists(postcode_dir)) {
  dir.create(postcode_dir)
}
crs <- 4326

###############################################################################
## Create postcode data
###############################################################################
# Get all pull data
pull_data_list <- purrr::imap(
  yaml_files,
  function(yaml_path, client) {
    message(paste0('\n', client))
    pull_data <- geo::generate_pull_data(yaml_path)
    return(pull_data)
  }
)
# Transpose data
postcode_data_list <- pull_data_list |>
  purrr::transpose() |>
  lapply(dplyr::bind_rows, .id = 'project')
# Save retail postcodes
retail_out_path <- base::file.path(
  postcode_dir,
  paste0(pull_id, '_retail_postcodes.csv')
)
postcode_data_list$retail |>
  dplyr::mutate(
    id = pull_id,
  ) |>
  dplyr::select(
    id, project, retail_area = area, postcode
  ) |>
  readr::write_csv(retail_out_path, progress = F)
# Save customer postcodes
customer_out_path <- base::file.path(
  postcode_dir,
  paste0(pull_id, '_customer_postcodes.csv')
)
postcode_data_list$customer |>
  dplyr::mutate(
    id = pull_id,
  ) |>
  dplyr::select(
    id, project, customer_area = area, postcode
  ) |>
  readr::write_csv(customer_out_path, progress = F)

###############################################################################
## Create plot data
###############################################################################
# Create plot data
voronoi <- readRDS(voronoi_path)
for (client in names(yaml_files)) {
  # Process client
  message(paste0('\n', client))
  client_str <- gsub(' ', '_', base::tolower(client))
  # Create output path
  out_path <- base::file.path(
    postcode_dir,
    paste0(client_str, '_plot_data.rds')
  )
  # Create output and save
  plot_data <- geo::generate_polygon_plot_data(
    retail_postcodes = dplyr::filter(
      postcode_data_list$retail, project == client
    ),
    customer_postcodes = dplyr::filter(
      postcode_data_list$customer, project == client
    ),
    voronoi = voronoi,
    crs = crs
  )
  saveRDS(plot_data, out_path)
}

###############################################################################
## Create plot dates
###############################################################################
# Create output path
out_path <- base::file.path(
  postcode_dir,
  paste0(pull_id, '_dates.csv')
)
# Create output and save
date_data <- tibble::tibble(
  id = pull_id,
  `start_date` = format(months_start, '%Y-%m-%d'),
  `end_date` = format(months_end, '%Y-%m-%d'),
  group = format(months_start, '%Y-%m-%d')
)
readr::write_csv(
  date_data, out_path, progress = F
)

###############################################################################
## Process west central postcodes
###############################################################################
# Read and check west central data
west_central_areas <- c(
  'Kings Road Postcodes', 'Kings Road Shape',
  'Knightsbridge Postcodes', 'Knightsbridge Shape',
  'Marylebone Postcodes', 'Marylebone Shape',
  'Sloane Square Postcodes', 'Sloane Square Shape'
)
west_central_path <- base::file.path(
  postcode_dir, 'west_central_plot_data.rds'
)
west_central_data <- readRDS(west_central_path)
stopifnot(identical(levels(west_central_data$retail$area), west_central_areas))
# Save kings road data
kings_road_path <- base::file.path(
  postcode_dir, 'kings_road_plot_data.rds'
)
saveRDS(west_central_data, kings_road_path)
# Save knightsbridge data
knightsbridge_path <- base::file.path(
  postcode_dir, 'knightsbridge_plot_data.rds'
)
knightsbridge_data <- west_central_data
knightsbridge_data$retail$area <- factor(
  knightsbridge_data$retail$area,
  levels = west_central_areas[c(3,4,1,2,5,6,7,8)]
)
knightsbridge_data$retail <- dplyr::arrange(
  knightsbridge_data$retail, area
)
saveRDS(knightsbridge_data, knightsbridge_path)

###############################################################################
## Process shropshirea areas
###############################################################################
# Read and check west central data
shrewsbury_areas <- c(
  'Shrewsbury Town Centre', 'Shrewsbury',
  'North Shropshire', 'South Shropshire',
  'Shropshire UA', 'Chester', 'Lincolnshire'
)
shrewsbury_path <- base::file.path(
  postcode_dir, 'shrewsbury_plot_data.rds'
)
shrewsbury_data <- readRDS(shrewsbury_path)
stopifnot(identical(levels(shrewsbury_data$retail$area), shrewsbury_areas))
# Save north shropshire data
north_shropshire_path <- base::file.path(
  postcode_dir, 'north_shropshire_plot_data.rds'
)
north_shropshire_data <- shrewsbury_data
north_shropshire_data$retail$area <- factor(
  north_shropshire_data$retail$area,
  levels = shrewsbury_areas[c(3,1,2,4,5,6,7)]
)
north_shropshire_data$retail <- dplyr::arrange(
  north_shropshire_data$retail, area
)
saveRDS(north_shropshire_data, north_shropshire_path)
# Save north shropshire data
south_shropshire_path <- base::file.path(
  postcode_dir, 'south_shropshire_plot_data.rds'
)
south_shropshire_data <- shrewsbury_data
south_shropshire_data$retail$area <- factor(
  south_shropshire_data$retail$area,
  levels = shrewsbury_areas[c(4,1,2,3,5,6,7)]
)
south_shropshire_data$retail <- dplyr::arrange(
  south_shropshire_data$retail, area
)
saveRDS(south_shropshire_data, south_shropshire_path)



