###############################################################################
## Set paths and parameters
###############################################################################
# Set paths
area_yaml <- '~/beauclair/monthly_data/formatted_input/2025_05/yaml/client_areas.yaml'
definition_dir <- '~/beauclair/monthly_data/formatted_input/2025_05/definitions'
query_dir <- '~/beauclair/monthly_data/formatted_input/2025_05/queries'
map_dir <- '~/beauclair/monthly_data/formatted_input/2025_05/maps'
all_paths <- c(area_yaml, definition_dir, query_dir, map_dir)
# Set months
month_range <- as.Date(c('2022-01-01', '2025-05-01'))
stopifnot(all(lubridate::day(month_range) == 1))
stopifnot(
  all(grepl(format(month_range[2], '%Y_%m'), all_paths))
)
pull_id <- paste(c('BID', format(month_range, '%b%y')), collapse = '_')
# Set query defaults
query_default <- list(
  `sector lookup` = 1L,
  `sector aggregation` = list('individual', 'aggregated'),
  `include subsectors` = FALSE,
  channels = list('offline'),
  cards = list('debit'),
  `geodemographic segmentation` = 'mosaic',
  `fixed base years` = 4L,
  `include rest of gb customers` = TRUE,
  `include rest of gb retail` = FALSE
)
# Get definitions
definition_yaml <- list.files(
  definition_dir, pattern = 'regions.yaml$', full.names = TRUE
)
names(definition_yaml) <- sapply(
  definition_yaml,
  function(z) {
    yaml::read_yaml(z)$client
  }
)
stopifnot(all(!duplicated(names(definition_yaml))))
# Create output directories
dir.create(query_dir, showWarnings = FALSE, recursive = FALSE)
dir.create(map_dir, showWarnings = FALSE, recursive = FALSE)

###############################################################################
## Check definitions and requirements are consistent
###############################################################################
# Get required areas from the area yaml file
required_areas <- yaml::read_yaml(area_yaml) |>
  purrr::imap(
    function(areas, client) {
      tidyr::expand_grid(
        client = client,
        retail_area = areas$retail,
        customer_area = areas$customer
      )
    }
  ) |>
  dplyr::bind_rows() |>
  dplyr::arrange(
    client, retail_area, customer_area
  )
# Get defined areas from the definition yaml files
defined_areas <- lapply(
    definition_yaml,
    function(path) {
      definition <- yaml::read_yaml(path)
      tidyr::expand_grid(
        client = definition$client,
        retail_area = definition$`retail areas`$selected,
        customer_area = c(definition$`customer areas`$selected, 'Rest Of GB')
      )
    }
  ) |>
  dplyr::bind_rows() |>
  dplyr::arrange(
    client, retail_area, customer_area
  )
# Check defined areas
stopifnot(
  identical(required_areas, dplyr::distinct(required_areas))
)
if (!identical(required_areas, defined_areas)) {
  required_unique <- dplyr::anti_join(
    required_areas,
    defined_areas,
    by = c('client', 'retail_area', 'customer_area')
  )
  defined_unique <- dplyr::anti_join(
    defined_areas,
    required_areas,
    by = c('client', 'retail_area', 'customer_area')
  )
  if (nrow(required_unique)) {
    message('required unique')
    print(required_unique)
    stop('missing defined')
  } else if (nrow(defined_unique)) {
    message('defined unique')
    print(defined_unique)
    stop('missing required')
  } else {
    stop('something is fishy')
  }
}

###############################################################################
## Get postcodes and plot data
###############################################################################
# Extract all postcode data
postcode_data_list <- lapply(
  definition_yaml,
  generate_postcode_data
)

###############################################################################
## Create and save tables
###############################################################################
# Create output paths
output_paths <- list(
  yaml = file.path(
    query_dir,
    paste0(pull_id, '.yaml')
  ),
  dates = file.path(
    query_dir,
    paste0(pull_id, '_dates.csv')
  ),
  retail = file.path(
    query_dir,
    paste0(pull_id, '_retail_postcodes.csv')
  ),
  customer = file.path(
    query_dir,
    paste0(pull_id, '_customer_postcodes.csv')
  )
)
# Create query yaml
yaml_output <- c(
  list(
    id = pull_id,
    projects = names(definition_yaml),
    `retail areas` = basename(output_paths$retail),
    `customer catchment areas` = basename(output_paths$customer),
    `dates` = basename(output_paths$dates)
  ),
  query_default
)
# Create months table
months_start <- seq.Date(
  from = month_range[1], to = month_range[2], by = 'month'
)
months_end <- lubridate::ceiling_date(months_start, unit = 'month') - 1
dates_table <- tibble::tibble(
  id = pull_id,
  start_date = months_start,
  end_date = months_end,
  group = months_start
)
# Extract retail area table
retail_area_table <- lapply(
  postcode_data_list,
  function(z) {z$retail_area$postcodes}
) |>
  dplyr::bind_rows(.id = 'project') |>
  dplyr::rename(retail_area = area) |>
  dplyr::mutate(id = pull_id, .before = project)
# Extract customer area table
customer_area_table <- lapply(
  postcode_data_list,
  function(z) {z$customer_area$postcodes}
) |>
  dplyr::bind_rows(.id = 'project') |>
  dplyr::rename(customer_area = area) |>
  dplyr::mutate(id = pull_id, .before = project)
# Save file
yaml::write_yaml(
  yaml_output, output_paths$yaml, indent = 2, indent.mapping.sequence = TRUE,
  handlers = list(logical = yaml::verbatim_logical)
)
readr::write_csv(
  dates_table, output_paths$dates, progress = FALSE
)
readr::write_csv(
  retail_area_table, output_paths$retail,
  progress = FALSE
)
readr::write_csv(
  customer_area_table, output_paths$customer, progress = FALSE
)

###############################################################################
## Save area polygons
###############################################################################
# Extract voronoi
voronoi_list <- lapply(
  postcode_data_list,
  function(z) {
    list(
      retail_areas = z$retail_areas$voronoi,
      customer_areas = z$customer_areas$voronoi
    )
  }
)
# Save to file
for (client in names(voronoi_list)) {
  client_str <- tolower(gsub(' ', '_', client))
  saveRDS(
    voronoi_list[[client]],
    file.path(
      map_dir,
      paste0(client_str, '_plot_data.rds')
    )
  )
}
# Create kings road polygons
kings_road_data <- readRDS(
  file.path(map_dir, 'west_central_plot_data.rds')
)
stopifnot(
  identical(
    levels(kings_road_data$retail_areas$area),
    c('Kings Road', 'Knightsbridge', 'Marylebone', 'Sloane Square')
  )
)
saveRDS(
  kings_road_data,
  file.path(map_dir, 'kings_road_plot_data.rds')
)
# Create kings road polygons
knightsbridge_data <- readRDS(
  file.path(map_dir, 'west_central_plot_data.rds')
)
knightsbridge_data$retail_areas <- knightsbridge_data$retail_areas |>
  dplyr::mutate(
    area = forcats::fct(
      as.character(area),
      levels = c('Knightsbridge', 'Kings Road', 'Marylebone', 'Sloane Square')
    )
  ) |>
  dplyr::arrange(area)
saveRDS(
  knightsbridge_data,
  file.path(map_dir, 'knightsbridge_plot_data.rds')
)
