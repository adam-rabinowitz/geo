# Read current postcodes
current_postcodes <- geo::read_ons_postcodes(
  '~/beauclair/data/ONSPD/ONSPD_AUG_2023_UK/Data/ONSPD_AUG_2023_UK.csv.gz',
  rm_terminated = T, rm_nogrid = T
) |>
  dplyr::mutate(
    pcds = geo::format_complete_postcodes(pcds)
  )
# Read retail data
postcode_regex <- geo::get_postcode_regex(level = 'complete')
all_retail_postcodes <- readr::read_csv(
  '~/beauclair/city_data/region_definitions/input_files/Imfoco_retail_catchment_areas.csv',
  progress = F, show_col_types = F
) |>
  dplyr::select(
    client = project,
    area = city,
    initial_pcds = targetpostcode
  ) |>
  dplyr::mutate(
    formatted_pcds = geo::format_complete_postcodes(
      initial_pcds, to_upper = TRUE, replace_malformed = TRUE
    ),
    current = formatted_pcds %in% current_postcodes$pcds
  )
# Malformed postcodes
malformed_postcodes <- all_retail_postcodes |>
  dplyr::filter(is.na(formatted_pcds)) |>
  dplyr::pull(initial_pcds)
# Retired
retired_postcodes <- all_retail_postcodes |>
  dplyr::filter(
    !is.na(formatted_pcds) & !current
  )
# Duplicated postcodes
duplicated_postcodes <- all_retail_postcodes |>
  dplyr::filter(
    !is.na(formatted_pcds)
  ) |>
  dplyr::summarise(
    intial_pcds = paste(initial_pcds, collapse=', '),
    n = dplyr::n(),
    .by = c(client, area, formatted_pcds)
  ) |>
  dplyr::filter(n > 1) |>
  dplyr::arrange(dplyr::desc(n))
# Find multi region postcodes
multiregion_postcodes <- all_retail_postcodes |>
  dplyr::filter(
    !is.na(formatted_pcds)
  ) |>
  dplyr::summarise(
    n_areas = length(unique(area)),
    areas = paste(unique(area), collapse=', '),
    .by = c(client, formatted_pcds)
  ) |>
  dplyr::filter(n_areas > 1)
# Postcode counts
n_postcodes_client <- all_retail_postcodes |>
  dplyr::select(
    client, formatted_pcds
  ) |>
  dplyr::distinct() |>
  dplyr::summarise(
    n = dplyr::n(),
    .by = client
  )
# Filter by current postcodes
current_retail_postcodes <- all_retail_postcodes |>
  dplyr::filter(
    !is.na(formatted_pcds) & current
  ) |>
  dplyr::select(
    client, area, pcds = formatted_pcds 
  ) |>
  dplyr::distinct()
# Find overlaps
client_list <- split(
  current_retail_postcodes,
  current_retail_postcodes$client
)
table(client_list$Reading$assignment, useNA = 'always')
# Save data
for (client in names (client_list)) {
  path <- paste0(
    '~/beauclair/city_data/region_definitions/current_asignments/',
    gsub(' ', '_', base::tolower(client)),  '_retail_areas.csv'
  )
  message(path)
  readr::write_csv(
    client_list[[client]][
      geo::order_postcodes(client_list[[client]]$pcds, level='complete'),
    ],
    file = path
  )
}







y <- geo::find_sector_intersect(
  x$pcds, current_postcodes$pcds
)