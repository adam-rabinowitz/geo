#' Read ons postcodes
#'
#' Read ons postcode csv, filter and convert to sf object 
#'
#' @param path Paths to postcode CSV
#' @export
read_ons_postcodes <- function(
  path, rm_terminated, rm_nogrid
) {
  # Check arguments
  stopifnot(base::is.logical(rm_terminated))
  stopifnot(base::length(rm_terminated) == 1)
  stopifnot(base::is.logical(rm_nogrid))
  stopifnot(base::length(rm_nogrid) == 1)
  # Select columns
  selected_columns <- c(
    'pcds', 'doterm', 'osgrdind', 'oscty', 'oslaua', 'lat', 'long'
  )
  # Create filter functions
  if (rm_terminated) {
    doterm_filter <- function(doterm) {is.na(doterm)}
  } else {
    doterm_filter <- function(doterm) {rep(TRUE, length(doterm))}
  }
  if (rm_nogrid) {
    # Added 99.9 latitude filter to deal with an edge case
    osgrdind_filter <- function(osgrdind, lat) {osgrdind < 9 & lat < 99.9} 
  } else {
    osgrdind_filter <- function(osgrdind, lat) {rep(TRUE, length(osgrdind))}
  }
  # Remove terminated and no grid
  postcodes <- readr::read_csv(
    path, progress=F, col_types=readr::cols(),
    col_select=dplyr::all_of(selected_columns)
  ) |>
    dplyr::filter(
      doterm_filter(doterm) &
      osgrdind_filter(osgrdind, lat)
    ) |>
    sf::st_as_sf(
      coords = c('long', 'lat'), remove = TRUE, crs = 4326
    )
  return(postcodes)
}

#' Read geojson polygon
#' 
#' Read geojson polygon and convert to 4326 CRS
#' @param geojson geojson string or path to geojson file
#' @param input_crs input coordinate reference system
#' @return simple feature collection containing polygons
# Read geojson
read_geojson_polygon <- function(
  geojson, input_crs, output_crs
) {
  # Read geojson, check geometry
  polygon <- sf::read_sf(geojson, drivers = 'GeoJSON')
  stopifnot(sf::st_geometry_type(polygon) == 'POLYGON')
  stopifnot(length(polygon) == 1)
  # Transform geometry and return
  base::suppressWarnings(sf::st_crs(polygon) <- input_crs)
  stopifnot(sf::st_can_transform(src = input_crs, dst = output_crs))
  transformed_polygon <- sf::st_transform(polygon, crs = output_crs)
  return(transformed_polygon)
}

#' Get selected postcodes
#' 
#' Get directly selected postcodes
#' 
#' @param postcodes ONS postcodes
#' @param selected Character vector of postcodes
#' @return sf object containing the selected postcodes
get_selected_postcodes <- function(
  postcodes, selected
) {
  # Check arguments
  stopifnot('pcds' %in% colnames(postcodes))
  stopifnot(!is.null(selected))
  stopifnot(length(selected) > 0)
  check_postcodes(selected, level = 'complete')
  # Format postcodes
  selected <- format_complete_postcodes(
    selected, to_upper = F, replace_malformed = F, sort = F
  )
  # Process duplicated postcodes
  if (any(base::duplicated(selected))) {
    # Report duplicated postcodes
    duplicate_selections <- base::unique(selected[base::duplicated(selected)])
    duplicated_message <- paste0(
      "The following selected postcodes are duplicated: '",
      paste(duplicate_selections, collapse = "', '"), "'"
    )
    message(duplicated_message)
    # Remove duplicates
    selected <- base::unique(selected)
  }
  # Find postcodes
  selected_indices <- base::match(selected, postcodes$pcds)
  # Report missing postcodes
  missing <- base::which(base::is.na(selected_indices))
  if (length(missing) > 0) {
    missing_message <- paste0(
      "The following selected postcodes are missing: '",
      paste(selected[missing], collapse="', '"), "'"
    )
    message(missing_message)
  }
  # Select postcodes and return
  present_indices <- base::sort(
    selected_indices[!is.na(selected_indices)]
  )
  selected_postcodes <- postcodes[present_indices,] |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  return(selected_postcodes)
}

#' Get selected postcodes
#' 
#' Get directly selected postcodes
#' 
#' @param postcodes ONS postcodes
#' @param selected Character vector of postal sectors
#' @return sf object containing the selected postcodes
get_selected_sectors <- function(
  postcodes, selected
) {
  # Check arguments
  stopifnot('pcds' %in% colnames(postcodes))
  stopifnot(!is.null(selected))
  stopifnot(length(selected) > 0)
  check_postcodes(selected, level = 'sector')
  # Process duplicated postcodes
  if (any(base::duplicated(selected))) {
    # Report duplicated postcodes
    duplicate_selections <- base::unique(selected[base::duplicated(selected)])
    duplicated_message <- paste0(
      "The following selected postal sectors are duplicated: '",
      paste(duplicate_selections, collapse = "', '"), "'"
    )
    message(duplicated_message)
    # Remove duplicates
    selected <- base::unique(selected)
  }
  # Find postcodes
  sector_list <- split(
    postcodes$pcds, get_postcode_sectors(postcodes$pcds)
  )
  selected_indices <- match(selected, names(sector_list))
  # Report missing postcodes
  missing <- base::which(base::is.na(selected_indices))
  if (length(missing) > 0) {
    missing_message <- paste0(
      "The following selected postal sectors are missing: '",
      paste(selected[missing], collapse="', '"), "'"
    )
    message(missing_message)
  }
  # Select postcodes
  present_indices <- selected_indices[!is.na(selected_indices)]
  sector_postcodes <- sector_list[present_indices] |>
    base::unlist(use.names = FALSE) |>
    sort_postcodes()
  # Select data and return
  selected_postcodes <- postcodes[
    match(sector_postcodes, postcodes$pcds),
  ]
  return(selected_postcodes)
}

#' Get regex postcodes
#' 
#' Get postcodes matching a regex definition
#' 
#' @param postcodes ONS postcodes
#' @param definition_list A regex definition from yaml file
#' @return sf object containing postcodes matching the regex
get_regex_postcodes <- function(
  postcodes, regex
) {
  # Check arguments
  stopifnot('pcds' %in% colnames(postcodes))
  stopifnot(base::class(regex) == 'character')
  stopifnot(length(regex) >= 1)
  # Get postcodes for each regex
  postcode_list <- lapply(
    regex,
    function(reg) {
      postcodes[grepl(reg, postcodes$pcds), , drop = FALSE]
    }
  )
  # Check for non_productive regex
  unproductive_regex <- regex[
    base::sapply(postcode_list, base::nrow) == 0
  ]
  if (length(unproductive_regex) > 0) {
    base::message(
      base::paste0(
        'The following regex were unproductive: "',
        base::paste0(unproductive_regex, collapse = '", "'),
        '"'
      )
    )
  }
  # Raise an error if redundant postcodes identified
  all_postcodes <- base::unlist(
    sapply(postcode_list, '[[', 'pcds'), use.names = FALSE
  )
  unique_postcodes <- base::unique(all_postcodes)
  if (length(unique_postcodes) < length(all_postcodes)) {
    stop(
      base::paste0(
        'The following regex identified duplicate postcodes: "',
        base::paste(definition_list$regex, collapse = '", "'),
        '"'
      )
    )
  }
  # Generate merged and sorted postcodes
  regex_postcodes <- postcode_list |>
    dplyr::bind_rows() |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  return(regex_postcodes)
}

#' Get point postcodes
#' 
#' Get postcodes matching a point definition
#' 
#' @param postcodes sf object containing postcodes
#' @param lat latitude of point
#' @param long longitude of points
#' @param crs coordinate reference system 
#' @param distance maximum distance from point
#' @param units units of distance
#' @return sf object containing postcodes within specified distance from point
get_point_postcodes <- function(
  postcodes, lat, long, crs, distance, units
) {
  # Check arguments
  stopifnot(sf::st_crs(postcodes)[['input']] == 'EPSG:4326')
  stopifnot('osgrdind' %in% colnames(postcodes))
  stopifnot(length(lat) == 1)
  stopifnot(is.numeric(lat))
  stopifnot(length(long) == 1)
  stopifnot(is.numeric(long))
  stopifnot(length(crs) == 1)
  stopifnot(is.integer(crs))
  stopifnot(sf::st_can_transform(src = crs, dst = 4326))
  stopifnot(length(distance) == 1)
  stopifnot(is.numeric(distance))
  stopifnot(length(units) == 1)
  stopifnot(units %in% c('m', 'km', 'mi'))
  # Create distance and point
  max_distance = units::as_units(distance, units)
  point <- sf::st_sfc(
    sf::st_point(c(long, lat))
  )
  sf::st_crs(point) <- crs
  if (crs != 4326) {
    point <- sf::st_transform(point, crs = crs)
  }
  # Create filter, apply and return
  osgrd_postcodes <- dplyr::filter(postcodes, osgrdind < 9)
  distance <- sf::st_distance(osgrd_postcodes, point)[, 1, drop = TRUE]
  filtered_postcodes <- osgrd_postcodes[
    distance <= max_distance, , drop = FALSE
  ] |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  # Check postcodes and return
  stopifnot(!base::any(base::duplicated(filtered_postcodes$pcds)))
  return(filtered_postcodes)
}

#' #' Get polygon postcodes
#' #' 
#' #' Get postcodes contained with a geojson polygon
#' #' 
#' #' @param postcodes sf object containing postcodes
#' #' @param geojson A geojson of a single polygon
#' #' @param crs Coordinate reference system of the polygon
#' #' @return sf object containing postcodes within the polygon
#' get_polygon_postcodes <- function(
#'   postcodes, geojson, crs
#' ) {
#'   # Check arguments
#'   stopifnot(sf::st_crs(postcodes)[['input']] == 'EPSG:4326')
#'   stopifnot('osgrdind' %in% colnames(postcodes))
#'   stopifnot(length(geojson) == 1)
#'   stopifnot(class(geojson) == 'character')
#'   stopifnot(length(crs) == 1)
#'   stopifnot(is.integer(crs))
#'   stopifnot(sf::st_can_transform(src = crs, dst = 4326))
#'   # Create polygon
#'   polygon <- read_geojson_polygon(
#'     geojson = geojson,
#'     input_crs = crs,
#'     output_crs = 4326
#'   )
#'   stopifnot(sf::st_geometry_type(polygon) == 'POLYGON')
#'   stopifnot(length(polygon) == 1)
#'   # Get postcodes
#'   filtered_postcodes <- sf::st_filter(
#'     x = dplyr::filter(postcodes, postcodes$osgrdind < 9),
#'     y = polygon
#'   ) |>
#'     dplyr::arrange(order_postcodes(pcds, level = 'complete'))
#'   # Check postcodes and return
#'   if (nrow(filtered_postcodes) == 0) {
#'     stop('Polygon did not identify any postcodes')
#'   }
#'   stopifnot(!base::any(base::duplicated(filtered_postcodes$pcds)))
#'   return(filtered_postcodes)
#' }

create_polygon_from_str <- function(
  coordinate_str, crs    
) {
  # Check arguments
  stopifnot(grepl('^\\[\\[-{0,1}\\d', coordinate_str))
  stopifnot(grepl('\\d\\]\\]$', coordinate_str))
  stopifnot(length(crs) == 1)
  stopifnot(is.integer(crs))
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
    sf::st_transform(crs = crs) |>
    sf::st_make_valid()
  # Convert polygon crs to 4326
  if (crs != 4326) {
    polygon <- sf::st_transform(
      polygon, crs = 4326
    )
  }
  return(polygon)
}

#' Get polygon postcodes
#' 
#' Get postcodes contained with a geojson polygon
#' 
#' @param postcodes sf object containing postcodes
#' @param coordinate_str A string listing xy coordinates for polygon vertices
#' @param crs Coordinate reference system of the polygon
#' @return sf object containing postcodes within the polygon
get_polygon_postcodes <- function(
  postcodes, coordinate_str, crs
) {
  # Check arguments
  stopifnot(sf::st_crs(postcodes)[['input']] == 'EPSG:4326')
  stopifnot(!is.null(coordinate_str))
  # Convert coordinates to a character vector
  polygon <- create_polygon_from_str(
    coordinate_str = coordinate_str,
    crs = crs
  )
  # Get postcodes
  filtered_postcodes <- sf::st_filter(
    x = dplyr::filter(postcodes, postcodes$osgrdind < 9),
    y = polygon
  ) |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  # Check postcodes and return
  if (nrow(filtered_postcodes) == 0) {
    stop('Coordinates did not identify any postcodes')
  }
  stopifnot(!base::any(base::duplicated(filtered_postcodes$pcds)))
  return(filtered_postcodes)
}

#' Get laua postcodes
#' 
#' Get postcodes contained within a local/unitary authority
#' 
#' @param postcodes sf object containing postcodes
#' @param laua A character vector of local/unitary authority codes
#' @return sf object containing postcodes within the selected laua
get_laua_postcodes <- function(
  postcodes, laua
) {
  # Check arguments
  stopifnot('oslaua' %in% colnames(postcodes))
  stopifnot(class(laua) == 'character')
  stopifnot(length(laua) >= 1)
  stopifnot(!any(duplicated(laua)))
  stopifnot(all(laua %in% postcodes$oslaua))
  # Get postcodes
  filtered_postcodes <- postcodes[
    postcodes$oslaua %in% laua, , drop = FALSE
  ] |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  # Check postcodes and return
  stopifnot(nrow(filtered_postcodes) > 0)
  stopifnot(!base::any(base::duplicated(filtered_postcodes$pcds)))
  return(filtered_postcodes)
}

#' Get counties postcodes
#' 
#' Get postcodes contained within a local/unitary authority
#' 
#' @param postcodes sf object containing postcodes
#' @param counties A character vector of county codes
#' @return sf object containing postcodes within the selected counties
get_county_postcodes <- function(
  postcodes, counties
) {
  # Check arguments
  stopifnot('oscty' %in% colnames(postcodes))
  stopifnot(class(counties) == 'character')
  stopifnot(length(counties) >= 1)
  stopifnot(!any(duplicated(counties)))
  stopifnot(all(counties %in% postcodes$oscty))
  # Get postcodes
  filtered_postcodes <- postcodes[
    postcodes$oscty %in% counties, , drop = FALSE
  ] |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  # Check postcodes and return
  stopifnot(nrow(filtered_postcodes) > 0)
  stopifnot(!base::any(base::duplicated(filtered_postcodes$pcds)))
  return(filtered_postcodes)
}

#' Get postcodes from definitions
#' 
#' Get list of postcodes from list of definitions
#' @param postcodes ONS postcodes
#' @param definiton List of area definition
#' @return returns tibble of postcodes
#' @export
get_postcodes_from_definition <- function(
  postcodes, definition
) {
  # Get postcodes
  if (is.null(definition$type)) {
    stop('no type defined')
  }
  # Get directly defined postcodes
  if (definition$type == 'postcodes') {
    selected_postcodes <- get_selected_postcodes(
      postcodes = postcodes,
      selected = definition$postcodes
    )
  # Get postal sectors
  } else if (definition$type == 'postal sectors') {
    selected_postcodes <- get_selected_sectors(
      postcodes = postcodes,
      selected = definition$sectors
    )
  # Get postcodes defined by regex
  } else if (definition$type == 'regex') {
    selected_postcodes <- get_regex_postcodes(
      postcodes = postcodes,
      regex = definition$regex
    )
  # Get postcodes defined by point
  } else if (definition$type == 'point') {
    selected_postcodes <- get_point_postcodes(
      postcodes = postcodes,
      lat = definition$lat,
      long = definition$long,
      crs = definition$crs,
      distance = definition$distance,
      units = definition$units
    )
  # Get postcodes defined by polygon
  } else if (definition$type == 'polygon') {
    selected_postcodes <- get_polygon_postcodes(
      postcodes = postcodes,
      coordinate_str = definition$coordinates,
      crs = definition$crs
    )
  # Get postcodes defined by laua
  } else if (definition$type == 'laua') {
    selected_postcodes <- get_laua_postcodes(
      postcodes = postcodes,
      laua = definition$laua
    )
  # Get postcodes defined by counties
  } else if (definition$type == 'counties') {
    selected_postcodes <- get_county_postcodes(
      postcodes = postcodes,
      counties = definition$counties
    )
  # Raise error for unknown definition type
  } else (
    stop(paste('unknown definition type:', definition$type))
  )
  return(selected_postcodes)
}

#' Get postcodes from definition list
#' 
#' Function to get postcodes from a list of definitions
#' 
#' @param postcodes sf object containing postcode points
#' @param definiton_list List of area definitions
#' @returns Simple feature collection of postcodes
get_postcodes_from_definition_list <- function(
  postcodes, definition_list
) {
  # Get list of postcodes from definitions
  selected_postcodes <- list()
  for (area in names(definition_list)) {
    message(area)
    # Get postcode from definition
    selected_postcodes[[area]] <- get_postcodes_from_definition(
      postcodes = postcodes,
      definition = definition_list[[area]]
    )
  }
  return(selected_postcodes)
}

# definition_list <- yaml::read_yaml(
#   '~/beauclair/city_data/Glasgow/glasgow_regions.yaml'
# )$retail_areas$definitions
# postcodes <- read_ons_postcodes(
#   '~/beauclair/data/ONSPD/ONSPD_MAY_2023_UK/Data/ONSPD_MAY_2023_UK.csv.gz',
#   rm_terminated = T, rm_nogrid = T
# )
# area_postcodes <- get_postcodes_from_definitions(
#   postcodes = postcodes, definition_list = definition_list
# )
