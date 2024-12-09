#' Read ons gb postcodes
#' 
#' Read GB postcodes in ONS postcode directory (ONSPD) file
#' @param path Path to ONSPD file
#' @param max_osgrdind Maximum allowable ONS grid reference quality indicator
#' @param min_doterm Minimum date of termination allowed
#' @return simple feature collection containing polygons
read_ons_gb_postcodes <- function(
  path, max_osgrdind, min_doterm, crs 
) {
  # Check arguments
  stopifnot(base::length(max_osgrdind) == 1)
  stopifnot((max_osgrdind %% 1) == 0)
  stopifnot(max_osgrdind > 0)
  stopifnot(length(min_doterm) == 1)
  stopifnot(base::inherits(min_doterm, 'Date'))
  stopifnot(base::length(crs) == 1)
  stopifnot((crs %% 1) == 0)
  stopifnot(sf::st_can_transform(27700, crs))
  # Read ons postcodes and return
  ons_gb_postcodes <- readr::read_csv(
    path,
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
      grepl('^(E|S|W)', oscty) &
      osgrdind <= max_osgrdind &
      (is.na(doterm) | doterm >= min_doterm)
    ) |>
    sf::st_as_sf(
      coords = c('oseast1m', 'osnrth1m'), remove = TRUE, crs = 27700
    ) |>
    sf::st_transform(crs = crs)
  return(ons_gb_postcodes)
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
    stop(duplicated_message)
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
    stop(missing_message)
  }
  # Select postcodes and return
  present_indices <- base::sort(
    selected_indices[!is.na(selected_indices)]
  )
  selected_postcodes <- postcodes[present_indices,] |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  return(selected_postcodes)
}

#' Get selected sectors
#' 
#' Get directly selected postal sectors
#' 
#' @param postcodes ONS postcodes
#' @param selected Character vector of postal sectors
#' @return sf object containing the selected postcodes
get_selected_sectors <- function(
  postcodes, selected
) {
  # Check arguments
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
    stop(duplicated_message)
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
    stop(missing_message)
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
    missing_message <- base::paste0(
      'The following regex were unproductive: "',
      base::paste0(unproductive_regex, collapse = '", "'), '"'
    )
    stop(missing_message)
  }
  # Raise an error if redundant postcodes identified
  all_postcodes <- base::unlist(
    sapply(postcode_list, '[[', 'pcds'), use.names = FALSE
  )
  unique_postcodes <- base::unique(all_postcodes)
  if (length(unique_postcodes) < length(all_postcodes)) {
    duplicate_message <- base::paste0(
      'The following regex identified duplicate postcodes: "',
      base::paste(definition_list$regex, collapse = '", "'), '"'
    )
    stop(duplicate_message)
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
  stopifnot(length(lat) == 1)
  stopifnot(is.numeric(lat))
  stopifnot(length(long) == 1)
  stopifnot(is.numeric(long))
  stopifnot(length(crs) == 1)
  stopifnot((crs %% 1) == 0)
  stopifnot(sf::st_can_transform(crs, sf::st_crs(postcodes)))
  stopifnot(length(distance) == 1)
  stopifnot(is.numeric(distance))
  stopifnot(distance > 0)
  stopifnot(length(units) == 1)
  stopifnot(units %in% c('m', 'km', 'mi'))
  # Create distance and point
  max_distance <- units::as_units(distance, units)
  point <- sf::st_sfc(
    sf::st_point(c(long, lat))
  ) |>
    sf::st_set_crs(crs) |>
    sf::st_transform(crs = sf::st_crs(postcodes))
  # Create filter, apply and return
  postcode_distance <- sf::st_distance(postcodes, point)[, 1, drop = TRUE]
  filtered_postcodes <- postcodes[
    postcode_distance <= max_distance, , drop = FALSE
  ] |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  return(filtered_postcodes)
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
  stopifnot(length(crs) == 1)
  stopifnot((crs %% 1) == 0)
  stopifnot(sf::st_can_transform(crs, sf::st_crs(postcodes)))
  # Convert coordinates to a character vector
  polygon <- create_polygon_from_str(
    coordinate_str = coordinate_str,
    in_crs = crs,
    out_crs = sf::st_crs(postcodes)
  )
  # Get postcodes
  filtered_postcodes <- sf::st_filter(
    x = postcodes,
    y = polygon
  ) |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  # Check postcodes and return
  if (nrow(filtered_postcodes) == 0) {
    stop('Coordinates did not identify any postcodes')
  }
  return(filtered_postcodes)
}

#' Get ward postcodes
#' 
#' Get postcodes contained within a local/unitary authority
#' 
#' @param postcodes sf object containing postcodes
#' @param wards A character vector of ward codes
#' @return sf object containing postcodes within the selected wards
get_ward_postcodes <- function(
  postcodes, wards
) {
  # Check arguments
  stopifnot('osward' %in% colnames(postcodes))
  stopifnot(class(wards) == 'character')
  stopifnot(length(wards) >= 1)
  stopifnot(all(!duplicated(wards)))
  stopifnot(all(wards %in% postcodes$osward))
  # Get postcodes
  filtered_postcodes <- postcodes[
    postcodes$osward %in% wards, , drop = FALSE
  ] |>
    dplyr::arrange(order_postcodes(pcds, level = 'complete'))
  # Check postcodes and return
  if (any(duplicated(filtered_postcodes$pcds))) {
    stop('Wards identified duplicate postcodes')
  }
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
  if (any(duplicated(filtered_postcodes$pcds))) {
    stop('laua identified duplicate postcodes')
  }
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
  if (any(duplicated(filtered_postcodes$pcds))) {
    stop('counties identified duplicate postcodes')
  }
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
  } else if (definition$type == 'wards') {
    selected_postcodes <- get_ward_postcodes(
      postcodes = postcodes,
      wards = definition$wards
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
  # Check names
  stopifnot('sfc_POINT' %in% base::attributes(sf::st_geometry(postcodes))$class)
  stopifnot('pcds' %in% colnames(postcodes))
  stopifnot(all(!duplicated(postcodes$pcds)))
  stopifnot(!is.null(names(definition_list)))
  stopifnot(all(!duplicated(names(definition_list))))
  # Select postcodes
  selected_postcodes <- purrr::imap(
    definition_list,
    function(definition, area) {
      message(area)
      get_postcodes_from_definition(
        postcodes = postcodes,
        definition = definition
      )
    }
  )
  return(selected_postcodes)
}

# definition_list <- yaml::read_yaml(
#   '~/beauclair/city_data/region_definitions/yaml_definitions/chester_regions.yaml'
# )$customer_areas$definitions
# postcodes <- read_ons_postcodes(
#   '~/beauclair/data/ONSPD/ONSPD_AUG_2023_UK/Data/ONSPD_AUG_2023_UK.csv.gz',
#   rm_terminated = T, rm_nogrid = T
# )
# area_postcodes <- get_postcodes_from_definition_list(
#   postcodes = postcodes, definition_list = definition_list
# )
