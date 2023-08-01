#' Read ons postcodes
#'
#' Read ons postcode csv, filter and convert to sf object 
#'
#' @param path Paths to postcode CSV
#' @export
read_ons_postcodes <- function(
  path, rm_terminated, rm_nogrid
) {
  # Read postcodes and filter
  postcodes <- readr::read_csv(
    path, progress=F, col_types=readr::cols()
  )
  # Filter terminated postcodes
  if (rm_terminated) {
    postcodes <- dplyr::filter(postcodes, base::is.na(doterm))
  }
  # Filter postcodes with no grid reference
  if (rm_nogrid) {
    postcodes <- dplyr::filter(postcodes, osgrdind < 9)
  }
  # Convert to sf and return
  postcodes <- sf::st_as_sf(
    postcodes, coords = c('long', 'lat'), remove = FALSE, crs = 4326
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
  geojson, input_crs
) {
  # Read geojson, check geometry
  polygon <- sf::read_sf(geojson, drivers = 'GeoJSON')
  stopifnot(sf::st_geometry_type(polygon) == 'POLYGON')
  stopifnot(length(polygon) == 1)
  # Transform geometry and return
  base::suppressWarnings(sf::st_crs(polygon) <- input_crs)
  stopifnot(sf::st_can_transform(src = input_crs, dst = 4326))
  transformed_polygon <- sf::st_transform(polygon, crs = 4326)
  return(transformed_polygon)
}

#' Filter postcodes regx
#'
#' Filter postcodes using regx
#'
#' @param postcodes Simple feature object containing postcodes
#' @param regx Regx pattern to search for in column
# Filter postcodes by regular expression
filter_postcodes_regx <- function(
  postcodes, regex
) {
  # Check arguments
  stopifnot('pcds' %in% colnames(postcodes))
  stopifnot(is.character(postcodes$pcds))
  stopifnot(is.character(regex))
  stopifnot(length(regex) == 1)
  # Get postcodes and return
  filtered_postcodes <- postcodes[
    grepl(pattern = regex, x = postcodes$pcds),
  ]
  return(filtered_postcodes)
}

#' Filter postcodes distance
#'
#' Filter sf object by distance from point
#'
#' @param postcodes sf object containing postcodes
#' @param point point from which to measure distance
#' @param max_distance unit object listing maximum distance
filter_postcodes_distance <- function(
  postcodes, point, max_distance
) {
  # Check arguments
  stopifnot('sf' %in% class(postcodes))
  stopifnot('sfc_POINT' %in% class(point))
  stopifnot(length(point) == 1)
  stopifnot(class(max_distance) == 'units')
  stopifnot(length(max_distance) == 1)
  stopifnot(sf::st_crs(postcodes)[['input']] == sf::st_crs(point)[['input']])
  stopifnot('osgrdind' %in% colnames(postcodes))
  # Create filter, apply and return
  osgrd_postcodes <- dplyr::filter(postcodes, osgrdind < 9)
  distance <- sf::st_distance(osgrd_postcodes, point)[, 1, drop=TRUE]
  filtered_postcodes <- osgrd_postcodes[distance <= max_distance,]
  return(filtered_postcodes)
}

#' Filter postcodes polygon
#' 
#' Filter postcodes by their location within a polygon
#' 
#' @param postcodes Simple feature object containing postcodes
#' @param polygon Simple feature collection containing single polygon
filter_postcodes_polygon <- function(
  postcodes, polygon    
) {
  # Check arguments
  stopifnot(sf::st_geometry_type(polygon) == 'POLYGON')
  stopifnot(length(polygon) == 1)
  stopifnot(sf::st_crs(postcodes)[['input']] == sf::st_crs(polygon)[['input']])
  # Get postcodes
  filtered_postcodes <- sf::st_filter(x = postcodes, y = polygon)
  return(filtered_postcodes)
}

#' Get yaml regex postcodes
#' 
#' Get postcodes matching a regex entry in yaml file
#' 
#' @param postcodes sf object containing postcodes
#' @param yaml_entry A regex entry from yaml file
#' @return sf object containing postcodes matching the regex
get_yaml_regex_postcodes <- function(
  postcodes, yaml_entry
) {
  # Get postcodes for each regex
  stopifnot(!is.null(yaml_entry$regex))
  postcode_list <- lapply(
    yaml_entry$regex, FUN = filter_postcodes_regx, postcodes = postcodes 
  )
  # Check for non_productive regex
  unproductive_regex <- yaml_entry$regex[
    base::sapply(postcode_list, base::nrow) == 0
  ]
  if (length(unproductive_regex) > 0) {
    base::message(
      base::paste0(
        'The following ',
        yaml_entry$id,
        ' regex were unproductive: "',
        base::paste0(unproductive_regex, collapse = '","'),
        '"'
      )
    )
  }
  # Raise an error if redundant postcodes identified
  all_postcodes <- base::unlist(
    sapply(postcodes_list, '[[', 'pcds'), use.names = FALSE
  )
  unique_postcodes <- base::unique(all_postcodes)
  if (length(unique_postcodes) < length(all_postcodes)) {
    stop(
      base::paste0(
        'The following ',
        yaml_entry$id,
        ' regex identified duplicate postcodes: "',
        base::paste(yaml_entry$regex, collapse = '","'),
        '"'
      )
    )
  }
  # Generate merged and sorted postcodes
  unique_postcodes <- postcode_list |>
    dplyr::bind_rows() |>
    dplyr::arrange(stringr::str_order(pcds, numeric = TRUE))
  return(unique_postcodes)
}

#' Get yaml point postcodes
#' 
#' Get postcodes matching a regex entry in yaml file
#' 
#' @param postcodes sf object containing postcodes
#' @param yaml_entry A point entry from yaml file
#' @return sf object containing postcodes matching the regex
get_yaml_point_postcodes <- function(
  postcodes, yaml_entry
) {
  # Check arguments
  required <- c('lat', 'long', 'distance', 'units')
  stopifnot(all(required %in% names(yaml_entry)))
  stopifnot(all(sapply(yaml_entry[required], length) == 1))
  stopifnot(yaml_entry$units %in% c('m', 'km', 'mi'))
  # Create distance and point
  max_distance = units::as_units(yaml_entry$distance, yaml_entry$units)
  point <- sf::st_sfc(
    sf::st_point(c(yaml_entry$long, yaml_entry$lat))
  )
  sf::st_crs(point) = 4326
  # Get postcodes
  proximal_postcodes <- filter_postcodes_distance(
    postcodes = postcodes,
    point = point,
    max_distance = max_distance
  )
  # Check postcodes and return
  stopifnot(!base::any(base::duplicated(proximal_postcodes$pcds)))
  return(proximal_postcodes)
}

#' Get yaml regex postcodes
#' 
#' Get postcodes matching a regex entry in yaml file
#' 
#' @param postcodes sf object containing postcodes
#' @param yaml_entry A regex entry from yaml file
#' @return sf object containing postcodes matching the regex
get_yaml_regex_postcodes <- function(
  postcodes, yaml_entry
) {
  # Get postcodes for each regex
  stopifnot(!is.null(yaml_entry$regex))
  postcode_list <- lapply(
    yaml_entry$regex, FUN = filter_postcodes_regx, postcodes = postcodes 
  )
  # Check for non_productive regex
  unproductive_regex <- yaml_entry$regex[
    base::sapply(postcode_list, base::nrow) == 0
  ]
  if (length(unproductive_regex) > 0) {
    base::message(
      base::paste0(
        'The following ',
        yaml_entry$id,
        ' regex were unproductive: "',
        base::paste0(unproductive_regex, collapse = '","'),
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
        'The following ',
        yaml_entry$id,
        ' regex identified duplicate postcodes: "',
        base::paste(yaml_entry$regex, collapse = '","'),
        '"'
      )
    )
  }
  # Generate merged and sorted postcodes
  unique_postcodes <- postcode_list |>
    dplyr::bind_rows() |>
    dplyr::arrange(stringr::str_order(pcds, numeric = TRUE))
  return(unique_postcodes)
}

#' Get yaml point postcodes
#' 
#' Get postcodes matching a regex entry in yaml file
#' 
#' @param postcodes sf object containing postcodes
#' @param yaml_entry A point entry from yaml file
#' @return sf object containing postcodes matching the regex
get_yaml_polygon_postcodes <- function(
  postcodes, yaml_entry
) {
  # Check arguments
  required <- c('geojson', 'crs')
  stopifnot(all(required %in% names(yaml_entry)))
  stopifnot(all(sapply(yaml_entry[required], length) == 1))
  # Create polygon
  polygon <- read_geojson_polygon(
    geojson = yaml_entry$geojson,
    input_crs = yaml_entry$crs
  )
  # Get plygon postcodes
  polygon_postcodes <- filter_postcodes_polygon(
    postcodes = postcodes,
    polygon = polygon
  )
  # Check postcodes and return
  stopifnot(!base::any(base::duplicated(polygon_postcodes$pcds)))
  return(polygon_postcodes)
}

#' Identify postcode overlap
#' 
#' Identify postcode overlap in a list of postcodes
#' 
#' @param postcode_list Named list of ONS postcodes for each area
#' @param raise_error Raise error if duplicate postcodes found
identify_postcode_overlap <- function(
  postcode_list, raise_error = FALSE
) {
  # Get all and unique postcodes
  all_postcodes <- base::unlist(
    base::sapply(postcode_list, '[[', 'pcds'), use.names = FALSE
  )
  unique_postcodes <- base::unique(all_postcodes)
  # Message overlaps
  if (length(unique_postcodes) < length(all_postcodes)) {
    # Find overlaps
    utils::combn(
      x = names(postcode_list),
      m = 2,
      FUN = function(names) {
        n_common <- base::length(
          base::intersect(
            postcode_list[[names[1]]]$pcds,
            postcode_list[[names[2]]]$pcds
          )
        )
        if (n_common > 0) {
          base::message(
            base::paste0(
              'Common postcodes: ', names[1], ' & ' ,names[2],  ' = ' ,n_common
            )
          )
          if (raise_error) {
            stop('common postcodes found')
          }
        }
        invisible(NULL)
      },
      simplify = FALSE
    )
  }
  base::invisible(NULL)
}

#' add rest of gb
#' 
#' add rest of gb to postcode list
#' 
#' @param postcodes ONS postcodes
#' @param postcode_list Named list of ONS postcodes for each area
#' @return updated postcode list
add_rest_of_gb <- function(
  postcodes, postcode_list
) {
  # Get current postcodes
  current_postcodes <- base::lapply(
    postcode_list, '[[', 'pcds'
  ) |>
    base::unlist(use.names = FALSE) |>
    base::unique()
  # Add new postcodes and return
  other_postcode_filter <- !(postcodes$pcds %in% current_postcodes)
  postcode_list[['Rest Of GB']] <- postcodes[
    other_postcode_filter, , drop=FALSE
  ]
  return(postcode_list)
}

#' Get yaml list postcodes
#' 
#' Get list of postcodes from yaml file
#' @param postcodes ONS postcodes
#' @param area_yaml Area defintions from yaml file
#' @return returns tibble of postcodes
#' @export
get_yaml_postcode_list <- function(
  postcodes, area_yaml
) {
  # Get list of postcodes
  postcode_list <- base::lapply(
    area_yaml$definitions,
    function(yaml_entry) {
      if (yaml_entry$type == 'regex') {
        unique_postcodes <- get_yaml_regex_postcodes(
          postcodes = postcodes,
          yaml_entry = yaml_entry
        )
      } else if (yaml_entry$type == 'point') {
        unique_postcodes <- get_yaml_point_postcodes(
          postcodes = postcodes,
          yaml_entry = yaml_entry
        )
      } else if (yaml_entry$type == 'polygon') {
        unique_postcodes <- get_yaml_polygon_postcodes(
          postcodes = postcodes,
          yaml_entry = yaml_entry
        )
      } else (
        stop(paste('unknown yaml entry type:', yaml_entry$type))
      )
      return(unique_postcodes)
    }
  )
  # Aggregate postcodes
  for (aggregate in names(area_yaml$aggregates)) {
    # Aggregate data
    stopifnot(!(aggregate %in% base::names(postcode_list)))
    constituents <- area_yaml$aggregates[[aggregate]]$constituents
    stopifnot(base::length(constituents) > 0)
    stopifnot(base::all(constituents %in% base::names(postcode_list)))
    postcode_list[[aggregate]] <- postcode_list[constituents] |>
      dplyr::bind_rows()
    # Check aggregate for unique values
    unique_value <- area_yaml$aggregates[[aggregate]]$unique
    stopifnot(base::is.logical(unique_value))
    stopifnot(base::length(unique_value) == 1)
    if (unique_value) {
      stopifnot(!base::any(base::duplicated(postcode_list[[aggregate]]$pcds)))
    }
    # Remove redundant
    remove_constituents_value <- area_yaml$aggregates[[aggregate]]$remove_constituents
    stopifnot(base::is.logical(remove_constituents_value))
    stopifnot(base::length(remove_constituents_value) == 1)
    if (remove_constituents_value) {
      postcode_list[constituents] <- NULL
    }
  }
  # Add rest of GB if required
  stopifnot(base::is.logical(area_yaml$`Rest Of GB`))
  stopifnot(base::length(area_yaml$`Rest Of GB`) == 1)
  if (area_yaml$`Rest Of GB`) {
    postcode_list <- add_rest_of_gb(
      postcodes = postcodes, postcode_list = postcode_list
    )
  }
  # Check for duplicates and return
  identify_postcode_overlap(postcode_list)
  return(postcode_list)
}

#' Find postcode distance bins
#' 
#' Find distance range of postcodes to reference postcodes
#' 
#' @param postcodes Query postcodes for which distances will be calculated
#' @param ref_postcodes Reference postcodes from which distance will be calculated
#' @param dist_breaks Distance breaks used to generate distance bins
#' @return binned postcode distances
find_postcode_distance_bins <- function(
  postcodes, ref_postcodes, dist_breaks
) {
  # Check postcodes
  stopifnot(all(ref_postcodes$pcds %in% postcodes$pcds))
  # Check distance breaks
  stopifnot(class(dist_breaks) == 'units')
  stopifnot(units::deparse_unit(dist_breaks) == 'm')
  stopifnot(all(diff(dist_breaks) > units::as_units(0, 'm')))
  # Set variables for filling the data
  query_filter <- !(postcodes$pcds %in% ref_postcodes$pcds)
  postcode_distances <- base::ifelse(
    test = query_filter,
    yes = Inf,
    no = 0
  )
  # Find distances
  ref_postcodes_union <- sf::st_union(ref_postcodes)
  for (distance in rev(dist_breaks)) {
    # Find proximal postcodes
    is_proximal <- sf::st_within(
      postcodes[query_filter,],
      sf::st_buffer(ref_postcodes_union, dist = distance),
      sparse = F
    )[,1]
    # Add distances
    postcode_distances[query_filter] <- ifelse(
      test = is_proximal,
      yes = distance,
      no = postcode_distances[query_filter]
    )
    # Adjust filter
    query_filter[query_filter] <- is_proximal
  }
  return(postcode_distances)
}

#' Create plot data
#' 
#' Create data for plotting in area app
#' 
#' @param postcodes All ONS postcodes
#' @param postcode_list Named list of ONS postcodes for each area
#' @param area_yaml Portion of YAML file used to generate postcode list
#' @param dist_breaks Breaks for calculating distance. Must have 'm' units
#' @return Returns plot data in format for postcode app
create_plot_data <- function(
  postcodes, postcode_list, area_yaml, dist_breaks
) {
  # Remove postcodes with osgrid and remove rest of GB
  postcodes <- dplyr::filter(postcodes, osgrdind < 9)
  postcode_list <- lapply(
    postcode_list,
    function(area_postcodes) {
      dplyr::filter(area_postcodes, osgrdind < 9)
    }
  )
  postcode_list[['Rest Of GB']] <- NULL
  # Remove redundant postcodes
  if (!identical(area_yaml$aggregates, 'none')) {
    for (aggregate in area_yaml$aggregates) {
      postcode_list[aggregate$constituents] <- NULL
    }
  }
  # Check for overlaps
  identify_postcode_overlap(
    postcode_list = postcode_list, raise_error = TRUE
  )
  # Get matrix of postcodes distance
  area_distances <- sapply(
    postcode_list,
    find_postcode_distance_bins,
    postcodes = postcodes,
    dist_breaks = dist_breaks
  )
  # Create filter for distances
  distance_filter <- base::rowSums(
    base::is.finite(area_distances)
  ) > 0
  # Create plot data
  plot_data <- dplyr::bind_cols(
    postcodes[distance_filter, , drop = FALSE] |>
      dplyr::as_tibble() |>
      dplyr::select(pcds, lat, long),
    area_distances[distance_filter, , drop = FALSE] |>
      dplyr::as_tibble()
  )
  # Add rest of GB and return
  plot_data[['Rest Of GB']] <- base::ifelse(
    test = rowSums(area_distances[distance_filter, , drop = T] == 0) == 0,
    yes = 0,
    no = Inf
  )
  return(plot_data)
}

#' Generate postcode output
#' 
#' Function generates postcode tables and postcode plot data
#' 
#' @param postcodes ONS postcodes
#' @param yaml_path Path to YAML defining areas
#' @param dist_breaks Breaks for calculating distance. Must have 'm' units
#' @returns A list containing postcode tables and postcode plot data
#' @export
generate_postcode_outputs <- function(
  postcodes, yaml_path, dist_breaks
) {
  # Read yaml and get postcodes
  yaml <- yaml::read_yaml(yaml_path)
  message('Getting retail postcodes')
  retail_postcodes <- get_yaml_postcode_list(
    postcodes = postcodes, area_yaml = yaml$retail_areas
  )
  message('Getting customer postcodes')
  customer_postcodes <- get_yaml_postcode_list(
    postcodes = postcodes, area_yaml = yaml$customer_areas
  )
  # Merge data as tb
  postcode_tbl <- list(
    'retail' = retail_postcodes |>
      dplyr::bind_rows(.id = 'area') |>
      dplyr::as_tibble() |>
      dplyr::select(area, pcds),
    'customer' = customer_postcodes |>
      dplyr::bind_rows(.id = 'area') |>
      dplyr::as_tibble() |>
      dplyr::select(area, pcds)
  ) |>
    dplyr::bind_rows(.id = 'area_type')
  # Create plot data
  plot_data <- list()
  message('Getting proximal retail postcodes')
  plot_data[['retail']] <-create_plot_data(
    postcodes = postcodes,
    postcode_list = retail_postcodes,
    area_yaml = yaml$retail_areas,
    dist_breaks = dist_breaks
  )
  message('Getting proximal customer postcodes')
  plot_data[['customer']] <- create_plot_data(
    postcodes = postcodes,
    postcode_list = customer_postcodes,
    area_yaml = yaml$customer_areas,
    dist_breaks = dist_breaks
  )
  # Create output and return
  output <- list(
    table = postcode_tbl,
    plot_data = plot_data
  )
  return(output)
}

#' Get post towns
#' 
#' Extract post toens from list of postcodes
#' 
#' @param postcodes A character vector of postcodes
#' @param unique_only Return only unique post towns
#' @param sort Sort post towns
#' @return Post towns
get_post_towns <- function(postcodes, unique_only=T, sort=T) {
  # Create regx
  postcode_regex <- '^([[:alnum:]]{2,4}) *?[[:alnum:]]{3}$'
  post_town <- gsub(postcode_regex, '\\1', postcodes)
  # Check regx has worked
  removed_char <- nchar(postcodes) - nchar(post_town)
  stopifnot(min(removed_char) >= 3)
  # Process post towns and return
  if (unique_only) {
    post_town <- unique(post_town)
  }
  if (sort) {
    post_town <- stringr::str_sort(post_town, numeric = TRUE)
  }
  return(post_town)
}
