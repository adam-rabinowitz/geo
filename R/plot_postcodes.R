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
