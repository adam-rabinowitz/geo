#' Split postcodes by sector
#' 
#' Split postcodes by their postal sector
#' 
#' @param postcodes Vector of postcodes
#' @export
split_postcodes_by_sector <- function(
  postcodes  
) {
  # Create postcode list
  postcode_list <- split(
    postcodes,
    truncate_postcodes(
      postcodes, level='sector', sort = F, unique = F
    )
  )
  return(postcode_list)
}

#' Find sector intersect
#' 
#' Find intersect of postcodes by postal sector
#' 
#' @param query_sector_list List of query postcodes split by postal sector
#' @param ref_postcodes List of reference postcodes split by postal sector
#' @param query_only Only include query postal sectors in output
#' @export
find_sector_list_intersect <- function(
  query_sector_list, ref_sector_list, query_only = TRUE  
) {
  # Find union and intersect
  if (query_only) {
    sectors <- names(query_sector_list) |>
      sort_postcodes(level = 'sector')
  } else {
    sectors <- base::union(
      names(query_sector_list), names(ref_sector_list)
    ) |>
      sort_postcodes(level = 'sector')
  }
  # Find coverage and return
  intersect_tb <- dplyr::tibble(
    'sector' = sectors,
    'n_query' = sapply(query_sector_list[sectors], length),
    'n_ref' = sapply(ref_sector_list[sectors], length),
    'n_intersect' = purrr::map2_int(
      .x = query_sector_list[sectors],
      .y = ref_sector_list[sectors],
      .f = function(x, y) {length(intersect(x, y))}
    )
  )
  return(intersect_tb)
}


#' Find sector list intersect
#' 
#' Find intersect of list of postcodes split by postal sector
#' 
#' @param query_sector_list List of query postcodes split by postal sector
#' @param ref_sector_list List of reference postcodes split by postal sector
#' @param query_only Only include query postal sectors in output
#' @export
find_sector_list_intersect <- function(
    query_sector_list, ref_sector_list, query_only = TRUE  
) {
  # Find union and intersect
  if (query_only) {
    sectors <- names(query_sector_list) |>
      sort_postcodes(level = 'sector')
  } else {
    sectors <- base::union(
      names(query_sector_list), names(ref_sector_list)
    ) |>
      sort_postcodes(level = 'sector')
  }
  # Find coverage and return
  intersect_tb <- dplyr::tibble(
    'sector' = sectors,
    'n_query' = sapply(query_sector_list[sectors], length),
    'n_ref' = sapply(ref_sector_list[sectors], length),
    'n_intersect' = purrr::map2_int(
      .x = query_sector_list[sectors],
      .y = ref_sector_list[sectors],
      .f = function(x, y) {length(intersect(x, y))}
    )
  )
  return(intersect_tb)
}

#' Find sector list intersect
#' 
#' Find intersect of postcodes split by postal sector
#' 
#' @param query_postcodes Vector of unique query postcodes
#' @param ref_sector_list Vector of unique reference postcodes
#' @param query_only Only include query postal sectors in output
#' @export
find_sector_intersect <- function(
  query_postcodes, ref_postcodes, query_only = TRUE  
) {
  # Check postcodes are unique
  stopifnot(!base::any(base::duplicated(query_postcodes)))
  stopifnot(!base::any(base::duplicated(ref_postcodes)))
  # Find overlap and return
  sector_intersect <- find_sector_list_intersect(
    query_sector_list = split_postcodes_by_sector(query_postcodes),
    ref_sector_list = split_postcodes_by_sector(ref_postcodes),
    query_only = query_only
  )
  return(sector_intersect)
}

#' Find group sector coverage
#' 
#' Find coverage of reference postal sectors by group
#' 
#' @param query_postcodes Vector of unique query postcodes
#' @param query_areas Vector of areas to which query postcodes belong
#' @param ref_postcodes Vector of unique reference postcodes
#' @export
find_area_sector_coverage <- function(
  query_postcodes, query_areas, ref_postcodes, unique = TRUE
) {
  # Check arguments
  stopifnot(base::all(query_postcodes %in% ref_postcodes))
  stopifnot(length(query_postcodes) == length(query_areas))
  stopifnot(is.logical(unique))
  stopifnot(length(unique) == 1)
  if (unique) {
    stopifnot(!base::any(base::duplicated(query_postcodes)))
    stopifnot(!base::any(base::duplicated(ref_postcodes)))
  }
  # Split postocdes by group and sector
  query_area_sector_list <- split(
    query_postcodes, query_areas
  ) |>
    lapply(split_postcodes_by_sector)
  ref_sector_list <- split_postcodes_by_sector(ref_postcodes)
  # Extract proportion of reference postal sectors in each query group
  ref_intersect_tbl <- sapply(
    query_area_sector_list,
    function(query_sector_list) {
        find_sector_list_intersect(
          query_sector_list = query_sector_list,
          ref_sector_list = ref_sector_list,
          query_only = FALSE
        ) |>
          dplyr::pull(n_intersect, name = sector)
    },
    simplify = 'matrix'
  ) |>
    dplyr::as_tibble(rownames = 'sector')
  # Add number of postcodes
  ref_intersect_tbl <- tibble::add_column(
    ref_intersect_tbl,
    n_postcodes = sapply(
      ref_sector_list[ref_intersect_tbl$sector], base::length
    ),
    .after = 'sector'
  )
  return(ref_intersect_tbl)
}

#' Assign sector to group
#' 
#' Find coverage of reference postal sectors by group
#' 
#' @param intersect_tbl Intersect values created by find_area_sector_coverage
#' @param cutoff Minimum ratio of overlaps required for assignment
#' @export
assign_sector_to_area <- function(
  intersect_tbl, cutoff = 0.9
) {
  # Check arguments
  stopifnot(cutoff > 0.5)
  stopifnot(cutoff <= 1)
  # Get areas
  areas <- base::setdiff(
    base::colnames(intersect_tbl), c('sector', 'n_postcodes')
  )
  # Get sector sum and maximum
  area_sum <- base::apply(intersect_tbl[,areas], 1, base::sum)
  area_max <- base::apply(intersect_tbl[,areas], 1, base::max)
  max_index <- base::apply(intersect_tbl[,areas], 1, base::which.max) 
  # Find which can be assigned
  intersect_tbl <- tibble::add_column(
    intersect_tbl,
    assignment = ifelse(
      area_sum == 0,
      'unassigned',
      ifelse(
        (area_max / area_sum) >= cutoff,
        areas[max_index],
        'mixed'
      )
    ),
    .after = 'n_postcodes'
  )
  return(intersect_tbl)
}
