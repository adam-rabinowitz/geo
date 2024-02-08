#' postcode regex
#' 
#' Returns complete postcode regular expression
#' 
#' @param level level at which postcode has been truncated
#' @returns postcode regular expression
#' @export
get_postcode_regex <- function(
  level
) {
  # Set elements of regex
  postcode_regex_elements <-c(
    'area' = '([A-Z]{1,2})',
    'district' = '(\\d{1,2}[A-Z]?|(?<=NP)T|(?<=GI)R)',
    'sector' = '\\s*(\\d)',
    'unit' = '([A-Z]{2})'
  )
  # Create postcode regx and return
  if (level == 'area') {
    postcode_regex <- postcode_regex_elements[1]
  } else if (level == 'district') {
    postcode_regex <- paste(postcode_regex_elements[1:2], collapse='')
  } else if (level == 'sector') {
    postcode_regex <- paste(postcode_regex_elements[1:3], collapse='')
  } else if (level == 'complete') {
    postcode_regex <- paste(postcode_regex_elements, collapse='')
  } else {
    stop("level must be one of 'area', 'district', 'sector' or 'complete'")
  }
  # Add start and stop and return
  complete_postcode_regex <- paste0('^', postcode_regex, '$')
  return(complete_postcode_regex)
}

#' Check postcodes
#' 
#' Checks all potcodes matches regular expressions
#'
#' @param postcodes character vector of postcodes
#' @param level level at which postcode has been truncated
#' @export
check_postcodes <- function(
  postcodes, level
) {
  # Check match
  matched <- grepl(
    get_postcode_regex(level = level), postcodes, perl = T
  )
  if (!all(matched)) {
    n_unmatched <- sum(!matched)
    example_unmatched <- head(postcodes[!matched], 5)
    message(paste(n_unmatched, 'postcodes do not match', level, 'regex'))
    message(paste0(
      'examples of unmatched postcodes are: "',
      paste0(example_unmatched, collapse='", "'),
      '"'
    ))
    stop('unmatched postcodes')
  }
  invisible()
}

#' identify postcode level
#'
#' Identify if postcodes are area, district, sector or complete
#' 
#' 
#' @param postcodes Character vector of postcodes
#' @returns level of postcodes
identify_postcode_level <- function(
  postcodes    
) {
  # Identify postcode level of first postcode
  if (grepl(get_postcode_regex('complete'), postcodes[1], perl=T)) {
    level <- 'complete'
  } else if (grepl(get_postcode_regex('sector'), postcodes[1], perl=T)) {
    level <- 'sector'
  } else if (grepl(get_postcode_regex('district'), postcodes[1], perl=T)) {
    level <- 'district'
  } else if (grepl(get_postcode_regex('area'), postcodes[1], perl=T)) {
    level = 'area'
  } else {
    error_message <- paste(
      'unable to idenitfy level of postcode:', postcodes[1]
    )
    stop(error_message)
  }
  # Check all postcodes match level
  check_postcodes(postcodes, level)
  return(level)
}

#' get postcode areas
#' 
#' get postcode areas from character vector of complete postcodes
#' 
#' @param postcodes character vector of complete postcodes
#' @param level level at which postcode has been 
#' @returns postcode areas
get_postcode_areas <- function(
  postcodes
) {
  # Check postcodes
  check_postcodes(postcodes, level='complete')
  # Get areas
  areas <- gsub(
    get_postcode_regex(level='complete'), '\\1', postcodes, perl = TRUE
  )
  return(areas)
}

#' get postcode districts
#' 
#' get postcode districts from character vector of complete postcodes
#' 
#' @param postcodes character vector of complete postcodes
#' @returns postcode districts
get_postcode_districts <- function(
  postcodes
) {
  # Check postcodes
  check_postcodes(postcodes, level='complete')
  # Get areas
  districts <- base::gsub(
    get_postcode_regex(level='complete'), '\\1\\2', postcodes, perl = TRUE
  )
  return(districts)
}

#' get postcode sectors
#' 
#' get postcode sectors from character vector of complete postcodes
#' 
#' @param postcodes character vector of complete postcodes
#' @param seperator separator between outward and inward codes
#' @returns postcode districts
get_postcode_sectors <- function(
  postcodes, separator = ' '
) {
  # Check postcodes
  check_postcodes(postcodes, level='complete')
  # Get areas
  replacement <- paste0('\\1\\2', separator, '\\3')
  sectors <- base::gsub(
    get_postcode_regex(level='complete'), replacement, postcodes, perl = TRUE
  )
  return(sectors)
}

#' order postcodes
#'
#' order postcodes by area, district, sector and unit
#' @param postcodes character vector of postcodes
#' @param level level at which postcode has been truncated
#' @returns integer vector of correct order
#' @export
order_postcodes <- function(
  postcodes, level = NULL
) {
  # Check postcodes
  if (is.null(level)) {
    level <- identify_postcode_level(postcodes)
  } else {
    check_postcodes(postcodes, level=level)
  }
  # Split postcodes into individual elements
  element_matrix <- stringr::str_match(
    postcodes, get_postcode_regex(level=level)
  )
  # Calculate order of elements
  element_rank <- apply(
    X = element_matrix[, 2:ncol(element_matrix)],
    MARGIN = 2, 
    FUN = stringr::str_rank,
    numeric = TRUE,
    simplify = FALSE
  )
  # Order postcodes and return
  postcode_order <- do.call(base::order, element_rank)
  return(postcode_order)
}

#' sort postcodes
#'
#' sort postcodes by area, district, sector and unit
#' @param postcodes character vector of postcodes
#' @param level level at which postcode has been truncated
#' @returns sorted postcodes
#' @export
sort_postcodes <- function(
  postcodes, level = NULL
) {
  # Sort psotcodes and return
  postcode_order <- order_postcodes(postcodes, level = level)
  sorted_postcodes <- postcodes[postcode_order]
  return(sorted_postcodes)
}

#' truncate postcodes
#' 
#' truncate postcodes to area, district or sector
#' 
#' @param postcodes character vector of postcodes
#' @param level one of area, district or sector
#' @returns truncated postcodes
#' @export 
truncate_postcodes <- function(
  postcodes, level, unique, sort
) {
  # Check postcodes
  check_postcodes(postcodes, level='complete')
  # Check arguments
  stopifnot(is.logical(unique))
  stopifnot(is.logical(sort))
  # Truncate postcodes and return
  if (level == 'area') {
    truncated_postcodes <- get_postcode_areas(postcodes)
  } else if (level == 'district') {
    truncated_postcodes <- get_postcode_districts(postcodes)
  } else if (level == 'sector') {
    truncated_postcodes <- get_postcode_sectors(postcodes)
  } else {
    stop("level must be one of 'area', 'district' or 'sector'")
  }
  # Get unique postcodes
  if (unique) {
    truncated_postcodes <- base::unique(truncated_postcodes)
  }
  # Sort postcodes
  if (sort) {
    truncated_postcodes <- sort_postcodes(truncated_postcodes, level = level)
  }
  # Return postcodes
  return(truncated_postcodes)
}

#' format complete postcodes
#' 
#' format complete postcodes with a single space between outward and inward code
#' 
#' @param postcodes character vector of postcodes
#' @param to_upper convert postcodes to uppercase
#' @param replace_malformed replace malformed postcodes with NA instead of raising error
#' @returns formatted postcodes with single space between outward and inward code
#' @export
format_complete_postcodes <- function(
  postcodes, to_upper = FALSE, replace_malformed = FALSE, sort = FALSE
) {
  # Check arguments
  stopifnot(base::is.logical(to_upper))
  stopifnot(base::length(to_upper) == 1)
  stopifnot(base::is.logical(replace_malformed))
  stopifnot(base::length(replace_malformed) == 1)
  # Convert postcodes to uppercase if requested
  if (to_upper) {
    postcodes <- base::toupper(postcodes)
  }
  # Check format of postcodes
  postcode_regex <- get_postcode_regex('complete')
  if (replace_malformed) {
    regular <- base::grepl(postcode_regex, postcodes, perl = TRUE)
  } else {
    check_postcodes(postcodes, level='complete')
    regular <- base::rep(T, base::length(postcodes))
  }
  # Format postcodes and return
  formatted_postcodes <- postcodes
  formatted_postcodes[regular] <- gsub(
    postcode_regex, '\\1\\2 \\3\\4', postcodes[regular], perl = TRUE
  )
  formatted_postcodes[!regular] <- NA
  # Sort if requested
  if (sort) {
    formatted_postcodes = sort_postcodes(
      formatted_postcodes, level = 'complete'
    )
  }
  return(formatted_postcodes)
}
