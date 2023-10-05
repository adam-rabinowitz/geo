#' postcode regex
#' 
#' Returns postcode regeluar expression
#' 
#' @returns postcode regular expression
#' @export
postcode_regex <- function() {
  # Create postcode regx and return
  postcode_regex <- '^([A-Z]{1,2})([0-9]{1,2}[A-Z]{0,1})\\s*([0-9]{1})([A-Z]{2})$'
  return(postcode_regex)
}

#' Check postcode regx
#' 
#' Checks all psotcodes match regular expressions
#'
#' @param postcodes character vector of postcodes
check_postcode_regex <- function(
  postcodes
) {
  # Check match
  matched <- grepl(postcode_regex(), postcodes)
  if (!all(matched)) {
    n_unmatched <- sum(!matched)
    example_unmatched <- head(postcodes[!matched], 5)
    message(paste(n_unmatched, 'postcodes do not match regex'))
    message(paste0(
      'examples of unmatched postcodes are: "',
      paste0(example_unmatched, collapse='", "'),
      '"'
    ))
    stop('unmatched postcodes')
  }
  invisible()
}

#' get postcode areas
#' 
#' get postcode areas from character vector of postcodes
#' 
#' @param postcodes character vector of postcodes
#' @returns postcode areas
#' @export
get_postcode_areas <- function(
  postcodes
) {
  # Check postcodes
  check_postcode_regex(postcodes)
  # Get areas
  areas <- gsub(postcode_regex(), '\\1', postcodes)
  return(areas)
}

#' get postcode districts
#' 
#' get postcode districts from character vector of postcodes
#' 
#' @param postcodes character vector of postcodes
#' @returns postcode districts
#' @export
get_postcode_districts <- function(
  postcodes
) {
  # Check postcodes
  check_postcode_regex(postcodes)
  # Get areas
  districts <- base::gsub(postcode_regex(), '\\1\\2', postcodes)
  return(districts)
}

#' get postcode sectors
#' 
#' get postcode sectors from character vector of postcodes
#' 
#' @param postcodes character vector of postcodes
#' @param seperator seperator between outward and inward codes
#' @returns postcode districts
#' @export
get_postcode_sectors <- function(
  postcodes, separator = ' '
) {
  # Check postcodes
  check_postcode_regex(postcodes)
  # Get areas
  replacement <- paste0('\\1', separator, '\\2')
  sectors <- base::gsub(postcode_regex(), replacement, postcodes)
  return(sectors)
}
