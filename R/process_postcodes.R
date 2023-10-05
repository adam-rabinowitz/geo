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

#' aggregate postcodes
#' 
#' merge postcodes
#' 
#' @param postcodes_x Query ONS postcodes
#' @param postcodes_y Subject ONS postcodes
#' @return postcodes present in query and not in subject
concatenate_postcodes <- function(
  postcode_list, queries
) {
  # Check operation yaml
  stopifnot(all(queries %in% names(postcode_list)))
  stopifnot(length(queries) >= 2)
  # Check query and subject contain unique postcodes
  postcodes <- dplyr::bind_rows(postcode_list[queries])
  return(postcodes)
}

#' setdiff postcodes
#' 
#' find postcodes only present in query and not in reference
#' 
#' @param postcodes_list Named list of ONS postcodes
#' @param query Name of query postcodes
#' @param subject Name of subject postcodes
#' @return ONS postcodes present in query but absent in subject
setdiff_postcodes <- function(
  postcode_list, query, subject
) {
  # Check arguments
  stopifnot(base::length(query) == 1)
  stopifnot(query %in% base::names(postcode_list))
  stopifnot(!base::any(base::duplicated(postcode_list[[query]]$pcds)))
  stopifnot(base::length(subject) == 1)
  stopifnot(subject %in% base::names(postcode_list))
  stopifnot(!base::any(base::duplicated(postcode_list[[subject]]$pcds)))
  # Check query and subject contain unique postcodes
  query_filter <- !(
    postcode_list[[query]]$pcds %in% postcode_list[[subject]]$pcds
  )
  postcodes <- postcode_list[[query]][query_filter, , drop = FALSE]
  return(postcodes)
}

#' perform postcode operations
#' 
#' Function to perform all operations on postcodes
#' 
perform_postcode_operations <- function(
  postcode_list, operations  
) {
  # Update postcode list and return
  for (area in names(operations)) {
    # Get operation
    operation <- operations[[area]]
    stopifnot('operation' %in% names(operation))
    # Perform operation
    if (operation$operation == 'concatenate') {
      postcodes = concatenate_postcodes(
        postcode_list = postcode_list,
        queries = operation$queries
      )
    } else if (operation$operation == 'setdiff') {
      postcodes = setdiff_postcodes(
        postcode_list = postcode_list,
        query = operation$query,
        subject = operation$subject
      )
    } else {
      stop('Uknown operation')
    }
    # Add postcodes to postcode list
    stopifnot(!(area %in% names(postcode_list)))
    postcode_list[[area]] <- postcodes
  }
  return(postcode_list)
}

#' add rest of gb
#' 
#' add rest of gb to postcode list
#' 
#' @param postcode_list Named list of ONS postcodes for each area
#' @param postcodes ONS postcodes from which to extract Rest Of GB 
#' @return updated postcode list
add_rest_of_gb <- function(
  postcode_list, postcodes
) {
  # Check postcode list
  stopifnot(!('Rest Of GB' %in% names(postcode_list)))
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
get_all_postcodes <- function(
  postcodes, area_list
) {
  # Get list of postcodes
  initial_postcodes <- get_postcodes_from_definitions(
    postcodes = postcodes,
    definition_list = area_list$definitions
  )
  # Perform operations
  operated_postcodes <- perform_postcode_operations(
    postcode_list = initial_postcodes,
    operations = area_list$operations
  )
  # Filter postcodes
  stopifnot(!('Rest Of GB' %in% names(operated_postcodes)))
  areas <- setdiff(area_list$complete, 'Rest Of GB')
  final_postcodes <- operated_postcodes[areas]
  # Add Rest of GB
  if ('Rest Of GB' %in% area_list$complete) {
    final_postcodes <- add_rest_of_gb(
      postcode_list = final_postcodes, postcodes = postcodes
    )
  }
  return(final_postcodes)
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
  postcode_path, yaml_path, rm_terminated, rm_nogrid
) {
  # Read yaml and get postcodes
  message('reading postcodes')
  postcodes <- read_ons_postcodes(
    path = postcode_path,
    rm_terminated = rm_terminated,
    rm_nogrid = rm_nogrid
  )
  message('getting retail postcodes')
  yaml <- yaml::read_yaml(yaml_path)
  retail_postcodes <- get_all_postcodes(
    postcodes = postcodes, area_list = yaml$retail_areas
  )
  message('getting customer postcodes')
  customer_postcodes <- get_all_postcodes(
    postcodes = postcodes, area_list = yaml$customer_areas
  )
  # Generate postcode table and return
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
  return(postcode_tbl)
}

# postcode_path <- '~/beauclair/data/ONSPD/ONSPD_MAY_2023_UK/Data/ONSPD_MAY_2023_UK.csv.gz'
# yaml_path <- '~/beauclair/city_data/Glasgow/glasgow_regions.yaml'
# postcode_tbl <- generate_postcode_outputs(
#   postcode_path = postcode_path,
#   yaml_path = yaml_path,
#   rm_terminated = T,
#   rm_nogrid = T
# )
# dplyr::summarise(
#   postcode_tbl,
#   n = dplyr::n(),
#   .by = c(area_type, area)
# )
