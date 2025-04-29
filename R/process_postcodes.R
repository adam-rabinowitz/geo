#' Identify postcode overlap
#' 
#' Identify postcode overlap in a list of postcodes
#' 
#' @param postcode_list Named list of ONS postcodes for each area
#' @param raise_error Raise error if duplicate postcodes found
identify_postcode_overlap <- function(
  postcode_list, raise_error = FALSE
) {
  # Process lists containing multiple entries
  if (length(postcode_list) > 1) {
    # Get all and unique postcodes
    all_postcodes <- base::unlist(postcode_list, use.names = FALSE)
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
              postcode_list[[names[1]]], postcode_list[[names[2]]]
            )
          )
          if (n_common > 0) {
            base::message(
              base::paste0(
                'common postcodes: ', names[1], ' & ' ,names[2],  ' = ',
                n_common
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
  }
  base::invisible(NULL)
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
    # Check area
    message(area)
    stopifnot(!(area %in% names(postcode_list)))
    # Get and check operation
    operation <- operations[[area]]
    stopifnot('operation' %in% names(operation))
    # Perform operations
    if (operation$operation == 'concatenate') {
      # Concatenate existing postcodes
      stopifnot(length(operation$queries) >= 2)
      stopifnot(all(operation$queries %in% names(postcode_list)))
      postcode_list[[area]] <- unlist(
        postcode_list[operation$queries], use.names = FALSE
      )
    } else if (operation$operation == 'setdiff') {
      # Perform setdiff operation on postcodes
      stopifnot(length(operation$query) == 1)
      stopifnot(operation$query %in% names(postcode_list))
      stopifnot(length(operation$subject) == 1)
      stopifnot(operation$subject %in% names(postcode_list))
      postcode_list[[area]] = base::setdiff(
        x = postcode_list[[operation$query]],
        y = postcode_list[[operation$subject]]
      )
    } else {
      stop('Unknown operation')
    }
  }
  return(postcode_list)
}

#' add rest of gb
#' 
#' add rest of gb to postcode list
#' 
#' @param postcode_list Named list of ONS postcodes for each area
#' @param all_postcodes Character vector of all postcodes
#' @param level level at which postcode has been truncated
#' @param label label for Rest Of GB
#' @return updated postcode list
add_rest_of_gb <- function(
  postcode_list, all_postcodes, level, label = 'Rest Of GB'
) {
  # Check postcode list
  if(label %in% names(postcode_list)) {
    message <- paste0(
      'Label (', label, ') already exists in postcodes'
    )
    stop(message)
  }
  # Get current postcodes
  current_postcodes <- unlist(postcode_list)
  check_postcodes(current_postcodes, level = level)
  # Truncate all postocdes if requested
  if (level != 'complete') {
    all_postcodes <- all_postcodes |>
      truncate_postcodes(
        level = level,
        unique = TRUE,
        sort = FALSE
      )
  }
  # Add missing postcodes
  postcode_list[[label]] <- base::setdiff(
    all_postcodes, current_postcodes
  )
  return(postcode_list)
}

#' Generate postcode list
#' 
#' Generate list of postcodes from supplied definitions and operations
#' 
#' @param postcodes ONS postcodes
#' @param definitions A list of definitions
#' @param operations A list of operations
#' @param areas A character vector of areas
#' @param level Postcode level
#' @returns A list of postcodes
generate_postcode_list <- function(
  postcodes, definitions, operations, areas, level  
) {
  # Check arguments
  stopifnot('sf' %in% class(postcodes))
  stopifnot('pcds' %in% colnames(postcodes))
  stopifnot(is.list(definitions))
  if (!is.null(operations)) {
    stopifnot(is.list(operations))
  }
  stopifnot(all(areas %in% c(names(definitions), names(operations))))
  stopifnot(level %in% c('area', 'district', 'sector', 'complete'))
  # Get list of initial postcodes
  initial_postcodes <- get_postcodes_from_definition_list(
    postcodes = postcodes,
    definition_list = definitions
  ) |>
    lapply('[[', 'pcds')
  # Perform truncation if requested
  if (level != 'complete') {
    initial_postcodes <- lapply(
      initial_postcodes,
      truncate_postcodes,
      level = level,
      unique = T,
      sort = T
    )
  }
  # Perform operations
  operated_postcodes <- perform_postcode_operations(
    postcode_list = initial_postcodes,
    operations = operations
  )
  # Get complete list of postcodes
  output_areas <- setdiff(areas, 'Rest Of GB')
  stopifnot(all(output_areas %in% names(operated_postcodes)))
  selected_postcodes <- operated_postcodes[output_areas]
  # Add rest of GB, if requested, and return data
  if ('Rest Of GB' %in% areas) {
    selected_postcodes <- add_rest_of_gb(
      postcode_list = selected_postcodes,
      all_postcodes = postcodes$pcds,
      level = level,
      label = 'Rest Of GB'
    )
  }
  return(selected_postcodes)
}

#' Get postcode polygons
#' 
#' Get plot data for postcodes from a list of areas
#' @param postcode_list A list of postcodes
#' @param voronoi A simple feature collection containing postcode voronoi
#' @param crs Coordinate refrence system for postcodes
#' @returns A simple feature collection containing polygons
get_postcode_polygons <- function(
  postcode_list, level, voronoi
) {
  # Get polygons for complete postcodes
  if (level == 'complete') {
    polygon_list <- lapply(
      postcode_list,
      function(z) {
        voronoi$geometry[
          match(z, voronoi$pcds)
        ] |>
          sf::st_union()
      }
    )
    # Or get polygons for truncated postcodes
  } else if (level == 'sector') {
    voronoi_list <- split(
      voronoi$geometry,
      truncate_postcodes(
        voronoi$pcds,
        level = 'sector',
        sort = F,
        unique = F
      )
    )
    polygon_list <- lapply(
      postcode_list,
      function(z) {
        voronoi_list[z] |>
          do.call(what = c) |>
          sf::st_union()
      }
    )
  } else {
    stop('unexpected postcode level')
  }
  # Merge polygons and retrun
  area_polygons <- sf::st_sf(
    area = factor(
      names(polygon_list),
      levels = names(polygon_list)
    ),
    geometry = do.call(c, polygon_list)
  ) |>
    dplyr::arrange(area)
  return(area_polygons)
}

#' Generate postcode data
#' 
#' Function generates postcode tables and postcode plot data
#' 
#' @param yaml_path Path to YAML defining areas
#' @returns A list containing postcode tables and postcode plot data
#' @export
generate_postcode_data <- function(
  yaml_path
) {
  # Read yaml
  yaml <- yaml::read_yaml(yaml_path)
  # Read postcodes
  message(paste0('\n', yaml$client))
  message('reading postcodes')
  postcode_points <- readRDS(yaml$postcodes[['point path']])
  postcode_voronoi <- readRDS(yaml$postcodes[['voronoi path']])
  if (!identical(postcode_points$pcds, postcode_voronoi$pcds)) {
    stop('postcode point and voronoi files do not match')
  }
  # Get retail postcodes
  areas <- c('retail areas', 'customer areas')
  stopifnot(all(areas %in% names(yaml)))
  output_list <- lapply(
    areas,
    function(area) {
      message(paste('generating', area))
      # Get postcodes
      postcode_list <- generate_postcode_list(
        postcodes = postcode_points,
        definitions = yaml[[area]][['definitions']],
        operations = yaml[[area]][['operations']],
        areas = yaml[[area]][['selected']],
        level = yaml[[area]][['postcode level']]
      )
      # Identify overlap
      identify_postcode_overlap(postcode_list, raise_error = FALSE)
      # Generate postcode table
      area_postcodes <- tibble::tibble(
        area = base::rep(
          names(postcode_list), sapply(postcode_list, length)
        ),
        postcode = base::unlist(postcode_list)
      )
      # Generate output
      area_voronoi <- get_postcode_polygons(
        postcode_list = postcode_list,
        level = yaml[[area]][['postcode level']],
        voronoi = postcode_voronoi
      )
      # Check output and return
      stopifnot(
        identical(unique(area_postcodes$area), levels(area_voronoi$area))
      )
      area_output <- list(
        postcodes = area_postcodes, voronoi = area_voronoi
      )
      return(area_output)
    }
  )
  names(output_list) <- gsub(' ', '_', areas)
  return(output_list)
}



#' #' Generate polygon plot data
#' #' 
#' #' Function generates postcode tables and postcode plot data
#' #' 
#' #' @param retail_postcodes Table containg retail postcodes
#' #' @param customer_postcodes Table containing customer postcodes
#' #' @param voronoi Simple feature collection containing postcode voronoi
#' #' @param crs Coordinate reference system for output polygons
#' #' @returns A list containing polygons of customer and retail areas
#' #' @export
#' generate_polygon_plot_data <- function(
#'   retail_postcodes, customer_postcodes, voronoi, crs
#' ) {
#'   # Check data
#'   stopifnot(identical(
#'     colnames(retail_postcodes),
#'     c('project', 'area', 'postcode')
#'   ))
#'   stopifnot(identical(
#'     colnames(customer_postcodes),
#'     c('project', 'area', 'postcode')
#'   ))
#'   stopifnot(length(unique(retail_postcodes$project)) == 1)
#'   stopifnot(length(unique(customer_postcodes$project)) == 1)
#'   stopifnot(retail_postcodes$project[1] == customer_postcodes$project[1])
#'   # Generate postcode lists and voronoi
#'   polygons <- list(
#'     retail = split(
#'       retail_postcodes$postcode,
#'       factor(
#'         retail_postcodes$area,
#'         base::unique(retail_postcodes$area)
#'       )
#'     ) |>
#'       get_postcode_polygons(
#'         voronoi = voronoi, crs = crs
#'       ),
#'     customer = customer_list <- split(
#'       customer_postcodes$postcode,
#'       factor(
#'         customer_postcodes$area,
#'         base::unique(customer_postcodes$area)
#'       )
#'     ) |>
#'       get_postcode_polygons(
#'         voronoi = voronoi, crs = crs
#'       )
#'   )
#'   return(polygons)
#' }

# # Create glasgow plot data
# manchester_plot_data <- generate_polygon_plot_data(
#   '~/beauclair/city_data/region_definitions/yaml_definitions/manchester_regions.yaml',
#   '~/beauclair/data/ONSPD/ONSPD_AUG_2023_UK/postcode_voronoi/postcode_voronoi.rds',
#   crs = 4326
# )
# saveRDS(manchester_plot_data, '~/beauclair/city_data/region_definitions/manchester_plot_data.rds')

# yaml_path <- '~/beauclair/city_data/Manchester/manchester_regions.yaml'
# postcode_tbl <- generate_postcode_outputs(
#   yaml_path = yaml_path
# )
# dplyr::summarise(
#   postcode_tbl,
#   n = dplyr::n(),
#   .by = c(area_type, area)
# )
