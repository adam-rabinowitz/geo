#' Calculate center and zoom
#' 
#' Calculate center and zoom for a mapbox plot
#' 
#' @param lat Latitudes
#' @param long Longitiudes
#' @return List containing center of plot and suggested zoom level
calc_center_zoom <- function(lat, long) {
  # Calculate center
  center_x <- base::mean(base::range(long))
  center_y <- base::mean(base::range(lat))
  center <- list(lon = center_x, lat = center_y)
  # Calculate zoom
  width_x <- base::diff(base::range(long))
  width_y <- base::diff(base::range(lat))
  zoom_x <- -1.415 * base::log(width_x) + 8.7068
  zoom_y <- -1.446 * base::log(width_y) + 7.2753
  zoom <- min(c(base::round(zoom_y, 2), base::round(zoom_x, 2)))
  # Create output and return
  center_zoom <- list(
    center = center,
    zoom = zoom
  )
  return(center_zoom)
}

#' Postcode plot data from list
#' 
#' Generate postcode plot data from a list of postcodes
#' 
#' @param postcode_list List of postcodes to plot
#' @param postcodes Table of current postcodes
#' @param postcode_col Column from postcode table to use for matching
#' @param postcode_list List of postcodes
#' @param warn_missing Warn if missing postcodes are supplied
#' @return A table for plotting postcodes
postcode_plot_data_from_list <- function(
  postcodes, postcode_col, postcode_list, warn_missing = TRUE
) {
  # Check missing
  if (warn_missing) {
    all_postcodes <- as.character(unlist(postcode_list))
    missing_postcodes <- setdiff(all_postcodes, postcodes[[postcode_col]])
    if (length(missing_postcodes) > 0) {
      warning_message <- paste0(
        length(all_postcodes), ' postcodes supplied & ',
        length(missing_postcodes), ' postcodes missing'
      )
      warning(warning_message)
    }
  }
  # Get postcodes
  plot_data <- lapply(
    postcode_list,
    function(p) {
      p_indices <- base::match(p, postcodes[[postcode_col]], nomatch=NULL)
      postcodes[p_indices[!is.na(p_indices)],]
    }
  ) |>
    dplyr::bind_rows(.id = 'group') |>
    dplyr::rename(text = dplyr::all_of(postcode_col)) |>
    dplyr::as_tibble() |>
    dplyr::select(lat, long, text, group)
  return(plot_data)
}

#' Plot postcodes
#' 
#' Calculate cnter and zoom for a mapbox plot
#' 
#' @param tb Table containing plot data
#' @return Return plot
# Requires filtering
plot_postcodes <- function(
  tb, center_zoom = NULL
) {
  # Check input
  base::stopifnot(
    base::all(c('lat', 'long', 'text', 'group') %in% base::colnames(tb))
  )
  # Calculate zoom
  if (is.null(center_zoom)) {
    center_zoom <- calc_center_zoom(lat = tb$lat, long = tb$long)
  }
  # Create plot and return
  plot <- plotly::plot_ly(
    data = tb,
    lat = ~lat,
    lon = ~long,
    color = ~group,
    hovertext = ~text,
    hoverinfo = 'text',
    type = 'scattermapbox',
    mode = 'markers'
  ) |>
    plotly::layout(
      mapbox = list(
        style = 'carto-positron',
        center = center_zoom$center,
        zoom = center_zoom$zoom
      )
    )
  return(plot)
}