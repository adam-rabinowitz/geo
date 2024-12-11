#' Check postcode intersect
#' 
#' Check similarity between two postcode tables
#' 
#' @param postcodes1 Table containing 1st set of postcodes
#' @param postcodes2 Table containing 2nd set of postcodes
#' @param id_cols Grouping columns within postcode tables
#' @param postcode_col Column containing postcodes
#' @returns Table containing postcode intersect by grouping column
#' @export
check_postcode_intersect <- function(
  postcodes1, postcodes2, id_cols, postcode_col = 'postcode'   
) {
  # Check columns
  all_cols <- c(id_cols, postcode_col)
  stopifnot(all(all_cols %in% colnames(postcodes1)))
  #stopifnot(all(!duplicated(postcodes1[, all_cols]))) # Uncomment in future
  stopifnot(all(all_cols %in% colnames(postcodes2)))
  #stopifnot(all(!duplicated(postcodes1[, all_cols]))) # Uncomment in future
  # Merge data, find intersect and return
  postcode_intersect <- dplyr::full_join(
    postcodes1 |>
      dplyr::select(dplyr::all_of(all_cols)) |>
      dplyr::distinct() |> # Remove this when updating
      dplyr::mutate(present1 = TRUE),
    postcodes2 |>
      dplyr::select(dplyr::all_of(all_cols)) |>
      dplyr::distinct() |> # Remove this when updating
      dplyr::mutate(present2 = TRUE),
    by = all_cols
  ) |>
    # Replace NA with false
    dplyr::mutate(
      dplyr::across(
        c(present1, present2),
        function(z) {ifelse(is.na(z), FALSE, z)}
      )
    ) |>
    # Count number of postcodes and intersect
    dplyr::summarise(
      n1 = sum(present1),
      n2 = sum(present2),
      intersect = sum(present1 & present2),
      .by = dplyr::all_of(id_cols)
    ) |>
    # Determine identical
    dplyr::mutate(
      identical = (n1 == n2) & (n1 == intersect)
    )
  return(postcode_intersect)
}
