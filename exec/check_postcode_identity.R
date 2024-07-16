check_postcode_identity <- function(
  path1, path2    
) {
  # Read dara
  data1 <- readr::read_csv(path1, progress = F, show_col_types = F)
  data2 <- readr::read_csv(path2, progress = F, show_col_types = F)
  # Check column names
  stopifnot(identical(colnames(data1), colnames(data2)))
  stopifnot(ncol(data1) == 3)
  stopifnot('postcode' %in% colnames(data1))
  stopifnot('project' %in% colnames(data1))
  # Check project identiity
  stopifnot(length(unique(data1$project)) == 1)
  stopifnot(length(unique(data2$project)) == 1)
  stopifnot(data1$project[1] == data2$project[1])
  # Split postcode by id
  id_col <- setdiff(colnames(data1), c('project', 'postcode'))
  list1 <- split(data1$postcode, data1[[id_col]])
  list2 <- split(data2$postcode, data2[[id_col]])
  # Check identity
  stopifnot(identical(names(list1), names(list2)))
  for (id in names(list1)) {
    stopifnot(setequal(list1[[id]], list2[[id]]))
  }
  return(TRUE)
}

path1 <- '~/beauclair/city_data/region_definitions/postcode_data/sunderland_retail_postcodes.csv'
path2 <- '~/beauclair/monthly_data/formatted_input/2024_02/postcodes/sunderland_retail_postcodes.csv'
check_identity(path1, path2)

