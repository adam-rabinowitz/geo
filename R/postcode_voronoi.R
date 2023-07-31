# # Get polygon points
# get_polygon_voronoi <- function(points, polygon) {
#   # Get voronoi
#   union_points <- sf::st_union(points)
#   voronoi <- sf::st_voronoi(
#     x = union_points,
#     envelope = polygon
#   ) |>
#     sf::st_collection_extract(type = 'POLYGON')
#   # Trim voronoi to polygon
#   polygon_voronoi <- sf::st_intersection(
#     x = voronoi,
#     y = polygon
#   )
#   # Reorder voronoi 
#   order <- sf::st_within(points, polygon_voronoi)
#   stopifnot(length(order) == nrow(points))
#   stopifnot(all(sapply(order, length) == 1))
#   stopifnot(!any(duplicated(unlist(order, use.names=F))))
#   ordered_polygon_voronoi <- polygon_voronoi[unlist(order, use.names=F)]
#   # Replace point geometry with voronoi and return
#   points$geometry <- ordered_polygon_voronoi
#   return(points)
# }
# 
# # Read in UK polygons
# sf::sf_use_s2(FALSE)
# uk_polygons <- sf::st_read(
#   '~/Downloads/Countries_December_2022_GB_BFC_-8802398211591794926/CTRY_DEC_2022_GB_BFC.shp'
# ) |>
#   sf::st_transform(crs = 4326) |>
#   sf::st_union() |>
#   sf::st_make_valid() |>
#   sf::st_cast('POLYGON')
# # Get postcodes for each polygon
# postcodes <- readRDS('manchester_app_data.rds') |>
#   dplyr::select(
#     pcds, lat, long
#   ) |>
#   dplyr::summarise(
#     pcds = paste(pcds, collapse=','),
#     .by = c(lat, long)
#   ) |>
#   sf::st_as_sf(
#     coords = c("long","lat"), remove = FALSE, crs = 4326
#   )
# postcode_uk_polygons <- sf::st_within(
#   postcodes, uk_polygons
# )
# 
# # Remove nothern ireland
# #stopifnot(all(sapply(postcode_uk_polygons, length) == 1))
# postcodes_filtered <- postcodes[sapply(postcode_uk_polygons, length) > 0,]
# postcode_list <- split(
#   postcodes_filtered, base::unlist(postcode_uk_polygons, use.names = F)
# )
# postcode_voronoi <- mapply(
#   get_polygon_voronoi,
#   points = postcode_list,
#   polygon = uk_polyg
# )
# # Plot datax <- 
# x <- get_polygon_voronoi(points = postcode_list[['23']], polygon = uk_polygons[23])
# 
# 
# 
# 
# wales_boundary <- uk_boundary[3,]
# plot(wales_boundary['CTRY22NM'])
# wales_postcodes <- readr::read_csv(
#   '~/beauclair/data/ONSPD/ONSPD_MAY_2023_UK/Data/ONSPD_MAY_2023_UK.csv.gz',
#   progress=F, col_types=readr::cols()
# ) |>
#   dplyr::filter(
#     base::is.na(doterm),
#     osgrdind < 9,
#     ctry == 'W92000004'
#   ) |>
#   sf::st_as_sf(
#     coords = c("long","lat"), remove = FALSE, crs = 4326
#   )
# sf::sf_use_s2(FALSE)
# x <- sf::st_cast(wales_boundary, 'POLYGON')
# x$AREA <- sf::st_area(x)
# smallest <- dplyr::arrange(x, AREA)[153,]
# smallest_postcodes <- sf::st_filter(
#   wales_postcodes, smallest
# )
# smallest_voronoi <- sf::st_voronoi(
#   x = sf::st_union(smallest), envelope = smallest$geometry
# ) |>
#   sf::st_cast('POLYGON')
# plot(anglesey_voronoi)
# sf::st_cast(anglesey_voronoi[1,], 'POINTS')
# 
# sf::st_coordinates(anglesey_voronoi[1,])
# 
# 
# sf::sf_use_s2(FALSE)
# 
# get_voronoi <- function(points, boundary) {
#   # Get union of points and boundary box
#   union_points <- sf::st_union(points$geometry)
#   bbox <- bbox_polygon(boundary)
#   # Get voronoi
#   voronoi <- sf::st_voronoi(union_points, boundary)
# }
# 
# 
# 
# 
# 
# 
# 
# library(sf)
# 
# # function to get polygon from boundary box
# 
# 
# 
# 
# 
# 
# bbox_polygon <- function(x) {
#   bb <- sf::st_bbox(x)
#   p <- matrix(
#     c(bb["xmin"], bb["ymin"], 
#       bb["xmin"], bb["ymax"],
#       bb["xmax"], bb["ymax"], 
#       bb["xmax"], bb["ymin"], 
#       bb["xmin"], bb["ymin"]),
#     ncol = 2, byrow = T
#   )
#   
#   sf::st_polygon(list(p))
# }
# 
# nc <- st_read(system.file("shape/nc.shp", package="sf"))["BIR79"]
# nc_centroids <- st_centroid(nc)
# box <- st_sfc(bbox_polygon(nc_centroids))
# 
# head(nc_centroids)
# 
# v <- st_voronoi(st_union(nc_centroids), box)
# plot(v, col = 0)
# 
# # Create voronoi
# wales_postcodes_voronoi <- sf::st_voronoi(
#   x = wales_postcodes, envelope = main_wales$geometry
# )
# 
# 
# 
