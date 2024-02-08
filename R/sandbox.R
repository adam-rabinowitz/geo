# plot_data <- sf::st_sf(
#   geom = do.call(c, app_data$customer)
# ) |>
#   sf::st_transform(crs = 4326)
# plot_data$region <- names(app_data$customer)
# plot_data <- plot_data[
#   plot_data$region %in% c('Residential City Centre', 'North Manchester Ring', 'South Manchester Ring'),
# ]
# 
# 
# 
# 
# 
# colorPal <- leaflet::colorFactor(
#   palette = RColorBrewer::brewer.pal(length(plot_data$region), 'Set1'),
#   domain = plot_data$region
# )
# 
# bbox <- sf::st_bbox(
#   plot_data[plot_data$region == 'Manchester',]
# )
# 
# 
# 
# plot_data <- sf::st_transform(app_data$customer, crs = 4326)
# bounds <- sf::st_bbox(plot_data)
# 
# # Create plot
# plot <- leaflet::leaflet(
#   data = plot_data,
#   options = leaflet::leafletOptions(
#     zoomControl = TRUE,
#     dragging = TRUE
#   )
# ) |>
#   leaflet::addProviderTiles(
#     provider = "CartoDB.Positron"
#   ) |>
#   leaflet::addPolygons(
#     label = ~region,
#     fillColor = ~colorPal(region),
#     stroke = F
#   ) |>
#   leaflet::addLegend(
#     title = 'Area',
#     values = ~region,
#     pal = colorPal
#   )
# plot |>
#   leaflet::setView(
#     
#   )
