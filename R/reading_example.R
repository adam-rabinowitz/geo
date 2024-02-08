# # Read postcodes
# active_postcodes <- geo::read_ons_postcodes(
#   '~/beauclair/data/ONSPD/ONSPD_AUG_2023_UK/Data/ONSPD_AUG_2023_UK.minus_GIR_NPT_postcodes.csv.gz',
#   rm_terminated = T, rm_nogrid = F
# )
# all_postcodes <- geo::read_ons_postcodes(
#   '~/beauclair/data/ONSPD/ONSPD_AUG_2023_UK/Data/ONSPD_AUG_2023_UK.minus_GIR_NPT_postcodes.csv.gz',
#   rm_terminated = F, rm_nogrid = F
# )
# # Get postcodes for reading city centre
# reading_active <- geo:::get_point_postcodes(
#   postcodes = active_postcodes,
#   lat = 51.45546314,
#   long = -0.972658996,
#   crs = 4326L,
#   distance = 7,
#   units = 'mi'
# )
# reading_all <- geo:::get_point_postcodes(
#   postcodes = all_postcodes,
#   lat = 51.45546314,
#   long = -0.972658996,
#   crs = 4326L,
#   distance = 7,
#   units = 'mi'
# )
# # Get postal sectors
# reading_active_sectors <- geo::truncate_postcodes(
#   reading_active$pcds, level='sector', unique=TRUE, sort=TRUE
# )
# reading_all_sectors <- geo::truncate_postcodes(
#   reading_all$pcds, level='sector', unique=TRUE, sort=TRUE
# )
# # Identify sectors that are present and active
# reading_all_specific_sectors <- setdiff(
#   reading_all_sectors, reading_active_sectors
# )
# active_sectors <- geo::truncate_postcodes(
#   active_postcodes$pcds, level='sector', unique=TRUE, sort=TRUE
# )
# intersect(reading_all_specific_sectors, active_sectors)






