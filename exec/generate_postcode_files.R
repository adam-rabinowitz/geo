# Create argument parser
parser <- argparser::arg_parser(
  description = 'generate postcode files using regions defined in yaml file',
  hide.opts = TRUE
)
parser <- argparser::add_argument(
  parser,
  arg = '--postcode-path',
  help = 'path to ONS postcode file',
  type = 'character',
  nargs = 1,
  default = NULL
)
parser <- argparser::add_argument(
  parser,
  arg = '--yaml-path',
  help = 'path to YAML file defining regions',
  type = 'character',
  nargs = 1,
  default = NULL
)
parser <- argparser::add_argument(
  parser,
  arg = '--dist-breaks',
  help = 'Distance breaks for identification of neighbouring postcodes',
  type = 'integer',
  nargs = Inf,
  default = NULL
)
parser <- argparser::add_argument(
  parser,
  arg = '--out-prefix',
  help = '',
  type = 'character',
  nargs = 1,
  default = NULL
)
parser <- argparser::add_argument(
  parser,
  arg = '--rm-terminated',
  help = 'Remove terminated postcodes',
  flag = TRUE
)
parser <- argparser::add_argument(
  parser,
  arg = '--rm-nogrid',
  help = 'Remove postcodes without grid location',
  flag = TRUE
)
# Parse arguments
args <- argparser::parse_args(parser)
if (base::is.null(args$postcode_path)) {
  stop('argument --postcode-path must be specified')
}
if (base::is.null(args$yaml_path)) {
  stop('argument --yaml-path must be specified')
}
if (base::is.null(args$dist_breaks)) {
  stop('argument --dist-breaks must be specified')
}
if (!all((args$dist_breaks %% 1000) == 0)) {
  stop('--dist-breaks must be multiples of 1000')
}
if (base::is.null(args$out_prefix)) {
  stop('argument --out-prefix must be specified')
}
print(args)
# Read in postcodes
postcodes <- read_ons_postcodes(
  path = args$postcode_path,
  rm_terminated = args$rm_terminated,
  rm_nogrid = args$rm_terminated
)
# Generate outputs
outputs <- generate_postcode_outputs(
  postcodes = postcodes,
  yaml_path = args$yaml_path,
  dist_breaks = units::as_units(args$dist_breaks, 'm')
)
# Save outputs to file
readr::write_csv(
  x = outputs$table,
  file = paste0(args$out_prefix, '.csv'),
  col_names = TRUE,
  quote = 'needed',
  progress = F
)
saveRDS(
  object = outputs$plot_data,
  file = paste0(args$out_prefix, '.rds')
)