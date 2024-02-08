% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_postcodes.R
\name{generate_polygon_plot_data}
\alias{generate_polygon_plot_data}
\title{Generate polygon plot data}
\usage{
generate_polygon_plot_data(yaml_path, voronoi_path, crs)
}
\arguments{
\item{yaml_path}{Path to YAML defining areas}

\item{voronoi_path}{Path to rds file containing postcode voronoi}

\item{crs}{Coordinate reference system for output polygons}
}
\value{
A list containing postcode tables and postcode plot data
}
\description{
Function generates postcode tables and postcode plot data
}