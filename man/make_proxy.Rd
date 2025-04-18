% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_proxy.R
\name{make_proxy}
\alias{make_proxy}
\alias{add_proxy}
\title{Generate spatial proxies}
\usage{
make_proxy(rast_grid, type, opts = NULL)

add_proxy(rast_grid, type, opts = NULL)
}
\arguments{
\item{rast_grid}{A raster object with the desired dimensions}

\item{type}{The type of proxy to generate. Options are coordinates ("coordinates"), Euclidean Distance Fields ("edf"), and Oblique Geographic Coordinates ("ogc")}

\item{opts}{A list of options to pass to the proxy generation function. Only used when \code{type = "edf"} or \code{type = "ogc"}.
When \code{type = "edf"}, the "\code{coords}" option is and sf object a matrix with two columns (x, y) representing the coordinates to use when generating Euclidean Distance Fields. When \code{type = "ogc"}, the "\code{n}" option is an integer representing the number of angles to use when generating Oblique Geographic Coordinates.}
}
\value{
A SpatRaster object with the generated spatial proxies
}
\description{
The \code{make_proxy()} function generates spatial proxies for a given raster object.
It generates three types of proxies: coordinates, Euclidean Distance Fields (EDF), and Oblique Geographic Coordinates (OGC).
Additionally, the \code{add_proxy()} function can be used to add a proxy to an existing raster object.
}
\examples{
rast_grid = terra::rast(ncols = 300, nrows = 100, xmin = 0, xmax = 300, ymin = 0, ymax = 100)
proxy_coords = make_proxy(rast_grid, "coordinates")
proxy_edf = make_proxy(rast_grid, "edf")
proxy_ogc = make_proxy(rast_grid, "ogc", opts = list(n = 5))
terra::plot(proxy_coords)
terra::plot(proxy_edf)
terra::plot(proxy_ogc)
}
\references{
Behrens, T., Schmidt, K., Viscarra Rossel, R. A., Gries, P., Scholten, T., & MacMillan, R. A. (2018). Spatial modelling with Euclidean distance fields and machine learning. European journal of soil science, 69(5), 757-770.

Møller, A. B., Beucher, A. M., Pouladi, N., & Greve, M. H. (2020). Oblique geographic coordinates as covariates for digital soil mapping. Soil, 6(2), 269-289.
}
