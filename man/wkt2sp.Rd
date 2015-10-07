\name{wkt2sp}
\alias{wkt2sp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Read PostGIS geometry table into spatial }
\description{
   From PostGIS database transforms the source geometries in WKT form to SpatalPolygons, SpatialLines or SpatialPoints object.
}
\usage{
wkt2sp(geoms, gcol, idcol)
}
\arguments{
  \item{geoms}{ table contains the WKT geometry field}
  \item{gcol}{ WKT field of \code{geoms} table, what can be transformed from PostGIS Geometry type field by SQL statement Astext() }
  \item{idcol}{ field of \code{geoms} table for identification }
}

\value{
Returns SpatalPolygons, SpatialLines or SpatialPoints depending on the source geometry type.
}

\author{ Norbert Solymosi <solymosi.norbert@gmail.com> }

\seealso{ \code{sp} package}



\keyword{ spatial }
