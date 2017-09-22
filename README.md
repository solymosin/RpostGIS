# RpostGIS
a parser of PostGIS geometries to R

Install RpostGIS from github

library(devtools)

devtools::install_github("solymosin/RpostGIS")

################################################ 
# for SpatiaLite

library(RSQLite)

drv = dbDriver("SQLite")
con = dbConnect(drv, dbname='db.sqlite', loadable.extensions=TRUE)
lext = dbGetQuery(con, "SELECT load_extension('mod_spatialite')")
