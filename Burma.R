library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)

gpxFile <- "/Users/dom/Downloads/activity_3465547420.gpx"
## gpxRoute <- readOGR(dsn = gpxFile, layer = "tracks")
gpxRoute <- readOGR(dsn = gpxFile, layer = "track_points")

gg_plot <- gpxRoute %>%
  fortify() %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = group, colour = "red")) 
gg_plot
