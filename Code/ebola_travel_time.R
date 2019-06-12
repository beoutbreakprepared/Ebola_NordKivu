### Calculating travel time from areas in the DRC infected with Ebola to surrounding areas ###
# All data used is publicly available
# Infected health area data: WHO Situation Report #42  https://apps.who.int/iris/bitstream/handle/10665/324843/SITREP_EVD_DRC_20190521-eng.pdf?ua=1
# Health area shapefile: https://www.arcgis.com/home/webmap/viewer.html?useExisting=1&layers=916988872d694a9ebe244c7a195c6874
# Population bias raster: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download
# Friction surface raster: https://map.ox.ac.uk/ accessed via getRaster(surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015")
# Background lakes shapefile: http://193.43.36.146/map?entryId=bd8def30-88fd-11da-a88f-000d939bc5d8

# Read in necessary libraries
library(data.table)
library(raster)
library(rgdal)
library(ggplot2)
library(sf)
library(malariaAtlas)
library(gdistance)
library(seegSDM)
library(viridis)
source('plot_funcs.R')

# Read in data
health_areas <- shapefile('health_areas_SR42.shp') #health area shapefile with binary present/absence variable for infection based on WHO SitRep 42 from May 21, 2019
friction <- raster('friction.tif') #friction raster is already cropped to the extent we will plot it to
pop_raster <- raster('pop_raster.tif')

# Subset health areas to only those infected
ha_infect <- health_areas[which(health_areas@data$sr_42_new == 1),]

# Sample infected health areas with population bias, if pop or area too small use spsample
poly_samp <- data.frame()
for (i in 1:length(ha_infect)){
  print(i)
  # reference individual polygon
  polygon <- ha_infect[i,]

  # mask with population raster
  raster <- mask(pop_raster, polygon, updatevalue=NA)
  values(raster)[which(is.infinite(values(raster)))] <- NA
  nas <- sum(!is.na(values(raster)))
  if(nas == 0){
    pts <- data.frame(spsample(x=polygon, n=200, type='random', iter=10))
  } else {
    pts <- data.frame(bgSample(n = 200,
                               raster = raster,
                               replace = TRUE, prob = TRUE))
  }

  names(pts) <- c('long', 'lat')
  poly_samp <- rbind(poly_samp, pts)
}

# Format matrix of points to use in calculating travel time
dataset <- poly_samp
coordinates(dataset) <- ~ long + lat
proj4string(dataset) <- proj4string(health_areas)
points <- as.matrix(dataset@coords)

# Calculate transition surface and travel time
Tr <- transition(friction, function(x) 1/mean(x), 8)
T.GC <- geoCorrection(Tr)
access.raster <- accCost(x = T.GC, fromCoords =  points)

# Convert infinite values to NA and format values to show hours
values(access.raster)[which(is.infinite(values(access.raster)))] <- NA
values(access.raster) <- values(access.raster)/60

# Plot access raster data
adm0 <- getShp(ISO = c("UGA", "COD", "RWA", "BDI", "TZA"), admin_level = 'admin0')
adm1 <- getShp(ISO = "COD", admin_level = "admin1")
lakes <- read_sf('waterbodies_africa.shp')

trav_time_plot <- plot_tt(infected_sf = ha_infect, access_raster = access.raster, bg_adm0 = adm0, bg_adm1 = adm1, lakes = lakes)
trav_time_plot

rw_plot <- plot_rel_map(infected_sf = ha_infect, access_raster = access.raster, bg_adm0 = adm0, lakes = lakes, co = 'RWA')
rw_plot

ug_plot <- plot_rel_map(infected_sf = ha_infect, access_raster = access.raster, bg_adm0 = adm0, lakes = lakes, co = 'UGA')
ug_plot

# To save plots use ggsave(plot, filename = 'plot.png', width = 8, height = 8)
