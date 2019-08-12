### Calculating travel time from areas in the DRC infected with Ebola to surrounding areas ###
# All data used is publicly available
# Infected health area data: WHO Situation Report #42  https://apps.who.int/iris/bitstream/handle/10665/324843/SITREP_EVD_DRC_20190521-eng.pdf?ua=1
# Health area shapefile: https://www.arcgis.com/home/webmap/viewer.html?useExisting=1&layers=916988872d694a9ebe244c7a195c6874
# Population bias raster: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download
# Friction surface raster: https://map.ox.ac.uk/ accessed via getRaster(surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015", extent = matrix(c(23.77, -3.37, 36.36 , 5.451)))
# Background lakes shapefile: http://193.43.36.146/map?entryId=bd8def30-88fd-11da-a88f-000d939bc5d8
# Ugandan parishes shapefile: https://www.arcgis.com/home/item.html?id=2897e7de50c84c189f47906c1db57c76

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
library(readr)
library(dplyr)
library(grid)
library(gridExtra)
source('Code/plot_funcs.R')


# Read in data
health_areas <- shapefile('Data/drc_health_areas.shp') #health area shapefile with binary present/absence variable for infection based on WHO SitRep 42 from May 21, 2019
ariwara_zone <- shapefile('Data/ariwara.shp')
# uga_parish <- shapefile('Data/Uganda_parish_crop.shp')


# ext <- bbox(matrix(c(37.5, 23.77, 12.5,  -3.37), nrow = 2, ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
friction <- raster('Data/friction.tif') #friction raster is already cropped to the extent we will plot it to getRaster(surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015", extent = ext)
pop_raster <- raster('Data/pop_raster.tif')
lakes <- shapefile('Data/waterbodies_africa.shp')
drc_hosp <- suppressMessages(read_csv('Data/DRC_hospitals.csv', locale = readr::locale(encoding = "latin1")))
rw_hosp <- suppressMessages(read_csv('Data/Rwanda_hospitals.csv', locale = readr::locale(encoding = "latin1")))
ug_hosp <- suppressMessages(read_csv('Data/Uganda_hospitals.csv', locale = readr::locale(encoding = "latin1")))
ss_hosp <- suppressMessages(read_csv('Data/SSudan_hospitals.csv', locale = readr::locale(encoding = "latin1")))

# Mask out lakes from friction and population rasters
pop_raster <- mask(pop_raster, lakes, inverse = TRUE)

# Ugandan cases are now >42 days older
# Subset health areas to only those infected (then include Ugandan parishes containing hospitals with patients)
# ug_points <- ug_hosp[which(ug_hosp$`Facility name` == 'Bwera Hospital' | ug_hosp$`Facility name` == 'Kagando Hospital'),]
# 
# coordinates(ug_points) <- ~ Longitude + Latitude
# proj4string(ug_points) <- proj4string(health_areas)
# 
# ug_infect <- raster::intersect(uga_parish, ug_points)
# ug_infect$aug8_new <- 1

ariwara_zone$aug8_new <- 1
# 
# 
health_areas <- bind(health_areas, ariwara_zone)
ha_infect <- health_areas[which(health_areas@data$aug8_new == 1),]


# Sample infected health areas with population bias, if pop or area too small use spsample
poly_samp <- data.frame()
for (i in 1:length(ha_infect)){
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
writeRaster(access.raster, filename = 'Outputs_Aug8_update/access_raster_raw.tif', overwrite = TRUE)

# Convert infinite values to NA and format values to show hours
values(access.raster)[which(is.infinite(values(access.raster)))] <- NA
values(access.raster) <- values(access.raster)/60
writeRaster(access.raster, filename = 'Outputs_Aug8_update/access_raster_hours.tif', overwrite = TRUE)

# Plot access raster data
adm0 <- getShp(ISO = c("UGA", "COD", "RWA", "BDI", "TZA", "SSD", "CAF", "ETH", "SDN"), admin_level = 'admin0')
adm1 <- getShp(ISO = "COD", admin_level = "admin1")
lakes <- st_as_sf(lakes)

trav_time_plot <- plot_tt(infected_sf = ha_infect, access_raster = access.raster, bg_adm0 = adm0, bg_adm1 = adm1, lakes = lakes)

rw_plot <- plot_rel_map(infected_sf = ha_infect, access_raster = access.raster, bg_adm0 = adm0, lakes = lakes, co = 'RWA')

ug_plot <- plot_rel_map(infected_sf = ha_infect, access_raster = access.raster, bg_adm0 = adm0, lakes = lakes, co = 'UGA')

sd_plot <- plot_rel_map(infected_sf = ha_infect, access_raster = access.raster, bg_adm0 = adm0, lakes = lakes, co = 'SSD')

# To save plots use
ggsave(trav_time_plot, filename = 'Outputs_Aug8_update/Travel_time_map.png', width = 8, height = 8)
ggsave(rw_plot, filename = 'Outputs_Aug8_update/Rwanda_map.png', width = 8, height = 8)
ggsave(ug_plot, filename = 'Outputs_Aug8_update/Uganda_map.png', width = 8, height = 8)
ggsave(sd_plot, filename = 'Outputs_Aug8_update/SSudan_map.png', width = 8, height = 8)

# For DRC, Rwanda and Uganda, create a list of hospital travel times
# Rbind all country hospitals and correct names
hospitals <- as.data.table(rbindlist(list(drc_hosp, ug_hosp, rw_hosp, ss_hosp), fill = TRUE))
names(hospitals)[which(names(hospitals) == 'Facility name')] <- 'Name'
hospitals[, Name := sub(' H.*$', ' Hospital', Name)]

# Extract travel times for each hospital and attach to parent df
TT <- extract(access.raster, hospitals[,.(Longitude, Latitude)])
hospitals <- cbind(hospitals, TT)
names(hospitals)[ncol(hospitals)] <- 'Travel_Time'
hospitals <- hospitals[!is.na(Travel_Time),]

# Select hospitals with lowest travel times by country
drc_hosp_tt <- hospitals[order(Travel_Time),][Country == 'Democratic Republic of the Congo',][1:20, .(Admin1, Name, Travel_Time, Longitude, Latitude)]
rw_hosp_tt <- hospitals[order(Travel_Time),][Country == 'Rwanda',][1:20, .(Admin1, Name, Travel_Time, Longitude, Latitude)]
ug_hosp_tt <- hospitals[order(Travel_Time),][Country == 'Uganda',][1:20, .(Admin1, Name, Travel_Time, Longitude, Latitude)]
ss_hosp_tt <- hospitals[order(Travel_Time),][Country == 'South Sudan',][1:20, .(Admin1, Name, Travel_Time, Longitude, Latitude)]

# Save to csv
write.csv(hospitals, file = file('Outputs_Aug8_update/hospital_tt.csv', encoding = 'UTF-8'))
write.csv(drc_hosp_tt, file = file('Outputs_Aug8_update/drc_hosp_tt.csv', encoding = 'UTF-8'))
write.csv(rw_hosp_tt, file = file('Outputs_Aug8_update/rw_hosp_tt.csv', encoding = 'UTF-8'))
write.csv(ug_hosp_tt, file = file('Outputs_Aug8_update/ug_hosp_tt.csv', encoding = 'UTF-8'))
write.csv(ss_hosp_tt, file = file('Outputs_Aug8_update/ss_hosp_tt.csv', encoding = 'UTF-8'))

# Save subset of columns to table output
drc_tab <- tableGrob(drc_hosp_tt[,.(District = Admin1, Name, `Travel Time (hours)` = round(Travel_Time, 2))], rows = NULL)
rw_tab <- tableGrob(rw_hosp_tt[,.(District = Admin1, Name, `Travel Time (hours)` = round(Travel_Time, 2))], rows = NULL)
ug_tab <- tableGrob(ug_hosp_tt[,.(District = Admin1, Name, `Travel Time (hours)` = round(Travel_Time, 2))], rows = NULL)
ss_tab <- tableGrob(ss_hosp_tt[,.(District = Admin1, Name, `Travel Time (hours)` = round(Travel_Time, 2))], rows = NULL)

ggsave(drc_tab, filename = 'Outputs_Aug8_update/drc_hosp_tt_prettytab.png', height = 8, width = 6)
ggsave(rw_tab, filename = 'Outputs_Aug8_update/rw_hosp_tt_prettytab.png', height = 8, width = 6)
ggsave(ug_tab, filename = 'Outputs_Aug8_update/ug_hosp_tt_prettytab.png', height = 8, width = 6)
ggsave(ss_tab, filename = 'Outputs_Aug8_update/ss_hosp_tt_prettytab.png', height = 8, width = 6)
