library(sf)
library(rgdal)
library(sp)
setwd("D:/Linkage/Data")

#Pull out the LGAs
# The input file geodatabase
fgdb <- "./Study region/project_boundaries/project_boundaries.gdb"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
# Read the feature class
lgas <- readOGR(dsn=fgdb,layer="nsw_lga_subset_GDA2020_all")
# Determine the FC extent, projection, and attribute information
summary(lgas)
# View the feature class
plot(lgas)
crs <- st_crs(lgas)

#Import property data
properties <- st_read("./Study region/Properties/properties_mosaic_final_private_2ha_not_intense_not_water.shp")
plot(properties, add = TRUE)
properties <- spTransform(properties, crs)

#Join property data with probability predictions
probs <- read.csv("D:/Linkage/DSF code/private_land_conservation_DSF/raw_data/Probability/spatial_predictions_10yr.csv")
properties_prob <- merge(properties, probs, by='CADID')

#Add LGA ID to the property data
properties_lgas <- st_join(properties_prob, left = FALSE, lgas["LG_PLY_PID"]) # join points

st_crs(properties_prob) == st_crs(lgas)


