library(sf)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(foreign)
library(spatialEco)
library(terra)
library(rgeos)

setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")

#Ranking
#The formula for ranking is 

#((condition*status*0.8)+(connectivity*proximity to sites*proximity to PAs*0.2))*duration(1)*risk*area

#Read in the property shp file
properties <- readOGR("./raw_data/Properties/Properties.shp")
#Import koala liklihood layer
KML <- readOGR("D:/Linkage/DSF code/private_land_conservation_DSF/raw_data/Biodiversity_KLM_v2.0_August_2019/KLM_v2.0_August_2019.shp")
crs <- crs(KML)
properties <- sp::spTransform(properties, crs(crs))
save(properties, file = "./preprocessing/properties_proj.RData")
#Load projected property layer
load("./preprocessing/properties_proj.RData")

#Get the coordiates of the polygons
coords <- coordinates(properties)
save(coords, file="./preprocessing/coords.RData")
load("./preprocessing/coords.RData")
#Get puid
points <- SpatialPoints(coords, proj4string=crs(crs))
pnts_properties <- over(points, properties)
save(pnts_properties, file="./preprocessing/pnts_properties.RData")
head(pnts_properties)
puid <- pnts_properties$gurasid
#Get area - note are in shp file has to be correct, check first in ArcMap
area <- pnts_properties$Shape__Are
#Get Koala likelihood
pnts_KML <- over(points, KML)
save(pnts_KML, file="./preprocessing/pnts_KML.RData")
pnts_KML_prob <- pnts_KML$p

#Condition
#Just using a condition layer for now
# The Ecological Condition of a site is measured using a combination of information of:
# Current ecological condition. This is assessed from multiple plot-based ecological
# surveys throughout the candidate site, based on principles set out in the Biodiversity
# Assessment Method (BAM). A minimum of one 50x20m plot is conducted for every
# vegetation class present within the proposed site. Where vegetation classes vary
# significantly in condition, additional plots are conducted to capture this variation in
# condition. Vegetation condition is assessed against benchmark values for vegetation
# classes in each IBRA subregion. Where relevant the BCT applies dynamic benchmarks
# developed for vegetation classes.
# and
#  Future predicted condition based on management actions agreed by the landholder to
# be undertaken at a site. Standard management actions are outlined in Appendix B. The
# BCT may identify specific management actions that are applicable for targeted offers. The
# rate of predicted gain in ecological condition as a result of proposed management actions
#is consistent with the BAM. 
#Using the condition layer from the Biodiversity Indicators Program
cond_rast <- raster("./raw_data/bba_31a_cond_V2_nsw_raw.tif")
#cond_rast <- projectRaster(cond_rast, crs = crs, ext = ext)
#cond <- raster::extract(cond_rast, points, method="bilinear")
cond <- zonal.stats(properties, cond_rast, stats="mean")
#Could be faster
#cond <- extract(cond_rast, properties, "mean") 

##Connectivity
connect_rast <- raster("./raw_data/bba_31b_connectivity_V2_nsw_raw.tif")
#crs <- crs(connect_rast)

#apply a 1500m buffer aroud each site
#have to do this on either a server or in QGIS
#properties_buff_1500m <- gBuffer(properties, width=1500) 
properties_buff_1500m <- readOGR("./preprocessing/Properties_buffered.shp")
properties_buff_1500m <- sp::spTransform(properties_buff_1500m, crs(crs))
save(properties_buff_1500m, file = "./preprocessing/properties_buff_proj.RData")
load("./preprocessing/properties_buff_proj.RData")
#connect <- extract(connect_rast, properties, "mean") 
connect <- zonal.stats(properties_buff_1500m, connect_rast, stats="mean")


##Proximity to nearby sites
cov <- readOGR("./preprocessing/All_BCT_agreements_merged/All_BCT_agreements_merged.shp")
#crs <- crs(pa)
#Switch to a crs that allows the distance raster to be in meters
cov <- sp::spTransform(cov, crs("+init=epsg:3857"))
#r <- raster(xmn=138, xmx=153.7, ymn=-39.37999, ymx=-27.97499, res=0.1)
#1km res
r <- raster(xmn=15696293, xmx=17100107, ymn=-4473529, ymx=-3279325, res=1000, crs="+init=epsg:3857")
r1 <- rasterize(cov, r, 1)
r2 <- mask(r1, r1, maskvalue=1, inverse=TRUE, updatevalue=NA)
dist_cov <- distance(r2)
#crs = crs(connect_rast)
dist_cov <- projectRaster(dist_cov, crs = crs)
writeRaster(dist_cov,'./preprocessing/dist_cov_m.tif', overwrite=TRUE)
dist_cov_prop <- zonal.stats(properties, dist_cov, stats="mean")

##Proximity to protected areas
#https://stackoverflow.com/questions/35555709/global-raster-of-geographic-distances
pa <- readOGR("./raw_data/tenure_npws_estate/NPWS_Estate.shp")
pa <- sp::spTransform(pa, crs("+init=epsg:3857"))
r <- raster(xmn=15696293, xmx=17100107, ymn=-4473529, ymx=-3279325, res=1000, crs="+init=epsg:3857")
r1 <- rasterize(pa, r, 1)
r2 <- mask(r1, r1, maskvalue=1, inverse=TRUE, updatevalue=NA)
dist_pa <- distance(r2)
dist_pa <- projectRaster(dist_pa, crs = crs)
writeRaster(dist_pa,'./preprocessing/dist_pa_m.tif', overwrite=TRUE)
dist_pa_prop <- zonal.stats(properties, dist_pa, stats="mean")

#Risk
#Somethin
load("./preprocessing/coords.RData")
risk <- readOGR("./raw_data/Land and Soil Capability Mapping for NSW/LSC_MstLmt_allHzd_NSW_v4_200923_proj.shp")
points <- SpatialPoints(coords, proj4string=crs(risk))
#points <- sp::spTransform(points, crs(risk))
pnts_risk <- over(points, risk)
pnts_risk <- pnts_risk$Label
#Convert the values into the scoring system
pnts_risk_v2 <- as.numeric(pnts_risk)
pnts_risk_v3 <- pnts_risk_v2*100
pnts_risk_v3[pnts_risk_v3 == 100] <- 6
pnts_risk_v3[pnts_risk_v3 == 200] <- 6
pnts_risk_v3[pnts_risk_v3 == 300] <- 6
pnts_risk_v3[pnts_risk_v3 == 400] <- 3
pnts_risk_v3[pnts_risk_v3 == 500] <- 2
pnts_risk_v3[pnts_risk_v3 == 600] <- 2
pnts_risk_v3[pnts_risk_v3 == 700] <- 1
pnts_risk_v3[pnts_risk_v3 == 800] <- 1
#pu.df <- cbind(pu.df, pnts_risk$Label)

#Tie it together in a dataframe
pu.df <- data.frame(puid=puid, area=area, kml=pnts_KML_prob, cond=cond, connect=connect, risk = pnts_risk_v3, dist_cov = dist_cov_prop, dist_pa = dist_pa_prop)
colnames(pu.df) <- c("puid", "area", "kml", "cond", "connect", "risk", "dist_cov", "dist_pa")

#Do the ranking formula

pu.df$rank <- (pu.df$cond*pu.df$kml*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area

save(pu.df, file="./preprocessing/pu.df.RData")
load("./preprocessing/pu.df.RData")

#pu.df <- cbind(pu.df, dist_cov_prop)

# #Pull out the LGAs
# # The input file geodatabase
# fgdb <- "./Study region/project_boundaries/project_boundaries.gdb"
# # List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)
# # Read the feature class
# lgas <- readOGR(dsn=fgdb,layer="nsw_lga_subset_GDA2020_all")
# # Determine the FC extent, projection, and attribute information
# summary(lgas)
# # View the feature class
# plot(lgas)
# crs <- proj4string(lgas)
# 
# #Import property data
# properties <- rgdal::readOGR("./Study region/Properties/properties_mosaic_final_private_2ha_not_intense_not_water.shp")
# proj4string(properties) <- CRS(crs)
# #plot(properties, add = TRUE)
# #properties <- spTransform(properties, crs)
#Add LGA ID to the property data
#intersect_lga_prop <- gIntersects(lgas, properties_prob, byid = TRUE)
#properties_lgas <- st_join(properties_prob, left = FALSE, lgas["LG_PLY_PID"]) # join points

#Intersect the property data with the LGA's - needed to do this in QGis, couldn't get it working in R
properties <- readOGR("./Study region/LGA_properties_intersection/LGA_properties_intersection.shp")
#Just for the table
properties.dbf <- read.dbf("./Study region/LGA_properties_intersection/LGA_properties_intersection.dbf", as.is = FALSE)
#Join property data with probability predictions
probs <- read.csv("D:/Linkage/DSF code/private_land_conservation_DSF/raw_data/Probability/spatial_predictions_10yr.csv")
properties_prob <- merge(properties.dbf, probs, by='CADID')








