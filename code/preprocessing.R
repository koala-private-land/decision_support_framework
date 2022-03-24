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
library(gurobi)
library(exactextractr)

setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")

#Ranking
#The formula for ranking is 
#((condition*status*0.8)+(connectivity*proximity to sites*proximity to PAs*0.2))*duration(1)*risk*area

# # The input file geodatabase
#fgdb <- "D:/Linkage/Data/Study region/properties1.gdb"
# # List all feature classes in a file geodatabase
#subset(ogrDrivers(), grepl("GDB", name))
#fc_list <- ogrListLayers(fgdb)
#print(fc_list)
# # Read the feature class
#properties <- readOGR(dsn=fgdb,layer="lots_mosaic_final_private_2ha_not_intense_not_water")
# # Determine the FC extent, projection, and attribute information
#summary(properties)
# # View the feature class
#plot(properties)

#Load dissolved property layer
properties <- readOGR("./preprocessing/Properties_dissolved.shp")
crs <- proj4string(properties)
save(properties, file = "./preprocessing/properties.RData")
load("./preprocessing/properties.RData")

#Read in the property shp file
#properties <- readOGR("./raw_data/Properties/Properties.shp")
#Import koala liklihood layer
# KML <- readOGR("D:/Linkage/DSF code/private_land_conservation_DSF/raw_data/Biodiversity_KLM_v2.0_August_2019/KLM_v2.0_August_2019.shp")
# save(KML, file = "./preprocessing/KML.RData")
# load("./preprocessing/KML.RData")
# crs <- crs(KML)

#Get Koala likelihood
# pnts_KML <- over(points, KML)
# save(pnts_KML, file="./preprocessing/pnts_KML.RData")
# load("./preprocessing/pnts_KML.RData")
# pnts_KML_prob <- pnts_KML$p
#Get area of Koala habitat

#Import koala habitat layer
#khab <- raster("./raw_data/khab_thresh.tif")
#Need to create an area raster
#cell_size <- area(khab, na.rm=TRUE, weights=FALSE)
#writeRaster(cell_size, './preprocessing/cell_size.tif')
#khab_zonal <- zonal.stats(properties, khab, stats="sum")
#save(khab_zonal, file="./preprocessing/khab_zonal.RData")
#load("./preprocessing/khab_zonal.RData")

#Had to do this zonal statistics in Arcmap - R couldn't handle it
koala <- read.csv("./preprocessing/propid_koalahabitat.csv")

#properties <- sp::spTransform(properties, crs(crs))
#save(properties, file = "./preprocessing/properties_proj.RData")
#Load projected property layer
#load("./preprocessing/properties_proj.RData")

#Get the coordinates of the polygons
coords <- coordinates(properties)
save(coords, file="./preprocessing/coords.RData")
load("./preprocessing/coords.RData")

#Get puid
points <- SpatialPoints(coords, proj4string=crs(crs))
proj4string(points) <- crs
proj4string(properties) <- crs
pnts_properties <- over(points, properties)
save(pnts_properties, file="./preprocessing/pnts_properties.RData")
load("./preprocessing/pnts_properties.RData")
head(pnts_properties)

propid <- pnts_properties$propid
#Get area - note are in shp file has to be correct, check first in ArcMap
#This is m2
area <- pnts_properties$Shape_Area

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
cond <- data.frame(properties$propid, cond)
save(cond, file="./preprocessing/cond.RData")
load("./preprocessing/cond.RData")

##Connectivity
connect_rast <- raster("./raw_data/bba_31b_connectivity_V2_nsw_raw.tif")
#crs <- crs(connect_rast)

#apply a 1500m buffer aroud each site
#have to do this on either a server or in QGIS
#properties_buff_1500m <- gBuffer(properties, width=1500) 
properties_buff_1500m <- readOGR("./preprocessing/Properties_buffered.shp")

connect <- zonal.stats(properties_buff_1500m, connect_rast, stats="mean")
#connect <- extract(connect_rast, properties_buff_1500m, fun='mean') 
connect <- data.frame(properties_buff_1500m$propid, connect)
save(connect, file="./preprocessing/connect.RData")
load("./preprocessing/connect.RData")

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
dist_cov <- projectRaster(dist_cov, crs = crs(properties_buff_1500m))
writeRaster(dist_cov,'./preprocessing/dist_cov_m.tif', overwrite=TRUE)
dist_cov <- raster("./preprocessing/dist_cov_m.tif")
dist_cov_prop <- zonal.stats(properties, dist_cov, stats="mean")
dist_cov_prop <- data.frame(properties$propid, dist_cov_prop)
save(dist_cov_prop, file="./preprocessing/dist_cov_prop.RData")
load("./preprocessing/dist_cov_prop.RData")

##Proximity to protected areas
#https://stackoverflow.com/questions/35555709/global-raster-of-geographic-distances
pa <- readOGR("./raw_data/tenure_npws_estate/NPWS_Estate.shp")
pa <- sp::spTransform(pa, crs("+init=epsg:3857"))
r <- raster(xmn=15696293, xmx=17100107, ymn=-4473529, ymx=-3279325, res=1000, crs="+init=epsg:3857")
r1 <- rasterize(pa, r, 1)
r2 <- mask(r1, r1, maskvalue=1, inverse=TRUE, updatevalue=NA)
dist_pa <- distance(r2)
dist_pa <- projectRaster(dist_pa, crs = crs(properties_buff_1500m))
writeRaster(dist_pa,'./preprocessing/dist_pa_m.tif', overwrite=TRUE)
dist_pa <- raster("./preprocessing/dist_pa_m.tif") 
dist_pa_prop <- zonal.stats(properties, dist_pa, stats="mean")
dist_pa_prop <- data.frame(properties$propid, dist_pa_prop)
save(dist_pa_prop, file="./preprocessing/dist_pa_prop.RData")
load("./preprocessing/dist_pa_prop.RData")

#Risk
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
save(pnts_risk_v3, file="./preprocessing/pnts_risk_v3.RData")
load("./preprocessing/pnts_risk_v3.RData")
#pu.df <- cbind(pu.df, pnts_risk$Label)


#Some of this can be tied together and some of it has to be merged
pu.df <- data.frame(propid=propid, area=area, risk = pnts_risk_v3)
colnames(pu.df) <- c("propid", "area", "risk")
pu.df = pu.df[pu.df$propid > 0,]

#cond
colnames(cond) <- c("propid", "cond")
cond = cond[cond$propid > 0,]
merged <- merge(pu.df, cond, by='propid')
#connect
colnames(connect) <- c("propid", "connect")
connect = connect[connect$propid > 0,]
merged <- merge(merged, connect, by='propid')
#dist_cov_prop
colnames(dist_cov_prop) <- c("propid", "dist_cov")
merged <- merge(merged, dist_cov_prop, by='propid')
#dist_pa_prop
colnames(dist_pa_prop) <- c("propid", "dist_pa")
merged <- merge(merged, dist_pa_prop, by='propid')

#pu.df <- merged[c("propid", "area", "cond.x", "connect.x", "risk", "dist_cov", "dist_pa",  cond=risk = pnts_risk_v3, dist_cov = dist_cov_prop, dist_pa = dist_pa_prop)]
colnames(pu.df) <- c("propid", "area", "risk", "cond", "connect", "dist_cov", "dist_pa")

###Need to add LGA. 
properties.dbf <- read.dbf("./preprocessing/LGA_properties_intersection_CADID_retained.dbf", as.is = FALSE)
#Pull out just LGA's
properties.dbf_LGA <- properties.dbf[c("propid", "CADID")]
#Remove duplicates
properties.dbf_LGA <- properties.dbf_LGA[!duplicated(properties.dbf_LGA), ]
merged <- merge(merged, properties.dbf_LGA, by='propid')
pu.df <- merged[c("CADID", "propid", "area", "cond", "connect", "risk", "dist_cov", "dist_pa")]

##Up to here, redoing the Koala values for the dissolved layer
##Add the koala/conservation values
merged <- merge(pu.df, koala, by='propid')
pu.df <- merged[c("CADID", "propid", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "k_area")]
colnames(pu.df) <- c("LGA", "propid", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala")

#Do the ranking formula
pu.df$rank <- (pu.df$cond*pu.df$koala*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area

#Bring in the probability data
#Intersect the property data with the LGA's - needed to do this in QGis, couldn't get it working in R
#LGA_prop_intersect <- readOGR("./preprocessing/LGA_properties_intersection_CADID_retained.shp")
#Just for the table
LGA_prop_intersect.dbf <- read.dbf("./preprocessing/LGA_properties_intersection_CADID_retained.dbf", as.is = FALSE)
colnames(LGA_prop_intersect.dbf)[3] <- "CADID_LGA"
colnames(LGA_prop_intersect.dbf)[21] <- "CADID"
#properties.dbf_cost <- properties.dbf[c("propid", "LValHa")]
#properties.dbf_ag <- aggregate(properties.dbf_cost, by = list(properties.dbf$propid), FUN = "sum")

#Join property data with probability predictions
probs <- read.csv("D:/Linkage/DSF code/private_land_conservation_DSF/raw_data/Probability/spatial_predictions_inf.csv")
properties_prob <- merge(LGA_prop_intersect.dbf, probs, by='CADID')
####Need to turn bid price into total. WTA == $1000/ha/yr 
properties_prob$MeanWTA.tot <- properties_prob$Area_H*properties_prob$MeanProp*properties_prob$MeanWTA*1000
##All of this aggregating won't be necessary with Jonathan's new predictions which are at the "propid" level
MeanWTA.tot <- aggregate(x = properties_prob[c("MeanWTA.tot")], by = properties_prob[c("propid")], FUN = sum)
MeanWTA.tot <- MeanWTA.tot[MeanWTA.tot$propid > 0,]
properties_prob.ag <- aggregate(x = properties_prob[c("MeanAdopt", "MeanProp")], by = properties_prob[c("propid")], FUN = mean)
properties_prob.ag <- properties_prob.ag[properties_prob.ag$propid > 0,]
merged <- merge(properties_prob.ag, MeanWTA.tot, by='propid')

#Merge with main dataframe
merged <- merge(pu.df, merged, by='propid')
pu.df <- merged[c("LGA", "propid", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala", "rank","MeanAdopt", "MeanWTA.tot", "MeanProp")]
colnames(pu.df) <- c("LGA", "propid", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala", "rank", "prob.property", "bid.price", "MeanProp")
pu.df$koala.w <- pu.df$koala*pu.df$MeanProp

#checks
#This should be zero
list <- which(pu.df$area < pu.df$koala.w)
list

##Need to estimate the amount that would realistically be spent on each tender
cost <- read.csv("./raw_data/bct_agreement_cost.csv")
#Get total values of conservation tenders
cost$mintot <- cost$total_area*cost$min_cost_ha
cost$maxtot <- cost$total_area*cost$max_cost_ha

length(unique(pu.df$LGA))
lga <- unique(pu.df$LGA)
npv <- rep(c(runif(c(1:99), min=mean(cost$mintot), max=mean(cost$maxtot))))
npv.df <- data.frame(LGA=lga, NPV=npv)
merged <- merge(pu.df, npv.df, by='LGA')
merged$admin <- 1
pu.df <- merged

save(pu.df, file="./preprocessing/pu.df.RData")
load("./preprocessing/pu.df.RData")

#Make the final df for the optimisation

df <- pu.df[c("LGA", "NPV", "admin", "rank", "prob.property", "bid.price", "koala.w")]
save(df, file="./preprocessing/df.RData")













