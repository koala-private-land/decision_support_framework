library(sf)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(foreign)
library(spatialEco)
library(terra)
library(gurobi)
library(exactextractr)
library(remotes)
library(climateStability)
library(dplyr)

setwd("E:/Linkage/DSF code/private_land_conservation_DSF/")

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
properties <- readOGR("./preprocessing/Properties_NewPropID.shp")
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

propid <- pnts_properties$NewPropID
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
cond <- data.frame(properties$NewPropID, cond)
save(cond, file="./preprocessing/cond.RData")
load("./preprocessing/cond.RData")

##Connectivity
connect_rast <- raster("./raw_data/bba_31b_connectivity_V2_nsw_raw.tif")
#crs <- crs(connect_rast)

#apply a 1500m buffer around each site
#have to do this on either a server or in QGIS
#properties_buff_1500m <- gBuffer(properties, width=1500) 
properties_buff_1500m <- readOGR("./preprocessing/Properties_buffered.shp")
connect <- zonal.stats(properties_buff_1500m, connect_rast, stats="mean")
#connect <- extract(connect_rast, properties_buff_1500m, fun='mean') 
connect <- data.frame(properties_buff_1500m$NewPropID, connect)
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
dist_cov_prop <- data.frame(properties$NewPropID, dist_cov_prop)
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
dist_pa_prop <- data.frame(properties$NewPropID, dist_pa_prop)
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

#########Climate change risk#######
#https://rdrr.io/github/johnbaums/things/man/gdal_sd.html 
#Create raster stack
setwd("D:/Linkage/Data/Climate change koala habitat/Adelotus_brevis/Adelotus_brevis/All")
file_list = list.files(pattern = ".asc$", full.names = T, recursive = T)
r.stack <- raster::stack(file_list)
# sdR <- calc(r.stack, fun = sd)
meanR <- calc(r.stack, fun = mean)
# crs(sdR) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(meanR) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# #https://search.r-project.org/CRAN/refmans/climateStability/html/rescale0to1.html
# sdR_proj <- projectRaster(sdR, crs = crs(properties))
meanR_proj <- projectRaster(meanR, crs = crs(properties))
# relativeClimateStability <- rescale0to1(sdR_proj)
# relativeClimateStability_inv <- (relativeClimateStability*-1)+1
plot(meanR_proj)
r = meanR_proj
r[r<0.5] <- 0
r[r>=0.5] <- 1
# prop_sd_cc <- zonal.stats(properties, relativeClimateStability_inv, stats="mean")
# prop_sd_cc <- data.frame(properties$propid, prop_sd_cc)
# save(prop_sd_cc, file="./preprocessing/prop_sd_cc.RData")
# load("./preprocessing/prop_sd_cc.RData")
prop_mean_cc <- zonal.stats(properties, r, stats="mean")
prop_mean_cc <- data.frame(properties$NewPropID, prop_mean_cc)
save(prop_mean_cc, file="D:/Linkage/DSF code/private_land_conservation_DSF/preprocessing/prop_mean_cc.RData")
load("./preprocessing/prop_mean_cc.RData")

###IF EVERYTHING IS PROCESSED CAN SKIP TO HERE#########
#Merge everything together to create the table
#Load everything in 
load("./preprocessing/pnts_properties.RData")
propid <- pnts_properties$NewPropID
#This is m2
area <- pnts_properties$Shape_Area
koala <- read.csv("./preprocessing/propid_koalahabitat.csv")
load("./preprocessing/cond.RData")
load("./preprocessing/connect.RData")
load("./preprocessing/dist_cov_prop.RData")
load("./preprocessing/dist_pa_prop.RData")
load("./preprocessing/pnts_risk_v3.RData")
load("./preprocessing/prop_mean_cc.RData")

setwd("E:/Linkage/DSF code/private_land_conservation_DSF/")
#Some of this can be tied together and some of it has to be merged
pu.df <- data.frame(NewPropID=propid, area=area, risk = pnts_risk_v3)
colnames(pu.df) <- c("NewPropID", "area", "risk")
pu.df = pu.df[pu.df$NewPropID > 0,]
head(pu.df)

#cond
colnames(cond) <- c("NewPropID", "cond")
cond = cond[cond$NewPropID > 0,]
merged <- merge(pu.df, cond, by='NewPropID')
head(merged)
#connect
colnames(connect) <- c("NewPropID", "connect")
connect = connect[connect$NewPropID > 0,]
merged <- merge(merged, connect, by='NewPropID')
head(merged)
#dist_cov_prop
colnames(dist_cov_prop) <- c("NewPropID", "dist_cov")
merged <- merge(merged, dist_cov_prop, by='NewPropID')
head(merged)
#dist_pa_prop
colnames(dist_pa_prop) <- c("NewPropID", "dist_pa")
merged <- merge(merged, dist_pa_prop, by='NewPropID')
head(merged)
#prop_sd_cc
colnames(prop_mean_cc) <- c("NewPropID", "mean_cc")
merged <- merge(merged, prop_mean_cc, by='NewPropID')
head(merged)
#pu.df <- merged[c("propid", "area", "cond.x", "connect.x", "risk", "dist_cov", "dist_pa",  cond=risk = pnts_risk_v3, dist_cov = dist_cov_prop, dist_pa = dist_pa_prop, sd_cc = sd_cc)]
#colnames(pu.df) <- c("propid", "area", "risk", "cond", "connect", "dist_cov", "dist_pa", "sd_cc")

###Created this LGA intersected with properties layer in Arcmap 
properties.dbf <- read.dbf("./preprocessing/LGA_properties_intersection_CADID_retained.dbf", as.is = FALSE)
#Pull out just LGA's
properties.dbf_LGA <- properties.dbf[c("NewPropID", "CADID")]
#Remove duplicates
properties.dbf_LGA <- properties.dbf_LGA[!duplicated(properties.dbf_LGA), ]
merged <- merge(merged, properties.dbf_LGA, by='NewPropID')
pu.df <- merged[c("CADID", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "mean_cc")]

##Add the koala/conservation values
merged <- merge(pu.df, koala, by='NewPropID')
pu.df <- merged[c("CADID", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "mean_cc", "AREA")]
colnames(pu.df) <- c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "mean_cc", "koala")

#Do the ranking formula
pu.df$rank <- (pu.df$cond*pu.df$koala*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area

#Bring in the probability data
#Intersect the property data with the LGA's - needed to do this in QGis, couldn't get it working in R
#LGA_prop_intersect <- readOGR("./preprocessing/LGA_properties_intersection_CADID_retained.shp")
#Just for the table
LGA_prop_intersect.dbf <- read.dbf("./preprocessing/LGA_properties_intersection_CADID_retained.dbf", as.is = FALSE)
colnames(LGA_prop_intersect.dbf)[4] <- "CADID_LGA"
colnames(LGA_prop_intersect.dbf)[27] <- "CADID"
#properties.dbf_cost <- properties.dbf[c("propid", "LValHa")]
#properties.dbf_ag <- aggregate(properties.dbf_cost, by = list(properties.dbf$propid), FUN = "sum")

#Join property data with probability predictions
probs <- read.csv("E:/Linkage/DSF code/private_land_conservation_DSF/raw_data/Probability/spatial_predictions_inf.csv")
properties_prob <- merge(LGA_prop_intersect.dbf, probs, by='CADID')
####Need to turn bid price into total. WTA == $1000/ha/yr 
###Be careful of areas - here I have converted m to ha
properties_prob$MeanWTA.tot <- (properties_prob$Area_m/10000)*properties_prob$MeanProp*properties_prob$MeanWTA*1000
##All of this aggregating won't be necessary with Jonathan's new predictions which are at the "propid" level
MeanWTA.tot <- aggregate(x = properties_prob[c("MeanWTA.tot")], by = properties_prob[c("NewPropID")], FUN = mean)
MeanWTA.tot <- MeanWTA.tot[MeanWTA.tot$NewPropID > 0,]
properties_prob.ag <- aggregate(x = properties_prob[c("MeanAdopt", "MeanProp")], by = properties_prob[c("NewPropID")], FUN = mean)
properties_prob.ag <- properties_prob.ag[properties_prob.ag$NewPropID > 0,]
merged <- merge(properties_prob.ag, MeanWTA.tot, by='NewPropID')
#####Change MeanWTA.tot to in perpetuity values
###Divide MeanWTA.tot by a discount rate of 3.5% (0.035)
merged$MeanWTA.tot <- merged$MeanWTA.tot/0.035

#Merge with main dataframe
merged <- merge(pu.df, merged, by='NewPropID')
pu.df <- merged[c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "mean_cc", "koala", "rank","MeanAdopt", "MeanWTA.tot", "MeanProp")]
colnames(pu.df) <- c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "mean_cc", "koala", "rank", "prob.property", "bid.price", "MeanProp")
pu.df$koala.w <- pu.df$koala*pu.df$MeanProp
pu.df$koala.cc.w <- pu.df$mean_cc*pu.df$MeanProp

#checks
#This should be zero
list <- which(pu.df$area < pu.df$koala.w)
list

##Need to estimate the amount that would realistically be spent on each tender
# cost <- read.csv("./raw_data/bct_agreement_cost.csv")
# #Get total values of conservation tenders
# cost$mintot <- cost$total_area*cost$min_cost_ha
# cost$maxtot <- cost$total_area*cost$max_cost_ha
# length(unique(pu.df$LGA))
# lga <- unique(pu.df$LGA)
# npv <- rep(c(runif(c(1:98), min=mean(cost$mintot), max=mean(cost$maxtot))))
# npv.df <- data.frame(LGA=lga, NPV=npv)

##Using data from BCT
cost <- read.csv("./raw_data/bct_cost_data_26_7_22.csv")
#lga <- unique(pu.df$LGA)
#Get the LGA areas
properties.dbf <- read.dbf("./preprocessing/LGA_properties_intersection_CADID_retained.dbf", as.is = FALSE)
#Pull out just LGA's
LGA_area <- properties.dbf[c("CADID", "Area_H")]
LGA_area <- aggregate(LGA_area, by = list(LGA_area$CADID), FUN = "mean")
#Remove duplicates
LGA_area <- LGA_area[!duplicated(LGA_area), ]
LGA_area_mean <- mean(LGA_area$Area_H)
LGA_area$LGA_area_mean_multiplier <- LGA_area$Area_H/LGA_area_mean
#npv_mean <- mean(cost$Approx_tot_investment)
LGA_area$npv.mean <- mean(cost$Approx_tot_investment) 
LGA_area$npv.adj <- LGA_area$npv.mean*LGA_area$LGA_area_mean_multiplier
cost <- na.omit(cost)
BCT_mean <- mean(cost$Estimated_BCT_costs)
LGA_area$admin.mean <- BCT_mean
LGA_area$admin.adj <- BCT_mean*LGA_area$LGA_area_mean_multiplier
colnames(LGA_area) <- c("Group", "LGA", "Area", "Multiplier", "npv.mean", "npv.adj", "admin.mean", "admin.adj")
npv.df <- LGA_area[c("LGA", "npv.adj", "npv.mean", "admin.adj", "admin.mean")]

#cost <- na.omit(cost)
# #Total investment
# plot(cost$Approx_tot_investment, cost$Conservation_area_ha)
# #abline(lm(Approx_tot_investment ~ Conservation_area_ha, data = cost))
# model <- lm(cost$Approx_tot_investment ~ cost$Conservation_area_ha)
# summary(model)
# #Test linear assumptions
# plot(model)
# 
# # ##Log the data
# # plot(log(cost$Approx_tot_investment), log(cost$Conservation_area_ha))
# # model <- lm(log(cost$Approx_tot_investment) ~ log(cost$Conservation_area_ha))
# # summary(model)
# 
# #The perha
# summary(aov(cost$Total_per_ha~cost$Region,data=cost))
# summary(aov(cost$Approx_tot_investment~cost$Region,data=cost))
# ##Per ha cost against region
# summary(lm(cost$Total_per_ha ~ cost$Region))
# 
# cost_mr <- filter(cost, Region == "Murray-Riverina")
# summary(lm(cost_mr$Approx_tot_investment ~ cost_mr$Conservation_area_ha))
# plot(cost_mr$Approx_tot_investment ~ cost_mr$Conservation_area_ha)

merged <- merge(pu.df, npv.df, by='LGA')
pu.df <- merged

#Add the adjusted conservation value
#pu.df$koala_adj <- pu.df$koala.w*pu.df$sd_cc

save(pu.df, file="./preprocessing/pu.df.RData")
load("./preprocessing/pu.df.RData")

#Make the final df for the optimisation

df <- pu.df[c("LGA", "NewPropID", "npv.adj", "npv.mean", "admin.adj", "admin.mean", "rank", "prob.property", "bid.price", "koala.w", "koala.cc.w")]
save(df, file="./preprocessing/df.RData")



