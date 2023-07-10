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
library(stringr)
library(foreign)

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
properties <- readOGR("./preprocessing/Properties_NewPropID_19.shp")
properties_cov_rem <- readOGR("./preprocessing/Properties_NewPropID_cov_rem.shp")
crs <- proj4string(properties)
save(properties, file = "./preprocessing/properties_v2.RData")
load("./preprocessing/properties_v2.RData")

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

#Koala data, current
#Had to do this zonal statistics in Arcmap - R couldn't handle it
#This is with data from the KHAB
#koala <- read.csv("./preprocessing/propid_koalahabitat.csv")
# khab_thresh <- raster("E:/Linkage/DSF code/private_land_conservation_DSF/raw_data/khab_thresh.tif")
# koala_curr <- raster("E:/Linkage/Data/Climate change koala habitat/Phascolarctos_cinereus/Phascolarctos_cinereus/1km/realized/vet.suit.cur.asc")
# crs(koala_curr) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# koala_curr_proj <- projectRaster(koala_curr, crs = crs)
# plot(koala_curr_proj)
# pnts_koala <- extract(koala_curr_proj, points)
# save(pnts_koala, file="./preprocessing/pnts_koala.RData")
# load("./preprocessing/pnts_koala.RData")

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

##Deforestation risk
# defor.rast <- raster("./preprocessing/deforestation_risk_mariana.tif")
# defor <- zonal.stats(properties, defor.rast, stats="mean")
# summary <- summary(defor.rast)
# #Median is 2.633193e-02
# defor[is.na(defor)] <- 2.633193e-02
# defor <- data.frame(properties$NewPropID, defor)
# save(defor, file="./preprocessing/defor.RData")
# load("./preprocessing/defor.RData")

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

#########Climate change risk ---> code to use a mean value#######
#https://rdrr.io/github/johnbaums/things/man/gdal_sd.html
#Create raster stack
# setwd("E:/Linkage/Data/Climate change koala habitat/Phascolarctos_cinereus/Phascolarctos_cinereus/1km/dispersal/")
# file_list = list.files(pattern = ".asc$", full.names = T, recursive = TRUE)
# r.stack <- raster::stack(file_list)
# # sdR <- calc(r.stack, fun = sd)
# meanR <- calc(r.stack, fun = mean)
# # crs(sdR) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# crs(meanR) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# # #https://search.r-project.org/CRAN/refmans/climateStability/html/rescale0to1.html
# # sdR_proj <- projectRaster(sdR, crs = crs(properties))
# meanR_proj <- projectRaster(meanR, crs = crs(properties))
# # relativeClimateStability <- rescale0to1(sdR_proj)
# # relativeClimateStability_inv <- (relativeClimateStability*-1)+1
# plot(meanR_proj)
# r = meanR_proj
# r[r<0.5] <- 0
# r[r>=0.5] <- 1
# # prop_sd_cc <- zonal.stats(properties, relativeClimateStability_inv, stats="mean")
# # prop_sd_cc <- data.frame(properties$propid, prop_sd_cc)
# # save(prop_sd_cc, file="./preprocessing/prop_sd_cc.RData")
# # load("./preprocessing/prop_sd_cc.RData")
# prop_mean_cc <- zonal.stats(properties, r, stats="mean")
# prop_mean_cc <- data.frame(properties$NewPropID, prop_mean_cc)
# writeRaster(meanR_proj, "E:/Linkage/DSF code/private_land_conservation_DSF/preprocessing/meanR_proj.tif", overwrite=TRUE)
# save(prop_mean_cc, file="E:/Linkage/DSF code/private_land_conservation_DSF/preprocessing/prop_mean_cc.RData")
# load("./preprocessing/prop_mean_cc.RData")


##WE DON'T NEED THIS ANYMORE, SWITCHING TO REMP MODELS##
#Current climate conditions
# cc_curr <- raster("E:/Linkage/Data/Climate change koala habitat/Phascolarctos_cinereus/Phascolarctos_cinereus/1km/realized/vet.suit.cur.asc")
# crs(cc_curr) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# rast <- projectRaster(cc_curr, crs = crs)
# cc_curr_pnts <- extract(rast, points)
# save(cc_curr_pnts, file="E:/Linkage/DSF code/private_land_conservation_DSF/preprocessing/cc_curr_pnts.RData")
# 
# #########Code to go through all cc scenarios#######
# setwd("E:/Linkage/Data/Climate change koala habitat/Phascolarctos_cinereus/Phascolarctos_cinereus/1km/dispersal/")
# file_list = list.files(pattern = ".asc$", full.names = T, recursive = TRUE)
# #r.stack <- raster::stack(file_list)
# #crs(r.stack) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# for (i in 1:length(file_list)){
# rast<- raster(file_list[i])
# crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# rast <- projectRaster(rast, crs = crs)
# l <- gsub("./RCP85_", "", file_list[i])
# l <- gsub("/2085_realized.asc", "", l)
# text <- str_replace(l, "-", "_")
# name <- paste0("pnts_koala_", text)
# ext <- extract(rast, points)
# assign(paste0(name), ext)
# temp <- get(name)
# save(temp, file=paste0("E:/Linkage/DSF code/private_land_conservation_DSF/preprocessing/", name, ".RData"))
# }

#Bring in REMP model data
#Get the input file geodatabase
fgdb <- "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
#Call them in one by one
t0 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_Avg_t0")
CCCMAR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CCCMA_R1_t7")
CCCMAR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CCCMA_R2_t7")
CCCMAR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CCCMA_R3_t7")
CSIROMAR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CSIRO_R1_t7")
CSIROMAR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CSIRO_R2_t7")
CSIROMAR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CSIRO_R3_t7")
ECHAMR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_ECHAM_R1_t7")
ECHAMR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_ECHAM_R2_t7")
ECHAMR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_ECHAM_R3_t7")
MIROCR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_MIROC_R1_t7")
MIROCR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_MIROC_R2_t7")
MIROCR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_MIROC_R3_t7")
t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_Avg_t7")

###IF EVERYTHING IS PROCESSED CAN SKIP TO HERE#########
setwd("E:/Linkage/DSF code/private_land_conservation_DSF/")
#Merge everything together to create the table
#Load everything in 
load("./preprocessing/pnts_properties.RData")
propid <- pnts_properties$NewPropID
#This is m2
area <- pnts_properties$Shape_Area
#koala <- read.csv("./preprocessing/propid_koalahabitat.csv")
#load("./preprocessing/pnts_koala.RData")
load("./preprocessing/cond.RData")
load("./preprocessing/connect.RData")
load("./preprocessing/dist_cov_prop.RData")
load("./preprocessing/dist_pa_prop.RData")
load("./preprocessing/pnts_risk_v3.RData")
load("./preprocessing/prop_mean_cc.RData")
#load("./preprocessing/defor.RData")
properties_cov_rem <- readOGR("./preprocessing/Properties_NewPropID_cov_rem.shp")

#cc 
# load("./preprocessing/cc_curr_pnts.RData")
# load("./preprocessing/pnts_koala_cccma_cgcm31.RData")
# pnts_koala_cccma_cgcm31 <- temp
# load("./preprocessing/pnts_koala_ccsr_miroc32hi.RData")
# pnts_koala_ccsr_miroc32hi <- temp
# load("./preprocessing/pnts_koala_ccsr_miroc32med.RData")
# pnts_koala_ccsr_miroc32med <- temp
# load("./preprocessing/pnts_koala_cnrm_cm3.RData")
# pnts_koala_cnrm_cm3 <- temp
# load("./preprocessing/pnts_koala_csiro_mk30.RData")
# pnts_koala_csiro_mk30 <- temp
# load("./preprocessing/pnts_koala_gfdl_cm20.RData")
# pnts_koala_gfdl_cm20 <- temp
# load("./preprocessing/pnts_koala_gfdl_cm21.RData")
# pnts_koala_gfdl_cm21 <- temp
# load("./preprocessing/pnts_koala_giss_modeleh.RData")
# pnts_koala_giss_modeleh <- temp
# load("./preprocessing/pnts_koala_giss_modeler.RData")
# pnts_koala_giss_modeler <- temp
# load("./preprocessing/pnts_koala_iap_fgoals10g.RData")
# pnts_koala_iap_fgoals10g <- temp
# load("./preprocessing/pnts_koala_inm_cm30.RData")
# pnts_koala_inm_cm30 <- temp
# load("./preprocessing/pnts_koala_ipsl_cm4.RData")
# pnts_koala_ipsl_cm4 <- temp
# load("./preprocessing/pnts_koala_mpi_echam5.RData")
# pnts_koala_mpi_echam5 <- temp
# load("./preprocessing/pnts_koala_mri_cgcm232a.RData")
# pnts_koala_mri_cgcm232a <- temp
# load("./preprocessing/pnts_koala_ncar_ccsm30.RData")
# pnts_koala_ncar_ccsm30 <- temp
# load("./preprocessing/pnts_koala_ncar_pcm1.RData")
# pnts_koala_ncar_pcm1 <- temp
# load("./preprocessing/pnts_koala_ukmo_hadcm3.RData")
# pnts_koala_ukmo_hadcm3 <- temp
# load("./preprocessing/pnts_koala_ukmo_hadgem1.RData")
# pnts_koala_ukmo_hadgem1 <- temp

#Bring in REMP model data
#Get the input file geodatabase
fgdb <- "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
#Call them in one by one
t0 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_Avg_t0")
CCCMAR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CCCMA_R1_t7")
CCCMAR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CCCMA_R2_t7")
CCCMAR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CCCMA_R3_t7")
CSIROMAR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CSIRO_R1_t7")
CSIROMAR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CSIRO_R2_t7")
CSIROMAR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_CSIRO_R3_t7")
ECHAMR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_ECHAM_R1_t7")
ECHAMR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_ECHAM_R2_t7")
ECHAMR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_ECHAM_R3_t7")
MIROCR1t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_MIROC_R1_t7")
MIROCR2t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_MIROC_R2_t7")
MIROCR3t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_MIROC_R3_t7")
t7 <- sf::st_read(dsn = "R:/KPRIVATE19-A2212/data/remp_models_2023_properties/kitl_prop_unmasked.gdb", layer = "kitl_prop_Avg_t7")

setwd("E:/Linkage/DSF code/private_land_conservation_DSF/")
#Some of this can be tied together and some of it has to be merged
# pu.df <- data.frame(NewPropID=propid, area=area, risk = pnts_risk_v3, koala = cc_curr_pnts, cccma_cgcm31 = pnts_koala_cccma_cgcm31, ccsr_miroc32hi = pnts_koala_ccsr_miroc32hi, ccsr_miroc32med = pnts_koala_ccsr_miroc32med, cnrm_cm3 = pnts_koala_cnrm_cm3, csiro_mk30 = pnts_koala_csiro_mk30, gfdl_cm20 = pnts_koala_gfdl_cm20, gfdl_cm21 = pnts_koala_gfdl_cm21, giss_modeleh = pnts_koala_giss_modeleh, giss_modeler = pnts_koala_giss_modeler, iap_fgoals10g = pnts_koala_iap_fgoals10g, inm_cm30 = pnts_koala_inm_cm30, ipsl_cm4 = pnts_koala_ipsl_cm4, mpi_echam5 = pnts_koala_mpi_echam5, mri_cgcm232a = pnts_koala_mri_cgcm232a, ncar_ccsm30 = pnts_koala_ncar_ccsm30, ncar_pcm1 = pnts_koala_ncar_pcm1, ukmo_hadcm3 = pnts_koala_ukmo_hadcm3, ukmo_hadgem1 = pnts_koala_ukmo_hadgem1)
# colnames(pu.df) <- c("NewPropID", "area", "risk", "koala_curr", "cccma_cgcm31", "ccsr_miroc32hi", "ccsr_miroc32med", "cnrm_cm3", "csiro_mk30", "gfdl_cm20", "gfdl_cm21", "giss_modeleh", "giss_modeler", "iap_fgoals10g", "inm_cm30", "ipsl_cm4", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1")
# pu.df = pu.df[pu.df$NewPropID > 0,]
# head(pu.df)

pu.df <- data.frame(NewPropID=propid, area=area, risk = pnts_risk_v3)
colnames(pu.df) <- c("NewPropID", "area", "risk")
pu.df = pu.df[pu.df$NewPropID > 0,]
head(pu.df)

#t0/Baseline
merged <- merge(pu.df, t0, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0")
#CCCMAR1t7
merged <- merge(merged, CCCMAR1t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7")
#CCCMAR2t7
merged <- merge(merged, CCCMAR2t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7")
#CCCMAR3t7
merged <- merge(merged, CCCMAR3t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7")
#CSIROMAR1t7
merged <- merge(merged, CSIROMAR1t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7")
#CSIROMAR2t7
merged <- merge(merged, CSIROMAR2t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7")
#CSIROMAR3t7
merged <- merge(merged, CSIROMAR3t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7")
#ECHAMR1t7
merged <- merge(merged, ECHAMR1t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7")
#ECHAMR2t7
merged <- merge(merged, ECHAMR2t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7","MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7")
#ECHAMR3t7
merged <- merge(merged, ECHAMR3t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7")
#MIROCR1t7
merged <- merge(merged, MIROCR1t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7")
#MIROCR2t7
merged <- merge(merged, MIROCR2t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MIROCR2t7")
#MIROCR3t7
merged <- merge(merged, MIROCR3t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MIROCR2t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MIROCR2t7", "MIROCR3t7")
#t7/average
merged <- merge(merged, t7, by='NewPropID')
merged <- merged[c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MIROCR2t7", "MIROCR3t7", "MEAN")]
colnames(merged) <- c("NewPropID", "area", "risk", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MIROCR2t7", "MIROCR3t7", "t7")

#cond
colnames(cond) <- c("NewPropID", "cond")
cond = cond[cond$NewPropID > 0,]
merged <- merge(merged, cond, by='NewPropID')
head(merged)
#connect
colnames(connect) <- c("NewPropID", "connect")
connect = connect[connect$NewPropID > 0,]
merged <- merge(merged, connect, by='NewPropID')
###NEED TO GO BACK AND FIX/LOOK AT ZEROS
head(merged)
merged[is.na(merged)] <- 0.001
#dist_cov_prop
colnames(dist_cov_prop) <- c("NewPropID", "dist_cov")
merged <- merge(merged, dist_cov_prop, by='NewPropID')
head(merged)
#dist_pa_prop
colnames(dist_pa_prop) <- c("NewPropID", "dist_pa")
merged <- merge(merged, dist_pa_prop, by='NewPropID')
head(merged)
#prop_sd_cc
# colnames(prop_mean_cc) <- c("NewPropID", "mean_cc")
# merged <- merge(merged, prop_mean_cc, by='NewPropID')
# head(merged)
#defor
# colnames(defor) <- c("NewPropID", "defor")
# merged <- merge(merged, defor, by='NewPropID')
# head(merged)

#pu.df <- merged[c("propid", "area", "cond.x", "connect.x", "risk", "dist_cov", "dist_pa",  cond=risk = pnts_risk_v3, dist_cov = dist_cov_prop, dist_pa = dist_pa_prop, sd_cc = sd_cc)]
#colnames(pu.df) <- c("propid", "area", "risk", "cond", "connect", "dist_cov", "dist_pa", "sd_cc")

###Created this LGA intersected with properties layer in Arcmap 
properties.dbf <- read.dbf("./preprocessing/Propid_and_LGAid.dbf", as.is = FALSE)
#Pull out just LGA's
properties.dbf_LGA <- properties.dbf[c("NewPropID", "LGA")]
merged <- merge(merged, properties.dbf_LGA, by='NewPropID')
pu.df <- merged[c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MIROCR2t7", "MIROCR3t7", "t7")]

##Add in the 19 priority koala areas
nineteen_areas <- properties[c("NewPropID", "Invest_PPA")]
merged <- merge(pu.df, nineteen_areas, by = "NewPropID")
pu.df <- merged[c("LGA", "Invest_PPA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "t0", "CCCMAR1t7", "CCCMAR2t7", "CCCMAR3t7", "CSIROMAR1t7", "CSIROMAR2t7", "CSIROMAR3t7", "ECHAMR1t7", "ECHAMR2t7", "ECHAMR3t7", "MIROCR1t7", "MIROCR2t7", "MIROCR3t7", "t7")]


#Add in area of koala habitat
#merged <- merge(koala, pu.df, by = "NewPropID")
#merged$koala_curr_adj <- merged$area*merged$MEAN

##Add the koala/conservation values
#merged <- merge(pu.df, koala, by='NewPropID')
#pu.df <- pu.df[c("CADID", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "mean_cc", "koala", "defor")]
#colnames(pu.df) <- c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala_curr", "cccma_cgcm31", "ccsr_miroc32hi", "ccsr_miroc32med", "cnrm_cm3", "csiro_mk30", "gfdl_cm20", "gfdl_cm21", "giss_modeleh", "giss_modeler", "iap_fgoals10g", "inm_cm30", "ipsl_cm4", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1")

#pu.df <- merged[c("CADID", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala_curr_adj", "koala_curr", "cccma_cgcm31", "ccsr_miroc32hi", "ccsr_miroc32med", "cnrm_cm3", "csiro_mk30", "gfdl_cm20", "gfdl_cm21", "giss_modeleh", "giss_modeler", "iap_fgoals10g", "inm_cm30", "ipsl_cm4", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1")]
#colnames(pu.df) <- c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala_area", "koala_curr", "cccma_cgcm31", "ccsr_miroc32hi", "ccsr_miroc32med", "cnrm_cm3", "csiro_mk30", "gfdl_cm20", "gfdl_cm21", "giss_modeleh", "giss_modeler", "iap_fgoals10g", "inm_cm30", "ipsl_cm4", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1")

####If we just want to optimise persistance (remp models) we don't need to do this
# pu.df$koala_curr <- pu.df$koala_area*pu.df$koala_curr
# pu.df$cccma_cgcm31 <- pu.df$koala_area*pu.df$cccma_cgcm31
# pu.df$ccsr_miroc32hi  <- pu.df$koala_area*pu.df$ccsr_miroc32hi 
# pu.df$ccsr_miroc32med  <- pu.df$koala_area*pu.df$ccsr_miroc32med
# pu.df$cnrm_cm3  <- pu.df$koala_area*pu.df$cnrm_cm3
# pu.df$csiro_mk30  <- pu.df$koala_area*pu.df$csiro_mk30
# pu.df$gfdl_cm20  <- pu.df$koala_area*pu.df$gfdl_cm20
# pu.df$gfdl_cm21  <- pu.df$koala_area*pu.df$gfdl_cm21
# pu.df$giss_modeleh  <- pu.df$koala_area*pu.df$giss_modeleh
# pu.df$giss_modeler  <- pu.df$koala_area*pu.df$giss_modeler
# pu.df$iap_fgoals10g  <- pu.df$koala_area*pu.df$iap_fgoals10g
# pu.df$inm_cm30  <- pu.df$koala_area*pu.df$inm_cm30
# pu.df$ipsl_cm4  <- pu.df$koala_area*pu.df$ipsl_cm4
# pu.df$mpi_echam5  <- pu.df$koala_area*pu.df$mpi_echam5
# pu.df$mri_cgcm232a  <- pu.df$koala_area*pu.df$mri_cgcm232a
# pu.df$ncar_ccsm30 <- pu.df$koala_area*pu.df$ncar_ccsm30
# pu.df$ncar_pcm1  <- pu.df$koala_area*pu.df$ncar_pcm1
# pu.df$ukmo_hadcm3  <- pu.df$koala_area*pu.df$ukmo_hadcm3
# pu.df$ukmo_hadgem1  <- pu.df$koala_area*pu.df$ukmo_hadgem1

###Make a column for koala values adjusted by deforestation
#pu.df$defor.resc <- (pu.df$defor - min(pu.df$defor)) / (max(pu.df$defor) - min(pu.df$defor)) 
#pu.df$defor.resc.invt <- (pu.df$defor.resc*-1)+1
#pu.df$koala.defor <- pu.df$koala*pu.df$defor.resc.invt
#pu.df$koala.defor.calc <- pu.df$koala*(1-pu.df$defor)

###Make a column for cc koala values adjusted by deforestation
# pu.df$defor.resc <- (pu.df$defor - min(pu.df$defor)) / (max(pu.df$defor) - min(pu.df$defor)) 
# pu.df$defor.resc.invt <- (pu.df$defor.resc*-1)+1
# pu.df$koala.defor.cc <- pu.df$mean_cc*pu.df$defor.resc.invt
# pu.df$cccma_cgcm31.defor.calc <- pu.df$cccma_cgcm31*(1-pu.df$defor)
# pu.df$ccsr_miroc32hi.defor.calc <- pu.df$ccsr_miroc32hi*(1-pu.df$defor)
# pu.df$ccsr_miroc32med.defor.calc <- pu.df$ccsr_miroc32med*(1-pu.df$defor)
# pu.df$cnrm_cm3.defor.calc <- pu.df$cnrm_cm3*(1-pu.df$defor)
# pu.df$csiro_mk30.defor.calc <- pu.df$csiro_mk30*(1-pu.df$defor)
# pu.df$gfdl_cm20.defor.calc <- pu.df$gfdl_cm20*(1-pu.df$defor)
# pu.df$gfdl_cm21.defor.calc <- pu.df$gfdl_cm21*(1-pu.df$defor)
# pu.df$giss_modeleh.defor.calc <- pu.df$giss_modeleh*(1-pu.df$defor)
# pu.df$giss_modeler.defor.calc <- pu.df$giss_modeler*(1-pu.df$defor)
# pu.df$iap_fgoals10g.defor.calc <- pu.df$iap_fgoals10g*(1-pu.df$defor)
# pu.df$inm_cm30.defor.calc <- pu.df$inm_cm30*(1-pu.df$defor)
# pu.df$ipsl_cm4.defor.calc <- pu.df$ipsl_cm4*(1-pu.df$defor)
# pu.df$mpi_echam5.defor.calc <- pu.df$mpi_echam5*(1-pu.df$defor)
# pu.df$mri_cgcm232a.defor.calc <- pu.df$mri_cgcm232a*(1-pu.df$defor)
# pu.df$ncar_ccsm30.defor.calc <- pu.df$ncar_ccsm30*(1-pu.df$defor)
# pu.df$ncar_pcm1.defor.calc <- pu.df$ncar_pcm1*(1-pu.df$defor)
# pu.df$ukmo_hadcm3.defor.calc <- pu.df$ukmo_hadcm3*(1-pu.df$defor)
# pu.df$ukmo_hadgem1.defor.calc <- pu.df$ukmo_hadgem1*(1-pu.df$defor)


######Make the codes for distance that are in the BCT document!
pu.df$dist_cov
pu.df$dist_cov[pu.df$dist_cov >= 0 & pu.df$dist_cov <= 500] <- 100  
pu.df$dist_cov[pu.df$dist_cov > 500 & pu.df$dist_cov <= 1000] <- 95  
pu.df$dist_cov[pu.df$dist_cov > 1000 & pu.df$dist_cov <= 1500] <- 90  
pu.df$dist_cov[pu.df$dist_cov > 1500 & pu.df$dist_cov <= 2000] <- 85  
pu.df$dist_cov[pu.df$dist_cov > 2000 & pu.df$dist_cov <= 2500] <- 80  
pu.df$dist_cov[pu.df$dist_cov > 2500 & pu.df$dist_cov <= 3000] <- 75  
pu.df$dist_cov[pu.df$dist_cov > 3000 & pu.df$dist_cov <= 3500] <- 70  
pu.df$dist_cov[pu.df$dist_cov > 3500 & pu.df$dist_cov <= 4000] <- 65  
pu.df$dist_cov[pu.df$dist_cov > 4000 & pu.df$dist_cov <= 4500] <- 60  
pu.df$dist_cov[pu.df$dist_cov > 4500 & pu.df$dist_cov <= 5000] <- 55  
pu.df$dist_cov[pu.df$dist_cov > 5000 & pu.df$dist_cov <= 5500] <- 50  
pu.df$dist_cov[pu.df$dist_cov > 5500 & pu.df$dist_cov <= 6000] <- 45  
pu.df$dist_cov[pu.df$dist_cov > 6000 & pu.df$dist_cov <= 6500] <- 40  
pu.df$dist_cov[pu.df$dist_cov > 6500 & pu.df$dist_cov <= 7000] <- 35  
pu.df$dist_cov[pu.df$dist_cov > 7000 & pu.df$dist_cov <= 7500] <- 30  
pu.df$dist_cov[pu.df$dist_cov > 7500 & pu.df$dist_cov <= 8000] <- 25 
pu.df$dist_cov[pu.df$dist_cov > 8000 & pu.df$dist_cov <= 8500] <- 20
pu.df$dist_cov[pu.df$dist_cov > 8500 & pu.df$dist_cov <= 9000] <- 15
pu.df$dist_cov[pu.df$dist_cov > 9000 & pu.df$dist_cov <= 9500] <- 10
pu.df$dist_cov[pu.df$dist_cov > 9500 & pu.df$dist_cov <= 10000] <- 5  
pu.df$dist_cov[pu.df$dist_cov > 10000] <- 1 
pu.df$dist_cov[pu.df$dist_cov <= 0] <- 100

pu.df$dist_pa
pu.df$dist_pa[pu.df$dist_pa >= 0 & pu.df$dist_pa <= 500] <- 100  
pu.df$dist_pa[pu.df$dist_pa > 500 & pu.df$dist_pa <= 1000] <- 95  
pu.df$dist_pa[pu.df$dist_pa > 1000 & pu.df$dist_pa <= 1500] <- 90  
pu.df$dist_pa[pu.df$dist_pa > 1500 & pu.df$dist_pa <= 2000] <- 85  
pu.df$dist_pa[pu.df$dist_pa > 2000 & pu.df$dist_pa <= 2500] <- 80  
pu.df$dist_pa[pu.df$dist_pa > 2500 & pu.df$dist_pa <= 3000] <- 75  
pu.df$dist_pa[pu.df$dist_pa > 3000 & pu.df$dist_pa <= 3500] <- 70  
pu.df$dist_pa[pu.df$dist_pa > 3500 & pu.df$dist_pa <= 4000] <- 65  
pu.df$dist_pa[pu.df$dist_pa > 4000 & pu.df$dist_pa <= 4500] <- 60  
pu.df$dist_pa[pu.df$dist_pa > 4500 & pu.df$dist_pa <= 5000] <- 55  
pu.df$dist_pa[pu.df$dist_pa > 5000 & pu.df$dist_pa <= 5500] <- 50  
pu.df$dist_pa[pu.df$dist_pa > 5500 & pu.df$dist_pa <= 6000] <- 45  
pu.df$dist_pa[pu.df$dist_pa > 6000 & pu.df$dist_pa <= 6500] <- 40  
pu.df$dist_pa[pu.df$dist_pa > 6500 & pu.df$dist_pa <= 7000] <- 35  
pu.df$dist_pa[pu.df$dist_pa > 7000 & pu.df$dist_pa <= 7500] <- 30  
pu.df$dist_pa[pu.df$dist_pa > 7500 & pu.df$dist_pa <= 8000] <- 25 
pu.df$dist_pa[pu.df$dist_pa > 8000 & pu.df$dist_pa <= 8500] <- 20
pu.df$dist_pa[pu.df$dist_pa > 8500 & pu.df$dist_pa <= 9000] <- 15
pu.df$dist_pa[pu.df$dist_pa > 9000 & pu.df$dist_pa <= 9500] <- 10
pu.df$dist_pa[pu.df$dist_pa > 9500 & pu.df$dist_pa <= 10000] <- 5  
pu.df$dist_pa[pu.df$dist_pa > 10000] <- 1 
pu.df$dist_pa[pu.df$dist_pa <= 0] <- 100 


#Do the ranking formula
pu.df$rank.t0 <- (pu.df$cond*pu.df$t0*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area

##Do the rank considering defor
#pu.df$rank.defor <- (pu.df$cond*pu.df$koala.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area

##Do the rank considering cc
# pu.df$rank.cccma_cgcm31 <- (pu.df$cond*pu.df$cccma_cgcm31*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ccsr_miroc32hi <- (pu.df$cond*pu.df$ccsr_miroc32hi*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ccsr_miroc32med <- (pu.df$cond*pu.df$ccsr_miroc32med*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.cnrm_cm3 <- (pu.df$cond*pu.df$cnrm_cm3*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.csiro_mk30 <- (pu.df$cond*pu.df$csiro_mk30*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.gfdl_cm20 <- (pu.df$cond*pu.df$gfdl_cm20*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.gfdl_cm21 <- (pu.df$cond*pu.df$gfdl_cm21*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.giss_modeleh <- (pu.df$cond*pu.df$giss_modeleh*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.giss_modeler <- (pu.df$cond*pu.df$giss_modeler*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.iap_fgoals10g <- (pu.df$cond*pu.df$iap_fgoals10g*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.inm_cm30 <- (pu.df$cond*pu.df$inm_cm30*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ipsl_cm4 <- (pu.df$cond*pu.df$ipsl_cm4*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.mpi_echam5 <- (pu.df$cond*pu.df$mpi_echam5*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.mri_cgcm232a <- (pu.df$cond*pu.df$mri_cgcm232a*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ncar_ccsm30 <- (pu.df$cond*pu.df$ncar_ccsm30*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ncar_pcm1 <- (pu.df$cond*pu.df$ncar_pcm1*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ukmo_hadcm3 <- (pu.df$cond*pu.df$ukmo_hadcm3*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ukmo_hadgem1 <- (pu.df$cond*pu.df$ukmo_hadgem1*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area

##Do the rank considering cc
pu.df$rank.CCCMAR1t7 <- ((pu.df$cond*pu.df$CCCMAR1t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.CCCMAR2t7  <- ((pu.df$cond*pu.df$CCCMAR2t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.CCCMAR3t7 <- ((pu.df$cond*pu.df$CCCMAR3t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.CSIROMAR1t7 <- ((pu.df$cond*pu.df$CSIROMAR1t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.CSIROMAR2t7 <- ((pu.df$cond*pu.df$CSIROMAR2t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.CSIROMAR3t7 <- ((pu.df$cond*pu.df$CSIROMAR3t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.ECHAMR1t7 <- ((pu.df$cond*pu.df$ECHAMR1t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.ECHAMR2t7 <- ((pu.df$cond*pu.df$ECHAMR2t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.ECHAMR3t7 <- ((pu.df$cond*pu.df$ECHAMR3t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.MIROCR1t7 <- ((pu.df$cond*pu.df$MIROCR1t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.MIROCR2t7 <- ((pu.df$cond*pu.df$MIROCR2t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.MIROCR3t7 <- ((pu.df$cond*pu.df$MIROCR3t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area
pu.df$rank.t7 <- ((pu.df$cond*pu.df$t7*0.8)+((pu.df$connect+(pu.df$dist_cov/100)+(pu.df$dist_pa/100))*0.2))*1*pu.df$risk*pu.df$area

##Do the rank considering cc and deforestation
# pu.df$rank.cccma_cgcm31.defor <- (pu.df$cond*pu.df$cccma_cgcm31.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ccsr_miroc32hi.defor <- (pu.df$cond*pu.df$ccsr_miroc32hi.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ccsr_miroc32med.defor <- (pu.df$cond*pu.df$ccsr_miroc32med.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.cnrm_cm3.defor <- (pu.df$cond*pu.df$cnrm_cm3.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.csiro_mk30.defor <- (pu.df$cond*pu.df$csiro_mk30.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.gfdl_cm20.defor <- (pu.df$cond*pu.df$gfdl_cm20.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.gfdl_cm21.defor <- (pu.df$cond*pu.df$gfdl_cm21.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.giss_modeleh.defor <- (pu.df$cond*pu.df$giss_modeleh.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.giss_modeler.defor <- (pu.df$cond*pu.df$giss_modeler.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.iap_fgoals10g.defor <- (pu.df$cond*pu.df$iap_fgoals10g.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.inm_cm30.defor <- (pu.df$cond*pu.df$inm_cm30.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ipsl_cm4.defor <- (pu.df$cond*pu.df$ipsl_cm4.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.mpi_echam5.defor <- (pu.df$cond*pu.df$mpi_echam5.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.mri_cgcm232a.defor <- (pu.df$cond*pu.df$mri_cgcm232a.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ncar_ccsm30.defor <- (pu.df$cond*pu.df$ncar_ccsm30.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ncar_pcm1.defor <- (pu.df$cond*pu.df$ncar_pcm1.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ukmo_hadcm3.defor <- (pu.df$cond*pu.df$ukmo_hadcm3.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area
# pu.df$rank.ukmo_hadgem1.defor <- (pu.df$cond*pu.df$ukmo_hadgem1.defor.calc*0.8)+((pu.df$connect+pu.df$dist_cov+pu.df$dist_pa)*0.2)*1*pu.df$risk*pu.df$area

#Bring in the probability data
#Intersect the property data with the LGA's - needed to do this in QGis, couldn't get it working in R
#LGA_prop_intersect <- readOGR("./preprocessing/LGA_properties_intersection_CADID_retained.shp")
#Just for the table
# LGA_prop_intersect.dbf <- read.dbf("./preprocessing/LGA_properties_intersection_CADID_retained.dbf", as.is = FALSE)
# colnames(LGA_prop_intersect.dbf)[4] <- "CADID_LGA"
# colnames(LGA_prop_intersect.dbf)[27] <- "CADID"
#properties.dbf_cost <- properties.dbf[c("propid", "LValHa")]
#properties.dbf_ag <- aggregate(properties.dbf_cost, by = list(properties.dbf$propid), FUN = "sum")

#Join property data with probability predictions
#probs <- read.csv("E:/Linkage/DSF code/private_land_conservation_DSF/raw_data/Probability/spatial_predictions_inf.csv")
#Updated predictions
probs <- read.csv("E:/Linkage/DSF code/private_land_conservation_DSF/preprocessing/bid_predictions/spatial_predictions_inf.csv")
# properties_prob <- merge(LGA_prop_intersect.dbf, probs, by='CADID')
# ####Need to turn bid price into total. WTA == $1000/ha/yr 
# ###Be careful of areas - here I have converted m to ha
#properties_prob$MeanWTA.tot <- (probs$Area_m/10000)*properties_prob$MeanProp*properties_prob$MeanWTA*1000
# ##All of this aggregating won't be necessary with Jonathan's new predictions which are at the "propid" level
# MeanWTA.tot <- aggregate(x = properties_prob[c("MeanWTA.tot")], by = properties_prob[c("NewPropID")], FUN = mean)
# MeanWTA.tot <- MeanWTA.tot[MeanWTA.tot$NewPropID > 0,]
# properties_prob.ag <- aggregate(x = properties_prob[c("MeanAdopt", "MeanProp")], by = properties_prob[c("NewPropID")], FUN = mean)
# properties_prob.ag <- properties_prob.ag[properties_prob.ag$NewPropID > 0,]
# merged <- merge(properties_prob.ag, MeanWTA.tot, by='NewPropID')
merged <- merge(pu.df, probs, by='NewPropID')
merged$MeanWTA.tot <- (merged$area/10000)*merged$MeanProp*merged$MeanWTA*1000

#####Change MeanWTA.tot to in perpetuity values
###Divide MeanWTA.tot by a discount rate of 3.5% (0.035)
merged$MeanWTA.tot <- merged$MeanWTA.tot/0.035

##Add land value
land_v <- read.dbf("./preprocessing/props_pu_private_2ha_not_intense_not_water_notroadrail_predictors.dbf", as.is = FALSE)
merged <- merge(merged, land_v, by='NewPropID')
merged$LValHa <- merged$LValHa*(merged$area/10000)

#Merge with main dataframe
#merged <- merge(pu.df, merged, by='NewPropID')
#pu.df <- merged[c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala", "defor", "koala.defor.calc", "koala.defor.calc.cc", "rank", "rank.defor", "rank.cc.defor", "rank.cc", "MeanAdopt", "MeanWTA.tot", "MeanProp", "LValHa", "cccma_cgcm31", "ccsr_miroc32hi", "ccsr_miroc32med", "cnrm_cm3", "csiro_mk30", "gfdl_cm20", "gfdl_cm21", "giss_modeleh", "giss_modeler", "iap_fgoals10g", "inm_cm30", "ipsl_cm4", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1")]
#pu.df <- merged[c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala_area", "koala_curr", "rank", "cccma_cgcm31",  "rank.cccma_cgcm31", "ccsr_miroc32hi", "rank.ccsr_miroc32hi", "ccsr_miroc32med", "rank.ccsr_miroc32med", "cnrm_cm3", "rank.cnrm_cm3", "csiro_mk30",  "rank.csiro_mk30", "gfdl_cm20",  "rank.gfdl_cm20", "gfdl_cm21",  "rank.gfdl_cm21", "giss_modeleh",  "rank.giss_modeleh", "giss_modeler",  "rank.giss_modeler", "iap_fgoals10g",  "rank.iap_fgoals10g", "inm_cm30",  "rank.inm_cm30", "ipsl_cm4",  "rank.ipsl_cm4", "mpi_echam5",  "rank.mpi_echam5", "mri_cgcm232a",  "rank.mri_cgcm232a", "ncar_ccsm30",  "rank.ncar_ccsm30", "ncar_pcm1",  "rank.ncar_pcm1", "ukmo_hadcm3",  "rank.ukmo_hadcm3", "ukmo_hadgem1",  "rank.ukmo_hadgem1", "MeanAdopt", "MeanWTA.tot", "MeanProp", "LValHa")] 
#colnames(pu.df) <- c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala", "defor", "koala.defor.calc", "koala.defor.calc.cc", "rank", "rank.defor", "rank.cc", "rank.cc.defor", "prob.property", "bid.price", "MeanProp", "LValHa", "cccma_cgcm31", "ccsr_miroc32hi", "ccsr_miroc32med", "cnrm_cm3", "csiro_mk30", "gfdl_cm20", "gfdl_cm21", "giss_modeleh", "giss_modeler", "iap_fgoals10g", "inm_cm30", "ipsl_cm4", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1")
pu.df <- merged[c("LGA", "Invest_PPA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "t0", "rank.t0", "CCCMAR1t7", "rank.CCCMAR1t7", "CCCMAR2t7", "rank.CCCMAR2t7", "CCCMAR3t7", "rank.CCCMAR3t7", "CSIROMAR1t7", "rank.CSIROMAR1t7", "CSIROMAR2t7", "rank.CSIROMAR2t7",  "CSIROMAR3t7", "rank.CSIROMAR3t7",  "ECHAMR1t7", "rank.ECHAMR1t7",  "ECHAMR2t7", "rank.ECHAMR2t7",  "ECHAMR3t7", "rank.ECHAMR3t7",  "MIROCR1t7", "rank.MIROCR1t7",  "MIROCR2t7", "rank.MIROCR2t7",  "MIROCR3t7", "rank.MIROCR3t7",  "t7", "rank.t7", "MeanAdopt", "MeanWTA.tot", "MeanProp", "LValHa")]

#colnames(pu.df) <- c("LGA", "NewPropID", "area", "cond", "connect", "risk", "dist_cov", "dist_pa", "koala", "defor", "koala.defor.calc", "koala.defor.calc.cc", "rank", "rank.defor", "rank.cc", "rank.cc.defor", "prob.property", "bid.price", "MeanProp", "LValHa", "cccma_cgcm31", "ccsr_miroc32hi", "ccsr_miroc32med", "cnrm_cm3", "csiro_mk30", "gfdl_cm20", "gfdl_cm21", "giss_modeleh", "giss_modeler", "iap_fgoals10g", "inm_cm30", "ipsl_cm4", "mpi_echam5", "mri_cgcm232a", "ncar_ccsm30", "ncar_pcm1", "ukmo_hadcm3", "ukmo_hadgem1")

#Make all benefits proportional to what landowners would coven
pu.df$t0.w <- pu.df$t0*pu.df$MeanProp
pu.df$CCCMAR1t7.w <- pu.df$CCCMAR1t7*pu.df$MeanProp
pu.df$CCCMAR2t7.w <- pu.df$CCCMAR2t7*pu.df$MeanProp
pu.df$CCCMAR3t7.w <- pu.df$CCCMAR3t7*pu.df$MeanProp
pu.df$CSIROMAR1t7.w <- pu.df$CSIROMAR1t7*pu.df$MeanProp
pu.df$CSIROMAR2t7.w <- pu.df$CSIROMAR2t7*pu.df$MeanProp
pu.df$CSIROMAR3t7.w <- pu.df$CSIROMAR3t7*pu.df$MeanProp
pu.df$ECHAMR1t7.w <- pu.df$ECHAMR1t7*pu.df$MeanProp
pu.df$ECHAMR2t7.w <- pu.df$ECHAMR2t7*pu.df$MeanProp
pu.df$ECHAMR3t7.w <- pu.df$ECHAMR3t7*pu.df$MeanProp
pu.df$MIROCR1t7.w <- pu.df$MIROCR1t7*pu.df$MeanProp
pu.df$MIROCR2t7.w <- pu.df$MIROCR2t7*pu.df$MeanProp
pu.df$MIROCR3t7.w <- pu.df$MIROCR3t7*pu.df$MeanProp
pu.df$t7.w <- pu.df$t7*pu.df$MeanProp
pu.df$area.w <- pu.df$area*pu.df$MeanProp

# pu.df$koala_curr.w <- pu.df$koala_curr*pu.df$MeanProp
# pu.df$cccma_cgcm31.w <- pu.df$cccma_cgcm31*pu.df$MeanProp
# pu.df$ccsr_miroc32hi.w <- pu.df$ccsr_miroc32hi*pu.df$MeanProp
# pu.df$ccsr_miroc32med.w <- pu.df$ccsr_miroc32med*pu.df$MeanProp
# pu.df$cnrm_cm3.w <- pu.df$cnrm_cm3*pu.df$MeanProp
# pu.df$csiro_mk30.w <- pu.df$csiro_mk30*pu.df$MeanProp
# pu.df$gfdl_cm20.w <- pu.df$gfdl_cm20*pu.df$MeanProp
# pu.df$gfdl_cm21.w <- pu.df$gfdl_cm21*pu.df$MeanProp
# pu.df$giss_modeleh.w <- pu.df$giss_modeleh*pu.df$MeanProp
# pu.df$giss_modeler.w <- pu.df$giss_modeler*pu.df$MeanProp
# pu.df$iap_fgoals10g.w <- pu.df$iap_fgoals10g*pu.df$MeanProp
# pu.df$inm_cm30.w <- pu.df$inm_cm30*pu.df$MeanProp
# pu.df$ipsl_cm4.w <- pu.df$ipsl_cm4*pu.df$MeanProp
# pu.df$mpi_echam5.w <- pu.df$mpi_echam5*pu.df$MeanProp
# pu.df$mri_cgcm232a.w <- pu.df$mri_cgcm232a*pu.df$MeanProp
# pu.df$ncar_ccsm30.w <- pu.df$ncar_ccsm30*pu.df$MeanProp
# pu.df$ncar_pcm1.w <- pu.df$ncar_pcm1*pu.df$MeanProp
# pu.df$ukmo_hadcm3.w <- pu.df$ukmo_hadcm3*pu.df$MeanProp
# pu.df$ukmo_hadgem1.w <- pu.df$ukmo_hadgem1*pu.df$MeanProp

# pu.df$koala.defor.calc.w <- pu.df$koala.defor.calc*pu.df$MeanProp
# #pu.df$koala.defor.calc.cc.w <- pu.df$koala.defor.calc.cc*pu.df$MeanProp
# pu.df$cccma_cgcm31.defor.calc.w <- pu.df$cccma_cgcm31.defor.calc*pu.df$MeanProp
# pu.df$ccsr_miroc32hi.defor.calc.w <- pu.df$ccsr_miroc32hi.defor.calc*pu.df$MeanProp
# pu.df$ccsr_miroc32med.defor.calc.w <- pu.df$ccsr_miroc32med.defor.calc*pu.df$MeanProp
# pu.df$cnrm_cm3.defor.calc.w <- pu.df$cnrm_cm3.defor.calc*pu.df$MeanProp
# pu.df$csiro_mk30.defor.calc.w <- pu.df$csiro_mk30.defor.calc*pu.df$MeanProp
# pu.df$gfdl_cm20.defor.calc.w <- pu.df$gfdl_cm20.defor.calc*pu.df$MeanProp
# pu.df$gfdl_cm21.defor.calc.w <- pu.df$gfdl_cm21.defor.calc*pu.df$MeanProp
# pu.df$giss_modeleh.defor.calc.w <- pu.df$giss_modeleh.defor.calc*pu.df$MeanProp
# pu.df$giss_modeler.defor.calc.w <- pu.df$giss_modeler.defor.calc*pu.df$MeanProp
# pu.df$iap_fgoals10g.defor.calc.w <- pu.df$iap_fgoals10g.defor.calc*pu.df$MeanProp
# pu.df$inm_cm30.defor.calc.w <- pu.df$inm_cm30.defor.calc*pu.df$MeanProp
# pu.df$ipsl_cm4.defor.calc.w <- pu.df$ipsl_cm4.defor.calc*pu.df$MeanProp
# pu.df$mpi_echam5.defor.calc.w <- pu.df$mpi_echam5.defor.calc*pu.df$MeanProp
# pu.df$mri_cgcm232a.defor.calc.w <- pu.df$mri_cgcm232a.defor.calc*pu.df$MeanProp
# pu.df$ncar_ccsm30.defor.calc.w <- pu.df$ncar_ccsm30.defor.calc*pu.df$MeanProp
# pu.df$ncar_pcm1.defor.calc.w <- pu.df$ncar_pcm1.defor.calc*pu.df$MeanProp
# pu.df$ukmo_hadcm3.defor.calc.w <- pu.df$ukmo_hadcm3.defor.calc*pu.df$MeanProp
# pu.df$ukmo_hadgem1.defor.calc.w <- pu.df$ukmo_hadgem1.defor.calc*pu.df$MeanProp

#checks
#This should be zero
# list <- which(pu.df$area < pu.df$koala.w)
# list

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
properties.dbf <- read.dbf("./preprocessing/LGA_study_region_clip_project.dbf", as.is = FALSE)
#Pull out just LGA's
LGA_area <- properties.dbf[c("LGA", "LGA_area")]
LGA_area <- aggregate(LGA_area, by = list(LGA_area$LGA), FUN = "mean")
#Remove duplicates
LGA_area <- LGA_area[!duplicated(LGA_area), ]
LGA_area_mean <- mean(LGA_area$LGA_area)
LGA_area$LGA_area_mean_multiplier <- LGA_area$LGA_area/LGA_area_mean
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

names(pu.df)[names(pu.df) == 'CADID'] <- 'LGA'

merged <- merge(pu.df, npv.df, by='LGA')
pu.df <- merged

#Add the adjusted conservation value
#pu.df$koala_adj <- pu.df$koala.w*pu.df$sd_cc

###Subset based on whether or not they have a covenant already
pu.df <- pu.df[as.character(pu.df$NewPropID) %in% c(properties_cov_rem$NewPropID),]
##Delete the 263 rows that the data doesn't cover 
#test <- pu.df[pu.df$risk == '0.001',]
pu.df <-filter(pu.df, risk != "0.001")

#save(pu.df, file="./preprocessing/pu.df_11.05.23.RData")
save(pu.df, file="./preprocessing/pu.df_03.07.23.RData")
write.csv(pu.df, file="./preprocessing/pu.df.csv")
#load("./preprocessing/pu.df_11.05.23.RData")

#Make the final df for the optimisation

#df <- pu.df[c("LGA", "NewPropID", "npv.adj", "npv.mean", "admin.adj", "admin.mean", "rank", "rank.defor", "rank.cc", "rank.cc.defor", "prob.property", "bid.price", "LValHa", "koala.w", "koala.defor.calc.w", "koala.cc.w", "koala.defor.calc.cc.w")]
#save(df, file="./preprocessing/df.RData")



