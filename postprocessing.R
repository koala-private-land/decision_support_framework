library(Matrix)
library(rgeos)
library(sp)

#Need to make the output for each solution straight away
####POST PROCESSING###
setwd("E:/Linkage/DSF code/private_land_conservation_DSF")
#Load the required functions
source('./code/functions.R')
#Load the data
load("./preprocessing/pu.df.RData")
#Load in base shp file
shp.pu <- readOGR("./raw_data/LGAs_study_region.shp")
#Load in result data
scen <- "scenario3gfdl_cm21"
load(paste0("./outputs_v4/", scen, ".RData"))

###Need to be super careful about indexing here
#NORMAL SELECTION
#First create a solutions matrix to show which LGA was selected with which budget increment
solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
#FOR HEAT MAPS
#Then put values to this solutions matrix so we can visualise the selected budget amounts
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol <- rowSums(solutions.budget.matrix)

#Generate results
setwd("E:/Linkage/DSF code/private_land_conservation_DSF/outputs_v4")

###Export maps

outfoldermaps <- "./maps_v4"
outfoldervals <- "./vals_v4"
make.maps(outfoldermaps, scen)

#Export values
exp.vals(outfoldervals, scen)





#Need to make the output for each solution straight away
####POST PROCESSING###
##Climate change selection frequency map##
setwd("E:/Linkage/DSF code/private_land_conservation_DSF")
#Load the required functions
source('./code/functions.R')
#Load the data
load("./preprocessing/pu.df.RData")
#Load in base shp file
shp.pu <- readOGR("./raw_data/LGAs_study_region.shp")
#Load in result data
scen <- "scenario3gfdl_cm21"
load(paste0("./outputs_v4/", scen, ".RData"))

#Get all of the climate change solutions
cc_scens_list =list.files("./outputs_v4/",pattern="scenario3")
#Tie the cc solutions together
i <- cc_scens_list[1]
load(paste0("./outputs_v4/", i))
solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
select_freq.sol <- rowSums(solutions)

for (i in cc_scens_list[2:length(cc_scens_list)]){
  load(paste0("./outputs_v4/", i))
  solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
  select_freq.sol <- rowSums(solutions)+select_freq.sol
}

#HEATMAP
###For heat map showing selected budget
##Join budget values to shp file
pu.and.budget <- data.frame(df_new$puid, select_freq.sol)
colnames(pu.and.budget) <- c("CADID", "select_freq.sol")
shp.pu.joined <- merge(shp.pu, pu.and.budget, by = "CADID")
shp.pu.joined[is.na(shp.pu.joined$budget.sol)] <- 0
#Export the map
#Generate results
setwd("E:/Linkage/DSF code/private_land_conservation_DSF/outputs_v4")

png(file=paste0(outfoldermaps, "/cc_selection_frequency.png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
#plot the polygons without colour
plot(shp.pu.joined)
# Set the palette
p <- colorRampPalette(c("white", "#FCFDA3", "#F2711D", "#AD305D", "#4D136C", "#000000"))(128)
palette(p)
# Scale the values to the palette
vals <- shp.pu.joined$select_freq.sol
vals[is.na(vals)] <- 0
cols <- (vals - min(vals))/diff(range(vals))*127+1
plot(shp.pu.joined, col=cols)

#Add a legend
levels <- unique(shp.pu.joined$select_freq.sol)
levels <- round(levels, digits=1)
levels <- sort(levels)
col.levels <- unique(cols)
col.levels <- sort(col.levels)

# add a legend to your map
legend("topright",   # location of legend
       legend = levels, # categories or elements to render in
       # the legend
       fill = col.levels, # color palette to use to fill objects in legend.
       title = "Frequency selected",
       bty = "n",
       cex = 1.2)
cex <- 1
scaleBar(shp.pu.joined, pos = "bottomleft",   
         cex=1,
         pt.cex = 1.1*cex,
         seg.len=10*cex,
         title.cex=cex,
         outer=FALSE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()





#####CODE TO MAKE DIFFERENCE MAP OF SCENARIOS 1, 2, AND 3#####
solution <- data.frame(df_new.ag$puid, result$x)
selected_pus_scen5 <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus_scen5

setwd("E:/Linkage/DSF code/private_land_conservation_DSF/outputs_v4/")

#Load in result data
scen <- "scenario1"
load(paste0(scen, ".RData"))
solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
binary.sol.1 <- rowSums(solutions)
binary.sol.1 <- data.frame(df_new$puid, binary.sol.1)

scen <- "scenario2"
load(paste0(scen, ".RData"))
solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
binary.sol.2 <- rowSums(solutions)
binary.sol.2 <- data.frame(df_new$puid, binary.sol.2)

sols <- merge(binary.sol.1, binary.sol.2)
sols$rowsum <- sols$binary.sol.1*sols$binary.sol.2
test <- binary.sol.1+binary.sol.2
common <- test[test$binary.sol.1 == '2',]
common <- common/2

cols <- c("CADID","scen1","scen2","both")
colnames(sols) <- cols

png(file=paste0(outfoldermaps, "/diff_scen_1_2.png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
merge <- merge(shp.pu, sols)
merge[is.na(merge$scen1)] <- 0
merge[is.na(merge$scen2)] <- 0
merge[is.na(merge$both)] <- 0


shp.onlyscen1 <- merge[merge$scen1 == "1",]
plot(shp.onlyscen1, col = "mistyrose2", add = TRUE)
shp.onlyscen2 <- merge[merge$scen2 == "1",]
plot(shp.onlyscen2, col = "slategray2", add = TRUE)
shp.common <- shp.pu[shp.pu$CADID %in% c(common$df_new.puid),]
plot(shp.common, col = "seagreen3", add = TRUE)

scaleBar(merge, pos = "bottomleft",   
         cex=1,
         pt.cex = 1.1*cex,
         seg.len=10*cex,
         title.cex=cex,
         outer=FALSE)
# #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()



###Scatter plot
png(file=paste0(outfoldermaps, "/scatter_prob_vs_cons.png"), width=1000, height=1000)
#par(mar=c(0,0,0,0))
plot(pu.df$MeanAdopt, pu.df$koala_curr/1000000, xlab = "Probability of a bid", ylab = "Suitability-weighted koala habitat (km2)", cex.lab=1.5)
dev.off()









####Code to make the plot in figure 5
#Need to make the output for each solution straight away
####POST PROCESSING###
setwd("E:/Linkage/DSF code/private_land_conservation_DSF")
#Load the required functions
source('./code/functions.R')
#Load the data
load("./preprocessing/pu.df.RData")
#Load in base shp file
shp.pu <- readOGR("./raw_data/LGAs_study_region.shp")
#Load in result data
scen <- "scenario2_20300000"
#load(paste0("./outputs_v4/", scen, ".RData"))
load("E:/Linkage/DSF code/private_land_conservation_DSF/outputs_v4/results_budget_1_20300000/scenario2.RData")
###Need to be super careful about indexing here
#NORMAL SELECTION
#First create a solutions matrix to show which LGA was selected with which budget increment
solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
#FOR HEAT MAPS
#Then put values to this solutions matrix so we can visualise the selected budget amounts
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol <- rowSums(solutions.budget.matrix)

#Generate results
setwd("E:/Linkage/DSF code/private_land_conservation_DSF/outputs_v4")

exp.vals(outfoldermaps, scen)``

###Code to get values
exp.vals <- function(outfoldervals, scen){
  
  # create folder:
  if (!dir.exists(outfoldervals)){
    dir.create(outfoldervals, recursive = TRUE)
  } else {
    print("Dir already exists!")
  }
  
  #Total cost
  cost <- sum(budget.sol)
  
  #Total cons benefit
  cons.matrix <- matrix(cons.all.incre, nrow=length(unique(pu.df$LGA)))
  solutions.cons.matrix <- solutions*cons.matrix
  cons <- sum(solutions.cons.matrix)
  
  #Probability of a bid value
  #Make a long string of all conservation benefit
  prob.all.incre <- df_new$prob.property
  for (i in 1:y){
    df.name <- paste0("df_new", i)
    prob.all.incre <- c(prob.all.incre, get(df.name)$prob.property)
    prob.all.incre[is.na(prob.all.incre)] = 1000000000000
  }
  
  prob.matrix <- matrix(prob.all.incre, nrow=length(unique(pu.df$LGA)))
  solutions.prob.matrix <- solutions*prob.matrix
  prob <- sum(solutions.prob.matrix)
  
  #Export the final 
  final.vals <- data.frame(scen = scen, cost = cost, cons.ben = cons, prob.bid = prob)
  write.csv(final.vals, file = paste0(outfoldervals, "./", scen, "_budget_", i, ".csv"), row.names = TRUE)
  
}












##############Code for mean budget allocated for CC scens#############
library(Matrix)
library(rgeos)
library(sp)
library(gurobi)
library(Matrix)
library(rgeos)
library(sp)
library(gurobi)
library(dplyr)
library (rgdal)
library(mapmisc)
library(purrr)
library(plyr)
library(stringr)


##Climate change selection frequency map##
#Heatmap
setwd("E:/Linkage/DSF code/private_land_conservation_DSF")
#Load the required functions
source('./code/functions.R')
#Load the data
load("./preprocessing/pu.df.RData")
#Load in base shp file
shp.pu <- readOGR("./raw_data/LGAs_study_region.shp")
#Load in result data
scen <- "scenario3gfdl_cm21"


load(paste0("./outputs_v5/cccma_cgcm31_101500000.RData"))
cccma_cgcm31 <- result$x 
load(paste0("./outputs_v5/ccsr_miroc32hi_101500000.RData"))
ccsr_miroc32hi <- result$x 
load(paste0("./outputs_v5/ccsr_miroc32med_101500000.RData"))
ccsr_miroc32med <- result$x 
load(paste0("./outputs_v5/cnrm_cm3_101500000.RData"))
cnrm_cm3 <- result$x 
load(paste0("./outputs_v5/csiro_mk30_101500000.RData"))
csiro_mk30 <- result$x 
load(paste0("./outputs_v5/gfdl_cm20_101500000.RData"))
gfdl_cm20 <- result$x 
load(paste0("./outputs_v5/gfdl_cm21_101500000.RData"))
gfdl_cm21 <- result$x 
load(paste0("./outputs_v5/giss_modeleh_101500000.RData"))
giss_modeleh <- result$x 
load(paste0("./outputs_v5/giss_modeler_101500000.RData"))
giss_modeler <- result$x 
load(paste0("./outputs_v5/iap_fgoals10g_101500000.RData"))
iap_fgoals10g <- result$x 
load(paste0("./outputs_v5/inm_cm30_101500000.RData"))
inm_cm30 <- result$x
load(paste0("./outputs_v5/ipsl_cm4_101500000.RData"))
ipsl_cm4 <- result$x
load(paste0("./outputs_v5/mpi_echam5_101500000.RData"))
mpi_echam5 <- result$x
load(paste0("./outputs_v5/mri_cgcm232a_101500000.RData"))
mri_cgcm232a <- result$x
load(paste0("./outputs_v5/ncar_ccsm30_101500000.RData"))
ncar_ccsm30 <- result$x
load(paste0("./outputs_v5/ncar_pcm1_101500000.RData"))
ncar_pcm1 <- result$x


#Load the required functions
source('./code/functions.R')
#Load the data
load("./preprocessing/pu.df.RData")
#Set number of simulations
sim <- 10
#Test overall budget level
b <- 20300000*5
b.vec <- c(1:10 * 20300000)
#Create output folders 
outfolder <- "outputs_v5"
outfoldermaps <- "outputs_v5/maps_v5"
outfoldervals <- "outputs_v5/vals_v5"
# create folder:
if (!dir.exists(outfoldermaps)){
  dir.create(outfoldermaps, recursive = TRUE)
} else {
  print("Dir already exists!")
}
if (!dir.exists(outfoldervals)){
  dir.create(outfoldervals, recursive = TRUE)
} else {
  print("Dir already exists!")
}
#Remove duplicates -> need to look into if and why there are duplicates
duplicates <- pu.df[duplicated(pu.df), ]
df.org <- pu.df[!duplicated(pu.df), ]
df <- df.org[c("LGA", "NewPropID", "npv.mean", "admin.mean", "rank", "MeanAdopt", "MeanWTA.tot", "koala_curr.w")]
colnames(df) <- c("puid", "NewPropID", "npv", "admin.cost", "property", "prob.property", "bid.price", "cons.benefit")

cost <- read.csv("./raw_data/bct_cost_data_26_7_22.csv")
min(cost$Approx_tot_investment)
#df$npv <- 1126260
y <- 33
min_npv <- min(cost$Approx_tot_investment)
max_npv <- max(cost$Approx_tot_investmen)
incre <- (max_npv-min_npv)/y
#Add new NPV values to the dataframe
#df$npv.min <- min_npv
x <- rep(min_npv, 93)
for(i in 2:y) {                                  
  k <- rep(incre*i, 93)                  
  x <- c(x, k)            
}

npv.all.incre <- x

solutions <- matrix(cccma_cgcm31, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.cccma_cgcm31 <- rowSums(solutions.budget.matrix)

solutions <- matrix(ccsr_miroc32hi, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.ccsr_miroc32hi <- rowSums(solutions.budget.matrix)

solutions <- matrix(ccsr_miroc32med, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.ccsr_miroc32med <- rowSums(solutions.budget.matrix)

solutions <- matrix(cnrm_cm3, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.cnrm_cm3 <- rowSums(solutions.budget.matrix)

solutions <- matrix(csiro_mk30, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.csiro_mk30 <- rowSums(solutions.budget.matrix)

solutions <- matrix(gfdl_cm20, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.gfdl_cm20 <- rowSums(solutions.budget.matrix)

solutions <- matrix(gfdl_cm21, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.gfdl_cm21 <- rowSums(solutions.budget.matrix)

solutions <- matrix(giss_modeleh, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.giss_modeleh <- rowSums(solutions.budget.matrix)

solutions <- matrix(giss_modeler, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.giss_modeler <- rowSums(solutions.budget.matrix)

solutions <- matrix(iap_fgoals10g, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.iap_fgoals10g <- rowSums(solutions.budget.matrix)

solutions <- matrix(inm_cm30, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.inm_cm30 <- rowSums(solutions.budget.matrix)

solutions <- matrix(ipsl_cm4, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.ipsl_cm4 <- rowSums(solutions.budget.matrix)

solutions <- matrix(mpi_echam5, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.mpi_echam5 <- rowSums(solutions.budget.matrix)

solutions <- matrix(mri_cgcm232a, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.mri_cgcm232a <- rowSums(solutions.budget.matrix)

solutions <- matrix(ncar_ccsm30, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.ncar_ccsm30 <- rowSums(solutions.budget.matrix)

solutions <- matrix(ncar_pcm1, nrow=length(unique(pu.df$LGA)))
binary.sol <- rowSums(solutions)
budget.matrix <- matrix(npv.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.budget.matrix <- solutions*budget.matrix
budget.sol.ncar_pcm1 <- rowSums(solutions.budget.matrix)


all.mean <- budget.sol.cccma_cgcm31+budget.sol.ccsr_miroc32hi+budget.sol.ccsr_miroc32med+budget.sol.cnrm_cm3+budget.sol.csiro_mk30+budget.sol.gfdl_cm20+budget.sol.gfdl_cm21+budget.sol.giss_modeleh+budget.sol.giss_modeler+budget.sol.iap_fgoals10g+budget.sol.inm_cm30+budget.sol.ipsl_cm4+budget.sol.mpi_echam5+budget.sol.mri_cgcm232a+budget.sol.ncar_ccsm30+budget.sol.ncar_pcm1/18



#load(paste0("./outputs_v4/", scen, ".RData"))

#Get all of the climate change solutions
cc_scens_list =list.files("./outputs_v4/",pattern="scenario3")
#Tie the cc solutions together
i <- cc_scens_list[1]
load(paste0("./outputs_v4/", i))
solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
select_freq.sol <- rowSums(solutions)

for (i in cc_scens_list[2:length(cc_scens_list)]){
  load(paste0("./outputs_v4/", i))
  solutions <- matrix(result$x, nrow=length(unique(pu.df$LGA)))
  select_freq.sol <- rowSums(solutions)+select_freq.sol
}

a <- unique(pu.df$LGA)
#HEATMAP
###For heat map showing selected budget
##Join budget values to shp file
pu.and.budget <- data.frame(a, all.mean)
colnames(pu.and.budget) <- c("CADID", "mean.all")
shp.pu.joined <- merge(shp.pu, pu.and.budget, by = "CADID", duplicateGeoms = TRUE)
shp.pu.joined[is.na(shp.pu.joined$budget.sol)] <- 0
#Export the map
#Generate results
setwd("E:/Linkage/DSF code/private_land_conservation_DSF/")

png(file=paste0(outfoldermaps, "/cc_mean_budget.png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
#plot the polygons without colour
plot(shp.pu.joined)
# Set the palette
p <- colorRampPalette(c("white", "#FCFDA3", "#F2711D", "#AD305D", "#4D136C", "#000000"))(128)
palette(p)
# Scale the values to the palette
vals <- shp.pu.joined$mean.all
vals[is.na(vals)] <- 0
cols <- (vals - min(vals))/diff(range(vals))*127+1
plot(shp.pu.joined, col=cols)

#Add a legend
levels <- unique(shp.pu.joined$mean.all)
levels <- round(levels, digits=1)
levels <- sort(levels)
col.levels <- unique(cols)
col.levels <- sort(col.levels)

# add a legend to your map
legend("topright",   # location of legend
       legend = levels, # categories or elements to render in
       # the legend
       fill = col.levels, # color palette to use to fill objects in legend.
       title = "Mean budget allocated",
       bty = "n",
       cex = 1.2)
cex <- 1
scaleBar(shp.pu.joined, pos = "bottomleft",   
         cex=1,
         pt.cex = 1.1*cex,
         seg.len=10*cex,
         title.cex=cex,
         outer=FALSE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()






