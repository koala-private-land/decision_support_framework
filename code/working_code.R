library(gurobi)
library(dplyr)
library (rgdal)
library(mapmisc)
library(purrr)
library(plyr)
library(stringr)

# #test dataset
# #Each planning unit is an LGA
# puid <- rep(c(1:20),each= 10)
# npu <- length(puid)
# #Ci is the Net present value of amount invested in a tender in planning unit i
# npv <- rep(c(runif(c(1:20), min=500, max=8000)),each= 10)
# #Ai is the administrative costs of running a tender in planning unit i.
# admin.cost <- rep(runif(c(1:20), min=5, max=30), each = 10)
# #property id - assumed to be in order of BCT ranking process
# property <- c(1:npu)
# #Iijk Bernoulli distributed probability that property j in planning unit i will put in a bid
# prob.property <- runif(npu, min=0, max=1)
# #pij net present value of the bid price of property j in planning unit i
# bid.price <- runif(npu, min=0, max=2000)
# #bij conservation benefit of each property
# cons.benefit <- runif(npu, min=0, max=3000)
# #Area in ha
# area <- runif(npu, min=0, max=150)
# #Tie it together to create dataframe
# df <- data.frame(puid, npv, admin.cost, property, prob.property, bid.price, cons.benefit)
# head(df)

setwd("D:/Linkage/DSF code/private_land_conservation_DSF")
#Remove duplicates -> need to look into why there are duplicates
load("./preprocessing/df.RData")
duplicates <- df[duplicated(df), ]
df <- df[!duplicated(df), ]

#Rename the columns if need be
#Columns must be named
#puid
#npv
#admin.cost
#propid
#property
#prob.property
#bid.price
#cons.benefit

colnames(df) <- c("puid", "NewPropID", "npv", "admin.cost", "property", "prob.property", "bid.price", "cons.benefit", "cons.benefit.adj")
#df <- data.frame(puid = pu.df$LGA, npv = pu.df$NPV, admin.cost = pu.df$admin, property = pu.df$rank, prob.property = pu.df$MeanAdopt, bid.price = pu.df$bid.price, cons.benefit = pu.df$koala.w)

##For testing
df$npv <- df$npv*20

####Function to select properties in order up until the NPV constraint
properties <- function(df){
df_new <- data.frame()
i <- unique(df$puid)[6]
for (i in unique(df$puid)){
  #for (i in df$puid){
  df1 <- df[which(df$puid == paste0(i)),]
  #Select only the first 30 for site assessment
  #Bernoulli selection
  if (dim(df1)[1] > 30) {
    ##Get a random sample
    # df1 <- df[which(df$puid == paste0(i)),]
    # sel <- rbernoulli(df1$NewPropID, p = 30/length(df1$NewPropID))
    # df1 <- df1[which(sel==TRUE),]
    
    ##To do simulations, and select the properties that were selected the most
    mylist <- list() #create an empty list
    simulations <- 1:1000 #Choose number of simulations
    results <- vector("list", length(simulations))
    for (y in simulations) {
      rep <- rbernoulli(df1$NewPropID, p = 30/length(df1$NewPropID))
      results[[y]] <- rep #put all vectors in the list
    }
    df.test <- do.call("cbind", results) #combine 
    df.test <- as.data.frame(df.test)
    freq <- t(apply(df.test, 1, function(u) table(factor(u, levels=c("TRUE","FALSE")))))
    freq <- as.data.frame(freq)
    df1$freq <- freq[,1]
    df1 <- df1[order(-df1$freq),]
    df1$freq <- 1:length(df1$freq)
    df1 <- df1 %>% top_n(30)
    #remove freq column 
    df1 <- df1[-10]
  } else {
    df1 <- df1
  }
  
  #Need to add a further constraint to ensure that the selected properties do not exceed the allocated funding for each LGA (NPV)
  if (sum(df1$bid.price) > df1[1,]$npv - df1[1,]$admin.cost) {
    #Place in the order reflecting the BCT prioritisation process
    df1 <- df1[order(df1$property),]
    #Create a new column that is the cumulative sum of bid price
    df1 <- df1 %>% group_by(puid) %>% mutate(csum = cumsum(df1$bid.price))
    #Turn into a dataframe
    df1 <- as.data.frame(df1)
    #Create a subset which is only those properties lower than the investment amount for the LGA
    df1 <- df1[which(df1$csum < df1[1,]$npv - df1[1,]$admin.cost),]
    #remove cumulative sum column 
    df1 <- df1[-10]
    #df1 <- select(df1, -csum)
  } else{
    df1 <- df1
  }
  
  #Next select 10-15 sites that are likely to put in a bit after site assessment
  #We base this on ranking metric (benefits)/bid price - this is the benefit cost ratio
  if (dim(df1)[1] > 15) {
    df1$bcr <- df1$property/df1$bid.price
    df1 <- df1[order(-df1$bcr),]
    df1 <- df1[1:15,] 
    df1 <- df1[-10]
    #df1 <- select(df1, -bcr)
  } else {
    df1 <- df1
  }
  df_new <- rbind(df_new, df1)
}
return(df_new)
#print("complete")
}

df_new <- properties(df)  
head(df_new)
View(df_new)

#Get the overall mean for each planning unit/LGA
df_new.ag <- aggregate(x = df_new[c("npv","admin.cost", "prob.property","bid.price","cons.benefit", "cons.benefit.adj")], by = df_new[c("puid")], FUN = mean)
head(df_new.ag)

#F represents the overall funding
#F <- 10000000
F <- c(101500000, 20300000, 15200000)
F <- 15200000

##to go through all of the scenarios
#Load in base shp file
shp.pu <- readOGR("./raw_data/LGAs_study_region.shp")

for (i in F){
#Scenario 1
#Set up the optimisation
#Select the planning units (LGA's)
model <- list()
model$A <- matrix(c(df_new.ag$npv), nrow=1)
model$obj        <- df_new.ag$cons.benefit
model$modelsense <- 'max'
model$rhs        <- c(i)
model$sense      <- c('<')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

#Run
result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

###Need to be careful about indexing here
solution <- data.frame(df_new.ag$puid, result$x)
selected_pus_scen1 <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus_scen1

###Create code to export maps
setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")
# create folder:
if (!dir.exists("outfoldermaps")){
  dir.create("outfoldermaps", recursive = TRUE)
} else {
  print("Dir already exists!")
}

png(file=paste0("./outfoldermaps/scen_1_budget_", i, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
shp.pu.sub <- shp.pu[shp.pu$CADID %in% c(selected_pus_scen1),]
plot(shp.pu.sub, col = "lightslateblue", add = TRUE)
cex <- 1
scaleBar(shp.pu.sub, pos = "bottomleft",   
         cex=1,
         pt.cex = 1.1*cex,
         seg.len=10*cex,
         title.cex=cex,
         outer=FALSE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()

#Scenario 2
#Set up the optimisation
#Select the planning units (LGA's)
model <- list()
model$A <- matrix(c(df_new.ag$npv), nrow=1)
model$obj        <- df_new.ag$prob.property*df_new.ag$bid.price
model$modelsense <- 'max'
model$rhs        <- c(i)
model$sense      <- c('<')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

#Run
result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

###Need to be careful about indexing here
solution <- data.frame(df_new.ag$puid, result$x)
selected_pus_scen2 <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus_scen2

###Create code to export maps
setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")
# create folder:
if (!dir.exists("outfoldermaps")){
  dir.create("outfoldermaps", recursive = TRUE)
} else {
  print("Dir already exists!")
}


png(file=paste0("./outfoldermaps/scen_2_budget_", i, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
shp.pu.sub <- shp.pu[shp.pu$CADID %in% c(selected_pus_scen2),]
plot(shp.pu.sub, col = "lightslateblue", add = TRUE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()

#Scenario 3
#Set up the optimisation
#Select the planning units (LGA's)
model <- list()
model$A <- matrix(c(df_new.ag$npv), nrow=1)
model$obj        <- (df_new.ag$prob.property*df_new.ag$bid.price)*df_new.ag$cons.benefit
model$modelsense <- 'max'
model$rhs        <- c(i)
model$sense      <- c('<')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

#Run
result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

###Need to be careful about indexing here
solution <- data.frame(df_new.ag$puid, result$x)
selected_pus_scen3 <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus_scen3

###Create code to export maps
setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")
# create folder:
if (!dir.exists("outfoldermaps")){
  dir.create("outfoldermaps", recursive = TRUE)
} else {
  print("Dir already exists!")
}

png(file=paste0("./outfoldermaps/scen_3_budget_", i, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
shp.pu.sub <- shp.pu[shp.pu$CADID %in% c(selected_pus_scen3),]
plot(shp.pu.sub, col = "lightslateblue", add = TRUE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()

#Scenario 4
#Set up the optimisation
#Select the planning units (LGA's)
model <- list()
model$A <- matrix(c(df_new.ag$npv), nrow=1)
model$obj        <- df_new.ag$cons.benefit.adj
model$modelsense <- 'max'
model$rhs        <- c(i)
model$sense      <- c('<')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

#Run
result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

###Need to be careful about indexing here
solution <- data.frame(df_new.ag$puid, result$x)
selected_pus_scen4 <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus_scen4

###Create code to export maps
setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")
# create folder:
if (!dir.exists("outfoldermaps")){
  dir.create("outfoldermaps", recursive = TRUE)
} else {
  print("Dir already exists!")
}

png(file=paste0("./outfoldermaps/scen_4_budget_", i, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
shp.pu.sub <- shp.pu[shp.pu$CADID %in% c(selected_pus_scen4),]
plot(shp.pu.sub, col = "lightslateblue", add = TRUE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()

#Scenario 5
#Set up the optimisation
#Select the planning units (LGA's)
model <- list()
model$A <- matrix(c(df_new.ag$npv), nrow=1)
model$obj        <- (df_new.ag$prob.property*df_new.ag$bid.price)*df_new.ag$cons.benefit.adj
model$modelsense <- 'max'
model$rhs        <- c(i)
model$sense      <- c('<')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

#Run
result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

###Need to be careful about indexing here
solution <- data.frame(df_new.ag$puid, result$x)
selected_pus_scen5 <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus_scen5

###Create code to export maps
setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")
# create folder:
if (!dir.exists("outfoldermaps")){
  dir.create("outfoldermaps", recursive = TRUE)
} else {
  print("Dir already exists!")
}

png(file=paste0("./outfoldermaps/scen_5_budget_", i, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
shp.pu.sub <- shp.pu[shp.pu$CADID %in% c(selected_pus_scen5),]
plot(shp.pu.sub, col = "lightslateblue", add = TRUE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()

png(file=paste0("./outfoldermaps/All_", i, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
common <- Reduce(intersect, list(selected_pus_scen1, selected_pus_scen2, selected_pus_scen3, selected_pus_scen4, selected_pus_scen5))
shp.common <- shp.pu[shp.pu$CADID %in% c(common),]
plot(shp.common, col = "seagreen3", add = TRUE)
scaleBar(shp.pu.sub, pos = "bottomleft",   
         cex=1,
         pt.cex = 1.1*cex,
         seg.len=10*cex,
         title.cex=cex,
         outer=FALSE)
# #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()

png(file=paste0("./outfoldermaps/diff_scen3_5_budget_", i, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
common <- Reduce(intersect, list(selected_pus_scen3,selected_pus_scen5))
shp.common <- shp.pu[shp.pu$CADID %in% c(common),]
plot(shp.common, col = "seagreen3", add = TRUE)
onlyscen3 <- selected_pus_scen3[!selected_pus_scen3 %in% selected_pus_scen5]
shp.onlyscen3 <- shp.pu[shp.pu$CADID %in% c(onlyscen3),]
plot(shp.onlyscen3, col = "mistyrose2", add = TRUE)
onlyscen5 <- selected_pus_scen5[!selected_pus_scen5 %in% selected_pus_scen3]
shp.onlyscen5 <- shp.pu[shp.pu$CADID %in% c(onlyscen5),]
plot(shp.onlyscen5, col = "slategray2", add = TRUE)
scaleBar(shp.pu.sub, pos = "bottomleft",   
         cex=1,
         pt.cex = 1.1*cex,
         seg.len=10*cex,
         title.cex=cex,
         outer=FALSE)
# #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()

png(file=paste0("./outfoldermaps/scatter_feasibility_vs_cons_", i, ".png"), width=1000, height=1000)
#par(mar=c(0,0,0,0))
plot(df_new$prob.property, df_new$cons.benefit, xlab = "Social Feasibility", ylab = "Conservation benefit", cex.lab=1.5)
dev.off()

png(file=paste0("./outfoldermaps/scatter_feasibility_vs_cons_cc_", i, ".png"), width=1000, height=1000)
#par(mar=c(0,0,0,0))
plot(df_new$prob.property, df_new$cons.benefit.adj, xlab = "Social Feasibility", ylab = "Conservation benefit - cc risk adjusted", cex.lab=1.5)
dev.off()
}




##Trade-offs between cons benefit and cost
#Weight matrix
nweights=11
weightm <- matrix(0, nrow=nweights, ncol=length(F))
weightm[,1] <- c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.01)
#Create benefit matrix
scen3_ben <- matrix(0, nrow=nweights, ncol=3)

#Run through the weights to create trade-off curves
for (w in weightm){
w <- w
#Scenario 3
#Set up the optimisation
#Select the planning units (LGA's)
model <- list()
model$A <- matrix(c(df_new.ag$npv), nrow=1)
model$obj        <- (w*df_new.ag$prob.property*df_new.ag$bid.price)+((1-w)*df_new.ag$cons.benefit)
#model$obj        <- (weightm[w]*(df_new.ag$prob.property*df_new.ag$bid.price))*((1-weightm[w])*df_new.ag$cons.benefit)
model$modelsense <- 'max'
model$rhs        <- c(i)
model$sense      <- c('<')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

#Run
result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

###Need to be careful about indexing here
solution <- data.frame(df_new.ag$puid, result$x)
selected_pus_scen3 <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus_scen3

#w <- 1
#make this the weight number
scen3_ben[which(weightm == w),1] <- w
##Pull out total benefits 
df_new.ag.sel <- df_new.ag[df_new.ag$puid %in% c(selected_pus_scen3),]
cons.ben <- sum(df_new.ag.sel$cons.benefit)
tot.cost <- sum(df_new.ag.sel$bid.price)
##Add it to the matrix
scen3_ben[which(weightm == w),2] <- cons.ben
scen3_ben[which(weightm == w),3] <- tot.cost

###Create code to export maps
setwd("D:/Linkage/DSF code/private_land_conservation_DSF/")
# create folder:
if (!dir.exists("outfoldermaps")){
  dir.create("outfoldermaps", recursive = TRUE)
} else {
  print("Dir already exists!")
}

png(file=paste0("./outfoldermaps/trade_offs/scen_3_budget_", i, "_weight_", w, ".png"), width=1000, height=1000)
par(mar=c(0,0,0,0))
#plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
plot(shp.pu)
shp.pu.sub <- shp.pu[shp.pu$CADID %in% c(selected_pus_scen3),]
plot(shp.pu.sub, col = "lightslateblue", add = TRUE)
#suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
dev.off()
}

save(scen3_ben, file = paste0("./outfoldermaps/trade_offs/", i, ".RData"))









