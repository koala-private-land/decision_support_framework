####Function to run simulation and select properties in order up until the NPV constraint
properties <- function(df, rep){
  df_new <- data.frame()
  df_final <- data.frame()
  #i <- unique(df$puid)[6]
  #i <- 108012388
  #i <- 108012496
  #i <- 108012496
  #i <- 178764487
  #i <- 108012380
  for (i in unique(df$puid)){
    #To keep track of where it is up to
    print(paste0("Planning_unit_", i))
    
    #Set up dataframes
    #Make a subset of the dataframe which is only data for the respective LGA
    df.LGA <- df[which(df$puid == paste0(i)),]
    #Create empty place to save result of the simulations
    result <- data.frame() 
    
    #For each simulation
    #rep <- 1000
    
    #Start looping through the simulations
    #x <- 1
    for (x in 1:rep){
      
      #To keep track of where it is up to
      print(paste0("Simulation_", x))
      
      #1. Select sites that put in an EOI
      #If there is only 1 property in an LGA we assume they will put in an EOI 
      if (nrow(df.LGA) > 1){
        y <- rbinom(n = length(df.LGA$NewPropID), size = 1, p = df.LGA$prob.property)
        eoi <- df.LGA[which(y == 1), ]
      } else {
        eoi <- df.LGA
      }
      
      #2. Rank EOIs based on BCT metric
      rank.metric <- eoi[order(-eoi$property),]
      
      #3. Select top 30 for site assessment
      #If there are less an 30 properties in an LGA all can assessed
      if (dim(rank.metric)[1] > 30) {
        rank.metric <- rank.metric[1:30,] 
      } else {
        rank.metric <- rank.metric
      }
      
      #4. Generate bid for top 30
      #(Already in dataframe from Jonathan's model)
      
      #5. Rank bids based on cost efficiency
      #6. Take top n bids until either 15 properties is reached or max budget is reached
      #Next select 10-15 sites that are likely to put in a bid after site assessment
      #We base this on ranking metric (benefits)/bid price - this is the benefit cost ratio
      if (dim(rank.metric)[1] > 15) {
        rank.metric$bcr <- rank.metric$property/rank.metric$bid.price
        rank.c.b <- rank.metric[order(-rank.metric$bcr),]
        rank.c.b <- rank.c.b[1:15,] 
        rank.c.b <- rank.c.b[-10]
        #df1 <- select(df1, -bcr)
      } else {
        rank.metric$bcr <- rank.metric$property/rank.metric$bid.price
        rank.c.b <- rank.metric[order(-rank.metric$bcr),]
        rank.c.b <- rank.c.b[-10]
      }
      
      
      ###If y from above "selected" 0 properties, then we need to create a dataframe that only has 0s to avoid errors in the next bit of the code, and for calculating aggregated means
      if (nrow(rank.c.b)==0){
        rank.c.b[1,] <- 0
      } else {
        rank.c.b <- rank.c.b 
      }
      
      #5a. Generate the % of property covenented
      #Already done in proprocessing code
      
      #Need to add a further constraint to ensure that the selected properties do not exceed the allocated funding for each LGA (NPV)
      #If the total sum of bids is greater than the allocated budget
      if (sum(rank.c.b$bid.price) > rank.c.b[1,]$npv - rank.c.b[1,]$admin.cost) {
        #Already in order of cost benefit ratio
        #Create a new column that is the cumulative sum of bid price
        rank.c.b <- rank.c.b %>% group_by(puid) %>% mutate(csum = cumsum(rank.c.b$bid.price))
        #Turn into a dataframe
        df.tot.cost <- as.data.frame(rank.c.b)
        #Create a subset which is only those properties lower than the investment amount for the LGA
        #It is possble that even one property is higher than the NPV for the tender, in which case no properties will be accepted
        if ((df.tot.cost[1,]$npv - df.tot.cost[1,]$admin.cost) < df.tot.cost[1,]$csum){
          df.tot.cost <- data.frame()
        }else{
          df.tot.cost <- df.tot.cost[which(df.tot.cost$csum < df.tot.cost[1,]$npv - df.tot.cost[1,]$admin.cost),]
          #remove cumulative sum column 
          df.tot.cost <- df.tot.cost[-10]
          #df1 <- select(df1, -csum)
        }
      } else{
        df.tot.cost <- rank.c.b
      }
      
      #Get the total benefits (sum) and save the result of each simulation
      if (dim(df.tot.cost)[1]>0){
        agg.benefits <- aggregate(x = df.tot.cost[c("prob.property","bid.price","cons.benefit", "area")], by = df.tot.cost[c("puid")], FUN = sum)
        agg.benefits$npv <- mean(df.tot.cost$npv)
        agg.benefits$admin.cost <- mean(df.tot.cost$admin.cost)
        agg.benefits <- agg.benefits[c("puid", "npv", "admin.cost", "prob.property", "bid.price", "cons.benefit", "area")]
        result <- rbind(result, agg.benefits)
      } else {
        result <- rbind(result, df.tot.cost)
      }
      
    }
    #Get the mean of all simulations
    if (dim(result)[1]>0){
      agg.result <- aggregate(x = result[c("npv","admin.cost", "prob.property","bid.price","cons.benefit", "area")], by = result[c("puid")], FUN = mean)
      df_final <- rbind(df_final, agg.result)
    } else {
      df_final <- rbind(df_final, result)
    }
    df_final <-df_final[which(df_final[,1]>0),]
  }
  
  return(df_final)
  #print("complete")
  
}

####Function to run simulation and select properties in order up until the NPV constraint for scenario 2
properties.scen2 <- function(df, rep){
  df_new <- data.frame()
  df_final <- data.frame()
  for (i in unique(df$puid)){
    #To keep track of where it is up to
    print(paste0("Planning_unit_", i))
    
    #Set up dataframes
    #Make a subset of the dataframe which is only data for the respective LGA
    df.LGA <- df[which(df$puid == paste0(i)),]
    #Create empty place to save result of the simulations
    result <- data.frame() 
    
    #For each simulation
    #rep <- 1000
    
    #Start looping through the simulations
    #x <- 1
    rep <- 10
    for (x in 1:rep){
      
      #To keep track of where it is up to
      print(paste0("Simulation_", x))
      
      #1. Select sites that put in an EOI
      #If there is only 1 property in an LGA we assume they will put in an EOI 
      if (nrow(df.LGA) > 1){
        y <- rbinom(n = length(df.LGA$NewPropID), size = 1, p = df.LGA$prob.property)
        eoi <- df.LGA[which(y == 1), ]
      } else {
        eoi <- df.LGA
      }
      
      #2. Rank EOIs based on BCT metric
      #For this scenario we need to ignore conservation values, so all values calculated from BCT's metric becomes 1
      if (nrow(eoi) > 1){
        eoi$property <- 1
        rank.metric <- eoi[order(-eoi$property),]
      } else {
        rank.metric <- eoi[order(-eoi$property),]
      }
      
      #3. Select top 30 for site assessment
      #If there are less an 30 properties in an LGA all can assessed
      if (dim(rank.metric)[1] > 30) {
        rank.metric <- rank.metric[1:30,] 
      } else {
        rank.metric <- rank.metric
      }
      
      #4. Generate bid for top 30
      #(Already in dataframe from Jonathan's model)
      
      #5. Rank bids based on cost efficiency
      #6. Take top n bids until either 15 properties is reached or max budget is reached
      #Next select 10-15 sites that are likely to put in a bid after site assessment
      #We base this on ranking metric (benefits)/bid price - this is the benefit cost ratio
      if (dim(rank.metric)[1] > 15) {
        rank.metric$bcr <- rank.metric$property/rank.metric$bid.price
        rank.c.b <- rank.metric[order(-rank.metric$bcr),]
        rank.c.b <- rank.c.b[1:15,] 
        rank.c.b <- rank.c.b[-10]
        #df1 <- select(df1, -bcr)
      } else {
        rank.metric$bcr <- rank.metric$property/rank.metric$bid.price
        rank.c.b <- rank.metric[order(-rank.metric$bcr),]
        rank.c.b <- rank.c.b[-10]
      }
      
      
      ###If y from above "selected" 0 properties, then we need to create a dataframe that only has 0s to avoid errors in the next bit of the code, and for calculating aggregated means
      if (nrow(rank.c.b)==0){
        rank.c.b[1,] <- 0
      } else {
        rank.c.b <- rank.c.b 
      }
      
      #5a. Generate the % of property covenented
      #Already done in proprocessing code
      
      #Need to add a further constraint to ensure that the selected properties do not exceed the allocated funding for each LGA (NPV)
      #If the total sum of bids is greater than the allocated budget
      if (sum(rank.c.b$bid.price) > rank.c.b[1,]$npv - rank.c.b[1,]$admin.cost) {
        #Already in order of cost benefit ratio
        #Create a new column that is the cumulative sum of bid price
        rank.c.b <- rank.c.b %>% group_by(puid) %>% mutate(csum = cumsum(rank.c.b$bid.price))
        #Turn into a dataframe
        df.tot.cost <- as.data.frame(rank.c.b)
        #Create a subset which is only those properties lower than the investment amount for the LGA
        #It is possble that even one property is higher than the NPV for the tender, in which case no properties will be accepted
        if ((df.tot.cost[1,]$npv - df.tot.cost[1,]$admin.cost) < df.tot.cost[1,]$csum){
          df.tot.cost <- data.frame()
        }else{
          df.tot.cost <- df.tot.cost[which(df.tot.cost$csum < df.tot.cost[1,]$npv - df.tot.cost[1,]$admin.cost),]
          #remove cumulative sum column 
          df.tot.cost <- df.tot.cost[-10]
          #df1 <- select(df1, -csum)
        }
      } else{
        df.tot.cost <- rank.c.b
      }
      
      #Get the total benefits (sum) and save the result of each simulation
      if (dim(df.tot.cost)[1]>0){
        agg.benefits <- aggregate(x = df.tot.cost[c("prob.property","bid.price","cons.benefit", "area")], by = df.tot.cost[c("puid")], FUN = sum)
        agg.benefits$npv <- mean(df.tot.cost$npv)
        agg.benefits$admin.cost <- mean(df.tot.cost$admin.cost)
        agg.benefits <- agg.benefits[c("puid", "npv", "admin.cost", "prob.property", "bid.price", "koala_curr.w", "area")]
        result <- rbind(result, agg.benefits)
      } else {
        result <- rbind(result, df.tot.cost)
      }
      
    }
    #Get the mean of all simulations
    if (dim(result)[1]>0){
      agg.result <- aggregate(x = result[c("npv","admin.cost", "prob.property","bid.price","cons.benefit", "area")], by = result[c("puid")], FUN = mean)
      df_final <- rbind(df_final, agg.result)
    } else {
      df_final <- rbind(df_final, result)
    }

    df_final <-df_final[which(df_final[,1]>0),]
  }
  
  return(df_final)
  #print("complete")
  
}

####Parallel processing function to run simulation and select properties in order up until the NPV constraint for scenario 2
#Split.df needs to be a list of tables
properties.par <- function(split.df, rep){
  ####Parallel processing
  parallel::detectCores()
  n.cores <- parallel::detectCores() - 2
  #create the cluster
  my.cluster <- parallel::makeCluster(
    n.cores, 
    type = "PSOCK")

  #check cluster definition (optional)
  print(my.cluster)
  doParallel::registerDoParallel(my.cluster)
  
  #register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster)
  
  #check if it is registered (optional)
  foreach::getDoParRegistered()
  
  #how many workers are available? (optional)
  foreach::getDoParWorkers()
  
  #Create new data structures for saving for each round
  df_new <- data.frame()
  df_final <- data.frame()
  se.df <- data.frame()
  sd.df <- data.frame()

#Run the simulations and ranking process  
foreach(d=split.df, run=1:length(split.df), .combine=c, .multicombine=TRUE, .packages=c("dplyr", "data.table", "stringi", "doParallel")) %do% {
  #Create empty place to save result of the simulations
  result <- data.frame() 
  #Start looping through the simulations
  for (i in 1:rep) {
  #1. Select sites that put in an EOI
  #If there is only 1 property in an LGA we assume they will put in an EOI 
  if (nrow(d) > 1){
    y <- rbinom(n = nrow(d), size = 1, p = d[[6]])
    #y <- rbinom(n = nrow(i[[1]]), size = 1, p = i[[1]][[6]])
    eoi <- d[which(y == 1), ]
  } else {
    eoi <- d
  }

  #2. Rank EOIs based on BCT metric
  rank.metric <- eoi[order(-eoi$property),]
  
  #3. Select top 30 for site assessment
  #If there are less an 30 properties in an LGA all can assessed
  if (dim(rank.metric)[1] > 30) {
    rank.metric <- rank.metric[1:30,] 
  } else {
    rank.metric <- rank.metric
  }
  
  #4. Generate bid for top 30
  #(Already in dataframe from Jonathan's model)
  
  #5. Rank bids based on cost efficiency
  #6. Take top n bids until either 15 properties is reached or max budget is reached
  #Next select 10-15 sites that are likely to put in a bid after site assessment
  #We base this on ranking metric (benefits)/bid price - this is the benefit cost ratio
  
  if (dim(rank.metric)[1] > 15) {
    rank.metric$bcr <- rank.metric$property/rank.metric$bid.price
    rank.c.b <- rank.metric[order(-rank.metric$bcr),]
    rank.c.b <- rank.c.b[1:15,] 
    rank.c.b <- rank.c.b[-11]
  } else {
    rank.metric$bcr <- rank.metric$property/rank.metric$bid.price
    rank.c.b <- rank.metric[order(-rank.metric$bcr),]
    rank.c.b <- rank.c.b[-11]
  }

  ###If y from above "selected" 0 properties, then we need to create a dataframe that only has 0s to avoid errors in the next bit of the code, and for calculating aggregated means
  if (nrow(rank.c.b)==0){
    rank.c.b[1,] <- 0
  } else {
    rank.c.b <- rank.c.b 
  }
  
  #5a. Generate the % of property covenented
  #Already done in proprocessing code
  
  #Need to add a further constraint to ensure that the selected properties do not exceed the allocated funding for each LGA (NPV)
  #If the total sum of bids is greater than the allocated budget
  if (sum(rank.c.b$bid.price) > rank.c.b[1,]$npv - rank.c.b[1,]$admin.cost) {
    #Already in order of cost benefit ratio
    #Create a new column that is the cumulative sum of bid price
    rank.c.b <- rank.c.b %>% group_by(puid) %>% mutate(csum = cumsum(rank.c.b$bid.price))
    #Turn into a dataframe
    df.tot.cost <- as.data.frame(rank.c.b)
    #Create a subset which is only those properties lower than the investment amount for the LGA
    #It is possble that even one property is higher than the NPV for the tender, in which case no properties will be accepted
    if ((df.tot.cost[1,]$npv - df.tot.cost[1,]$admin.cost) < df.tot.cost[1,]$csum){
      df.tot.cost <- data.frame()
    }else{
      df.tot.cost <- df.tot.cost[which(df.tot.cost$csum < df.tot.cost[1,]$npv - df.tot.cost[1,]$admin.cost),]
      #remove cumulative sum column 
      df.tot.cost <- df.tot.cost[-11]
    }
  } else{
    df.tot.cost <- rank.c.b
  }
  
  #Get the total benefits (sum) and save the result of each simulation
  if (dim(df.tot.cost)[1]>0){
    agg.benefits <- aggregate(x = df.tot.cost[c("prob.property","bid.price","cons.benefit", "area")], by = df.tot.cost[c("puid")], FUN = sum)
    agg.benefits$npv <- mean(df.tot.cost$npv)
    agg.benefits$admin.cost <- mean(df.tot.cost$admin.cost)
    agg.benefits <- agg.benefits[c("puid", "npv", "admin.cost", "prob.property", "bid.price", "cons.benefit", "area")]
    result <- rbind(result, agg.benefits)
  } else {
    result <- rbind(result, df.tot.cost)
  }
  
  }
  
  se <- standard_error(result$cons.benefit)
  se.df <- rbind(se.df, se)
  sd <- sd(result$cons.benefit)
  sd.df <- rbind(sd.df, sd)
  
  #Get the mean of all simulations
  if (nrow(result)[1]>0){
    agg.result <- aggregate(x = result[c("npv","admin.cost", "prob.property","bid.price","cons.benefit", "area")], by = result[c("puid")], FUN = mean)
    df_final <- rbind(df_final, agg.result)
  } else {
    df_final <- rbind(df_final, result)
  }

  save(df_final, file = paste0("./preprocessing/par/df_final", paste(run), ".RData"))
  write.csv(se.df, file = paste0("./preprocessing/par/SE_", scen, "_", b, "_", paste(run), "_se.csv"))
  write.csv(sd.df, file = paste0("./preprocessing/par/SD_", scen, "_", b, "_", paste(run), "_sd.csv"))
  
}
parallel::stopCluster(cl = my.cluster)
}

standard_error <- function(x) sd(x) / sqrt(length(x))

##Function to delete all files and keep and load the one we want (the last one)
#This is a work around as parallel processing function doesn't save results in a single environment, 
#so this exports results for each loop
ld <- function(pt){
ll <- list.files(path=pt, pattern = '.RData', full.names=TRUE)
ll <- ll[1:length(ll)-1]
file.remove(ll)
load(paste0(pt, "df_final93.RData"))
}
ld.se <- function(pt){
  ll <- list.files(path=pt, pattern = '.RData', full.names=TRUE)
  ll <- ll[1:length(ll)-1]
  file.remove(ll)
  load(paste0(pt, "df_final93.RData"))
}

#####Post processing functions#####
make.maps <- function(outfoldermaps, scen){
  
  # create folder:
  if (!dir.exists(outfoldermaps)){
    dir.create(outfoldermaps, recursive = TRUE)
  } else {
    print("Dir already exists!")
  }
  
  ###For selected or not selected map
  solution <- data.frame(df.to.merge$puid, binary.sol)
  selected_pus_scen1 <- solution$df.to.merge.puid[solution$binary.sol==1]
  
  png(file=paste0(outfoldermaps, "/", scen, "_bin_budget_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  plot(shp.pu)
  shp.pu.sub <- shp.pu[shp.pu$CADID %in% c(selected_pus_scen1),]
  plot(shp.pu.sub, col = "#8a2be2", add = TRUE)
  cex <- 1
  # scaleBar(shp.pu.sub, pos = "bottomleft",   
  #          cex=1,
  #          pt.cex = 1.1*cex,
  #          seg.len=10*cex,
  #          title.cex=cex,
  #          outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
  #HEATMAP
  ###For heat map showing selected budget
  ##Join budget values to shp file
  pu.and.budget <- data.frame(df.to.merge$puid, budget.sol.prop)
  colnames(pu.and.budget) <- c("CADID", "budget.sol.prop")
  shp.pu.joined <- merge(shp.pu, pu.and.budget, by = "CADID")
  shp.pu.joined[is.na(shp.pu.joined$budget.sol.prop)] <- 0
  #Export the map
  png(file=paste0(outfoldermaps, "./", scen, "_heatmap_budget_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  #plot the polygons without colour
  plot(shp.pu.joined)
  # Set the palette
  p <- colorRampPalette(c("white", "#fbb040", "#ef4236"))(128)
  palette(p)
  # Scale the values to the palette
  vals <- shp.pu.joined$budget.sol.prop
  cols <- (vals - min(vals))/diff(range(vals))*127+1
  plot(shp.pu.joined, col=cols)
  
  #Add a legend
  levels <- unique(shp.pu.joined$budget.sol.prop)
  levels <- round(levels, digits=1)
  levels <- sort(levels)
  col.levels <- unique(cols)
  col.levels <- sort(col.levels)
  
  # add a legend to your map
  legend("topright",   # location of legend
         legend = levels, # categories or elements to render in
         # the legend
         fill = col.levels, # color palette to use to fill objects in legend.
         title = "Budget allocated $ AUD",
         bty = "n",
         cex = 1.2)
  cex <- 1
  # scaleBar(shp.pu.sub, pos = "bottomleft",   
  #          cex=1,
  #          pt.cex = 1.1*cex,
  #          seg.len=10*cex,
  #          title.cex=cex,
  #          outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
  
  #HEATMAP
  ###For heat map showing selected conservation benefit
  ##Join budget values to shp file
  pu.and.consben <- data.frame(df.to.merge$puid, consben.sol)
  colnames(pu.and.consben) <- c("CADID", "consben.sol")
  shp.pu.joined <- merge(shp.pu, pu.and.consben, by = "CADID")
  shp.pu.joined[is.na(shp.pu.joined$consben.sol)] <- 0
  #Export the map
  png(file=paste0(outfoldermaps, "./", scen, "_consben_heatmap_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  #plot the polygons without colour
  plot(shp.pu.joined)
  # Set the palette
  p <- colorRampPalette(c("white", "#009933", "#003300"))(128)
  palette(p)
  # Scale the values to the palette
  vals <- shp.pu.joined$consben.sol
  cols <- (vals - min(vals))/diff(range(vals))*127+1
  plot(shp.pu.joined, col=cols)
  
  #Add a legend
  levels <- unique(shp.pu.joined$consben.sol)
  levels <- round(levels, digits=8)
  levels <- sort(levels)
  col.levels <- unique(cols)
  col.levels <- sort(col.levels)
  
  # add a legend to your map
  legend("right",   # location of legend
         legend = levels, # categories or elements to render in
         # the legend
         fill = col.levels, # color palette to use to fill objects in legend.
         # title = "Suitability weighted koala 
         # habitat km2",
         title = "Summed suitability", 
         bty = "n",
         cex = 1.2)
  cex <- 1
  # scaleBar(shp.pu.sub, pos = "bottomleft",   
  #          cex=1,
  #          pt.cex = 1.1*cex,
  #          seg.len=10*cex,
  #          title.cex=cex,
  #          outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
  ###For heat map showing selected conservation benefit/area
  ##Join budget values to shp file
  pu.and.consbenarea <- data.frame(df.to.merge$puid, consben.area.sol)
  colnames(pu.and.consbenarea) <- c("CADID", "consben.area.sol")
  shp.pu.joined <- merge(shp.pu, pu.and.consbenarea, by = "CADID")
  shp.pu.joined[is.na(shp.pu.joined$consben.area.sol)] <- 0
  #Export the map
  png(file=paste0(outfoldermaps, "./", scen, "_consben_area_heatmap_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  #plot the polygons without colour
  plot(shp.pu.joined)
  # Set the palette
  p <- colorRampPalette(c("white", "#009933", "#003300"))(128)
  palette(p)
  # Scale the values to the palette
  vals <- shp.pu.joined$consben.area.sol
  cols <- (vals - min(vals))/diff(range(vals))*127+1
  plot(shp.pu.joined, col=cols)
  
  #Add a legend
  levels <- unique(shp.pu.joined$consben.area.sol)
  levels <- round(levels, digits=8)
  levels <- sort(levels)
  col.levels <- unique(cols)
  col.levels <- sort(col.levels)
  
  # add a legend to your map
  legend("right",   # location of legend
         legend = levels, # categories or elements to render in
         # the legend
         fill = col.levels, # color palette to use to fill objects in legend.
         # title = "Suitability weighted koala 
         # habitat (km2)/area of property (km2)",
         title = "Summed suitability/
         area of property (km2)",
         bty = "n",
         cex = 1.2)
  cex <- 1
  # scaleBar(shp.pu.sub, pos = "bottomleft",   
  #          cex=1,
  #          pt.cex = 1.1*cex,
  #          seg.len=10*cex,
  #          title.cex=cex,
  #          outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
}
make.maps.19areas <- function(outfoldermaps, scen){
  
  # create folder:
  if (!dir.exists(outfoldermaps)){
    dir.create(outfoldermaps, recursive = TRUE)
  } else {
    print("Dir already exists!")
  }
  
  ###For selected or not selected map
  solution <- data.frame(df.to.merge$puid, binary.sol)
  selected_pus_scen1 <- solution$df.to.merge.puid[solution$binary.sol==1]
  
  png(file=paste0(outfoldermaps, "/", scen, "_bin_budget_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  plot(study_region_outline)
  plot(shp.pu, add = TRUE)
  shp.pu.sub <- shp.pu[shp.pu$Invest_PPA %in% c(selected_pus_scen1),]
  plot(shp.pu.sub, col = "#8a2be2", add = TRUE)
  cex <- 1
  scaleBar(shp.pu.sub, pos = "bottomleft",   
           cex=1,
           pt.cex = 1.1*cex,
           seg.len=10*cex,
           title.cex=cex,
           outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
  #HEATMAP
  ###For heat map showing selected budget
  ##Join budget values to shp file
  pu.and.budget <- data.frame(df.to.merge$puid, budget.sol)
  colnames(pu.and.budget) <- c("Invest_PPA", "budget.sol")
  shp.pu.joined <- merge(shp.pu, pu.and.budget, by = "Invest_PPA")
  shp.pu.joined[is.na(shp.pu.joined$budget.sol)] <- 0
  #Export the map
  png(file=paste0(outfoldermaps, "./", scen, "_heatmap_budget_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  #plot the polygons without colour
  plot(study_region_outline)
  plot(shp.pu.joined, add = TRUE)
  # Set the palette
  p <- colorRampPalette(c("white", "#fbb040", "#ef4236"))(128)
  palette(p)
  # Scale the values to the palette
  vals <- shp.pu.joined$budget.sol
  cols <- (vals - min(vals))/diff(range(vals))*127+1
  plot(study_region_outline)
  plot(shp.pu.joined, col=cols, add = TRUE)
  
  #Add a legend
  levels <- unique(shp.pu.joined$budget.sol)
  levels <- round(levels, digits=1)
  levels <- sort(levels)
  col.levels <- unique(cols)
  col.levels <- sort(col.levels)
  
  # add a legend to your map
  legend("topright",   # location of legend
         legend = levels, # categories or elements to render in
         # the legend
         fill = col.levels, # color palette to use to fill objects in legend.
         title = "Budget allocated $ AUD",
         bty = "n",
         cex = 1.2)
  cex <- 1
  scaleBar(shp.pu.sub, pos = "bottomleft",   
           cex=1,
           pt.cex = 1.1*cex,
           seg.len=10*cex,
           title.cex=cex,
           outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
  
  #HEATMAP
  ###For heat map showing selected conservation benefit
  ##Join budget values to shp file
  pu.and.consben <- data.frame(df.to.merge$puid, consben.sol)
  colnames(pu.and.consben) <- c("CADID", "consben.sol")
  shp.pu.joined <- merge(shp.pu, pu.and.consben, by = "CADID")
  shp.pu.joined[is.na(shp.pu.joined$consben.sol)] <- 0
  #Export the map
  png(file=paste0(outfoldermaps, "./", scen, "_consben_heatmap_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  #plot the polygons without colour
  plot(shp.pu.joined)
  # Set the palette
  p <- colorRampPalette(c("white", "#009933", "#003300"))(128)
  palette(p)
  # Scale the values to the palette
  vals <- shp.pu.joined$consben.sol
  cols <- (vals - min(vals))/diff(range(vals))*127+1
  plot(shp.pu.joined, col=cols)
  
  #Add a legend
  levels <- unique(shp.pu.joined$consben.sol)
  levels <- round(levels, digits=8)
  levels <- sort(levels)
  col.levels <- unique(cols)
  col.levels <- sort(col.levels)
  
  # add a legend to your map
  legend("right",   # location of legend
         legend = levels, # categories or elements to render in
         # the legend
         fill = col.levels, # color palette to use to fill objects in legend.
         # title = "Suitability weighted koala 
         # habitat km2",
         title = "Summed suitability", 
         bty = "n",
         cex = 1.2)
  cex <- 1
  scaleBar(shp.pu.sub, pos = "bottomleft",   
           cex=1,
           pt.cex = 1.1*cex,
           seg.len=10*cex,
           title.cex=cex,
           outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
  ###For heat map showing selected conservation benefit/area
  ##Join budget values to shp file
  pu.and.consbenarea <- data.frame(df.to.merge$puid, consben.area.sol)
  colnames(pu.and.consbenarea) <- c("CADID", "consben.area.sol")
  shp.pu.joined <- merge(shp.pu, pu.and.consbenarea, by = "CADID")
  shp.pu.joined[is.na(shp.pu.joined$consben.area.sol)] <- 0
  #Export the map
  png(file=paste0(outfoldermaps, "./", scen, "_consben_area_heatmap_", b, ".png"), width=1000, height=1000)
  par(mar=c(0,0,0,0))
  #plotRGB(b.bg, maxpixels=max(500000, 1000*1000), ext=extent(shp.pu), asp=TRUE)
  #plot the polygons without colour
  plot(shp.pu.joined)
  # Set the palette
  p <- colorRampPalette(c("white", "#009933", "#003300"))(128)
  palette(p)
  # Scale the values to the palette
  vals <- shp.pu.joined$consben.area.sol
  cols <- (vals - min(vals))/diff(range(vals))*127+1
  plot(shp.pu.joined, col=cols)
  
  #Add a legend
  levels <- unique(shp.pu.joined$consben.area.sol)
  levels <- round(levels, digits=8)
  levels <- sort(levels)
  col.levels <- unique(cols)
  col.levels <- sort(col.levels)
  
  # add a legend to your map
  legend("right",   # location of legend
         legend = levels, # categories or elements to render in
         # the legend
         fill = col.levels, # color palette to use to fill objects in legend.
         # title = "Suitability weighted koala 
         # habitat (km2)/area of property (km2)",
         title = "Summed suitability/
         area of property (km2)",
         bty = "n",
         cex = 1.2)
  cex <- 1
  scaleBar(shp.pu.sub, pos = "bottomleft",   
           cex=1,
           pt.cex = 1.1*cex,
           seg.len=10*cex,
           title.cex=cex,
           outer=FALSE)
  #suppressWarnings(plot(v.bnd, col="black", lwd=2, add=TRUE))
  dev.off()
  
}

###Code to get values - Simulation sensitivity analysis
exp.vals.simtest <- function(outfoldervals, scen){
  
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

#Total cons benefit
karea.matrix <- matrix(karea.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.karea.matrix<- solutions*karea.matrix
karea <- sum(solutions.karea.matrix)

#Probability of a bid value
#Make a long string of all conservation benefit
prob.all.incre <- df_new0$prob.property
for (i in 1:y){
  df.name <- paste0("df_new", i)
  prob.all.incre <- c(prob.all.incre, get(df.name)$prob.property)
  prob.all.incre[is.na(prob.all.incre)] = 1000000000000
}

prob.matrix <- matrix(prob.all.incre, nrow=length(unique(pu.df$LGA)))
solutions.prob.matrix <- solutions*prob.matrix
prob <- sum(solutions.prob.matrix)

#Export the final 
final.vals <- data.frame(scen = scen, cost = cost, cons.ben = cons, karea = karea, prob.bid = prob)
write.csv(final.vals, file = paste0(outfoldervals, "./", scen, "_budget_", b, "_", q, ".csv"), row.names = TRUE)
}

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
  
  #Total area
  karea.matrix <- matrix(karea.all.incre, nrow=length(unique(pu.df$LGA)))
  solutions.karea.matrix<- solutions*karea.matrix
  karea <- sum(solutions.karea.matrix)
  
  #Probability of a bid value
  #Make a long string of all conservation benefit
  prob.all.incre <- df_new0$prob.property
  for (i in 1:y){
    df.name <- paste0("df_new", i)
    prob.all.incre <- c(prob.all.incre, get(df.name)$prob.property)
    prob.all.incre[is.na(prob.all.incre)] = 0
  }
  
  prob.matrix <- matrix(prob.all.incre, nrow=length(unique(pu.df$LGA)))
  solutions.prob.matrix <- solutions*prob.matrix
  prob <- sum(solutions.prob.matrix)
  
  #Get a measure of SD
  sd.matrix <- matrix(sd.all.incre, nrow=length(unique(pu.df$LGA)))
  solutions.sd.matrix<- solutions*sd.matrix
  st.dev <- mean(solutions.sd.matrix[solutions.sd.matrix>0])
  
  #Get a measure of SE
  se.matrix <- matrix(se.all.incre, nrow=length(unique(pu.df$LGA)))
  solutions.se.matrix<- solutions*se.matrix
  s.err <- mean(solutions.se.matrix[solutions.se.matrix>0])
  
  #Get amount of budget allocated to each planning unit and export it
  pu.and.budget.to.save <- data.frame(df.to.merge$puid, budget.sol)
  #write.csv(pu.and.budget.to.save, file = paste0(outfoldervals, "./", scen, "_pu_vs_budget_", b, ".csv"), row.names = TRUE)
  
  #Export the final 
  final.vals <- data.frame(scen = scen, cost = cost, cons.ben = cons, karea = karea, prob.bid = prob, sd = st.dev, se = s.err)
  write.csv(final.vals, file = paste0(outfoldervals, "./", scen, "_budget_", b, ".csv"), row.names = TRUE)
  
}
exp.vals.19areas <- function(outfoldervals, scen){
  
  # create folder:
  if (!dir.exists(outfoldervals)){
    dir.create(outfoldervals, recursive = TRUE)
  } else {
    print("Dir already exists!")
  }
  
  #Total cost
  cost <- sum(budget.sol)
  
  #Total cons benefit
  cons.matrix <- matrix(cons.all.incre, nrow=length(unique(df$puid)))
  solutions.cons.matrix <- solutions*cons.matrix
  cons <- sum(solutions.cons.matrix)
  
  #Total area
  karea.matrix <- matrix(karea.all.incre, nrow=length(unique(df$puid)))
  solutions.karea.matrix<- solutions*karea.matrix
  karea <- sum(solutions.karea.matrix)
  
  #Probability of a bid value
  #Make a long string of all conservation benefit
  prob.all.incre <- df_new0$prob.property
  for (i in 1:y){
    df.name <- paste0("df_new", i)
    prob.all.incre <- c(prob.all.incre, get(df.name)$prob.property)
    prob.all.incre[is.na(prob.all.incre)] = 1000000000000
  }
  
  prob.matrix <- matrix(prob.all.incre, nrow=length(unique(df$puid)))
  solutions.prob.matrix <- solutions*prob.matrix
  prob <- sum(solutions.prob.matrix)
  
  #Get a measure of SD
  sd.matrix <- matrix(sd.all.incre, nrow=length(unique(df$puid)))
  solutions.sd.matrix<- solutions*sd.matrix
  st.dev <- mean(solutions.sd.matrix[solutions.sd.matrix>0])
  
  #Get a measure of SE
  se.matrix <- matrix(se.all.incre, nrow=length(unique(df$puid)))
  solutions.se.matrix<- solutions*se.matrix
  s.err <- mean(solutions.se.matrix[solutions.se.matrix>0])
  
  #Get amount of budget allocated to each planning unit and export it
  pu.and.budget.to.save <- data.frame(df.to.merge$puid, budget.sol)
  write.csv(pu.and.budget.to.save, file = paste0(outfoldervals, "./", scen, "_pu_vs_budget_", b, ".csv"), row.names = TRUE)
  
  #Export the final 
  final.vals <- data.frame(scen = scen, cost = cost, cons.ben = cons, karea = karea, prob.bid = prob, sd = st.dev, se = s.err)
  write.csv(final.vals, file = paste0(outfoldervals, "./", scen, "_budget_", b, ".csv"), row.names = TRUE)
  
}
exp.vals.scen1 <- function(outfoldervals, scen){
  
  # create folder:
  if (!dir.exists(outfoldervals)){
    dir.create(outfoldervals, recursive = TRUE)
  } else {
    print("Dir already exists!")
  }
  
  #Total cost
  cost <- sum(budget.sol)
  
  #Total cons benefit
  cons <- sum(consben.sol)
  
  #Total area
  karea <- sum(area.sol)
  
  #Probability of a bid value
  prob <- sum(df_final$prob.property)
  
  #Get a measure of SD
  st.dev <- mean(sd.df_scen1[sd.df_scen1>0])
  
  #Get a measure of SE
  s.err <- mean(se.df_scen1[se.df_scen1>0])
  
  #Get amount of budget allocated to each planning unit and export it
  pu.and.budget.to.save <- data.frame(df.to.merge$puid, budget.sol)
  write.csv(pu.and.budget.to.save, file = paste0(outfoldervals, "./", scen, "_pu_vs_budget_", b, ".csv"), row.names = TRUE)
  
  #Export the final 
  final.vals <- data.frame(scen = scen, cost = cost, cons.ben = cons, karea = karea, prob.bid = prob, sd = st.dev, se = s.err)
  write.csv(final.vals, file = paste0(outfoldervals, "./", scen, "_budget_", b, ".csv"), row.names = TRUE)
  
}

###Code to get values
exp.vals2 <- function(outfoldervals, scen){
  
  # create folder:
  if (!dir.exists(outfoldervals)){
    dir.create(outfoldervals, recursive = TRUE)
  } else {
    print("Dir already exists!")
  }
  
  #Total cost
  cost <- sum(budget.sol)
  
  #Total cons benefit
  cons.matrix <- matrix(koala.all.incre, nrow=length(unique(pu.df$LGA)))
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
  write.csv(final.vals, file = paste0(outfoldervals, "./", scen, "_budget_", b, ".csv"), row.names = TRUE)
  
}

