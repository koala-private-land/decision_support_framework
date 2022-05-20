library(gurobi)
library(dplyr)
library (rgdal)
library(mapmisc)
library(purrr)
library(plyr)
library(stringr)

# To create test dataset
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

#Steps discussed with J
#1. Select sites that put in an EOI
#2. Rank EOIs based on BCT metric
#3. Select top 30 for site assessment
#4. Generate bid for top 30
#5. Rank bids based on cost efficiency
#5a. Generate the % of property covenented
#6. Take top n bids until either 15 properties is reached or max budget is reached
#7. Output property cost + environmental benefit + IDs

####Function to select properties in order up until the NPV constraint
properties <- function(df){
  df_new <- data.frame()
  df_final <- data.frame()
  #i <- unique(df$puid)[6]
  #i <- 108012388
  for (i in unique(df$puid)){
    #To keep track of where it is up to
    print(paste0("Planning_unit_", i))
    
    #Set up dataframes
    #Make a subset of the dataframe which is only data for the respective LGA
    df.LGA <- df[which(df$puid == paste0(i)),]
    #Create empty place to save result of the simulations
    result <- data.frame() 
    
    #For each simulation
    rep <- 10
    
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
        agg.benefits <- aggregate(x = df.tot.cost[c("npv","admin.cost", "prob.property","bid.price","cons.benefit", "cons.benefit.adj")], by = df.tot.cost[c("puid")], FUN = sum)
        result <- rbind(result, agg.benefits)
      } else {
        result <- rbind(result, df.tot.cost)
      }
      
    }
    #Get the mean of all simulations
    if (dim(result)[1]>0){
      agg.result <- aggregate(x = result[c("npv","admin.cost", "prob.property","bid.price","cons.benefit", "cons.benefit.adj")], by = result[c("puid")], FUN = mean)
      df_final <- rbind(df_final, agg.result)
    } else {
      df_final <- rbind(df_final, result)
    }
  }
  
  
  return(df_final)
  #print("complete")
  
}
