library(gurobi)
library(dplyr)

#test dataset
#Each planning unit is an LGA
puid <- rep(c(1:20),each= 10)
npu <- length(puid)
#Ci is the Net present value of amount invested in a tender in planning unit i 
npv <- rep(c(runif(c(1:20), min=500, max=8000)),each= 10)
#Ai is the administrative costs of running a tender in planning unit i. 
admin.cost <- rep(runif(c(1:20), min=5, max=30), each = 10)
#property id - assumed to be in order of BCT ranking process
property <- c(1:npu)
#Iijk Bernoulli distributed probability that property j in planning unit i will put in a bid
prob.property <- runif(npu, min=0, max=1)
#pij net present value of the bid price of property j in planning unit i
bid.price <- runif(npu, min=0, max=2000)
#bij conservation benefit of each property
cons.benefit <- runif(npu, min=0, max=3000)
#Area in ha
area <- runif(npu, min=0, max=150)
#Tie it together to create dataframe
df <- data.frame(puid, npv, admin.cost, property, prob.property, bid.price, cons.benefit)
head(df)

#Functions
####Function to select properties in order up until the NPV constraint
properties <- function(df){
df_new <- data.frame()
for (i in 1:length(unique(df$puid))){
  df1 <- df[which(df$puid == paste0(i)),] 
  # #####Constrint to ensure that the selected properties do not exceed the allocated funding (F)
  if (sum(df1$bid.price) > df1[1,]$npv - df1[1,]$admin.cost) {
    #Place in the order reflecting the BCT prioritisation process
    df1 <- df1[order(df1$property),]
    #Create a new column that is the cummulative sum of bid price
    df1 <- df1 %>% group_by(puid) %>% mutate(csum = cumsum(df1$bid.price))
    #Turn into a dataframe
    df1 <- as.data.frame(df1)
    #Create a subset which is only those properties lower than the investment amount for the LGA
    df1 <- df1[which(df1$csum < df1[1,]$npv - df1[1,]$admin.cost),]    
    #Remove the cummlative sum column
    df1 <- df1[1:ncol(df1)-1]   
    df1
  } else{
    df1 <- df1
  }
  df_new <- rbind(df_new, df1)
}
return(df_new)
}

df_new <- properties(df)  
head(df_new)

#Get the overall mean for each planning unit/LGA
df_new.ag <- aggregate(x = df_new[c("npv","admin.cost", "prob.property","bid.price","cons.benefit")], by = df_new[c("puid")], FUN = mean)
head(df_new.ag)

#F represents the overall funding
F <- 10000

#Set up the optimisation
#Select the planning units (LGA's)
model <- list()
model$A <- matrix(c(df_new.ag$npv), nrow=1)
model$obj        <- (df_new.ag$prob.property*df_new.ag$bid.price)*df_new.ag$cons.benefit
model$modelsense <- 'max'
model$rhs        <- c(F)
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
selected_pus <- solution$df_new.ag.puid[solution$result.x==1]
selected_pus





























######Old Code##############
#S represents the number of simulations
S<- 8
#Function to select random properties among the LGAs with budget constraint
#The assumption here is that the order of the property ids needs to match the priority of BCT
random.properties <- function(df, S){
  if (S > length(unique(df$puid))){
    print("Random selection is greater than number of properties in LGA, choose a lower number")} else {
      df_new <- data.frame()
      for (i in 1:length(unique(df$puid))){
        df1 <- df[which(df$puid == paste0(i)),] 
        #Select random sample
        df1 <- df1[sample(nrow(df1), S), ]
        
        # #####Constrint to ensure that the selected properties do not exceed the allocated funding (F)
        if (sum(df1$bid.price) > df1[1,]$npv - df1[1,]$admin.cost) {
          #Place in the order reflecting the BCT prioritisation process
          df1 <- df1[order(df1$property),]
          #Create a new column that is the cummulative sum of bid price
          df1 <- df1 %>% group_by(puid) %>% mutate(csum = cumsum(df1$bid.price))
          #Turn into a dataframe
          df1 <- as.data.frame(df1)
          #Create a subset which is only those properties lower than the investment amount for the LGA
          df1 <- df1[which(df1$csum < df1[1,]$npv - df1[1,]$admin.cost),]    
          #Remove the cummlative sum column
          df1 <- df1[1:ncol(df1)-1]   
          df1
        } else{
          next
        }
        
        df_new <- rbind(df_new, df1)
      }
      return(df_new)
    }
}

#one simulated dataframe
sim.df <- random.properties(df, 6)
#simulated dataframe n = number of simulations
sim.df <- replicate(n=1000, random.properties(df, 6))
#Check matrix
sim.df
#Get the mean of each simulation for each planning unit
mean.sim <- data.frame()
for (i in 1:ncol(sim.df)){
  ag.test <- aggregate(x = sim.df[,3][c("npv","admin.cost", "prob.property","bid.price","cons.benefit")], by = sim.df[,3][c("puid")], FUN = mean)
  mean.sim <- rbind(mean.sim, ag.test)
}
#Get the overall mean for each planning unit
sim.df.ag <- aggregate(x = mean.sim[c("npv","admin.cost", "prob.property","bid.price","cons.benefit")], by = mean.sim[c("puid")], FUN = mean)



#Function to select random properties among the LGAs
# random.properties <- function(df, S){
#   df_new <- data.frame()
#   for (i in 1:length(unique(df$puid))){
#     df1 <- df[which(df$puid == paste0(i)),] 
#     #Select random sample
#     df1 <- df1[sample(nrow(df1), S), ]
#     #Join the dataframe with all of the selected properties together
#       df_new <- rbind(df_new, df1)
#   }
#   df_new
# }
# 
# random.properties(df, 2)



# #   #Remove rows from the end until it is under this ammount
#    for (m in 1:nrow(df1)){
#      if (sum(df1$npv[1:m]-df1$admin.cost[1:m]) > B){
#        df1.const <- df1[1:m-1,]
#        df_new <- rbind(df_new, df1.const)

benefits <- list()
for (i in 1:length(unique(df$puid))){
  
  df1 <- df[which(df$puid == paste0(i)),] 
  
  #Select random sample
  df1 <- df1[sample(nrow(df1), S), ]
  
  #The inequality here ensured they don’t spend more than the money they have. That is they choose the maximum number of properties that they invest in so that the cost is still less than the budget, C. 
  constr.matrix <- matrix(df1$npv - df1$admin.cost, nrow=1)
  
  model <- list()
  model$A <- constr.matrix
  model$obj        <- df1$prob.property*df1$bid.price
  model$modelsense <- 'max'
  model$rhs        <- F
  model$sense      <- '<'
  model$vtype      <- 'B'
  
  params <- list(OutputFlag=0)
  
  result <- gurobi(model, params)
  
  print('Solution:')
  print(result$objval)
  print(result$x)
  
  benefits[[length(benefits) + 1]] <- result$objval/S
  
}

benefits

#BEN is the expected benefit when implementing a tender in planning unit i,
df$BEN <- rep(c(benefits),each= 10)

benefits <- list()
objective.function <- 

