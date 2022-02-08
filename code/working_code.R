library(gurobi)

#test dataset
puid <- c(1:20)
#number of planning units in property
no.p.pu <- runif(20, min=5, max=100)
#probability that property i in planning unit j protects habitat with engagement
prob.e <- runif(20, min=0, max=1)
#probability that property i in planning unit j protects habitat without engagement
prob.no.e <- runif(20, min=0, max=1)
#Area in ha
A <- runif(20, min=0, max=150)
#cost
cost <- runif(20, min=5, max=1000)
#benefit <- runif(20, min=5, max=10)

df <- data.frame(puid,no.p.pu, prob.e, prob.no.e, A, cost)

#functions
# benefit <- function(df){
#   df$prob.e*df$A+df$prob.no.e*df$A
# }

benefit <- df$prob.e*df$A+df$prob.no.e*df$A
df$benefit <- benefit

npu <- length(df$puid)
#Budget
B <- 1000

model <- list()
model$A <- matrix(df$cost, nrow=1)
model$obj        <- df$cost*df$benefit
model$modelsense <- 'max'
model$rhs        <- B
model$sense      <- '<'
model$vtype      <- 'B'

params <- list(OutputFlag=0)

result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

###Need to be careful about indexing here
solution <- data.frame(puid, result$x)
selected_pus <- solution$puid[solution$result.x==1]
selected_pus










# Copyright 2021, Gurobi Optimization, LLC
#
# This example formulates and solves the following simple MIP model:
#  maximize
#        x +   y + 2 z
#  subject to
#        x + 2 y + 3 z <= 4
#        x +   y       >= 1
#        x, y, z binary

library(gurobi)

model <- list()

model$A          <- matrix(c(1,2,3,1,1,0), nrow=2, ncol=3, byrow=T)
model$obj        <- c(1,1,2)
model$modelsense <- 'max'
model$rhs        <- c(4,1)
model$sense      <- c('<', '>')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

# Clear space
rm(model, result, params)
