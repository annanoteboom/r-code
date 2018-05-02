## Idea of the visualisation: Variate: different models; backbone+, ACF & backbone
## Keep constant: belieftreeprofile
## Plot: Evolution of the beliefs of the agents

agents3S0 <- read.csv("~/TB Honours/Project/Data question 2/1_agents_3S_0.csv")
agents_ACF_0 <- read.csv("~/TB Honours/Project/Data question 2/1_agents_ACF_0.csv")
agents_BP0 <- read.csv("~/TB Honours/Project/Data question 2/1_agents_B+_0.csv")
agents_B_0 <- read.csv("~/TB Honours/Project/Data question 2/1_agents_B_0.csv")
Experiments_LHS <- read.csv("~/TB Honours/Project/Data question 2/Experiments_LHS.csv", header=FALSE)

library(ggplot2)
##Function that plots a matrix of the aims of all agents on 1 item in 1 model
matrixbuild <- function(Dataset, colname, policynumber, modelname) {j = 1
datalist <- Dataset[,colname]
datalist <- as.character(datalist)
datalist <- gsub("\\[|\\]| ","",datalist)
datalist <- unlist(strsplit(datalist,","))
datalist <- as.numeric(datalist)
aimlst <- list()
for (i in 1:nrow(Dataset)) {if (i>1) {aimlst[[j]]<-datalist[(i-1)*30+(((policynumber-1)*3)+1)]} else {aimlst[[j]]<- datalist[(((policynumber-1)*3)+1)]}; j <- j + 1}
Aimmatrix1 <- matrix(unlist(aimlst), nrow = 30)
Aimdataframe <- as.data.frame(Aimmatrix1)
#matplot(t(Aimmatrix1),type='l',xlab = "time", ylab = "Aim", main = paste("Policy", policynumber, modelname))
}
ggplot(Aimdataframe)

##Example for policy 2, 3s model

matrixbuild(agents3S0,5,2, "3s Model")
matrixbuild(agents_ACF_0,5,2,"ACF Model")
p3 <- matrixbuild(agents_BP0,5,2,"Backbone+ Model")
multiplot(p1,p2,p3,cols=2)

### The matrixbuildfunction, broken down
## Turn into a character string
belieftree3s <- as.character(agents3S0$Belieftree_policy)
##Omit the spaces and square brackets
belieftree3s <- gsub("\\[|\\]| ","",belieftree3s)
#Take the first row
#belieftree3s1 <- belieftree3s[1]
## Split on the "," character so it will become a row of values
belieftree3s <- unlist(strsplit(belieftree3s,","))
## Turn the values into numeric type instead of character
belieftree3s <- as.numeric(belieftree3s)

## Making a list of all the aims
aimlst <- list()
## Making a matrix of all the aims of the agents over time
j = 1
for (i in 1:nrow(agents3S0)) {if (i>1) {aimlst[[j]]<-belieftree3s[(i-1)*30+1]} else {aimlst[[j]]<- belieftree3s[1]}; j <- j + 1}
Aimmatrix1 <- matrix(unlist(aimlst), nrow = 30)
#Plotting the matrix (Aim over time for 1 specific policy, in this case policy 1)
matplot(t(Aimmatrix1),type = 'l', xlab = "time", ylab = "Aim")
title("Policy 1, 3 Streams")