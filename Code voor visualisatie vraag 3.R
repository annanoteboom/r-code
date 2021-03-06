## Importing the data sets we need
`Agents_3S_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_3S_0.csv")
`Agents_ACF_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_ACF_0.csv")
`Agents_B+_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_B+_0.csv")
`Links_3S_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_3S_0.csv")
`Links_ACF_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_ACF_0.csv")
`Links_B+_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_B+_0.csv")

#Function that plots a matrix of the trustlevels between agents with roll x + affiliation x and agents with roll y + affiliation y
TrustGraph <- function(Roll1,Roll2,Aff1,Aff2,datasetLinks,datasetAgents) {
  datasetLinks$Agent1 <- as.character(datasetLinks$Agent1)
  datasetLinks$Agent2 <- as.character(datasetLinks$Agent2)
  
  agent1nr <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {agent1nr[[j]] <- substr(datasetLinks$Agent1[i], regexpr(":", datasetLinks$Agent1[i])+2, nchar(datasetLinks$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
  datasetLinks$agent1nr = agent1nr
  
  agent2nr <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {agent2nr[[j]] <- substr(datasetLinks$Agent2[i], regexpr(":", datasetLinks$Agent2[i])+2, nchar(datasetLinks$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
  datasetLinks$agent2nr = agent2nr
  
  agent1aff <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {agent1aff[[j]] <- datasetLinks$agent1nr[[i]]+1; agent1aff[[j]] <- datasetAgents$Affiliation[agent1aff[[j]]]; j = j + 1}
  datasetLinks$agent1aff = agent1aff
  
  agent2aff <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {agent2aff[[j]] <- datasetLinks$agent2nr[[i]]+1; agent2aff[[j]] <- datasetAgents$Affiliation[agent2aff[[j]]]; j = j + 1}
  datasetLinks$agent2aff = agent2aff
  
  RowlstName <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {if (grepl(Roll1,datasetLinks$Agent2[[i]]) && grepl(Roll2,datasetLinks$Agent1[[i]]) || grepl(Roll1,datasetLinks$Agent1[[i]]) && grepl(Roll2,datasetLinks$Agent2[[i]])) {RowlstName[[j]] = i; j = j+1}}
  
  CleanName <- list()
  j <- 1
  for (i in 1:length(RowlstName)) { if (datasetLinks$agent1aff[RowlstName[[i]]] == Aff1 && datasetLinks$agent2aff[RowlstName[[i]]] == Aff2 || datasetLinks$agent2aff[RowlstName[[i]]] == Aff1 && datasetLinks$agent1aff[RowlstName[[i]]] == Aff2){CleanName[[j]] <- RowlstName[[i]]; j = j+1}}
  
  for(i in 1:length(CleanName)){if( CleanName[i]>max(datasetLinks$LinksID )) {tempx <- i-1;break}}
  CleanName <- matrix(unlist(CleanName),nrow = tempx)
  
  for(row in 1:nrow(CleanName)) {
    for(col in 1:ncol(CleanName)) {
      CleanName[row,col] <- datasetLinks$Agent3[[CleanName[row,col]]]
    }
  }
  
  matplot(t(CleanName),type = 'l',xlab = "time", ylab = "Trust", main = paste("Trustlevel for Links between",Roll1, "and\n", Roll2, "with affiliations",Aff1, "and", Aff2))
}

## Broken down into our thinking process

# Convert the Agent column into character
Links_3S_0$Agent1 <- as.character(Links_3S_0$Agent1)
Links_3S_0$Agent2 <- as.character(Links_3S_0$Agent2)

# Adding two columns to the dataset with the agentnumbers of the Agents
agent1nr <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {agent1nr[[j]] <- substr(Links_3S_0$Agent1[i], regexpr(":", Links_3S_0$Agent1[i])+2, nchar(Links_3S_0$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
Links_3S_0$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {agent2nr[[j]] <- substr(Links_3S_0$Agent2[i], regexpr(":", Links_3S_0$Agent2[i])+2, nchar(Links_3S_0$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
Links_3S_0$agent2nr = agent2nr

# Adding a columns to the datasets with the affiliations of the Agents
agent1aff <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {agent1aff[[j]] <- Links_3S_0$agent1nr[[i]]+1; agent1aff[[j]] <- Agents_3S_0$Affiliation[agent1aff[[j]]]; j = j + 1}
Links_3S_0$agent1aff = agent1aff

agent2aff <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {agent2aff[[j]] <- Links_3S_0$agent2nr[[i]]+1; agent2aff[[j]] <- Agents_3S_0$Affiliation[agent2aff[[j]]]; j = j + 1}
Links_3S_0$agent2aff = agent2aff

# Extracting the rows in which the desired rolls and Affiliations (is going into a function so you can choose that yourself)
# In this case (for example) We used the rows that linked a policy maker with a External agent and the affiliations had to be 1 and 2
RowlstName <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {if (grepl('Policy maker',Links_3S_0$Agent2[[i]]) && grepl('External party',Links_3S_0$Agent1[[i]]) || grepl('Policy maker',Links_3S_0$Agent1[[i]]) && grepl('External party',Links_3S_0$Agent2[[i]])) {RowlstName[[j]] = i; j = j+1}}

CleanName <- list()
j <- 1
for (i in 1:length(RowlstName)) { if (Links_3S_0$agent1aff[RowlstName[[i]]] == 2 && Links_3S_0$agent2aff[RowlstName[[i]]] == 1 || Links_3S_0$agent2aff[RowlstName[[i]]] == 2 && Links_3S_0$agent1aff[RowlstName[[i]]] == 1){CleanName[[j]] <- RowlstName[[i]]; j = j+1}}

## Turning the rownumbers into a matrix with 1 specific agent trustlevel in a row over time
for(i in 1:length(CleanName)){if( CleanName[i]>max(Links_3S_0$LinksID )) {tempx <- i-1;break}}
CleanName <- matrix(unlist(CleanName),nrow = tempx)

for(row in 1:nrow(CleanName)) {
  for(col in 1:ncol(CleanName)) {
    CleanName[row,col] <- Links_3S_0$Agent3[[CleanName[row,col]]]
  }
}
## Plotting the matrix
matplot(t(CleanName),type = 'l',xlab = "time", ylab = "Trust", main = paste("Trustlevel over time"))

