## Importing the data sets we need
`Agents_3S_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_3S_0.csv")
`Agents_ACF_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_ACF_0.csv")
`Agents_B+_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_B+_0.csv")
`Links_3S_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_3S_0.csv")
`Links_ACF_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_ACF_0.csv")
`Links_B+_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_B+_0.csv")

library(igraph)
library(animation)
library(ggplot2)
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

# Making a list of all the links between agents that are aware of each other (agent 3 > 0) in the first tick
newlist1 <- list()
newlist2 <- list()
newlist3 <- list()
j <- 1
for (i in 1:435) {if (Links_3S_0$Agent3[i] > 0) {newlist1[[j]] <- as.integer(Links_3S_0$agent1nr[i]); newlist2[[j]] <- as.integer(Links_3S_0$agent2nr[i]); newlist3[[j]] <- unlist(Links_3S_0$Agent3[i]); j = j+1}}

# Making a dataframe out of these two lists
edges = data.frame(as.double(newlist1),as.double(newlist2))
edges$TrustLevel <- newlist3

# Making a dataframe with the agenttypes per agent ID
#Vertices <- data.frame(Agents_3S_0$AgentID[1:(max(Agents_3S_0$AgentID)+1)])

# Plottig the network with different colors per Agent type
network <- graph_from_data_frame(d=edges, directed=F)
lay <- layout.fruchterman.reingold(network)
image1 <- plot(network, layout=lay)

newlist1 <- list()
newlist2 <- list()
newlist3 <- list()
j <- 1
for (i in 435:870) {if (Links_3S_0$Agent3[i] > 0) {newlist1[[j]] <- as.integer(Links_3S_0$agent1nr[i]); newlist2[[j]] <- as.integer(Links_3S_0$agent2nr[i]); newlist3[[j]] <- unlist(Links_3S_0$Agent3[i]); j = j+1}}
edges = data.frame(as.double(newlist1),as.double(newlist2))
edges$TrustLevel <- newlist3
network2 <- graph_from_data_frame(d=edges, directed=F)
lay <- layout.fruchterman.reingold(network2)
image2 <- plot(network2, layout=lay)

#Werkt nog niet: probeer filmpje te maken van de afbeeldingen
#vectorrr <- c('rplot2.png','rplot1.png')
#test1 <- jpeg('rplot1.png')
#plot(network)
#dev.off()

# Attempts to color the different nodes per Agent type, unsuccessful
#V(network)$color = z
#V(network)$color <- unlist(V(network)$VertexColor)
#V(network)$color = c("blue","blue", "blue","blue","blue", "blue","blue","blue", "blue","red", "red", "red","red", "red", "red","red", "red", "red","red", "red", "red","red", "red", "red","red", "red", "red","red", "red", "red")
#colrs <- adjustcolor( c("gray50", "tomato", "gold"), alpha=.6)
#plot(network, vertex.color=colrs[z])
#V(network)$color <- colrs[V(network)$Agenttype]
#E(network)$width <- edges$TrustLevel
#Typelist <- list()
#Colorlist <- list()
#for (i in 1:(max(Agents_3S_0$AgentID)+1)) {if (grepl("Externalparties",Agents_3S_0$Type[i])) {Typelist[[i]] <- "External Party" ; Colorlist[[i]] <- "yellow"}; if (grepl("Policymakers",Agents_3S_0$Type[i])){Typelist[[i]] <- "Policy maker"; Colorlist[[i]] <- "red"}; if (grepl("Policyentres",Agents_3S_0$Type[i])){Typelist[[i]] <- "Policy Entres"; Colorlist[[i]] <- "blue"}}
#Vertices$Agenttype <- Typelist
#Vertices$VertexColor <- Colorlist
#z = unlist(Vertices$VertexColor[1])
#for (i in 2:30) {z = c(z,unlist(Vertices$VertexColor[i]))}

# Ideas
# Plot a Dijkstra graph as well
# Centrality measure (igraph) or clustering coefficient
# Identify the top 5 agents that are "most centralised" 
Centrality_agents <- closeness(network,vids=V(network))
sort(Centrality_agents,decreasing = TRUE)[1:5]
# Plot the number of links with agents that has the strongest bond
Toplist_nr_of_links = list()
for (i in 1:max(edges$as.double.newlist2.)) {for (j in 1:length(edges$as.double.newlist1.)) {if (edges$as.double.newlist1.[j] == i || edges$as.double.newlist2.[j] == i) L = L+1}; Toplist_nr_of_links[i] = L; L = 0}
Toplist_nr_of_links <- as.numeric(Toplist_nr_of_links)
Topagents_nr_of_links <- sort.list(Toplist_nr_of_links,decreasing=TRUE)[1:5]
Topagents_nr_of_links
# Print the top 5 of agents with the strongest aggregate bonds
Toplist = list()
L = 0
for (i in 1:max(edges$as.double.newlist2.)) {for (j in 1:length(edges$as.double.newlist1.)) {if (edges$as.double.newlist1.[j] == i || edges$as.double.newlist2.[j] == i) L = L+as.numeric(edges$TrustLevel[j])}; Toplist[i] = L; L = 0}
Toplist <- as.numeric(Toplist)
Topagents <- sort.list(Toplist,decreasing=TRUE)[1:5]
Topagents
# Take a few timesteps, replot the same network but drop the agents with a trustlevel
# below a various level of trustlevel (with a faced grid)
Trustgraph <- function(MinimumTrust,Links_Data_Frame,lay_out) {
  newlist1 <- list()
  newlist2 <- list()
  newlist3 <- list()
  j = 1
  for (i in 1:(length(Links_Data_Frame$Agent3)/500)) {if (Links_Data_Frame$Agent3[i] > MinimumTrust) {newlist1[[j]] <- as.integer(Links_Data_Frame$agent1nr[i]); newlist2[[j]] <- as.integer(Links_Data_Frame$agent2nr[i]); newlist3[[j]] <- unlist(Links_Data_Frame$Agent3[i]); j = j+1}}
  edges = data.frame(as.double(newlist1),as.double(newlist2))
  edges$TrustLevel <- newlist3
  network <- graph_from_data_frame(d=edges, directed=F)
  plot(network, layout=lay)
}

p1 <- Trustgraph(0.8, Links_3S_0, lay)
p2 <- Trustgraph(0.85,Links_3S_0,lay)
p3 <- Trustgraph(0.9,Links_3S_0,lay)
p4 <- Trustgraph(0.95,Links_3S_0,lay)

multiplot(p1,p2,p3,p4,cols=2)

#Trustgraphfunction broken down into pieces for 3 streams model
Minimum_Trust <- 0.95
newlist1 <- list()
newlist2 <- list()
newlist3 <- list()
j <- 1
for (i in 1:435) {if (Links_3S_0$Agent3[i] > Minimum_Trust) {newlist1[[j]] <- as.integer(Links_3S_0$agent1nr[i]); newlist2[[j]] <- as.integer(Links_3S_0$agent2nr[i]); newlist3[[j]] <- unlist(Links_3S_0$Agent3[i]); j = j+1}}

# Making a dataframe out of these two lists
edges = data.frame(as.double(newlist1),as.double(newlist2))
edges$TrustLevel <- newlist3
# plotting the graph
lay <- layout.fruchterman.reingold(network)
plot(network, layout=lay_out)

# Plot the centrality over time, first by seeing which actors are in the top 5 over time

# Plot the aggregated trustlevel over time for all agents that have been in the top 5 over the run

# Plot the perceptions of these agents over time

# Make a table of the ticks at which the agents in the network of the "top 5" dropped in or out of their network

# Maybe make a dumbbell graph of the prefered policies for the important agents with a dot for everytime the policy changes (in excel)