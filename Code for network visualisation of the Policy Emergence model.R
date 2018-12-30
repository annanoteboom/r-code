# This file consists of functions that visualizes the outcomes of the Policy Emergence model made by Raphael Klein (2017).
# For each function a example is shown so anyone with basic R knowledge can work the functions.
# For most functions, explanations on the way the function is set up is provided so alterations can be made.
# Apart from functions, a few widgets were made as well so that people without R knowledge can still work the visualizations.
# Enjoy! 

## Run always (untill next ##)
# Importing the data sets we need ----
`Agents_3S_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_3S_0.csv")
`Agents_ACF_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_ACF_0.csv")
`Agents_B+_0` <- read.csv("~/TB Honours/Project/Data question 3/1_agents_B+_0.csv")
agents_3S_5 <- read.csv("~/TB Honours/Project/Data question 4/1_agents_3S_5.csv")
agents_3S_15 <- read.csv("~/TB Honours/Project/Data question 4/1_agents_3S_15.csv")
agents_ACF_2 <- read.csv("~/TB Honours/Project/Data question 4/1_agents_ACF_2.csv")
agents_ACF_15 <- read.csv("~/TB Honours/Project/Data question 4/1_agents_ACF_15.csv")
`Links_3S_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_3S_0.csv")
`Links_ACF_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_ACF_0.csv")
`Links_B+_0` <- read.csv("~/TB Honours/Project/Data question 3/1_links_B+_0.csv")
links_3S_5 <- read.csv("~/TB Honours/Project/Data question 4/1_links_3S_5.csv")
links_3S_15 <- read.csv("~/TB Honours/Project/Data question 4/1_links_3S_15.csv")
links_ACF_2 <- read.csv("~/TB Honours/Project/Data question 4/1_links_ACF_2.csv")
links_ACF_15 <- read.csv("~/TB Honours/Project/Data question 4/1_links_ACF_15.csv")
Coalitions_ACF <- read.csv("~/TB Honours/Project/1_coalitions_pf_ACF_exp_0.csv")
coalitions_as_ACF_2 <- read.csv("~/TB Honours/Project/Data question 4/1_coalitions_as_ACF_2.csv")
coalitions_as_ACF_15 <- read.csv("~/TB Honours/Project/Data question 4/1_coalitions_as_ACF_15.csv")
coalitions_pf_ACF_2 <- read.csv("~/TB Honours/Project/Data question 4/1_coalitions_pf_ACF_2.csv")
coalitions_pf_ACF_15 <- read.csv("~/TB Honours/Project/Data question 4/1_coalitions_pf_ACF_15.csv")
teams_as_3S_11 <- read.csv("~/TB Honours/Project/Data question 4/1_teams_as_3S_11.csv")
teams_pf_3S_11 <- read.csv("~/TB Honours/Project/Data question 4/1_teams_pf_3S_11.csv")
teams_as_3S_2 <- read.csv("~/TB Honours/Project/Data question 4/1_teams_as_3S_2.csv")
teams_as_3S_13 <- read.csv("~/TB Honours/Project/Data question 4/1_teams_as_3S_13.csv")
teams_as_3S_14 <- read.csv("~/TB Honours/Project/Data question 4/1_teams_as_3S_14.csv")
model_ACF_exp_1 <- read.csv("~/TB Honours/Project/Data question 1/1_model_ACF_exp_1.csv")
model_3S_2 <- read.csv("~/TB Honours/Project/Data question 4/1_model_3S_2.csv")

# Importing the packages we need ----
library(igraph)
library(ggplot2)
library(ggnet)
library(chorddiag)
library(circlize)
library(dplyr)
library(mapmate)
library(shiny)
library(gridExtra)

# Setting up multiplot (not our code, we got it from the internet) ----
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Convert the Agent columns into a charactertype in stead of a listtype ----
Links_3S_0$Agent1 <- as.character(Links_3S_0$Agent1)
Links_3S_0$Agent2 <- as.character(Links_3S_0$Agent2)
Links_ACF_0$Agent1 <- as.character(Links_ACF_0$Agent1)
Links_ACF_0$Agent2 <- as.character(Links_ACF_0$Agent2)
`Links_B+_0`$Agent1 <- as.character(`Links_B+_0`$Agent1)
`Links_B+_0`$Agent2 <- as.character(`Links_B+_0`$Agent2)
links_3S_5$Agent1 <- as.character(links_3S_5$Agent1)
links_3S_5$Agent2 <- as.character(links_3S_5$Agent2)
links_3S_15$Agent1 <- as.character(links_3S_15$Agent1)
links_3S_15$Agent2 <- as.character(links_3S_15$Agent2)
links_ACF_2$Agent1 <- as.character(links_ACF_2$Agent1)
links_ACF_2$Agent2 <- as.character(links_ACF_2$Agent2)
links_ACF_15$Agent1 <- as.character(links_ACF_15$Agent1)
links_ACF_15$Agent2 <- as.character(links_ACF_15$Agent2)

# Adding two columns to the dataset with the agentnumbers of the Agents ----
agent1nr <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {agent1nr[[j]] <- substr(Links_3S_0$Agent1[i], regexpr(":", Links_3S_0$Agent1[i])+2, nchar(Links_3S_0$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
Links_3S_0$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {agent2nr[[j]] <- substr(Links_3S_0$Agent2[i], regexpr(":", Links_3S_0$Agent2[i])+2, nchar(Links_3S_0$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
Links_3S_0$agent2nr = agent2nr

agent1nr <- list()
j <- 1
for (i in 1:nrow(Links_ACF_0)) {agent1nr[[j]] <- substr(Links_ACF_0$Agent1[i], regexpr(":", Links_ACF_0$Agent1[i])+2, nchar(Links_ACF_0$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
Links_ACF_0$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(Links_ACF_0)) {agent2nr[[j]] <- substr(Links_ACF_0$Agent2[i], regexpr(":", Links_ACF_0$Agent2[i])+2, nchar(Links_ACF_0$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
Links_ACF_0$agent2nr = agent2nr

agent1nr <- list()
j <- 1
for (i in 1:nrow(Links_3S_0)) {agent1nr[[j]] <- substr(`Links_B+_0`$Agent1[i], regexpr(":", `Links_B+_0`$Agent1[i])+2, nchar(`Links_B+_0`$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
`Links_B+_0`$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(`Links_B+_0`)) {agent2nr[[j]] <- substr(`Links_B+_0`$Agent2[i], regexpr(":", `Links_B+_0`$Agent2[i])+2, nchar(`Links_B+_0`$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
`Links_B+_0`$agent2nr = agent2nr

agent1nr <- list()
j <- 1
for (i in 1:nrow(links_3S_5)) {agent1nr[[j]] <- substr(links_3S_5$Agent1[i], regexpr(":", links_3S_5$Agent1[i])+2, nchar(links_3S_5$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
links_3S_5$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(links_3S_5)) {agent2nr[[j]] <- substr(links_3S_5$Agent2[i], regexpr(":", links_3S_5$Agent2[i])+2, nchar(links_3S_5$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
links_3S_5$agent2nr = agent2nr

agent1nr <- list()
j <- 1
for (i in 1:nrow(links_3S_15)) {agent1nr[[j]] <- substr(links_3S_15$Agent1[i], regexpr(":", links_3S_15$Agent1[i])+2, nchar(links_3S_15$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
links_3S_15$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(links_3S_15)) {agent2nr[[j]] <- substr(links_3S_15$Agent2[i], regexpr(":", links_3S_15$Agent2[i])+2, nchar(links_3S_15$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
links_3S_15$agent2nr = agent2nr

agent1nr <- list()
j <- 1
for (i in 1:nrow(links_ACF_2)) {agent1nr[[j]] <- substr(links_ACF_2$Agent1[i], regexpr(":", links_ACF_2$Agent1[i])+2, nchar(links_ACF_2$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
links_ACF_2$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(links_ACF_2)) {agent2nr[[j]] <- substr(links_ACF_2$Agent2[i], regexpr(":", links_ACF_2$Agent2[i])+2, nchar(links_ACF_2$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
links_ACF_2$agent2nr = agent2nr

agent1nr <- list()
j <- 1
for (i in 1:nrow(links_ACF_15)) {agent1nr[[j]] <- substr(links_ACF_15$Agent1[i], regexpr(":", links_ACF_15$Agent1[i])+2, nchar(links_ACF_15$Agent1[i])); agent1nr[[j]] <- as.integer(agent1nr[[j]]); j = j + 1}
links_ACF_15$agent1nr = agent1nr

agent2nr <- list()
j <- 1
for (i in 1:nrow(links_ACF_15)) {agent2nr[[j]] <- substr(links_ACF_15$Agent2[i], regexpr(":", links_ACF_15$Agent2[i])+2, nchar(links_ACF_15$Agent2[i])); agent2nr[[j]] <- as.integer(agent2nr[[j]]); j = j + 1}
links_ACF_15$agent2nr = agent2nr

##----

# Function that plots a matrix of the trustlevels between agents with roll x + affiliation x and agents with roll y + affiliation y
TrustGraph <- function(Roll1,Roll2,Aff1,Aff2,datasetLinks,datasetAgents) {
  # Adding a column to the datasets with the affiliations of the Agents
  agent1aff <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {agent1aff[[j]] <- datasetLinks$agent1nr[[i]]+1; agent1aff[[j]] <- datasetAgents$Affiliation[agent1aff[[j]]]; j = j + 1}
  datasetLinks$agent1aff = agent1aff
  
  agent2aff <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {agent2aff[[j]] <- datasetLinks$agent2nr[[i]]+1; agent2aff[[j]] <- datasetAgents$Affiliation[agent2aff[[j]]]; j = j + 1}
  datasetLinks$agent2aff = agent2aff
  
  # Extracting the rows that contain Agents with the desired (from the input) roles en affiliations
  RowlstName <- list()
  j <- 1
  for (i in 1:nrow(datasetLinks)) {if (grepl(Roll1,datasetLinks$Agent2[[i]]) && grepl(Roll2,datasetLinks$Agent1[[i]]) || grepl(Roll1,datasetLinks$Agent1[[i]]) && grepl(Roll2,datasetLinks$Agent2[[i]])) {RowlstName[[j]] = i; j = j+1}}
  
  CleanName <- list()
  j <- 1
  for (i in 1:length(RowlstName)) { if (datasetLinks$agent1aff[RowlstName[[i]]] == Aff1 && datasetLinks$agent2aff[RowlstName[[i]]] == Aff2 || datasetLinks$agent2aff[RowlstName[[i]]] == Aff1 && datasetLinks$agent1aff[RowlstName[[i]]] == Aff2){CleanName[[j]] <- RowlstName[[i]]; j = j+1}}
  
  # Turning the rownumbers into a matrix with 1 specific agent trustlevel in a row over time
  for(i in 1:length(CleanName)){if( CleanName[i]>max(datasetLinks$LinksID )) {tempx <- i-1;break}}
  CleanName <- matrix(unlist(CleanName),nrow = tempx)
  
  for(row in 1:nrow(CleanName)) {
    for(col in 1:ncol(CleanName)) {
      CleanName[row,col] <- datasetLinks$Agent3[[CleanName[row,col]]]
    }
  }
  
  # Plotting the matrix
  matplot(t(CleanName),type = 'l',xlab = "time", ylab = "Trust", main = paste("Trustlevel for Links between",Roll1, "and\n", Roll2, "with affiliations",Aff1, "and", Aff2))
}
# Example for the Trustlevel for links between policy makers with affiliation 2 and external parties with affiliation 1
TrustGraph(Roll1 = 'Policy maker',Roll2 = 'External party',Aff1 = 2,Aff2 = 1,datasetLinks = Links_3S_0,datasetAgents = Agents_3S_0)

# Function that returns dataframe that shows in what coalition (Defined by it's coalition leader) each agent is at a specific tick
Coalitiondf <- function(Coalitiondataframe,tick){
  vblist <- as.list(1:30)
  vblist2 <- as.list(rep(31,30))
  tick1 <- list()
  tick2 <- list()
  for (i in 1:length(Coalitiondataframe$Step)){if (Coalitiondataframe$Step[i]==(tick-1)){tick1 <- i; break}}
  for (i in 1:length(Coalitiondataframe$Step)){if (Coalitiondataframe$Step[i]==(tick-1)){tick2 <- i}}
  for (j in tick1:tick2){
    for (k in 1:30){
      if(Coalitiondataframe$Lead[j]==k){xlst <- Coalitiondataframe$Members[j]
      xlst <- as.character(xlst)
      xlst <- gsub("\\[|\\]|","",xlst)
      xlst <- unlist(strsplit(xlst,","))
      xlst <- as.numeric(xlst)
      for (el in xlst) {
        vblist2 = replace(vblist2, c(el), k)
      }}}}
  Agent <- as.double(vblist)
  Coalitionleader <- as.double(vblist2)
  bvlist <- data.frame(Agent, Coalitionleader)
  return(bvlist)
}

# Example
View(Coalitiondf(Coalitions_ACF,1))

# General function for any link dataset that gives the network for a given tick at a given threshold.
# The node colors vary with the agent roles
Trustgraphggnet <- function(Links_Data_Frame,Agents_Data_Frame,tick,MinimumTrust=0,lay_out="circle") {
  newlist1 <- list()
  newlist2 <- list()
  newlist3 <- list()
  
  # Define of how many rows one tick consist in the Links dataset
  num_per_tick <- length(Links_Data_Frame$Agent3)/500
  
  #Listing all the links between the agents in a dataframe with the agents that are linked in the first two columns and the trustlevel between the two in the third column (decides the edge size)
  j = 1
  for (i in (1+num_per_tick*(tick-1)):(1+num_per_tick*(tick-1)+num_per_tick)) {if (Links_Data_Frame$Agent3[i] > MinimumTrust) {newlist1[[j]] <- as.integer(Links_Data_Frame$agent1nr[i]); newlist2[[j]] <- as.integer(Links_Data_Frame$agent2nr[i]); newlist3[[j]] <- unlist(Links_Data_Frame$Agent3[i]); j = j+1}}
  edges = data.frame(as.double(newlist1),as.double(newlist2))
  edges$TrustLevel <- as.double(newlist3)
  
  # Creating a matrix of the links, the agents make up the rows and columns, a 1 means that the agent at that column is linked with the agent at that row (undirected)
  networkmatrix = matrix(ncol=30,nrow=30)
  for (k in 1:length(edges$as.double.newlist1.)){for (i in 0:29){for (j in 0:29){if(edges$as.double.newlist1.[k]==i & edges$as.double.newlist2.[k]==j){networkmatrix[i+1,j+1]=1}}}}
  networkmatrix[is.na(networkmatrix)]<-0
  
  # Creating a data frame with the ID, agent role and popularity listed for all agents (decides the nodesize and nodecolor)
  AgentIDlist <- Agents_Data_Frame[1:30,2]
  Agentrolelist <- Agents_Data_Frame[1:30,4]
  node_features = data.frame(as.double(AgentIDlist),as.double(Agentrolelist))
  for (i in 1:length(node_features$as.double.Agentrolelist.)){if(node_features$as.double.Agentrolelist.[i]==1){node_features$as.double.Agentrolelist.[i]="External Party"}}
  for (i in 1:length(node_features$as.double.Agentrolelist.)){if(node_features$as.double.Agentrolelist.[i]==2){node_features$as.double.Agentrolelist.[i]="Policy Maker"}}
  for (i in 1:length(node_features$as.double.Agentrolelist.)){if(node_features$as.double.Agentrolelist.[i]==3){node_features$as.double.Agentrolelist.[i]="Policy Entre"}}
  node_features$label = node_features$as.double.AgentIDlist.
  
  populist <- list()
  for (i in 1:length(networkmatrix[1,])){populist[i]=sum(networkmatrix[i,])}
  node_features$popularity <- as.double(populist)
  
  ggnet2(networkmatrix,color=node_features$as.double.Agentrolelist.,palette = c("External Party"="yellow", "Policy Maker"="blue", "Policy Entre"="green")
         ,label = node_features$label,mode=lay_out, size=node_features$popularity,edge.size = edges$TrustLevel,legend.size = 9,legend.position = "bottom") + ggtitle(paste("Networkdiagram of the network at tick",tick))
}

# Example of a multiplot of the graph at the beginning and 3 other points in time of the model run
p1 <- Trustgraphggnet(Links_3S_0,Agents_3S_0,1)
p2 <- Trustgraphggnet(Links_3S_0,Agents_3S_0,100)
p3 <- Trustgraphggnet(Links_3S_0,Agents_3S_0,200)
p4 <- Trustgraphggnet(Links_3S_0,Agents_3S_0,300)
multiplot(p1,p2,p3,p4,cols=2)

# Widget that shows two networkdiagrams of runs with a very high and a very low Trust decay coefficient 
# Requires Trustgraphggnet so run that first ----
u <- shinyUI(fluidPage(
  titlePanel("Networkdiagram of the actor network"),
  sidebarLayout(position = "left",
                sidebarPanel("Adjustables",
                             checkboxInput("donum1", "View plot with a high trust decay coefficient", value = T),
                             checkboxInput("donum2", "View plot with a low trust decay coefficient", value = T),
                             sliderInput(inputId = "tick",
                                         label = "Time plot high trust decay:",
                                         min = 1,
                                         max = 500,
                                         value = 30),
                             sliderInput(inputId = "tick2",
                                         label = "Time plot low trust decay:",
                                         min = 1,
                                         max = 500,
                                         value = 30)
                ),
                mainPanel("main panel",
                          column(6,plotOutput(outputId="Trustgraphggnet", width="500px",height="900px"))
                ))))

s <- function(input, output) 
{
  pt1 <- reactive({
    if(!input$donum1) return(NULL)
    Trustgraphggnet(Links_3S_0,Agents_3S_0,input$tick)
  })
  pt2 <- reactive({
    if(!input$donum2) return(NULL)
    Trustgraphggnet(links_3S_15,agents_3S_15,input$tick2)
  })
  
  output$Trustgraphggnet = renderPlot({
    ptlist <- list(pt1(),pt2())
    # remove the null plots from ptlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,ncol=1)
  })
}
shinyApp(u,s)
#----

# Same function as Trustgraphggnet but the coloring is now per coalition instead of per agent role
Trustgraphcoalggnet <- function(Coalitionsdataframe,Links_Data_Frame,Agents_Data_Frame,tick,MinimumTrust=0,lay_out="circle") {
  
  newlist1 <- list()
  newlist2 <- list()
  newlist3 <- list()
  
  num_per_tick <- length(Links_Data_Frame$Agent3)/500
  
  j = 1
  for (i in (1+num_per_tick*(tick-1)):(1+num_per_tick*(tick-1)+num_per_tick)) {if (Links_Data_Frame$Agent3[i] > MinimumTrust) {newlist1[[j]] <- as.integer(Links_Data_Frame$agent1nr[i]); newlist2[[j]] <- as.integer(Links_Data_Frame$agent2nr[i]); newlist3[[j]] <- unlist(Links_Data_Frame$Agent3[i]); j = j+1}}
  edges = data.frame(as.double(newlist1),as.double(newlist2))
  edges$TrustLevel <- as.double(newlist3)
  
  networkmatrix = matrix(ncol=30,nrow=30)
  for (k in 1:length(edges$as.double.newlist1.)){for (i in 0:29){for (j in 0:29){if(edges$as.double.newlist1.[k]==i & edges$as.double.newlist2.[k]==j){networkmatrix[i+1,j+1]=1}}}}
  networkmatrix[is.na(networkmatrix)]<-0
  
  node_features = Coalitiondf(Coalitionsdataframe,tick)
  
  populist <- list()
  for (i in 1:length(networkmatrix[1,])){populist[i]=sum(networkmatrix[i,])}
  node_features$popularity <- as.double(populist)
  
  ggnet2(networkmatrix,color=node_features$Coalitionleader,palette = c("1"="red", "2"="blue", "3"="green", "4" = "yellow", "5" = "pink", "6" = "orange", "7" = "brown", "8" = "darkred", "9" = "cornflowerblue", "10" = "chartreuse", "11" = "brown4",
                                                                       "12" = "burlywood", "13" = "burlywood4", "14" = "chartreuse3", "15" = "antiquewhite", "16" = "antiquewhite4", "17" = "aquamarine1", "18" = "aquamarine4", "19" = "azure2", "20" = "azure4", "21" = "blueviolet",
                                                                       "22" = "bisque", "23" = "bisque4", "24" = "darkolivegreen", "25" = "darkolivegreen1", "26" = "coral", "27" = "coral3", "28" = "chocolate", "29" = "chocolate4", "30" = "cadetblue", "31" = "black")
         ,label = node_features$Agent,mode=lay_out, size=node_features$popularity,edge.size = edges$TrustLevel,legend.size = 9,legend.position = "bottom")+ ggtitle(paste("Networkdiagram of the network at tick",tick))
}

# Example
Trustgraphcoalggnet(Coalitions_ACF,Links_ACF_0,Agents_ACF_0,1)

# Same function as Trustgraphggnet but now shown as a chord diagram, each agent has it's own color
Chordgraph <- function(MinimumTrust,Links_Data_Frame,tick) {
  colorvector = colors()[c(501:530)]
  
  newlist1 <- list()
  newlist2 <- list()
  newlist3 <- list()
  
  num_per_tick <- length(Links_Data_Frame$Agent3)/500
  
  j = 1
  for (i in (1+num_per_tick*(tick-1)):(1+num_per_tick*(tick-1)+num_per_tick)) {if (Links_Data_Frame$Agent3[i] > MinimumTrust) {newlist1[[j]] <- as.integer(Links_Data_Frame$agent1nr[i]); newlist2[[j]] <- as.integer(Links_Data_Frame$agent2nr[i]); newlist3[[j]] <- unlist(Links_Data_Frame$Agent3[i]); j = j+1}}
  edges = data.frame(as.double(newlist1),as.double(newlist2))
  edges$TrustLevel <- as.double(newlist3)
  
  networkmatrix = matrix(ncol=30,nrow=30)
  for (k in 1:length(edges$as.double.newlist1.)){for (i in 0:29){for (j in 0:29){if(edges$as.double.newlist1.[k]==i & edges$as.double.newlist2.[k]==j){networkmatrix[i+1,j+1]=1}}}}
  networkmatrix[is.na(networkmatrix)]<-0
  rownames(networkmatrix) = paste0("A",1:30)
  colnames(networkmatrix) = paste0("A",1:30)
  
  chordDiagram(networkmatrix, grid.col= colorvector,title(paste("Chord diagram of the network at tick",tick)))
}

# Example
Chordgraph(0,Links_3S_0,1)

# Code that creates a widget for the Chord diagram in which you can fill in the tick at which you want the network to be shown ----
# Define UI for app that draws the Chord diagram
ui <- fluidPage(
  # App title
  titlePanel("Chord diagram of the actor network"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Slider for the time (tick) at which the graph hsa to be shown
      sliderInput(inputId = "tick",
                  label = "Time:",
                  min = 1,
                  max = 500,
                  value = 30)
    ),
    # Main panel for displaying outputs
    mainPanel(
      # Output: Chordgraph
      plotOutput(outputId = "chordgraph")
    )
  )
)
# Define server logic required to draw a Chord diagram
server <- function(input, output) {
  
  # This expression that generates a Chord diagram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$tick) change
  # 2. Its output type is a plot
  output$chordgraph <- renderPlot({
    Chordgraph(0,Links_3S_0,input$tick)
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
#----

# Code that puts all Chord graphs over the run into a mp4 file so you can see what happens over time
# In this code I only went to tick 30 because the graphs take a long time to run
#----
for(i in 1:30){
  jpeg(paste0("rplot",sprintf("%04d",i),".png"))
  Chordgraph(0,Links_3S_0,i)
  dev.off()
}

imgs <- list.files(pattern="*.png")
saveVideo({for(img in imgs){im <- magick::image_read(img)
plot(as.raster(im))}},video.name = "Chorddiagram_Over_Time.mp4")

#----

# Function that plots the the top 5 most centralised agents over time 
Centralityplot <- function(Links_Data_Frame){
  ticks = c(1:499)
  Topagent1 = c()
  Topagent2 = c()
  Topagent3 = c()
  Topagent4 = c()
  Topagent5 = c()
  num_per_tick <- length(Links_Data_Frame$Agent3)/500
  
  #Calculate for each tick for each agent the aggregated trustlevel and add the 5 agents with the largest aggregated trustlevel to one of the 5 topagentlists
  for(k in 1:499){
    newlist1 <- list()
    newlist2 <- list()
    newlist3 <- list()
    j = 1
    for (i in (1+num_per_tick*(k-1)):(1+num_per_tick*(k-1)+num_per_tick)) {if (Links_Data_Frame$Agent3[i] > 0) {newlist1[[j]] <- as.integer(Links_Data_Frame$agent1nr[i]); newlist2[[j]] <- as.integer(Links_Data_Frame$agent2nr[i]); newlist3[[j]] <- unlist(Links_Data_Frame$Agent3[i]); j = j+1}}
    edges = data.frame(as.double(newlist1),as.double(newlist2))
    edges$TrustLevel <- as.double(newlist3)
    Toplist = list()
    L = 0
    for (i in 1:max(edges$as.double.newlist2.)) {for (j in 1:length(edges$as.double.newlist1.)) {if (edges$as.double.newlist1.[j] == i | edges$as.double.newlist2.[j] == i) L = L+as.numeric(edges$TrustLevel[j])}; Toplist[i] = L; L = 0}
    Toplist <- as.numeric(Toplist)
    Topagents <- sort.list(Toplist,decreasing=TRUE)[1:5]
    Topagent1 = c(Topagent1,Topagents[1])
    Topagent2 = c(Topagent2,Topagents[2])
    Topagent3 = c(Topagent3,Topagents[3])
    Topagent4 = c(Topagent4,Topagents[4])
    Topagent5 = c(Topagent5,Topagents[5])
  }
  
  # Plot the composition of the topagents in a dotplot
  Topdataframe = data.frame(ticks,Topagent1,Topagent2,Topagent3,Topagent4,Topagent5)
  ggplot(Topdataframe,aes(x=ticks,y=Topagent1))+geom_point()+geom_point(aes(y=Topagent2))+geom_point(aes(y=Topagent3))+geom_point(aes(y=Topagent4))+geom_point(aes(y=Topagent5))+ ggtitle("Composition of the top 5 most popular \n agents over time")
}

#Example
Centralityplot(Links_ACF_0)

# Function that lists the agents that have been in the top 5 of aggregated trustlevel over the run
Topagentlistfun <- function(Links_Data_Frame){
  ticks = c(1:499)
  Topagentlist = c()
  num_per_tick <- length(Links_Data_Frame$Agent3)/500
  for(k in 1:499){
    newlist1 <- list()
    newlist2 <- list()
    newlist3 <- list()
    j = 1
    for (i in (1+num_per_tick*(k-1)):(1+num_per_tick*(k-1)+num_per_tick)) {if (Links_Data_Frame$Agent3[i] > 0) {newlist1[[j]] <- as.integer(Links_Data_Frame$agent1nr[i]); newlist2[[j]] <- as.integer(Links_Data_Frame$agent2nr[i]); newlist3[[j]] <- unlist(Links_Data_Frame$Agent3[i]); j = j+1}}
    edges = data.frame(as.double(newlist1),as.double(newlist2))
    edges$TrustLevel <- as.double(newlist3)
    Toplist = list()
    L = 0
    for (i in 1:max(edges$as.double.newlist2.)) {for (j in 1:length(edges$as.double.newlist1.)) {if (edges$as.double.newlist1.[j] == i || edges$as.double.newlist2.[j] == i) L = L+as.numeric(edges$TrustLevel[j])}; Toplist[i] = L; L = 0}
    Toplist <- as.numeric(Toplist)
    Topagents <- sort.list(Toplist,decreasing=TRUE)[1:5]
    p=0
    for(i in Topagents){if(i %in% Topagentlist){}else{Topagentlist = c(Topagentlist,i)}}
  }
  return(Topagentlist)
}

# Function that plots a graph of the aggregated trustlevel of the agents that have been in the top 5 over the run
# Requires Topagentlistfun
AggTrustLvlPlot <- function(Links_Data_Frame){
  num <- Topagentlistfun(Links_Data_Frame)
  num_per_tick <- length(Links_Data_Frame$Agent3)/500
  Trustlevelmatrix <- matrix(nrow=499, ncol=length(num))
  L=0
  for (k in 1:499){
    newlist1 <- list()
    newlist2 <- list()
    newlist3 <- list()
    j = 1
    for (i in (1+num_per_tick*(k-1)):(1+num_per_tick*(k-1)+num_per_tick)) {if (Links_Data_Frame$Agent3[i] > 0) {newlist1[[j]] <- as.integer(Links_Data_Frame$agent1nr[i]); newlist2[[j]] <- as.integer(Links_Data_Frame$agent2nr[i]); newlist3[[j]] <- unlist(Links_Data_Frame$Agent3[i]); j = j+1}}
    edges = data.frame(as.double(newlist1),as.double(newlist2))
    edges$TrustLevel <- as.double(newlist3)
    for (i in 1:length(num)){for (j in 1:length(edges$as.double.newlist1)){if (edges$as.double.newlist1.[j]==num[i]||edges$as.double.newlist2.[j]==num[i])L=L+as.numeric(edges$TrustLevel[j])};Trustlevelmatrix[k,i]=L;L=0
    }
  }
  matplot(Trustlevelmatrix,type='l',xlab="Time",ylab="Aggregated Trustlevel",main = "Aggregated trustlevel of top 5 agents")
}

#Example
AggTrustLvlPlot(Links_ACF_0)

# Function that plots the composition of a coalition (defined by it's leader) over time
Coalitiondevplot <- function(Coalitiondataframe,leader){
  vblist <- list()
  vblist2 <- list()
  # Making a dataframe with the first column consists of the agents that were in the coalition of the leader and the
  # second column consists of the tick at which this agent was in the coalition
  for(i in 1:length(Coalitiondataframe$Lead)){
    if(Coalitiondataframe$Lead[i]==leader){xlst <- Coalitiondataframe$Members[i]
    xlst <- as.character(xlst)
    xlst <- gsub("\\[|\\]|","",xlst)
    xlst <- unlist(strsplit(xlst,","))
    xlst <- as.numeric(xlst)
    for (el in xlst) {
      vblist = c(vblist,el)
      vblist2 = c(vblist2,Coalitiondataframe$Creation[i])}}}
  dataframe <- data.frame(as.double(vblist),as.double(vblist2))
  
  ggplot(dataframe,aes(x = as.double.vblist2.,y = as.double.vblist.))+geom_point()+xlab("Time")+ylab("Agents")+ ggtitle(paste("Development of the coalition with leader",leader))
}

# Example
Coalitiondevplot(Coalitions_ACF,8)

# Function that plots a pie chard of the number of ticks each agent spend in a specific coalition (defined by it's leader)
CoalitionPieChard <- function(Coalitiondataframe,leader){
  # Making a list that consists of the agents that were in the coalition of the leader
  vblist <- list()
  for(i in 1:length(Coalitiondataframe$Lead)){
    if(Coalitiondataframe$Lead[i]==leader){xlst <- Coalitiondataframe$Members[i]
    xlst <- as.character(xlst)
    xlst <- gsub("\\[|\\]|","",xlst)
    xlst <- unlist(strsplit(xlst,","))
    xlst <- as.numeric(xlst)
    for (el in xlst) {
      vblist = c(vblist,el)
    }}}
  
  # Checking if the leader from the input ever was a leader
  if(length(vblist)==0){return(c(leader,"was never a coalition leader"))}
  
  # Making two lists, lijst3 consists of all the agents that were in the coalition
  # Lijst 2 shows for each of these agents how many ticks they were in the coalition
  lijst3 <- vblist[1]
  for(el in vblist){if (!(el %in% lijst3)){lijst3 <- c(lijst3,el)}}
  lijst2 <- as.list(rep(0,length(lijst3)))
  for(el in vblist){for (i in 1:length(lijst3)){if(el == lijst3[i]) {j = lijst2[i];
  lijst2 <- replace(lijst2,c(i),c(as.double(j)+1))
  }}}
  
  lijst2 <- as.double(lijst2)
  lijst3 <- as.double(lijst3)
  
  # Plotting the pie chard
  slices <- lijst2
  lbls <- lijst3
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls,pct)
  lbls <- paste (lbls,"%",sep=" ")
  pie(slices,labels=lbls,main=paste("Pie Chart of time spend per agent \n in coalition with leader",leader))
}

# Example
CoalitionPieChard(Coalitions_ACF,0)

# Code that creates a Widget for the pie chart with a slider with which you can decide the coalition to be shown
# Requires CoalitionPieChard so run that first ----
ui <- fluidPage(
  titlePanel("Pie chart"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Agent",
                  label = "Agent-leader:",
                  min = 0,
                  max = 29,
                  value = 7)
    ),
    mainPanel(
      plotOutput(outputId = "Piechart")
    )
  )
)

server <- function(input, output) {
  output$Piechart <- renderPlot({
    CoalitionPieChard(Coalitions_ACF,input$Agent)
  })
}
# Create Shiny app
shinyApp(ui = ui, server = server)
#----

# Same function but now done in ggplot, slightly prettier
CoalitionPieChardggplot <- function(Coalitiondataframe,leader){
  # Making a list that consists of the agents that were in the coalition of the leader
  vblist <- list()
  for(i in 1:length(Coalitiondataframe$Lead)){
    if(Coalitiondataframe$Lead[i]==leader){xlst <- Coalitiondataframe$Members[i]
    xlst <- as.character(xlst)
    xlst <- gsub("\\[|\\]|","",xlst)
    xlst <- unlist(strsplit(xlst,","))
    xlst <- as.numeric(xlst)
    for (el in xlst) {
      vblist = c(vblist,el)
    }}}
  
  # Checking if the leader from the input ever was a leader
  if(length(vblist)==0){return(c(leader,"was never a coalition leader"))}
  
  # Making two lists, lijst3 consists of all the agents that were in the coalition
  # Lijst 2 shows for each of these agents how many ticks they were in the coalition
  lijst3 <- vblist[1]
  for(el in vblist){if (!(el %in% lijst3)){lijst3 <- c(lijst3,el)}}
  lijst2 <- as.list(rep(0,length(lijst3)))
  for(el in vblist){for (i in 1:length(lijst3)){if(el == lijst3[i]) {j = lijst2[i];
  lijst2 <- replace(lijst2,c(i),c(as.double(j)+1))
  }}}
  
    # Plotting the pie chard
  slices <- as.integer(lijst2)
  lbls <- lijst3
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls,pct,sep = " - ")
  lbls <- paste (lbls,"%",sep=" ")
  df = data.frame(slices=slices,label=lbls)
  ggplot(df,aes(x="",y=slices,fill=label))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)+ggtitle(paste("Pie chart of time spend per agent \n in coalition with leader",leader))+theme(axis.text.x = element_blank(),axis.ticks = element_blank(), panel.grid = element_blank())
}

# Example
CoalitionPieChardggplot(Coalitions_ACF,0)

# Same graph but shown as a barplot, more intuitive
CoalitionBarPlot <- function(Coalitiondataframe,leader){
  # Making a list that consists of the agents that were in the coalition of the leader
  vblist <- list()
  for(i in 1:length(Coalitiondataframe$Lead)){
    if(Coalitiondataframe$Lead[i]==leader){xlst <- Coalitiondataframe$Members[i]
    xlst <- as.character(xlst)
    xlst <- gsub("\\[|\\]|","",xlst)
    xlst <- unlist(strsplit(xlst,","))
    xlst <- as.numeric(xlst)
    for (el in xlst) {
      vblist = c(vblist,el)
    }}}
  
  # Checking if the leader from the input ever was a leader
  if(length(vblist)==0){return(c(leader,"was never a coalition leader"))}
  
  # Making two lists, lijst3 consists of all the agents that were in the coalition
  # Lijst 2 shows for each of these agents how many ticks they were in the coalition
  lijst3 <- vblist[1]
  for(el in vblist){if (!(el %in% lijst3)){lijst3 <- c(lijst3,el)}}
  lijst2 <- as.list(rep(0,length(lijst3)))
  for(el in vblist){for (i in 1:length(lijst3)){if(el == lijst3[i]) {j = lijst2[i];
  lijst2 <- replace(lijst2,c(i),c(as.double(j)+1))
  }}}
  
  # Plotting the pie chard
  slices <- as.integer(lijst2)
  lbls <- lijst3
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls,pct)
  lbls <- paste (lbls,"%",sep=" ")
  df = data.frame(slices=slices,label=lbls)
  ggplot(df,aes(x="",y=slices,fill=label))+geom_bar(width=1,stat="identity")+ggtitle(paste("Barplot of time spend per agent in coalition with leader",leader))
}

# Example
CoalitionBarPlot(Coalitions_ACF,0)

# Function that plots the resources of a specific coalition (defined by it's leader) over time
Resourceplot <- function(Coalitiondataframe,leader){
  # Making a dataframe with in the first column the resources of the coalition and in the second the ticks
  vblist <- list()
  vblist2 <- list()
  for(i in 1:length(Coalitiondataframe$Lead)){
    if(Coalitiondataframe$Lead[i]==leader){
      vblist = c(vblist,Coalitiondataframe$Resources[i])
      vblist2 = c(vblist2,Coalitiondataframe$Creation[i])}}
  bvlist <- data.frame(as.double(vblist),as.double(vblist2))
  # plotting the dataframe as a lineplot
  ggplot(bvlist,aes(x = as.double.vblist2.,y = as.double.vblist.))+geom_line()+xlab("Time")+ylab("Resources")+ggtitle(paste("Resources of coalition with leader",leader))
}

# Example
Resourceplot(Coalitions_ACF,7)

# Function that plots the trustlevels in coalitions over all agents at a specific tick
# Will make more sense when the model is adjusted so all members in coalitions actually know one another
Coalitionplotggnet <- function(Coalition_Dataframe, Links_Dataframe, Agents_Data_Frame,tick){
  newlist1 <- list()
  newlist2 <- list()
  newlist3 <- list()
  xlst <- list()
  p = 1
  
  # Finding which rows make up the tick from the input
  for (i in 1:length(Coalition_Dataframe$Step)){if (Coalition_Dataframe$Step[i]==(tick-1)){startrow <- i; break}}
  for (i in 1:length(Coalition_Dataframe$Step)){if (Coalition_Dataframe$Step[i]==(tick-1)){endrow <- i}}
  
  # Define of how many rows one tick consist in the Links dataset
  num_per_tick <- length(Links_Dataframe$Agent3)/500
  
  #Listing all the links between the agents in a dataframe with the agents that are linked in the first two columns and the trustlevel between the two in the third column (decides the edge size)
  for (j in startrow:endrow){xlst <- Coalition_Dataframe$Members[j]
  xlst <- as.character(xlst)
  xlst <- gsub("\\[|\\]|","",xlst)
  xlst <- unlist(strsplit(xlst,","))
  xlst <- as.numeric(xlst) 
  for (i in (1+num_per_tick*(tick-1)):(1+num_per_tick*(tick-1)+num_per_tick)) {if (Links_Dataframe$Agent3[i] > 0 & Links_Dataframe$agent1nr[i] %in% xlst & Links_Dataframe$agent2nr[i] %in% xlst) {newlist1[[p]] <- as.integer(Links_Dataframe$agent1nr[i]); newlist2[[p]] <- as.integer(Links_Dataframe$agent2nr[i]); newlist3[[p]] <- unlist(Links_Dataframe$Agent3[i]); p = p+1}}}
  edges = data.frame(as.double(newlist1),as.double(newlist2))
  edges$TrustLevel <- newlist3
  
  # Creating a matrix out of the first 2 columns of the dataframe with the links between the agents as a 1 in the matrix
  networkmatrix = matrix(ncol=30,nrow=30)
  for (k in 1:length(edges$as.double.newlist1.)){for (i in 0:29){for (j in 0:29){if(edges$as.double.newlist1.[k]==i & edges$as.double.newlist2.[k]==j){networkmatrix[i+1,j+1]=1}}}}
  networkmatrix[is.na(networkmatrix)]<-0
  
  
  # Creating a data frame with the ID, agent role and popularity listed for all agents (decides the nodesize and nodecolor)
  list1 <- Agents_Data_Frame[1:30,2]
  list2 <- Agents_Data_Frame[1:30,4]
  node_features = data.frame(as.double(list1),as.double(list2))
  for (i in 1:length(node_features$as.double.list2.)){if(node_features$as.double.list2.[i]==1){node_features$as.double.list2.[i]="External Party"}}
  for (i in 1:length(node_features$as.double.list2.)){if(node_features$as.double.list2.[i]==2){node_features$as.double.list2.[i]="Policy Maker"}}
  for (i in 1:length(node_features$as.double.list2.)){if(node_features$as.double.list2.[i]==3){node_features$as.double.list2.[i]="Policy Entre"}}
  node_features$label = node_features$as.double.list1.
  
  populist <- list()
  for (i in 1:length(networkmatrix[1,])){populist[i]=sum(networkmatrix[i,])+sum(networkmatrix[,i])}
  node_features$popularity <- as.double(populist)
  
  # plotting the matrix
  ggnet2(networkmatrix,color=node_features$as.double.list2.,palette = c("External Party"="skyblue", "Policy Maker"="skyblue2", "Policy Entre"="skyblue4"),label = node_features$label, size=node_features$popularity,legend.size = 9,legend.position = "bottom")+ggtitle(paste("Trustlevels in the coalitions at tick",tick))
}

#Example
Coalitionplotggnet(Coalitions_ACF,Links_ACF_0,Agents_ACF_0,3)

# Same graph but now the coloring represents the issue the Coalition is pushing for
CoalitionplotIssue <- function(Coalition_Dataframe, Links_Dataframe, Agents_Data_Frame,tick){
  newlist1 <- list()
  newlist2 <- list()
  newlist3 <- list()
  xlst <- list()
  p = 1
  Agentlist <- Agents_Data_Frame[1:30,2]
  Issuelist <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  # Finding which rows make up the tick from the input
  for (i in 1:length(Coalition_Dataframe$Step)){if (Coalition_Dataframe$Step[i]==(tick-1)){startrow <- i; break}}
  for (i in 1:length(Coalition_Dataframe$Step)){if (Coalition_Dataframe$Step[i]==(tick-1)){endrow <- i}}
  
  # Define of how many rows one tick consist in the Links dataset
  num_per_tick <- length(Links_Dataframe$Agent3)/500
  
  #Listing all the links between the agents in a dataframe with the agents that are linked in the first two columns and the trustlevel between the two in the third column (decides the edge size)
  for (j in startrow:endrow){xlst <- Coalition_Dataframe$Members[j]
  xlst <- as.character(xlst)
  xlst <- gsub("\\[|\\]|","",xlst)
  xlst <- unlist(strsplit(xlst,","))
  xlst <- as.numeric(xlst) 
  if(Coalition_Dataframe$Issue[j]==1){issue = "Camp fires"}
  if(Coalition_Dataframe$Issue[j]==2){issue = "Planting"}
  if(Coalition_Dataframe$Issue[j]==3){issue = "Monitoring"}
  if(Coalition_Dataframe$Issue[j]==4){issue = "Firefighters"}
  if(Coalition_Dataframe$Issue[j]==5){issue = "Prevention"}
  for (el in xlst){Issuelist[el+1] <- issue}
  for (i in (1+num_per_tick*(tick-1)):(1+num_per_tick*(tick-1)+num_per_tick)) {if (Links_Dataframe$Agent3[i] > 0 & Links_Dataframe$agent1nr[i] %in% xlst & Links_Dataframe$agent2nr[i] %in% xlst) {newlist1[[p]] <- as.integer(Links_Dataframe$agent1nr[i]); newlist2[[p]] <- as.integer(Links_Dataframe$agent2nr[i]); newlist3[[p]] <- unlist(Links_Dataframe$Agent3[i]); p = p+1}}}
  edges = data.frame(as.double(newlist1),as.double(newlist2))
  edges$TrustLevel <- newlist3
  
  for(i in 1:length(Issuelist)){if(Issuelist[i]==0){Issuelist[i] <- "Not in Coalition"}}
  
  # Creating a matrix out of the first 2 columns of the dataframe with the links between the agents as a 1 in the matrix
  networkmatrix = matrix(ncol=30,nrow=30)
  for (k in 1:length(edges$as.double.newlist1.)){for (i in 0:29){for (j in 0:29){if(edges$as.double.newlist1.[k]==i & edges$as.double.newlist2.[k]==j){networkmatrix[i+1,j+1]=1}}}}
  networkmatrix[is.na(networkmatrix)]<-0
  
  # Creating a data frame with the ID, issue the agents coalition is pushing for and popularity listed for all agents (decides the nodesize and nodecolor)
  node_features = data.frame(Agentlist,Issuelist)
  node_features$label = node_features$Agentlist
  
  populist <- list()
  for (i in 1:length(networkmatrix[1,])){populist[i]=sum(networkmatrix[i,])+sum(networkmatrix[,i])}
  node_features$popularity <- as.double(populist)
  
  # plotting the matrix
  ggnet2(networkmatrix,color=node_features$Issuelist,palette = c("Camp fires"="green","Planting"="red","Monitoring"="yellow","Firefighters"="blue","Prevention"="orange","Not in Coalition"="grey"),label = node_features$label, size=node_features$popularity,legend.size = 9,legend.position = "bottom")+ggtitle(paste("Trustlevels in the coalitions at tick",tick))
}

#Example
CoalitionplotIssue(coalitions_as_ACF_2,links_ACF_2,agents_ACF_2,3)


# Widget that shows the coalition diagram of two ACF runs that have a very high and a very low ACF threshold
# Requires Coalitionplotggnet so run that first ----
u <- shinyUI(fluidPage(
  titlePanel("The trustlevel in coalitions"),
  sidebarLayout(position = "left",
                sidebarPanel("Adjustables",
                             checkboxInput("donum1", "View plot with high ACF theshold", value = T),
                             checkboxInput("donum2", "View plot with low ACF threshold", value = T),
                             sliderInput(inputId = "tick",
                                         label = "Time plot high ACF threshold:",
                                         min = 1,
                                         max = 500,
                                         value = 30),
                             sliderInput(inputId = "tick2",
                                         label = "Time plot low ACF threshold:",
                                         min = 1,
                                         max = 500,
                                         value = 30)
                ),
                mainPanel("Plots",
                          column(6,plotOutput(outputId="Coalitionplotggnet", width="500px",height="900px"))
                ))))

s <- function(input, output) 
{
  pt1 <- reactive({
    if(!input$donum1) return(NULL)
    Coalitionplotggnet(coalitions_as_ACF_2,links_ACF_2,agents_ACF_2,input$tick)
  })
  pt2 <- reactive({
    if(!input$donum2) return(NULL)
    Coalitionplotggnet(coalitions_as_ACF_15,links_ACF_15,agents_ACF_15,input$tick2)
  })
  
  output$Coalitionplotggnet = renderPlot({
    ptlist <- list(pt1(),pt2())
    # remove the null plots from ptlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,ncol=1)
  })
}
shinyApp(u,s)

#----

# Code that plots the network between agents in 1 coalition (defined by it's leader) at a specific tick
Coalitionnetworkplotggnet <- function(Links_Dataframe, Coalitiondataframe,Agents_Data_Frame,tick,leader){
  newlist1 <- list()
  newlist2 <- list()
  newlist3 <- list()
  xlst <- list()
  p = 1
  k = 0
  
  # Define of how many rows one tick consist in the Links dataset
  num_per_tick <- length(Links_Dataframe$Agent3)/500
  
  # Finding the row that represents the coalition at the tick from the input
  for (i in 1:length(Coalitiondataframe$Lead)){
    if (Coalitiondataframe$Lead[i]==leader & Coalitiondataframe$Creation[i]==tick) {k <- i}}
  
  # Checking if the leader from the input was a coalition leader at the tick from the input
  if(k==0){return(c(leader,"was not a coalition leader at this tick"))}
  
  # Converting into a listtype
  xlst <- Coalitiondataframe$Members[k]
  xlst <- as.character(xlst)
  xlst <- gsub("\\[|\\]|","",xlst)
  xlst <- unlist(strsplit(xlst,","))
  xlst <- as.numeric(xlst)
  
  # Checking if there were other agents in the coalition of the leader at this time
  if(length(xlst)==1){return(c(leader,"was a lone wolf at this tick"))}
  
  #Listing all the links between the agents in the coalition in a dataframe with the agents that are linked in the first two columns and the trustlevel between the two in the third column (decides the edge size)
  for (i in (1+num_per_tick*(tick-1)):(1+num_per_tick*(tick-1)+num_per_tick)) {
    if (Links_Dataframe$Agent3[i] > 0 & Links_Dataframe$agent1nr[i] %in% xlst & Links_Dataframe$agent2nr[i] %in% xlst) {
      newlist1[[p]] <- as.integer(Links_Dataframe$agent1nr[i]); newlist2[[p]] <- as.integer(Links_Dataframe$agent2nr[i]); newlist3[[p]] <- unlist(Links_Dataframe$Agent3[i]); p = p+1}}
  edges = data.frame(as.double(newlist1),as.double(newlist2))
  edges$TrustLevel <- newlist3
  
  # Checking if there are any links between agents in the coalition
  # After the model is adjusted to fix this problem, this line of code will be redundant
  if(length(edges$as.double.newlist1.)==0){return(c("Sorry, no one in coalition",xlst,"knew one another"))}
  
  # Creating a matrix out of the first 2 columns of the dataframe with the links between the agents as a 1 in the matrix
  # First by finding the size of the matrix since not all agents are shown anymore
  newlist4 <- list()
  for(el in edges$as.double.newlist1.){
    if (el %in% newlist4){}else{newlist4 <- c(newlist4,el)}}
  for (i in xlst){if (i %in% newlist4){}else {newlist4 <- c(newlist4,i)}}
  networkmatrix <- matrix(ncol = length(newlist4),nrow=length(newlist4))
  rownames(networkmatrix) <- newlist4
  colnames(networkmatrix) <- newlist4
  for (s in 1:length(edges$as.double.newlist1.)){
    for (i in newlist4){for (j in newlist4){
      if(edges$as.double.newlist1.[s]==i & edges$as.double.newlist2.[s]==j){
        networkmatrix[as.character(i),as.character(j)] = 1}}}}
  networkmatrix[is.na(networkmatrix)]<-0
  
  # Creating a data frame with the ID, agent role and popularity listed for all agents (decides the nodesize and nodecolor)
  newlist5 <- list()
  for(i in 1:30){
    if (Agents_Data_Frame[i,2] %in% newlist4) {
      newlist5 <- c(newlist5,Agents_Data_Frame[i,4])}}
  node_features = data.frame(as.double(newlist4))
  newlist5 <- as.double(newlist5)
  
  for (i in 1:length(newlist5)){if(newlist5[i]==1){newlist5[i]="External Party"}}
  for (i in 1:length(newlist5)){if(newlist5[i]==2){newlist5[i]="Policy Maker"}}
  for (i in 1:length(newlist5)){if(newlist5[i]==3){newlist5[i]="Policy Entre"}}
  for (i in 1:length(newlist5)){if(newlist4[i]==Coalitiondataframe$Lead[k]){newlist5[i]="Coalition Leader"}}
  node_features$colour = newlist5
  populist <- list()
  for (i in 1:length(networkmatrix[1,])){
    populist[i]=sum(networkmatrix[i,])+sum(networkmatrix[,i])}
  node_features$popularity <- as.double(populist)
  
  # Plot the network, the strange way of coding is needed for the Coalitionsintickplot to work properly
  p1 <- ggnet2(networkmatrix,color=node_features$colour,palette = c("External Party"="skyblue", "Policy Maker"= "skyblue2", "Policy Entre" = "skyblue4","Coalition Leader"="red"), node.label = node_features$as.double.newlist4.,size=node_features$popularity) + ggtitle(paste("Trustlevels in coalition with leader",leader,"at tick",tick))
  print(p1)
  return(1)
}

# Example
Coalitionnetworkplotggnet(Links_ACF_0,Coalitions_ACF,Agents_ACF_0,333,25)

# Sometimes no one knows any other member, the member is not the coalition leader or the coalition consist of only one person, in that case, the function returns that message
# Examples of these events occuring
Coalitionnetworkplotggnet(Links_ACF_0,Coalitions_ACF,Agents_ACF_0,39,27)
Coalitionnetworkplotggnet(Links_ACF_0,Coalitions_ACF,Agents_ACF_0,463,14)
Coalitionnetworkplotggnet(Links_ACF_0,Coalitions_ACF,Agents_ACF_0,339,8)

# function that plots all the sensible Coalitionnetworkplotggnet-plots in a tick
# requires Coalitionnetworkplotggnet
Coalitionsintickplot <- function(Links_Data_Frame, Coalitions_Data_Frame, Agents_Data_Frame,tick){
  options(warn=-1)
  for(i in 1:30){
    if(Coalitionnetworkplotggnet(Links_Data_Frame,Coalitions_Data_Frame,Agents_Data_Frame,tick,i)==1){
    }
  }
  options(warn=0)
}

# Example
Coalitionsintickplot(Links_ACF_0,Coalitions_ACF,Agents_ACF_0,8)

# Function that returns a dataframe with the teams per member at a specific tick
Teamdf <- function(Teamdataframe,tick){
  memlist <- list()
  leadlist <- list()
  idlist <- list()
  issuelist <- list()
  resourcelist <- list()
  tick1 <- list()
  tick2 <- list()
  for (i in 1:length(Teamdataframe$Step)){if (Teamdataframe$Step[i]==(tick-1)){tick1 <- i; break}}
  for (i in 1:length(Teamdataframe$Step)){if (Teamdataframe$Step[i]==(tick-1)){tick2 <- i}}
  for (j in tick1:tick2){
    for (k in 1:30){
      if(Teamdataframe$Lead[j]==k){xlst <- Teamdataframe$Members[j]
      xlst <- as.character(xlst)
      xlst <- gsub("\\[|\\]|","",xlst)
      xlst <- unlist(strsplit(xlst,","))
      xlst <- as.numeric(xlst)
      for (el in xlst) {
        memlist <- c(memlist,el)
        leadlist <- c(leadlist,Teamdataframe$Lead[j])
        idlist <- c(idlist,Teamdataframe[j,2])
        issuelist <- c(issuelist,Teamdataframe$Issue[j])
        resourcelist <- c(resourcelist,round(Teamdataframe$Resources[j],1))
      }}}}
  Members <- as.double(memlist)
  Coalitionleader <- as.double(leadlist)
  Coalition <- as.double(idlist)
  Issue <- as.double(issuelist)
  Resources <- as.double(resourcelist)
  Teamframe <- data.frame(Members, Coalitionleader,Coalition,Issue,Resources)
  return(Teamframe)
}

# Example
View(Teamdf(teams_as_3S_11,4))

# Function that plots all teams in a tick with their characteristics
Teamplotggnet <- function(Team_Dataframe,tick){
  # Creating a data frame of the team members and their characteristics
  memlist <- list()
  typelist <- c()
  idlist <- list()
  issuelist <- c()
  resourcelist <- list()
  tick2 = 0
  for (i in 1:length(Team_Dataframe$Step)){if (Team_Dataframe$Step[i]==(tick-1)){tick1 <- i; break}}
  for (i in 1:length(Team_Dataframe$Step)){if (Team_Dataframe$Step[i]==(tick-1)){tick2 <- i}}
  if(tick2 == 0){return(c("no teams existed in tick",tick))}
  for (j in tick1:tick2){
    for (k in 1:30){
      if(Team_Dataframe$Lead[j]==k){xlst <- Team_Dataframe$Members[j]
      xlst <- as.character(xlst)
      xlst <- gsub("\\[|\\]|","",xlst)
      xlst <- unlist(strsplit(xlst,","))
      xlst <- as.numeric(xlst)
      for (el in xlst) {
        memlist <- c(memlist,el)
        if(Team_Dataframe[j,5]=="problem"){typelist <- c(typelist,"Problem")
        }else{typelist <- c(typelist,"Policy")}
        idlist <- c(idlist,Team_Dataframe[j,2])
        if(Team_Dataframe$Issue[j]==1){issue = "Camp fires"}
        if(Team_Dataframe$Issue[j]==2){issue = "Planting"}
        if(Team_Dataframe$Issue[j]==3){issue = "Monitoring"}
        if(Team_Dataframe$Issue[j]==4){issue = "Firefighters"}
        if(Team_Dataframe$Issue[j]==5){issue = "Prevention"}
        issuelist <- c(issuelist,issue)
        resourcelist <- c(resourcelist,round(Team_Dataframe$Resources[j],1))
      }}}}
  Members <- as.double(memlist)
  Issue_type <- typelist
  Coalition <- as.double(idlist)
  Issue <- issuelist
  Resources <- as.double(resourcelist)
  Teams <- data.frame(Members, Issue_type,Coalition,Issue,Resources)
  
  # Creating a matrix with links between the team members
  networkmatrix = matrix(ncol=length(Teams$Members),nrow=length(Teams$Members))
  for (i in 1:length(Teams$Members)){for (j in 1:length(Teams$Members)){if(Teams$Coalition[i]==Teams$Coalition[j]){networkmatrix[i,j]=1}}}
  networkmatrix[is.na(networkmatrix)]<-0
  
  # plotting the matrix
  ggnet2(networkmatrix,label= Teams$Members,node.color = Teams$Issue,palette = c("Camp fires"="green","Planting"="red","Monitoring"="yellow","Firefighters"="blue","Prevention"="orange"),shape=Teams$Issue_type,shape.palette = c("Policy"=19,"Problem"=15),legend.size = 9,legend.position = "bottom") + theme(panel.background = element_rect(color="black")) +ggtitle(paste("Teams in the model at tick",tick))
}

#Examples
Teamplotggnet(teams_pf_3S_11,1)
Teamplotggnet(teams_as_3S_11,5)

# Widget that shows the teams of four 3S runs with varying belief gaps, closeness of problem/policy gaps
# Requires Teamplotggnet so run that first ----
u <- shinyUI(fluidPage(
  titlePanel("Teams"),
  sidebarLayout(position = "left",
                sidebarPanel("Adjustables",
                             checkboxInput("donum1", "View plot with high belief gap and problem gap", value = T),
                             checkboxInput("donum2", "View plot with low problem gap", value = T),
                             checkboxInput("donum3", "View plot with low policy gap", value = T),
                             checkboxInput("donum4", "View plot with high policy gap", value = T),
                             sliderInput(inputId = "tick",
                                         label = "Time plot high belief gap and problem gap:",
                                         min = 1,
                                         max = 500,
                                         value = 30),
                             sliderInput(inputId = "tick2",
                                         label = "Time plot low problem gap:",
                                         min = 1,
                                         max = 500,
                                         value = 30),
                             sliderInput(inputId = "tick3",
                                         label = "Time plot low policy gap:",
                                         min = 1,
                                         max = 500,
                                         value = 30),
                             sliderInput(inputId = "tick4",
                                         label = "Time plot high policy gap:",
                                         min = 1,
                                         max = 500,
                                         value = 30)
                ),
                mainPanel("Plots",
                          column(6,plotOutput(outputId="Teamplotggnet", width= "500px",height="700px"))
                ))))

s <- function(input, output) 
{
  pt1 <- reactive({
    if(!input$donum1) return(NULL)
    Teamplotggnet(teams_as_3S_2,input$tick)
  })
  pt2 <- reactive({
    if(!input$donum2) return(NULL)
    Teamplotggnet(teams_as_3S_11,input$tick2)
  })
  pt3 <- reactive({
    if(!input$donum3) return(NULL)
    Teamplotggnet(teams_as_3S_13,input$tick3)
  })
  pt4 <- reactive({
    if(!input$donum4) return(NULL)
    Teamplotggnet(teams_as_3S_14,input$tick4)
  })
  
  output$Teamplotggnet = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3(),pt4())
    # remove the null plots from ptlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,ncol=2)
  })
}
shinyApp(u,s)

#----

# Function that creates a pie chart showing the composition of the grounds in terms of burnt areas, camp sites, empty areas and forests
GroundcompositionPieChard <- function(modeldataset,tick){
  lijst1 <- list(modeldataset[tick,4],modeldataset[tick,5],modeldataset[tick,7],modeldataset[tick,11],modeldataset[tick,12])
  slices <- as.double(lijst1)
  lbls <- list("Burnt","Camp site","Empty","Thick forest","Thin forest")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls,pct)
  lbls <- paste (lbls,"%",sep=" ")
  pie(slices,labels=lbls,main = paste("Pie chard of the composition of \n the grounds at tick",tick))
}

# Example
GroundcompositionPieChard(model_ACF_exp_1,5)

# Code that creates a Widget for the pie chart with a slider with which you can decide the tick to be shown ----
ui <- fluidPage(
  titlePanel("Pie chart"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "tick",
                  label = "Time:",
                  min = 1,
                  max = 500,
                  value = 250)
    ),
    mainPanel(
      plotOutput(outputId = "Piechart")
    )
  )
)

server <- function(input, output) {
  output$Piechart <- renderPlot({
    GroundcompositionPieChard(model_ACF_exp_1,input$tick)
  })
}
# Create Shiny app
shinyApp(ui = ui, server = server)
#----

# Same plot but done in ggplot2, slightly prettier
GroundcompositionPieChartggplot <- function(modeldataset,tick){
  surface <- c(modeldataset[tick,4],modeldataset[tick,5],modeldataset[tick,7],modeldataset[tick,11],modeldataset[tick,12])
  lbls <- list("Burnt","Camp site","Empty","Thick forest","Thin forest")
  pct <- round(surface/sum(surface)*100)
  lbls <- paste(lbls,pct)
  lbls <- paste (lbls,"%",sep=" ")
  df = data.frame(surface=surface,sort=lbls)
  ggplot(df,aes(x="",y=surface,fill=sort))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)+ggtitle(paste("Pie chart of the composition of the \n grounds at tick",tick))+theme(axis.text.x = element_blank(),axis.ticks = element_blank(), panel.grid = element_blank())
}

# Example
GroundcompositionPieChartggplot(model_ACF_exp_1,5)

# Same plot but then as a barplot, this is easier to interpret because people are better at comparing these things as opposed to pie charts
Groundcompositionbarplot <- function(modeldataset,tick){
  surface <- c(modeldataset[tick,4],modeldataset[tick,5],modeldataset[tick,7],modeldataset[tick,11],modeldataset[tick,12])
  lbls <- list("Burnt","Camp site","Empty","Thick forest","Thin forest")
  pct <- round(surface/sum(surface)*100)
  lbls <- paste(lbls,pct)
  lbls <- paste (lbls,"%",sep=" ")
  df = data.frame(surface=surface,sort=lbls)
  ggplot(df,aes(x="",y=surface,fill=sort))+geom_bar(width=1,stat="identity")+ labs(title = paste("Barplot of the composition of \n the grounds at tick",tick))
}

# Example
Groundcompositionbarplot(model_ACF_exp_1,5)

# Making a video out of the ggplot pie charts so you can see over time what happens ----
for(i in 1:500){
  jpeg(paste0("rplot",sprintf("%04d",i),".png"))
  myplot <- GroundcompositionPieChartggplot(model_ACF_exp_1,i)
  print(myplot)
  dev.off()
}

imgs <- list.files(pattern="*.png")
saveVideo({for(img in imgs){im <- magick::image_read(img)
plot(as.raster(im))}},video.name = "Ground_PieChart_Over_Time.mp4")

# ----

## The policy instruments and their meaning
# policy instrument 1 - More camp fires
# policy instrument 2 - Less camp fires
# policy instrument 3 - More planting
# policy instrument 4 - Less planting
# policy instrument 5 - More monitoring
# policy instrument 6 - Less monitoring
# policy instrument 7 - More firefigthers
# policy instrument 8 - Less firefigthers
# policy instrument 9 - More prevention
# policy instrument 10 - Less prevention
# policy instrument 11 - More planting, monitoring and prevention
# policy instrument 12 - less planting, monitoring and prevention
# policy instrument 13 - less camp fires, more planting, more monitoring, less firefighters, less prevention
# policy instrument 14 - More camp fires, less planting, less monitoring, more firefighters, more prevention
# policy instrument 15 - Less camp fires, more firefighters
# policy instrument 16 - More camp fires, less firefighters

# Function that plots the perception of the state of an issue and their desired state of the issue.
# The color of the second bar states wheter the chosen instrument is in line with their desire
perceptionplot <- function(model_data_frame,agent_data_frame, issue, urgencythreshold = 0.5){
  if(model_data_frame$Chosen_instrument[500]==1){y1=1;y2 =0;y3=0;y4=0;y5=0}
  if(model_data_frame$Chosen_instrument[500]==2){y1=2;y2 =0;y3=0;y4=0;y5=0}
  if(model_data_frame$Chosen_instrument[500]==3){y1=0;y2 =1;y3=0;y4=0;y5=0}
  if(model_data_frame$Chosen_instrument[500]==4){y1=0;y2 =2;y3=0;y4=0;y5=0}
  if(model_data_frame$Chosen_instrument[500]==5){y1=0;y2 =0;y3=1;y4=0;y5=0}
  if(model_data_frame$Chosen_instrument[500]==6){y1=0;y2 =0;y3=2;y4=0;y5=0}
  if(model_data_frame$Chosen_instrument[500]==7){y1=0;y2 =0;y3=0;y4=1;y5=0}
  if(model_data_frame$Chosen_instrument[500]==8){y1=0;y2 =0;y3=0;y4=2;y5=0}
  if(model_data_frame$Chosen_instrument[500]==9){y1=0;y2 =0;y3=0;y4=0;y5=1}
  if(model_data_frame$Chosen_instrument[500]==10){y1=0;y2 =0;y3=0;y4=0;y5=2}
  if(model_data_frame$Chosen_instrument[500]==11){y1=0;y2 =1;y3=1;y4=0;y5=1}
  if(model_data_frame$Chosen_instrument[500]==12){y1=0;y2 =2;y3=2;y4=0;y5=2}
  if(model_data_frame$Chosen_instrument[500]==13){y1=2;y2 =1;y3=1;y4=2;y5=2}
  if(model_data_frame$Chosen_instrument[500]==14){y1=1;y2 =2;y3=2;y4=1;y5=1}
  if(model_data_frame$Chosen_instrument[500]==15){y1=2;y2 =0;y3=0;y4=1;y5=0}
  if(model_data_frame$Chosen_instrument[500]==16){y1=1;y2 =0;y3=0;y4=2;y5=0}
  if(issue=="Camp_fires"){y = y1;iss = 1}
  if(issue=="Planting"){y = y2;iss = 2}
  if(issue=="Monitoring"){y = y3;iss = 3}
  if(issue=="Firefighters"){y = y4;iss = 4}
  if(issue=="Prevention"){y = y5;iss = 5}
  x <- as.character(model_data_frame$Belieftruth_tree[500])
  x <- gsub("\\[|\\]| ","",x)
  x <- unlist(strsplit(x,","))
  x <- as.numeric(x)
  agentlist <- c(0)
  Legend <- c("Actual state")
  aimlist <- c(x[5+iss])
  for(i in 14971:15000){
    x <- as.character(agent_data_frame$Belieftree[i])
    x <- gsub("\\[|\\]| ","",x)
    x <- unlist(strsplit(x,","))
    x <- as.numeric(x)
    agentlist <- c(agentlist,i-14970,i-14970)
    aimlist <- c(aimlist,x[13+(iss*3)],x[14+(iss*3)])
    if(y==1 & x[13+(iss*3)]<x[14+(iss*3)]){color = "Good policy"}
    if(y==1 & x[13+(iss*3)]>x[14+(iss*3)]){color = "Bad policy"}
    if(y==2 & x[13+(iss*3)]<x[14+(iss*3)]){color = "Bad policy"}
    if(y==2 & x[13+(iss*3)]>x[14+(iss*3)]){color = "Good policy"}
    if(y==0 & abs(x[14+(iss*3)]-x[13+(iss*3)])>urgencythreshold){color ="Bad policy"}
    #print(abs(x[14+(iss*3)]-x[13+(iss*3)])>urgencythreshold)
    if(y==0 & abs(x[14+(iss*3)]-x[13+(iss*3)])<urgencythreshold){color = "Neutral policy"}
    Legend <- c(Legend,"Perception",color)
  }
  instrumentdataframe <- data.frame(as.double(agentlist),as.double(aimlist),Legend)
  ggplot(instrumentdataframe, aes(factor(as.double.agentlist.), as.double.aimlist., fill = Legend)) + 
    geom_bar(stat="identity", position = "dodge") + scale_fill_manual(values=c( "Actual state" = "black", "Perception"="grey", "Bad policy"="red", "Good policy"="green","Neutral policy"="darkgrey")) + labs(title = paste("Perceptions on issue",issue),x = "Agent number",y = "State of the issue")
}

perceptionplot(model_3S_2,agents_3S_2,"Camp_fires",0.8)
