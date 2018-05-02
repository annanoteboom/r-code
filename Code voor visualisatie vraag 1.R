##Importing the needed datasets
`1_model_ACF_exp_0` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_0.csv")
`1_model_ACF_exp_1` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_1.csv")
`1_model_ACF_exp_2` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_2.csv")
`1_model_ACF_exp_3` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_3.csv")
`1_model_ACF_exp_4` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_4.csv")
`1_model_ACF_exp_5` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_5.csv")
`1_model_ACF_exp_6` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_6.csv")
`1_model_ACF_exp_7` <- read.csv("C:/Users/Anna/Downloads/1_model_ACF_exp_7.csv")
Exploration_LHS_ACF <- read.csv("C:/Users/Anna/Downloads/Exploration_LHS_ACF.data", header=FALSE)

### For the input file, structuring
input1 <- Exploration_LHS_ACF
#Eventually, only the first 16 inputs were run
input1 <- input1[1:16,]
colnames(input1) <- c("belieftreeprofile(0,1,2,3)","Affiliation1 weight","Affiliation 2 weight","Affiliation 3 weight", "PAA1(25-50)","PAA2(25,50)","EIOP(0.01-0.1)","RPP(1-10)","RWA(0.05,20)","TDC(0.01,0.1)","CLC1(0.7-0.8)","CLC2(0.8-0.9)","CLC3(0.9-1.0)","Team Gap Belief(0.6-1.0)","Team Gap CloseProb(0.2-0.7)","Team Gap ClosePol(0.2-0.7)","ACF Thres(0.15-0.55)")

## Plot the Agendalist against the different belieftreeprofiles in the input (row 1:8, first colomn)
#Create list with last values for the agenda issue of the runs
Agenda_outcomeLst <- c(tail(`1_model_ACF_exp_0`$Agenda_issue,n=1),tail(`1_model_ACF_exp_1`$Agenda_issue,n=1),tail(`1_model_ACF_exp_2`$Agenda_issue,n=1),tail(`1_model_ACF_exp_3`$Agenda_issue,n=1),tail(`1_model_ACF_exp_4`$Agenda_issue,n=1),tail(`1_model_ACF_exp_5`$Agenda_issue,n=1),tail(`1_model_ACF_exp_6`$Agenda_issue,n=1),tail(`1_model_ACF_exp_7`$Agenda_issue,n=1))
# Plot the thing
plot(input1[1:8,1],Agenda_outcomeLst)

## This works the same for the other columns, except for the belieftruthtree, we don't know how to visualise that

###Probably not nessecairy but this code plots the development of affiliations per agent
## For the agent file: showing the development of the agent affiliations
# Extracting the two columns we need
Agentlijst <- `1_agents_ACF_exp_0`[,c(2,6)]
# Making a matric of Affiliations per matrix
Agentmatrix = matrix(Agentlijst$Affiliation,
                     nrow = (max(Agentlijst$AgentID)+1),
                     ncol = length(Agentlijst$AgentID)/(max(Agentlijst$AgentID)+1),
                     byrow = FALSE)
#plotting the development/behaviour of the affiliation per agent over time
matplot(t(Agentmatrix), type = "l", xlab = "time", ylab = "Affiliation")
title("Initial belief 0")
