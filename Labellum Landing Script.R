#Packages
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
#raw data table
LandingData <- read.csv("https://docs.google.com/spreadsheets/d/1VXV89CtOK_Ly8-E8nicnZcj95Qv6uaNwwIp-NS67JFE/gviz/tq?tqx=out:csv")

####Calculate the proporation of choices that are Labellum (L) for each bee ####
LandingData$numL = str_count(LandingData$Choices.Lip.Nolip.,"L")
LandingData$numN = str_count(LandingData$Choices.Lip.Nolip.,"N")
LandingData$propL = with(LandingData, (LandingData$numL / (LandingData$numN +LandingData$numL)))
LandingData$propN = with(LandingData, (LandingData$numN / (LandingData$numN +LandingData$numL)))
#Calculate the proporation of choices that are successful (S) for each bee
LandingData$numS = str_count(LandingData$ChoiceSuccess,"S")
LandingData$numU = str_count(LandingData$ChoiceSuccess,"U")
LandingData$propS = with(LandingData, (LandingData$numS / (LandingData$numS +LandingData$numU)))
LandingData$propU = with(LandingData, (LandingData$numU / (LandingData$numS +LandingData$numU)))

#### #make choice strings (L/N & S/U) into individual rows and make new data frame ####
Choice = strsplit(as.character(LandingData$Choices.Lip.Nolip.), split=",")
Choices = unlist(Choice)
Success = strsplit(as.character(LandingData$ChoiceSuccess), split=",")
Successes = unlist(Success)
ColonyID = rep(LandingData$ColonyID, sapply(Choice, length))
BeeID = rep(LandingData$BeeID, sapply(Choice, length))
ThoraxWidth = rep(LandingData$ThoraxWidth.mm., sapply(Choice, length))
ChoiceNumber = sequence(tabulate(BeeID)) #creates column that numbers the sequence of choices
#New data frame to use for second analysis:
LandingDataNew = data.frame(ColonyID,BeeID,ChoiceNumber,Choices, Successes, ThoraxWidth)
LandingDataNew$Successes= ifelse(LandingDataNew$Successes == "S",1,0)

#Overall success
mean(LandingDataNew$Successes)



####Question 1: do bees show a preference for labellums? ####

#test proportion of visits to labellum flowers against 0.5
wilcox.test(LandingData$propL,mu=0.5)
boxplot(LandingData$propL, ylab = "Percent of visits to a labellum flower", xlab="Flowers with a Labellum")
abline(h=0.5, col ="Red") #run this to add a line at 0.5 to the box plot above

#same as above but with first landing only
firstlanding = subset(LandingDataNew, ChoiceNumber=="1")
firstlanding$Choices = ifelse(firstlanding$Choices == "L", 1,0)
wilcox.test(firstlanding$Choices,mu=0.5)

####Question 2: Does the presence of labellum influences landing success? Chi square test####

#make data frame with separate columns for labellum or no labellum success rate for each bee
LabellumChoice = subset(LandingDataNew, Choices == "L")
LSuccess = LabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))
LSuccess = LSuccess[,c(1,5:6)]

NoLabellumChoice = subset(LandingDataNew, Choices == "N")
NSuccess = NoLabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))
NSuccess = NSuccess[,c(1,5)]

#combine above data and test for difference 
NLSuccess = merge(LSuccess,NSuccess,by="BeeID", all=T)
countNA
colnames(NLSuccess)[2] = "Lsuccess"
colnames(NLSuccess)[4] = "Nsuccess"
NLSuccess = na.omit(NLSuccess) #omit bees that didn't visit each flower type
wilcox.test(NLSuccess$Lsuccess,NLSuccess$Nsuccess,paired = T)

#plot results
nl = data.frame(Labellum = NLSuccess$Lsuccess, NoLabellum = NLSuccess$Nsuccess)
ggpaired(nl,cond1 = "Labellum",cond2="NoLabellum", fill="grey",line.size = 1, point.size = 1.5, line.color="dark grey") + ylab("Proportion success") + xlab("Flower type")


####Side question: Correlation between size and preference or success ####
cor(LandingData$ThoraxWidth.mm.,LandingData$propS, method = "spearman")
cor(LandingData$ThoraxWidth.mm.,LandingData$propL, method = "spearman")

plot(Lsuccess~ThoraxWidth
     ,data=NLSuccess
)
points(Nsuccess~ThoraxWidth
       ,data=NLSuccess,col="red")
