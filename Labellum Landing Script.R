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




####Question 1: do bees show a preference for labellums? ####
wilcox.test(LandingData$propL,mu=0.5)
boxplot(LandingData$propL, ylab = "Percent of visits to a labellum flower", xlab="Flowers with a Labellum")
abline(h=0.5, col ="Red") #run this to add a line at 0.5 to the box plot above



####Question 2: Does the presence of labellum influences landing success? Chi square test####
LabellumChoice = subset(LandingDataNew, Choices == "L")
LSuccess = LabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))
LSuccess = LSuccess[,c(1,5:6)]

NoLabellumChoice = subset(LandingDataNew, Choices == "N")
NSuccess = NoLabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))
NSuccess = NSuccess[,c(1,5)]


NLSuccess = merge(LSuccess,NSuccess,by="BeeID", all=T)
colnames(NLSuccess)[2] = "Lsuccess"
colnames(NLSuccess)[4] = "Nsuccess"
NLSuccess = na.omit(NLSuccess)
wilcox.test(NLSuccess$Lsuccess,NLSuccess$Nsuccess,paired = T)

nl = data.frame(Labellum = NLSuccess$Lsuccess, NoLabellum = NLSuccess$Nsuccess)
ggpaired(nl,cond1 = "Labellum",cond2="NoLabellum")


####Correlation between size and preference or success ####
cor(LandingData$ThoraxWidth.mm.,LandingData$propS, method = "spearman")
cor(LandingData$ThoraxWidth.mm.,LandingData$propL, method = "spearman")

