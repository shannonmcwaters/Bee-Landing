#Packages
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(lme4)
#raw data table
LandingData <- read.csv("https://raw.githubusercontent.com/shannonmcwaters/Bee-Landing/main/Labellum%20Data%20(1).csv")

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
# Add a column for the total number of choices (numL + numN)
LandingData$total_choices <- LandingData$numL + LandingData$numN


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
LandingDataNew$Choices = ifelse(LandingDataNew$Choices == "L", 1,0)
#Overall success
mean(LandingDataNew$Successes)

#make a first landing dataframe 
firstlanding = subset(LandingDataNew, ChoiceNumber=="1")
firstlanding$Choices = ifelse(firstlanding[["Choices"]] == "L", 1,0)

####Question 1: do bees show a preference for labellums? ####


# Logistic regression for labellum preference
labellum_pref_model <- glm(cbind(numL, numN) ~ 1, 
                           data = LandingData, 
                           family = binomial)
summary(labellum_pref_model)


#same as above but with first landing only
firstlanding_model <- glm(Choices ~ 1, 
                          data = firstlanding, 
                          family = binomial)
summary(firstlanding_model)


####Question 2: Does the presence of labellum influences landing success? Chi square test####

#make data frame with separate columns for labellum or no labellum success rate for each bee
LabellumChoice = subset(LandingDataNew, Choices == "L")
LSuccess = LabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))
LSuccess = LSuccess[,c(1,4:6)]
LSuccess$Choices = LSuccess$Choices %>% replace_na(1)

NoLabellumChoice = subset(LandingDataNew, Choices == "N")
NSuccess = NoLabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))
NSuccess = NSuccess[,c(1,4:6)]
NSuccess$Choices = NSuccess$Choices %>% replace_na(0)

# Paired logistic regression for success on labellum vs. no labellum
paired_success_model <- glmer(Successes ~ Choices + (1 | BeeID), 
                              data = LandingDataNew, 
                              family = binomial)
summary(paired_success_model)

#test for just first attempts
LabellumfirstChoice = subset(firstlanding, Choices == "1")
mean(LabellumfirstChoice[["Successes"]])

NoLabellumfirstChoice = subset(firstlanding, Choices == "0")
mean(NoLabellumfirstChoice[["Successes"]])

chisq.test(table(firstlanding$Choices,firstlanding$Successes))

#Wrangle data for the plot: 
NLSuccess = merge(LSuccess,NSuccess,by="BeeID", all=T)
NLSuccess$ColonyID = NULL

colnames(NLSuccess)[3] = "Lsuccess"
colnames(NLSuccess)[6] = "Nsuccess"

NLSuccess = na.omit(NLSuccess) #omit bees that didn't visit each flower type

#plot results 
nl = data.frame(Labellum = NLSuccess$Lsuccess, NoLabellum = NLSuccess$Nsuccess) 
Lab_Plot = ggpaired(nl,cond1 = "Labellum",cond2="NoLabellum", fill="grey",line.size = 1, point.size = 1.5, line.color="dark grey") + ylab("Proportion success") + xlab("Flower type") + geom_count() 

####Side question: Correlation between size and preference or success ####
size_pref = glm(propL ~ ThoraxWidth.mm., 
    family = binomial(link = "logit"), 
    data = LandingData, 
    weights = total_choices) 
summary(size_pref)
library(ResourceSelection)
hoslem.test(LandingData$numL, fitted(size_pref))

size_success <- glm(propS ~ ThoraxWidth.mm., 
                     family = binomial(link = "logit"), 
                     data = LandingData, 
                     weights = total_choices)
summary(size_success)
library(ResourceSelection)
hoslem.test(LandingData$propS, fitted(size_success))
# Get predicted probabilities
LandingData$predicted_prob <- predict(mod, type = "response")
# Plot the raw data with predicted trend line
ggplot(LandingData, aes(x = ThoraxWidth.mm., y = propL)) +
  geom_point(color = "black", alpha = 0.4) +  # Set point color to black and remove gradient
  geom_line(aes(y = predicted_prob), color = "red") +  # Red trend line
  geom_ribbon(aes(ymin = lower_prob, ymax = upper_prob), fill = "blue", alpha = 0.2) +  # Blue shaded area
  labs(x = "Thorax Width (mm)", y = "Proportion of L Flower Choices") +
  theme_minimal() +
  guides(color = "none")  # Remove legend
