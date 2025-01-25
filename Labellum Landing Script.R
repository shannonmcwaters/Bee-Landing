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

# overall mean successe
mean_all_success <- mean(LandingDataNew$Successes)

# Mean successe for labellum
mean_success_choices_1 <- LandingDataNew %>%
  filter(Choices == 1) %>%
  summarise(mean_success = mean(Successes)) %>%
  pull(mean_success)

# Mean successe no labellum
mean_success_choices_0 <- LandingDataNew %>%
  filter(Choices == 0) %>%
  summarise(mean_success = mean(Successes)) %>%
  pull(mean_success)

#make a first landing dataframe 
firstlanding = subset(LandingDataNew, ChoiceNumber=="1")

####Question 1: do bees show a preference for labellums? ####


# Logistic regression for labellum preference
labellum_pref_model <- glm(propL ~ 1, 
                           data = LandingData, 
                           weights = total_choices,
                           family = binomial)
summary(labellum_pref_model)


#same as above but with first landing only
firstlanding_model <- glm(Choices ~ 1, 
                          data = firstlanding, 
                          family = binomial)
summary(firstlanding_model)


####Question 2: Does the presence of labellum influences landing success? Chi square test####

#make data frame with separate columns for labellum or no labellum success rate for each bee
LabellumChoice = subset(LandingDataNew[,2:6], Choices == "1")
LSuccess = LabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))

NoLabellumChoice = subset(LandingDataNew, Choices == "0")
NSuccess = NoLabellumChoice %>% group_by(BeeID) %>% summarise(across(everything(),mean))
NSuccess = NSuccess[,c(1,4:6)]
NSuccess$Choices = NSuccess$Choices %>% replace_na(0)


#logistic regression for success on labellum vs. no labellum
success_model <- glmer(Successes ~ Choices + (1 | BeeID), 
                              data = LandingDataNew, 
                              family = binomial)
summary(success_model)

#test for just first attempts
LabellumfirstChoice = subset(firstlanding, Choices == "1")
mean(LabellumfirstChoice[["Successes"]])

NoLabellumfirstChoice = subset(firstlanding, Choices == "0")
mean(NoLabellumfirstChoice[["Successes"]])

chisq.test(table(firstlanding$Choices,firstlanding$Successes))

#Wrangle data for the plot: 
NLSuccess = merge(LSuccess,NSuccess,by="BeeID", all=T)
NLSuccess$ColonyID = NULL

colnames(NLSuccess)[4] = "Lsuccess"
colnames(NLSuccess)[7] = "Nsuccess"

####plot results ####
nl = data.frame(Labellum = NLSuccess$Lsuccess, NoLabellum = NLSuccess$Nsuccess) 
# Calculate the weight (number of overlapping observations)
nl_summarized <- nl %>%
  group_by(Labellum, NoLabellum) %>%
  summarise(Weight = n(), .groups = "drop")  # Count overlapping points
Lab_Plot <- ggpaired(nl, cond1 = "Labellum", cond2 = "NoLabellum",fill = "grey",line.color = "dark grey") +
  # Add custom lines with thickness based on weight
  geom_segment(
    data = nl_summarized,
    aes(
      x = 1, xend = 2,
      y = Labellum, yend = NoLabellum,
      linewidth = Weight
    ),
    color = "dark grey", inherit.aes = FALSE, alpha = 0.8
  ) +
  # Add weighted dots for Labellum
  geom_count(
    aes(x = 1, y = Labellum, size = Weight),
    data = nl_summarized,
    color = "black", alpha = 0.8, inherit.aes = FALSE
  ) +
  # Add weighted dots for NoLabellum
  geom_count(
    aes(x = 2, y = NoLabellum, size = Weight),
    data = nl_summarized,
    color = "black", alpha = 0.8, inherit.aes = FALSE
  ) +
  # Adjust the scales for line width and dot size
  scale_size(name = "Number of Bees") +         
  scale_linewidth(name = "Number of Bees") +     
  ylab("Proportion success") +
  xlab("Flower type") +
  theme_minimal()



####Side question: Correlation between size and preference or success ####
size_pref = glm(propL ~ ThoraxWidth.mm., 
    family = binomial, 
    data = LandingData, 
    weights = total_choices) 
summary(size_pref)
library(ResourceSelection)
hoslem.test(LandingData$propL, fitted(size_pref))


size_success <- glm(propS ~ ThoraxWidth.mm., 
                    family = binomial(link = "logit"), 
                    data = LandingData, 
                    weights = total_choices)
summary(size_success)
library(ResourceSelection)
hoslem.test(LandingData$propS, fitted(size_success))

# Let's plot the sig. result: 
#Get predicted probabilities
LandingData$predicted_prob <- predict(size_success, type = "response")
# Calculate the confidence intervals for the predicted probabilities
link <- predict(size_success, type = "link", se.fit = TRUE)
LandingData$lower_prob <- plogis(link$fit - 1.96 * link$se.fit)  # Lower confidence bound
LandingData$upper_prob <- plogis(link$fit + 1.96 * link$se.fit)  # Upper confidence bound
# Plot the raw data with predicted trend line
ggplot(LandingData, aes(x = ThoraxWidth.mm., y = propS)) +
  geom_point(color = "black", alpha = 0.4) +  # Set point color to black and remove gradient
  geom_line(aes(y = predicted_prob), color = "red") +  # Red trend line
  geom_ribbon(aes(ymin = lower_prob, ymax = upper_prob), fill = "blue", alpha = 0.2) +  # Blue shaded area
  labs(x = "Thorax Width (mm)", y = "Proportion of Successful Lands") +
  theme_minimal() +
  guides(color = "none")  # Remove legend




#wrangle data so we can look at the interaction between size and flower type on success to see if preference potentially is driven by landing success
interaction_data <- LandingDataNew %>%
  group_by(BeeID, Choices) %>%
  summarise(
    SuccessRate = mean(Successes),  
    NumChoices = n(), 
    .groups = "drop"
  )
interaction_data <- interaction_data %>%
  left_join(LandingDataNew %>% select(BeeID, ThoraxWidth) %>% distinct(), by = "BeeID")

interaction_model = glmer(SuccessRate ~ ThoraxWidth*Choices+(1|BeeID), 
            family = binomial(link = "logit"), 
            data = interaction_data, 
            weights = NumChoices) 
summary(model)


