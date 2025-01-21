#Packages
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
#read in raw data
verthorizdata <- read.csv("https://raw.githubusercontent.com/shannonmcwaters/Bee-Landing/main/Horizontal%20Landing%20Data%20(1).csv")
verthorizdata = na.omit(verthorizdata[,1:5])

#first attempt only 
verthorizfirst <- subset(verthorizdata, attemptNr== 1)
verthorizfirst = na.omit(verthorizfirst[,1:5])

####Overall average successful landings####
mean(verthorizdata$success)

#Wrangle some data for the analyses
vh_sum <- verthorizdata %>%
  group_by(bee, horz0_vert1) %>%
  summarise(
    num_observations = n(),  # Number of observations for each flower type
    num_success = sum(success),  # Number of successes for each flower type
    .groups = "drop"  # Prevents nested grouping in the output
  )
vh_sum_wide <- vh_sum %>%
  pivot_wider(
    names_from = horz0_vert1, 
    values_from = c(num_observations, num_success), 
    values_fill = list(num_observations = 0, num_success = 0)
  )

# Rename columns
colnames(vh_sum_wide)[2:3] <- c("num_observations_vert", "num_observations_horz")
colnames(vh_sum_wide)[4:5] <- c("num_success_vert", "num_success_horz")

####Question 1: do bees show a preference for horizonal or vertical?####  
#
ori_pref_model <- glm(cbind(num_observations_vert, num_observations_horz) ~ 1, 
                           data = vh_sum_wide, 
                           family = binomial)
summary(ori_pref_model)


#same thing but first attempt for each bee only
ori_pref_first <- glm(horz0_vert1 ~ 1, 
                      data = verthorizfirst, 
                      family = binomial)
summary(ori_pref_first)


####Question 2: Does the orientation of the flower influence landing success? ####
paired_vh_success <- glmer(success ~ horz0_vert1 + (1 | bee), 
                              data = verthorizdata, 
                              family = binomial)
summary(paired_vh_success)

#first attempt
chisq.test(table(verthorizfirst$horz0_vert1,verthorizfirst$success))

#box plot of results
#subset data into only vertical choices and then only horizontal choices and find the average success for them separately
VertChoice = subset(verthorizdata, horz0_vert1 == "1")
VSuccess = VertChoice %>% group_by(bee) %>% summarise(across(everything(),mean))
VSuccess = VSuccess[,c(1,4:5)]

HorzChoice = subset(verthorizdata, horz0_vert1 == "0")
HSuccess = HorzChoice %>% group_by(bee) %>% summarise(across(everything(),mean))
HSuccess = HSuccess[,c(1,4:5)]

#make table with the vertical and horizontal success rate for each bee
VHSuccess = merge(VSuccess,HSuccess,by="bee", all=T)
na_count = 
  colnames(VHSuccess)[3] = "Vsuccess"
colnames(VHSuccess)[5] = "Hsuccess"
VHSuccess = na.omit(VHSuccess) #omit bees that did not visit each type of flower

vh = data.frame(Vertical = VHSuccess$Vsuccess, Horizontal = VHSuccess$Hsuccess)
Ori_plot = ggpaired(vh,cond1 = "Vertical",cond2="Horizontal", fill="grey",line.size = 1, point.size = 1.5, line.color="dark grey")+ ylab("Proportion success") + xlab("Flower Orientation") +
  geom_count()

