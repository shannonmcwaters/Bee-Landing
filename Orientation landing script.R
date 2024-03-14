#Packages
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
#read in raw data
verthorizdata <- read.csv("https://raw.githubusercontent.com/shannonmcwaters/Bee-Landing/main/Horizontal%20Landing%20Data%20(1).csv")
#first attempt only 
verthorizfirst <- subset(verthorizdata, attemptNr== 1)

####Overall average successful landings####
mean(verthorizdata$success)

####Question 1: do bees show a preference for horizonal or vertical?####  
#find the proportion of visits to vertical flowers and test against 0.5
verticlemean = aggregate(horz0_vert1~bee,verthorizdata, mean)
wilcox.test(verticlemean$horz0_vert1,mu=0.5)
#same thing but first attempt for each bee only
wilcox.test(verthorizfirst$horz0_vert1, mu=0.5)
mean(verthorizfirst$horz0_vert1)

####Question 2: Does the orientation of the flower influence landing success? ####

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
wilcox.test(VHSuccess$Vsuccess,VHSuccess$Hsuccess,paired = T)

#first attempt
chisq.test(table(verthorizfirst$horz0_vert1,verthorizfirst$success))

#box plot of results
vh = data.frame(Vertical = VHSuccess$Vsuccess, Horizontal = VHSuccess$Hsuccess)
ggpaired(vh,cond1 = "Vertical",cond2="Horizontal", fill="grey",line.size = 1, point.size = 1.5, line.color="dark grey")+ ylab("Proportion success") + xlab("Flower Orientation") +
  geom_count()

