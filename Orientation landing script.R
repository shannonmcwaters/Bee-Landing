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

####Paired plot####
vh = data.frame(Vertical = VHSuccess$Vsuccess, Horizontal = VHSuccess$Hsuccess)
# Calculate the weight (number of overlapping observations)
vh_summarized <- vh %>%
  group_by(Vertical, Horizontal) %>%
  summarise(Weight = n(), .groups = "drop")  # Count overlapping points

Ori_Plot <- ggpaired(vh, cond1 = "Vertical", cond2 = "Horizontal",fill = "grey",line.color = "dark grey") +
   # Add custom lines with thickness based on weight
  geom_segment(
    data = vh_summarized,
    aes(
      x = 1, xend = 2,
      y = Vertical, yend = Horizontal,
      linewidth = Weight
    ),
    color = "dark grey", inherit.aes = FALSE, alpha = 0.8
  ) +
  # Add weighted dots for Labellum
  geom_count(
    aes(x = 1, y = Vertical, size = Weight),
    data = vh_summarized,
    color = "black", alpha = 0.8, inherit.aes = FALSE
  ) +
  # Add weighted dots for NoLabellum
  geom_count(
    aes(x = 2, y = Horizontal, size = Weight),
    data = vh_summarized,
    color = "black", alpha = 0.8, inherit.aes = FALSE
  ) +
  # Adjust the scales for line width and dot size
  scale_size(name = "Number of Bees") +         
  scale_linewidth(name = "Number of Bees") +     
  ylab("Proportion success") +
  xlab("Flower type") +
  theme_minimal()


