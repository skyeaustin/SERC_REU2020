#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(glmm)
#set working directory
setwd("~/SERC/ExperimentData_and_Code/SERC_REU2020") #Skye's desktop
file.choose()
respt1 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SERC\\ExperimentData_and_Code\\SERC_REU2020\\resprout_combined_SAUS_11172020.csv")
ggqqplot(respt1$resprouts)#see if the data is normally distributed
shapiro.test(respt1$resprouts)#test statistical significance

#add treatment#
##nitrogen treatment##
respt2 <- respt1 %>% 
  mutate(treatment = substring(pot_id, 5,5))

#remove NA's#
respt3 <- subset(respt2, resprouts != "NA", select = pot_id:treatment)
