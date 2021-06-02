#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(glmm)
#set working directory
setwd("~/SERC/ExperimentData_and_Code/SERC_REU2020") #Skye's desktop


###Resprout Data Code###
file.choose()
leaf1 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SERC\\ExperimentData_and_Code\\SERC_REU2020\\leaves_datasheet_SAUS_06012021.csv")
##species combination##
leaf2 <- leaf1 %>% 
  mutate(combination = substring(pot_id, 7, 10))
##hetero v. con specific##
leaf3 <- leaf2 %>% 
  mutate(mono_hetero = if_else(leaf2$combination %in% c("EpEV", "CnEv", "CnEp"), "hetero", "mono"))
##nitrogen treatment##
leaf4 <- leaf3 %>% 
  mutate(nitrogen = substring(pot_id, 5,5))

#make columns numeric#
leaf4$leaf_height <- as.numeric(as.character(leaf4$leaf_height))
leaf4$leaf_width <- as.numeric(as.character(leaf4$leaf_width))
leaf4$leaves_emerged <- as.numeric(as.character(leaf4$leaves_emerged))
leaf4$leaves_emerging <- as.numeric(as.character(leaf4$leaves_emerging))
leaf4$leaves_dead <- as.numeric(as.character(leaf4$leaves_dead))

#stat tests#
ggqqplot(leaf4$leaves_emerged)#see if the data is normally distributed
shapiro.test(leaf4$leaf_height)#test statistical significance




