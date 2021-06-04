#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#set working directory
setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac

###Resprout Data Code###
file.choose()
biom1 <- read.csv("C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\Data\\Data_For_Analysis\\plant_biomass_all.csv") #skye's desktop
leaf1 <- read.csv("/Users/saus/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/leaves_datasheet_SAUS_06012021.csv") #skye's mac

#remove weird columns#
biom1.5 <- select(biom1, -starts_with("X"))

##species combination##
biom2<- biom1.5 %>% 
  mutate(combination = substring(pot_id, 7, 10))
##hetero v. con specific##
biom3 <- biom2%>% 
  mutate(mono_hetero = if_else(biom2$combination %in% c("EpEV", "CnEv", "CnEp"), "hetero", "mono"))
##nitrogen treatment##
biom4 <- biom3 %>% 
  mutate(nitrogen = substring(pot_id, 5,5))


#make columns numeric#
biom4$above_biomass_g <- as.numeric(as.character(biom4$above_biomass_g))
biom4$below_biomass_g <- as.numeric(as.character(biom4$below_biomass_g))

#make total biomass column#
biom5 <- biom4 %>% 
  mutate(total_biomass = above_biomass_g+below_biomass_g)

#alive plant set#
biom5.1 <- na.omit(biom5)

#dead plant set#
# <- subset(biom4, above_biomass_g != NA, select = pot_id:nitrogen)


#stat tests#
ggqqplot(biom4$above_biomass_g)#see if the data is normally distributed
shapiro.test(leaf4$leaf_height)#test statistical significance
