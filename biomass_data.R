#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#set working directory
setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac

###Biomass Data Code###
#choose file#
file.choose()
biom1 <- read.csv("C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\Data\\Data_For_Analysis\\plant_biomass_all.csv") #skye's desktop
biom1 <- read.csv("/Users/saus/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis/plant_biomass_all.csv") #skye's mac

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

#make total biomass column#
biom5 <- biom4 %>% 
  mutate(total_biomass = above_biomass_g+below_biomass_g)


#make columns numeric#
biom5$above_biomass_g <- as.numeric(as.character(biom4$above_biomass_g))
biom5$below_biomass_g <- as.numeric(as.character(biom4$below_biomass_g))
biom5$total_biomass <- as.numeric(as.character(biom4$total_biomass))



#alive plant set#
biom5.1 <- na.omit(biom5)


#stat tests#
ggqqplot(biom4$above_biomass_g)#see if the data is normally distributed
ggqqplot(biom4$below_biomass_g)
shapiro.test(biom4$above_biomass_g)#test statistical significance
shapiro.test(biom4$below_biomass_g)

#plot by spp abv#
biom5.1 %>%
  ggplot(aes(x=plant_species, y=above_biomass_g, fill=above_biomass_g)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Aboveground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

##spp below#
biom5.1 %>%
  ggplot(aes(x=plant_species, y=below_biomass_g, fill=below_biomass_g)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Belowground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")


#spp and treatment abv
biom5.1 %>%
  ggplot(aes(x=plant_species, y=above_biomass_g, fill=nitrogen)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Aboveground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#plot by spp treat, below
biom5.1 %>%
  ggplot(aes(x=plant_species, y=below_biomass_g, fill=nitrogen)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Belowground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#total biomass#
biom5.1 %>%
  ggplot(aes(x=plant_species, y=total_biomass, fill=nitrogen)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")


##look at belowground biomass by treatment##


