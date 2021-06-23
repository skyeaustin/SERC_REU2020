#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#set working directory
setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac

###leaves Data Code###
file.choose()
leaf1 <- read.csv("C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\leaves_datasheet_SAUS_06012021.csv") #skye's desktop
leaf1 <- read.csv("/Users/saus/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/leaves_datasheet_SAUS_06012021.csv") #skye's mac
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

#set of alive plants#
leaf5.1 <- subset(leaf4, dead == "N", select = yard:check.notes)

#set of dead plants#
leaf5.2 <- subset(leaf4, dead=="Y", select = yard:check.notes)

##make leaf area column?##
leaf6.1 <- leaf5.1 %>% 
  mutate(leaf_area = leaf_width*leaf_height)

#plot by spp#
leaf6.1 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=leaf_area)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")


#plot by treatment with spp#
leaf6.1 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=nitrogen)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Treatment") +
  xlab("Treatment Type")+
  ylab("Leaf Area")

#spp w/treatment#
leaf6.1 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=nitrogen)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")

#by yard#
leaf6.1 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=yard)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")

##to do: leaf status by spp and by treatment (and yard?#
leaf6.1 %>%
  ggplot(aes(x=plant_species, y=leaves_emerged, fill=leaves_emerged)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Emerged Leaves by Species") +
  xlab("Species")+
  ylab("Number of Emerged Leaves") ##how to get multiple 3rd axis?

##add total leaves column##
leaf7 <- leaf6.1 %>% 
  mutate(total_leafnumber=leaves_emerged+leaves_emerging)
 
#total leaf number by species over time# 
leaf7 %>% 
  ggplot(aes(x=date, y=total_leafnumber, fill=plant_species, color=plant_species),
         leaf7+scale_color_manual(values=c("#69b3a2", "purple", "black")))+ #this line does nothing lol
  geom_jitter()+
  theme(
    plot.title = element_text(size=11),
  ) +
  ggtitle("Total leaves over time") +
  xlab("time")+
  ylab("Number of Leaves")
##why does total leaf number go down at the last date?##

#abline is linear model
