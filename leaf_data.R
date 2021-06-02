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

#set of alive plants#
leaf5.1 <- subset(leaf4, dead == "N", select = yard:check.notes)

#set of dead plants#
leaf5.2 <- subset(leaf4, dead=="Y", select = yard:check.notes)

##make leaf area column?##
leaf6.1 <- leaf5.1 %>% 
  mutate(leaf_area = leaf_width*leaf_height)

relocate(leaf6.1$leaf_area, .before = dead, .after = leaf_width) #tried to put leaf area somewhere else, doesn't work

#plot by spp#
##plot resprouts by spp##
#leaf5.1 %>%
  #ggplot(aes(x=plant_species, y=resprouts, fill=resprouts)) +
  #geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  #theme(
    #legend.position="none",
    #plot.title = element_text(size=11)
 # ) +
 # ggtitle("Resprouts boxplot") +
 # xlab("Species")+
  #ylab("Number of Resprouts")


#plot by treatment#
