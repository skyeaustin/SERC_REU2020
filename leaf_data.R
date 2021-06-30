#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#set working directory
setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac

#read base csv
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
    #not normally distributed
shapiro.test(leaf4$leaf_height)#test statistical significance
    #significant, but probably missing something (like a variable?)

#set of alive plants#
leaf5.1 <- subset(leaf4, dead == "N", select = yard:check.notes)

#set of dead plants#
leaf5.2 <- subset(leaf4, dead=="Y", select = yard:check.notes)

#make leaf area column#
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
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")                                 ##these are both the same but different?????

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

###don't think this is helpful, keeping it in case###
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
##########

##add total leaves column##
leaf7 <- leaf6.1 %>% 
  mutate(total_leafnumber=leaves_emerged+leaves_emerging)

#total leaf number x treatment
leaf7 %>%
  ggplot(aes(x=nitrogen, y=total_leafnumber, fill=plant_species)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Leaf Number by Treatment") +
  xlab("Treatment")+
  ylab("Number of Emerged Leaves")
#####this plot is so bad i'm keeping it to laugh at it#####
 
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
####this is the worst plot i've ever seen please fix this####
##why does total leaf number go down at the last date?##

#total leaves over time by nitrogen (ni)
leaf7 %>% 
  ggplot(aes(x=date, y=total_leafnumber, fill=nitrogen, color=nitrogen),
         leaf7+scale_color_manual(values=c("#69b3a2", "purple", "black")))+ #this line does nothing lol
  geom_jitter()+
  theme(
    plot.title = element_text(size=11),
  ) +
  ggtitle("Total leaves over time") +
  xlab("time")+
  ylab("Number of Leaves")
####this is the worst plot i've ever seen please fix this####

#leaf area over time by spp (ni)
leaf7 %>% 
  ggplot(aes(x=date, y=leaf_area, fill=plant_species, color=plant_species),
         leaf7+scale_color_manual(values=c("#69b3a2", "purple", "black")))+ #this line does nothing lol
  geom_jitter()+
  theme(
    plot.title = element_text(size=11),
  ) +
  ggtitle("Total leaves over time") +
  xlab("time")+
  ylab("Number of Leaves")
####this is the worst plot i've ever seen please fix this####

#leaf area over time by treatment (ni)
leaf7 %>% 
  ggplot(aes(x=date, y=leaf_area, fill=nitrogen, color=nitrogen),
         leaf7+scale_color_manual(values=c("#69b3a2", "purple", "black")))+ #this line does nothing lol
  geom_jitter()+
  theme(
    plot.title = element_text(size=11),
  ) +
  ggtitle("Total leaves over time") +
  xlab("time")+
  ylab("Number of Leaves")
####this is the worst plot i've ever seen please fix this####




#linear model??#
leafLM <- lm(leaf7$total_leafnumber ~ leaf7$date + leaf7$nitrogen)
summary(leafLM)
par(mfrow = c(2,2))
plot(leafLM)

#trial 1
glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$nitrogen, family = poisson(link = "log"))
summary(glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$nitrogen, family = poisson(link = "log"))
)

#trail 2
glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$plant_species, family = poisson(link = "log"))
summary(glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$plant_species, family = poisson(link = "log")))


###stuff to try###
#N*leaf#*time
#leaf*combo*time
#leaf*biomass*time
#n*biom*spp
#n*biom*time
#leaf*biom*n
#N*plant C&N
#C&N*spp
#respt*N
#respt*C&N*spp
#respt#C&N*N

#use timepoint as random effect






