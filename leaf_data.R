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

#remove na's/dead plants#
leaf5 <- na.omit(leaf4)
#set of dead plants#
leaf5.1 <- subset(leaf4, dead=="Y", select = yard:check.notes)
#make leaf area column#
leaf6 <- leaf5 %>% 
  mutate(leaf_area = leaf_width*leaf_height)
##add total leaves column##
leaf7 <- leaf6 %>% 
  mutate(total_leafnumber=leaves_emerged+leaves_emerging)
#subset to the last date
leaf8 <- leaf7 %>% 
  subset(leaf7$date == "8/3/2020")

###from here: leaf7 is all dates, leaf8 is a subset of the last date##

##stat tests##
#see if the data is normally distributed
ggqqplot(leaf7$leaves_emerged)
ggqqplot(leaf8$leaves_emerged)
ggqqplot(leaf7$total_leafnumber)
ggqqplot(leaf8$total_leafnumber)
#test statistical significance
shapiro.test(leaf8$leaf_height)
shapiro.test(leaf7$leaf_height)
shapiro.test(leaf8$total_leafnumber)
shapiro.test(leaf7$total_leafnumber)
#significant, but probably missing something (like a variable?)


##07/01/2021 running some t tests##
t.test(leaf7$leaf_area~leaf7$nitrogen) #p==0.34
t.test(leaf8$leaf_area~leaf8$nitrogen) #p==0.398
t.test(leaf7$total_leafnumber~leaf7$nitrogen) #p==0.95
t.test(leaf8$total_leafnumber~leaf8$nitrogen) #p==0.9572
sppaov1 <- aov(leaf7$leaf_area~leaf7$plant_species) #p==<2e-16
sppaov2 <- aov(leaf7$total_leafnumber~leaf7$plant_species) #p==<2e-16
sppaov3 <- aov(leaf8$leaf_area~leaf8$plant_species) #p==7.42e-09
sppaov4 <- aov(leaf8$total_leafnumber~leaf8$plant_species) #p==<2e-16

##leaf area by species##
leaf7 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=leaf_area)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")

leaf8 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=leaf_area)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")

##leaf area by plant species with treatment##
leaf7 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area") 

leaf8 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area") 


##total leaf number by plant species##
leaf7 %>%
  ggplot(aes(x=plant_species, y=leaves_emerged, fill=leaves_emerged)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Leaf Number by Species") +
  xlab("Species")+
  ylab("Total Number of Leaves") 

leaf8 %>%
  ggplot(aes(x=plant_species, y=leaves_emerged, fill=leaves_emerged)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Leaf Number by Species") +
  xlab("Species")+
  ylab("Total Number of Leaves") 


##total leaf number by treatment##
leaf7 %>%
  ggplot(aes(x=nitrogen, y=total_leafnumber, fill=total_leafnumber)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Leaf Number by Treatment") +
  xlab("Treatment")+
  ylab("Total Number of Leaves") #every time i run this plot it changes?

leaf8 %>%
  ggplot(aes(x=nitrogen, y=total_leafnumber, fill=total_leafnumber)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Leaf Number by Treatment") +
  xlab("Treatment")+
  ylab("Total Number of Leaves") #same with this one

 
#linear model#
leafLM7 <- lm(leaf7$total_leafnumber ~ leaf7$date + leaf7$nitrogen)
summary(leafLM7)
par(mfrow = c(2,2))
plot(leafLM7)


#glm trial 1
glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$nitrogen, family = poisson(link = "log"))
summary(glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$nitrogen, family = poisson(link = "log"))
)

#glm trial 2
glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$plant_species, family = poisson(link = "log"))
summary(glm(leaf7$total_leafnumber ~ leaf7$date + leaf7$plant_species, family = poisson(link = "log")))

#use timepoint as random effect# (?)

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



##leaf area by plant species with yard##
leaf7 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=yard)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")

leaf8 %>%
  ggplot(aes(x=plant_species, y=leaf_area, fill=yard)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")
