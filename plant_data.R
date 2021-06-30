#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac
plant1 <- read.csv("/Users/saus/Documents/r_stuff/SERC/csvs/plant_datasheet_06252021_SAUS.csv") #skye's mac

#species combination#
plant2 <- plant1 %>% 
  mutate(combination = substring(pot_id, 7, 10))
#hetero v. con specific#
plant3 <- plant2 %>% 
  mutate(mono_hetero = if_else(plant2$combination %in% c("EpEV", "CnEv", "CnEp"), "hetero", "mono"))
#nitrogen treatment#
plant4 <- plant3 %>% 
  mutate(nitrogen = substring(pot_id, 5,5))

#remove weird columns#
plant4 <- select(plant3, -starts_with("X"))

plant4$height <- as.numeric(as.character(plant4$height)) #make height column numeric

##volume column##
plant5 <- plant4 %>% 
  mutate(plant_volume = height*width_max*width_perp)

#height, combo
plant5 %>%
  ggplot(aes(x=combination, y=height, fill=combination)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Height by Combonation") +
  xlab("Combonation")+
  ylab("Plant Height")

#combo, volume, 
plant5 %>%
  ggplot(aes(x=combination, y=plant_volume, fill=combination)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Volume by Comonation") +
  xlab("Combonation")+
  ylab("Volume")

#species, volume, 
plant5 %>%
  ggplot(aes(x=plant_species, y=plant_volume, fill=plant_species)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Plant Volume by Species") +
  xlab("Species")+
  ylab("Volume")

plantLM <- lm(plant5$height ~ plant5$date + plant5$plant_species)
summary(plantLM)
par(mfrow = c(2,2))
plot(plantLM)

glm(plant5$height ~ plant5$date + plant5$plant_species, family = poisson(link = "log"))
summary(glm(plant5$height ~ plant5$date + plant5$plant_species, family = poisson(link = "log"))
)     
     
     
#survivability
#i want plant survivability x spp, and plant surv x spp x treatment
#how??

#spp treatment survival
plant5 %>%
  ggplot(aes(x=nitrogen, y=dead, fill=plant_species, shape=plant_species, size=plant_species)) +
  geom_jitter(color="black", size=0.7, alpha=0.9) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Area by Species") +
  xlab("Species")+
  ylab("Leaf Area")





     
     
     
     