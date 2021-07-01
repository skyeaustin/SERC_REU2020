#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#set working directory
setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac

#choose file#
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
biom5$above_biomass_g <- as.numeric(as.character(biom5$above_biomass_g))
biom5$below_biomass_g <- as.numeric(as.character(biom5$below_biomass_g))
biom5$total_biomass <- as.numeric(as.character(biom5$total_biomass))

#alive plant set#
biom6 <- na.omit(biom5)

##stat tests##
#normal distribution
ggqqplot(biom6$above_biomass_g)
ggqqplot(biom6$below_biomass_g)
ggqqplot(biom6$total_biomass)
#statistical significance#
shapiro.test(biom6$above_biomass_g) #p==< 2.2e-16
shapiro.test(biom6$below_biomass_g) #p==< 2.2e-16
shapiro.test(biom6$total_biomass) #p==< 2.2e-16
#t.tests and aov's#
t.test(biom6$total_biomass~biom5.1$nitrogen) #p==0.4472
t.test(biom6$above_biomass_g~biom5.1$nitrogen)#p==0.9636
t.test(biom6$below_biomass_g~biom5.1$nitrogen)#p==0.1601
ac <- aov(biom6$above_biomass_g~biom6$combination) #p==1.49e-05
bc <- aov(biom6$below_biomass_g~biom6$combination) #p==7.93e-05
tc <- aov(biom6$total_biomass~biom6$combination) #p==0.00234
as <- aov(biom6$above_biomass_g~biom6$plant_species) #p==6.31e-12
bs <- aov(biom6$below_biomass_g~biom6$plant_species) #p==3.85e-11
ts <- aov(biom6$total_biomass~biom6$plant_species) #p==3.5e-08
#chisq test#
biomCSQ1 <- chisq.test(biom6$total_biomass, biom6$nitrogen) 
biomCSQ2 <- chisq.test(biom6$plant_species, biom6$total_biomass) 
    #not sure what these did
#models#
biomLM_spp <- lm(biom5.1$total_biomass ~ biom5.1$plant_species)
summary(biomLM_spp)
par(mfrow = c(2,2))
plot(biomLM_spp)

biomLM_combo <- lm(biom5.1$total_biomass ~ biom5.1$combination)
summary(biomLM_combo)
par(mfrow = c(2,2))
plot(biomLM_combo)

biomLM_n <- lm(biom5.1$total_biomass ~ biom5.1$nitrogen)
summary(biomLM_n)
par(mfrow = c(2,2))
plot(biomLM_n)

#glm trial 1
glm(biom6$total_biomass ~ biom6$plant_species, family = poisson(link = "log"))
summary(glm(biom6$total_biomass ~ biom6$plant_species, family = poisson(link = "log"))
)
#glm trial 2
glm(biom6$total_biomass ~ biom6$nitrogen, family = poisson(link = "log"))
summary(glm(biom6$total_biomass ~ biom6$nitrogen, family = poisson(link = "log"))) #not significant


#aboveground biomass by species#
biom6 %>%
  ggplot(aes(x=plant_species, y=above_biomass_g, fill=above_biomass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Aboveground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#belowground biomass by species#
biom6 %>%
  ggplot(aes(x=plant_species, y=below_biomass_g, fill=below_biomass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Belowground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#total biomass by species#
biom6 %>%
  ggplot(aes(x=plant_species, y=total_biomass, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#aboveground biomass by species, fill treatment#
biom6 %>%
  ggplot(aes(x=plant_species, y=above_biomass_g, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Aboveground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#belowground biomass by species, fill treatment#
biom6 %>%
  ggplot(aes(x=plant_species, y=below_biomass_g, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Belowground Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#total biomass by species, fill treatment#
biom6 %>%
  ggplot(aes(x=plant_species, y=total_biomass, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Species") +
  xlab("Species")+
  ylab("Biomass (g)")

#aboveground biomass by combination#
biom6 %>%
  ggplot(aes(x=combination, y=above_biomass_g, fill=combination)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Combination") +
  xlab("Combination")+
  ylab("Biomass (g)")

#belowground biomass by combination#
biom6 %>%
  ggplot(aes(x=combination, y=below_biomass_g, fill=combination)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Combination") +
  xlab("Combination")+
  ylab("Biomass (g)")

#total biomass by combination#
biom6 %>%
  ggplot(aes(x=combination, y=total_biomass, fill=combination)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Combination") +
  xlab("Combination")+
  ylab("Biomass (g)")

#aboveground biomass by combonation, fill nitrogen#
biom6 %>%
  ggplot(aes(x=combination, y=above_biomass_g, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Combination") +
  xlab("Combination")+
  ylab("Biomass (g)")

#belowground biomass by combonation, fill nitrogen#
biom6 %>%
  ggplot(aes(x=combination, y=below_biomass_g, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Combination") +
  xlab("Combination")+
  ylab("Biomass (g)")

#total biomass by combonation, fill nitrogen#
biom6 %>%
  ggplot(aes(x=combination, y=total_biomass, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Total Biomass by Combination") +
  xlab("Combination")+
  ylab("Biomass (g)")



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
