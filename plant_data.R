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
plant5 <- select(plant4, -starts_with("X"))
#make height column numeric#
plant5$height <- as.numeric(as.character(plant4$height)) 
#volume column#
plant6 <- plant5 %>% 
  mutate(plant_volume = height*width_max*width_perp)
#subset to last date#
plant7 <- plant6 %>% 
  subset(plant6$date == "8/5/2020")

###from here: plant6==all dates, plant7==only last date###

##stat tests##
#test for normal distribution#
ggqqplot(plant6$height) 
ggqqplot(plant7$plant_volume) 
ggqqplot(plant7$height) 
ggqqplot(plant7$plant_volume) #all look normal
#test for significance#
shapiro.test(plant6$height) #p==< 2.2e-16
shapiro.test(plant6$plant_volume) #p==< 2.2e-16
shapiro.test(plant7$height) #p==1.042e-10
shapiro.test(plant7$plant_volume) #p==< 2.2e-16
#t.tests and aov's#
t.test(plant6$height~plant6$nitrogen) #p==0.004744
t.test(plant6$plant_volume~plant6$nitrogen) #p==0.0007921
t.test(plant7$height~plant7$nitrogen) #p==0.1134
t.test(plant7$plant_volume~plant7$nitrogen)#p==0.08863
hc6 <- aov(plant6$height~plant6$combination) #p==9.68e-12
vc6 <- aov(plant6$plant_volume~plant6$combination) #p==2.7e-13
hc7 <- aov(plant7$height~plant7$combination) #p==4.34e-09
vc7 <- aov(plant7$plant_volume~plant7$combination) #p==1.63e-05
#models#
plantLM6 <- lm(plant6$height ~ plant6$date + plant6$plant_species)
summary(plantLM6)
par(mfrow = c(2,2))
plot(plantLM6)
#not a clue what this does or what it means (there are significant things?)
glm(plant6$height ~ plant6$date + plant6$plant_species, family = poisson(link = "log"))
summary(glm(plant6$height ~ plant6$date + plant6$plant_species, family = poisson(link = "log"))
)     
#not sure what this did either (there are significant things?)

#height by combonation#
  plant6 %>%
  ggplot(aes(x=combination, y=height, fill=combination)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Height by Combonation") +
  xlab("Combonation")+
  ylab("Height") 
  
  plant7 %>%
    ggplot(aes(x=combination, y=height, fill=combination)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
    theme(
      legend.position = "none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("Height by Combonation") +
    xlab("Combonation")+
    ylab("Height") 

#volume by combonation# 
plant6 %>%
  ggplot(aes(x=combination, y=plant_volume, fill=combination)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Volume by Combonation") +
  xlab("Combonation")+
  ylab("Volume")

plant7 %>%
  ggplot(aes(x=combination, y=plant_volume, fill=combination)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Volume by Combonation") +
  xlab("Combonation")+
  ylab("Volume")

#volume by species#
plant6 %>%
  ggplot(aes(x=plant_species, y=plant_volume, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Plant Volume by Species") +
  xlab("Species")+
  ylab("Volume")

plant7 %>%
  ggplot(aes(x=plant_species, y=plant_volume, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Plant Volume by Species") +
  xlab("Species")+
  ylab("Volume")

#plant height by species#
plant6 %>%
  ggplot(aes(x=plant_species, y=height, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Plant Height by Species") +
  xlab("Species")+
  ylab("Height")

plant7 %>%
  ggplot(aes(x=plant_species, y=height, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Plant Height by Species") +
  xlab("Species")+
  ylab("Height")

##code! (correct survivability code)
#survivability by species
species<-rep(c("Cn","Ef","Ev"),2)
surv<-c(rep("alive",3),rep("dead",3))
val<-c(108,102,118,12,18,2)
ndat<-data.frame(species,surv,val)
ggplot(ndat,aes(species,val, group=surv))+geom_bar(stat="identity", position=position_dodge(),aes(fill=as.factor(surv)))

################################





table(plant5$dead)



plant7 <- plant6 %>% 
  group_by(plant6$plant_species)

cnsub <- plant6 %>% 
  subset(plant6$plant_species=="Chamaecrista nictitans")

efsub <- plant6 %>% 
  subset(plant6$plant_species=="Eutrochium fistulosum")

evsub <- plant6 %>% 
  subset(plant6$plant_species=="Elymus virginicus")


species <- c("Cn", "Ef", "Ev")
alive <- c(108, 102, 118)
dead <- c(12, 18, 2)

surv_frame <- data.frame(species, alive, dead)

##code!
species<-rep(c("Cn","Ef","Ev"),2)
surv<-c(rep("alive",3),rep("dead",3))
val<-c(108,102,118,12,18,2)
ndat<-data.frame(species,surv,val)
ggplot(ndat,aes(species,val, group=surv))+geom_bar(stat="identity", position=position_dodge(),aes(fill=as.factor(surv)))






     
     
     