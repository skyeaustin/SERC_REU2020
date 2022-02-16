#load packages
library(tidyverse) # for data organization, manipulation, & visualization; includes ggplot2 and dplyr # 
library(ggpubr) # for customizing plots made with ggplot2
library(RColorBrewer) # color palettes for graphing
library(rcompanion) # for normality tests and transformations
library(nlme) # for mixed linear and generalized linear models
library(lme4) # for mixed linear and generalized linear models
library(devtools) # simplify r commands
library(lmerTest)

#set wd
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac
setwd("C:/Users/hrusk/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis")

#read in csv
plant1 <- read.csv("/Users/saus/Documents/r_stuff/SERC/csvs/plant_datasheet_06252021_SAUS.csv") #skye's mac
plant1 <- read.csv("/Users/saus/Documents/r_stuff/SERC/csvs/plant_datasheet_06252021_SAUS.csv")
plant1 <- read.csv("plant_datasheet_06252021_SAUS.csv")

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

##stat tests##
#test for normal distribution#
ggqqplot(plant7$height)
plant7$tukey_height = transformTukey(plant7$height) #transform to fit normality
plant7$log_height = log(plant7$height)
ggqqplot(plant7$log_height)
ggqqplot(plant7$tukey_height)

ggqqplot(plant7$plant_volume)
plant7$tukey_vol = transformTukey(plant7$plant_volume) #transform to fit normality
plant7$sqrt_vol = (sqrt(plant7$plant_volume))

#save lambda)#
tukey_height_lambda <- transformTukey(plant7$height, returnLambda = TRUE)
tukey_vol_lambda <- transformTukey(plant7$plant_volume, returnLambda = TRUE)

#test for significance/normality#
shapiro.test(plant7$tukey_height) #p==0.05789
shapiro.test(plant7$tukey_vol) #p==0.006434

#models#
#plant height by n*combo
lmer_h_ncomb <- lmer(data = plant7, tukey_height~nitrogen*combination + (1|yard))

#plant vol by n*combo
lmer_vol_ncomb <- lmer(data = plant7, tukey_vol~nitrogen*combination + (1|yard))

#plant height by n*mono/het
lmer_h_nmonhet <- lmer(data = plant7, tukey_height~nitrogen*mono_hetero + (1|yard))

#plant vol by n*mono/het
lmer_vol_nmonhet <- lmer(data = plant7, tukey_vol~nitrogen*mono_hetero + (1|yard))

#height by combonation#
plant7 %>%
  ggplot(aes(x=combination, y=tukey_height, fill=combination)) +
    geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6", "#149F86", "#596475", "#FBE118"))+
    geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Height by Combonation") +
  xlab("Combonation")+
  ylab("Height (cm)") 

#volume by combonation# 
plant7 %>%
  ggplot(aes(x=combination, y=tukey_vol, fill=combination)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6", "#149F86", "#596475", "#FBE118"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Volume by Combonation") +
  xlab("Combonation")+
  ylab("Volume (cm^3)")

#volume by species#
plant7 %>%
  ggplot(aes(x=plant_species, y=tukey_vol, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Plant Volume by Species") +
  xlab("Species")+
  ylab("Volume (cm^3)")

#plant height by species#
plant7 %>%
  ggplot(aes(x=plant_species, y=tukey_height, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Plant Height by Species") +
  xlab("Species")+
  ylab("Height (cm)")

#height by mono/het
plant7 %>%
  ggplot(aes(x=mono_hetero, y=tukey_height, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text( size=7), 
    legend.text=element_text(size=7),
  ) +
  ggtitle("Height by Specificity") +
  xlab("Specificity")+
  ylab("Height (cm)") 

#volume by mono/het
plant7 %>%
  ggplot(aes(x=mono_hetero, y=tukey_vol, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text( size=7), 
    legend.text=element_text(size=7),
  ) +
  ggtitle("Volume by Specificity") +
  xlab("Specificity")+
  ylab("Volume (cm^3)") 

#volume by nitrogen#
plant7 %>%
  ggplot(aes(x=nitrogen, y=tukey_vol, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text( size=7), 
    legend.text=element_text(size=7),
  ) +
  ggtitle("Plant Volume by Nitrogen Treatment") +
  xlab("Treatment")+
  ylab("Volume (cm^3)")

#plant height by nitrogen#
plant7 %>%
  ggplot(aes(x=nitrogen, y=tukey_height, fill=plant_species)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text( size=7), 
    legend.text=element_text(size=7),
  ) +
  ggtitle("Plant Height by Nitrogen Treatment") +
  xlab("Treatment")+
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




#t.tests and aov's#
#t.test(plant6$height~plant6$nitrogen) #p==0.004744
#t.test(plant6$plant_volume~plant6$nitrogen) #p==0.0007921
#t.test(plant7$height~plant7$nitrogen) #p==0.1134
#t.test(plant7$plant_volume~plant7$nitrogen)#p==0.08863
#hc6 <- aov(plant6$height~plant6$combination) #p==9.68e-12
#vc6 <- aov(plant6$plant_volume~plant6$combination) #p==2.7e-13
#hc7 <- aov(plant7$height~plant7$combination) #p==4.34e-09
#vc7 <- aov(plant7$plant_volume~plant7$combination) #p==1.63e-05
par(mfrow = c(2,2))
ph_glm1 <-  glm(tukey_height ~ mono_hetero*nitrogen + yard, family = poisson, data = plant7)
ph_glm1 <-  glm(tukey_height ~ combination*nitrogen + yard, family = normal, data = plant7)

pv_glm1 <-  glm(tukey_vol ~ combination*nitrogen + yard, family = poisson, data = plant7)
ph_glm2 <-  glm(tukey_height ~ mono_hetero*nitrogen + yard, family = poisson, data = plant7)
pv_glm2 <-  glm(tukey_vol ~ mono_hetero*nitrogen + yard, family = poisson, data = plant7)


