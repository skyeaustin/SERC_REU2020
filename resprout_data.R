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
respt1 <- read.csv("C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\Data\\Data_For_Analysis\\resprout_combined_SAUS_06032021.csv") #skye's desktop
respt1 <- read.csv("/Users/saus/Documents/r_stuff/SERC/csvs/resprout_combined_SAUS_11172020.csv") #skye's mac

##species combination##
respt2 <- respt1 %>% 
  mutate(combination = substring(pot_id, 7, 10))
##hetero v. con specific##
respt3 <-respt2 %>% 
  mutate(mono_hetero = if_else(respt2$combination %in% c("EpEV", "CnEv", "CnEp"), "hetero", "mono"))
##nitrogen treatment##
respt4 <- respt3 %>% 
  mutate(nitrogen = substring(pot_id, 5,5))
#remove NA's#
respt5 <- subset(respt4, resprouts != "NA", select = pot_id:nitrogen)

##stat tests##
#test for normal distribution#
ggqqplot(respt5$resprouts)
#test for significance#
shapiro.test(respt5$resprouts) #p==2.406e-14
#t.test and aov#
t.test(respt5$resprouts~respt5$nitrogen) #p==0.4796
resptaov1 <- aov(respt5$resprouts~respt5$combination) #p==1.63e-10
resptaov2 <- aov(respt5$resprouts~respt5$plant_species) #p==<2e-16

##subset by species#
respt_CN <- filter(respt5, plant_species=="Chamaecrista nictitans")
respt_EF <- filter(respt5, plant_species=="Eutrochium fistulosum")
respt_EV <- filter(respt5, plant_species=="Elymus virginicus")

#summary for CN#
respt_CN_sum <- respt_CN %>% 
  summarise(mean_respt_CN = mean(resprouts),
            sd_respt_CN = sd(resprouts),
            n_respt_CN = n(),
            SE_respt_CN = sd(resprouts)/sqrt(n())
  )
##summary for EF#
respt_EF_sum <- respt_EF %>% 
  summarise(mean_respt_EF = mean(resprouts),
            sd_respt_EF = sd(resprouts),
            n_respt_EF = n(),
            SE_respt_EF = sd(resprouts)/sqrt(n())
  )
#summary for EV#
respt_EV_sum <- respt_EV %>% 
  summarise(mean_respt_EV = mean(resprouts),
            sd_respt_EV = sd(resprouts),
            n_respt_EV = n(),
            SE_respt_EV = sd(resprouts)/sqrt(n())
  )

##number of resprouts by species##
respt5 %>%
  ggplot(aes(x=plant_species, y=resprouts, fill=resprouts)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of Resprouts by Species") +
  xlab("Species")+
  ylab("Number of Resprouts")
#make geom bar?

##respt per combo
respt5 %>%
  ggplot(aes(x=combination, y=resprouts, fill=resprouts)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of Resprouts by Species") +
  xlab("Species")+
  ylab("Number of Resprouts")

#respts by treatment#
respt_X <- filter(respt5, nitrogen=="X")
respt_N <- filter(respt5, nitrogen=="N")

#summary for respt X
respt_X_sum <- respt_X %>% 
  summarise(mean_respt_X = mean(resprouts),
            sd_respt_X = sd(resprouts),
            n_respt_X = n(),
            SE_respt_X = sd(resprouts)/sqrt(n())
  )
#summary for EV
respt_N_sum <- respt_N %>% 
  summarise(mean_respt_N = mean(resprouts),
            sd_respt_N = sd(resprouts),
            n_respt_N = n(),
            SE_respt_N = sd(resprouts)/sqrt(n())
  )

##number of resprouts by treatment##
respt5 %>%
  ggplot(aes(x=nitrogen, y=resprouts, fill=resprouts)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of Resprouts by Treatment") +
  xlab("Treatment")+
  ylab("Number of Resprouts")


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
