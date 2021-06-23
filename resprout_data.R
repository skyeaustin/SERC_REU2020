#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#set working directory
setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac

###Resprout Data Code###
file.choose()
respt1 <- read.csv("C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\Data\\Data_For_Analysis\\resprout_combined_SAUS_06032021.csv") #skye's desktop
respt1 <- read.csv("/Users/saus/Documents/r_stuff/SERC/SERC_REU2020/resprout_combined_SAUS_11172020.csv") #skye's mac
ggqqplot(respt1$resprouts)#see if the data is normally distributed
shapiro.test(respt1$resprouts)#test statistical significance

#add treatment#
##nitrogen treatment##
respt2 <- respt1 %>% 
  mutate(treatment = substring(pot_id, 5,5))

#remove NA's#
respt3 <- subset(respt2, resprouts != "NA", select = pot_id:treatment)

##subset by species#
respt_CN <- filter(respt3, plant_species=="Chamaecrista nictitans")
respt_EF <- filter(respt3, plant_species=="Eutrochium fistulosum")
respt_EV <- filter(respt3, plant_species=="Elymus virginicus")

#summary for CN
respt_CN_sum <- respt_CN %>% 
  summarise(mean_respt_CN = mean(resprouts),
            sd_respt_CN = sd(resprouts),
            n_respt_CN = n(),
            SE_respt_CN = sd(resprouts)/sqrt(n())
  )

##summary for EF
respt_EF_sum <- respt_EF %>% 
  summarise(mean_respt_EF = mean(resprouts),
            sd_respt_EF = sd(resprouts),
            n_respt_EF = n(),
            SE_respt_EF = sd(resprouts)/sqrt(n())
  )


#summary for EV
respt_EV_sum <- respt_EV %>% 
  summarise(mean_respt_EV = mean(resprouts),
            sd_respt_EV = sd(resprouts),
            n_respt_EV = n(),
            SE_respt_EV = sd(resprouts)/sqrt(n())
  )

##plot resprouts by spp##
respt3 %>%
  ggplot(aes(x=plant_species, y=resprouts, fill=resprouts)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Resprouts boxplot") +
  xlab("Species")+
  ylab("Number of Resprouts")



#respts by treatment#
respt_X <- filter(respt3, treatment=="X")
respt_N <- filter(respt3, treatment=="N")

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

#plot by treatment
##plot resprouts by spp##
respt3 %>%
  ggplot(aes(x=treatment, y=resprouts, fill=resprouts)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Resprouts boxplot") +
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
