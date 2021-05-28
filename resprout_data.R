#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(glmm)
#set working directory
setwd("~/SERC/ExperimentData_and_Code/SERC_REU2020") #Skye's desktop
file.choose()
respt1 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SERC\\ExperimentData_and_Code\\SERC_REU2020\\resprout_combined_SAUS_11172020.csv")
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
