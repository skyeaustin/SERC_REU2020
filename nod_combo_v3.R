#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#set working directory
setwd(setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE")) #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac


#make base csv an object
nodv3 <- read.csv( "C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\Data\\Data_For_Analysis\\nodule_combined_AMH.csv") #skye's desktop
nodv3 <- read.csv("/Users/saus/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis/nodule_combined_AMH.csv") #skye's mac

##make the nodule column numeric##
nodv3$nodules <- as.numeric(as.character(nodv3$nodules)) 
##species combination##
nod1 <- nodv3 %>% 
  mutate(combination = substring(pot_id, 7, 10))
##hetero v. con specific##
nod2 <- nod1 %>% 
  mutate(mono_hetero = if_else(nod1$combination %in% c("EpEV", "CnEv", "CnEp"), "hetero", "mono"))
##nitrogen treatment##
nod3 <- nod2 %>% 
  mutate(nitrogen = substring(pot_id, 5,5))
##remove dead plants##
nod4 <- subset(nod3, nodules != "NA", select = yard:notes)

##stat tests##
#test for normal distribution#
ggqqplot(nod4$nodules)
#test for statistical significance#
shapiro.test(nod4$nodules)
    #significant, but probably missing something (like another variable?)
#t.tests/aov's#
t.test(nod4$nodules~nod4$nitrogen) #p==0.004136
nodxcombo <- aov(nod4$nodules~nod4$combination) #p==0.264

    
##nodule summary for "X" treatment##
nod1_X <- filter(nod4, nitrogen=="X")
nod2_X <- nod1_X %>% 
  summarise(mean_nodX = mean(nodules),
            sd_nodX = sd(nodules),
            n_nodX = n(),
            SE_nodX = sd(nodules)/sqrt(n())
            )
print(nod2_X)
##nodule summary for "N" treatment##
nod1_N <- filter(nod4, nitrogen=="N")
nod2_N <- nod1_N %>% 
  summarise(mean_nodN = mean(nodules),
            sd_nodN = sd(nodules),
            n_nodN = n(),
            SE_nodN = sd(nodules)/sqrt(n())
  )
print(nod2_N)
#c: fewer avg nods, as i thought. fewer nods, but bigger#

##number of nodules by treatment##
nod4 %>%
  ggplot(aes(x=nitrogen, y=nodules, fill=nodules)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of Nodules by Treatment") +
  xlab("Treatment")+
  ylab("Number of Nodules")

##number of nodules by combination##
nod4 %>%
  ggplot(aes(x=combination, y=nodules, fill=nodules)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of Nodules by Combination") +
  xlab("Combination")+
  ylab("Number of Nodules")
#CnEf combo looks interesting



