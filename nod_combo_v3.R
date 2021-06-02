#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
#set working directory
setwd("~/SERC/ExperimentData_and_Code/SERC_REU2020") #Skye's desktop
file.choose()
#make base csv an object
nodv3 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SERC\\ExperimentData_and_Code\\SERC_REU2020\\nodule_combined_AMH.csv")
file.choose()
is.numeric(nodv3$nodules) #ask if the nodule column is numeric
is.factor(nodv3$nodules) #ask if the nodule column is a factor
is.character(nodv3$nodules) #ask if the nodule column is a character
nodv3$nodules <- as.numeric(as.character(nodv3$nodules)) #make the nodule column numeric
is.numeric(nodv3$nodules)

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

##test for normal distribution and statistical significance##
ggqqplot(nod4$nodules)#see if the data is normally distributed
shapiro.test(nod4$nodules)#test for normality and statistical significance

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

nod4 %>%
  ggplot(aes(x=nitrogen, y=nodules, fill=nodules)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Nodules Boxplot") +
  xlab("Treatment Type")+
  ylab("Number of Nodules")


