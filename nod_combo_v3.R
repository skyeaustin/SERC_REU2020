#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(moments)
library(rcompanion) # for normality tests and transformations
library(nlme) # for mixed linear and generalized linear models
library(lme4) # for mixed linear and generalized linear models
library(devtools) # simplify r commands
library(lmerTest)

library(tidyverse) # for data organization, manipulation, & visualization; includes ggplot2 and dplyr # 
library(ggpubr) # for customizing plots made with ggplot2
library(RColorBrewer) # color palettes for graphing
library(rcompanion) # for normality tests and transformations
library(nlme) # for mixed linear and generalized linear models
library(lme4) # for mixed linear and generalized linear models
library(devtools) # simplify r commands

#set working directory
setwd("C:/Users/Airsi/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #Skye's desktop
setwd("~/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/R_CODE") #skye's mac
setwd("C:/Users/hrusk/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis")


#make base csv an object
nodv3 <- read.csv( "C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\Data\\Data_For_Analysis\\nodule_combined_AMH.csv") #skye's desktop
nodv3 <- read.csv("/Users/saus/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis/nodule_combined_AMH.csv") #skye's mac
nodv3 <- read.csv("nodule_combined_AMH_012221.csv")

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

test = lm(plant7$nodules~nitrogen + yard, data=plant7)


nodmod1 <-  glm(nodules ~ mono_hetero*nitrogen + yard, family = poisson, data = nod4)
confint(nodmod1)
nodmod2 <-  glm(nodules ~ mono_hetero*nitrogen, family = poisson, data = nod4)


##transform data##
ggqqplot(nod4$nodules)
plotNormalHistogram(nod4$nodules)
skewness(nod4$nodules, na.rm = TRUE)

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
  ggplot(aes(x=nitrogen, y=nodules, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#149F86"))+
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
  ggplot(aes(x=combination, y=nodules, fill=combination)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500", "#0F83C6", "#149F86"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of Nodules by Combination") +
  xlab("Combination")+
  ylab("Number of Nodules")
#CnEf combo looks interesting

#nods by mon/het
nod4 %>%
  ggplot(aes(x=mono_hetero, y=nodules, fill=mono_hetero)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#461763", "#C0F500"))+
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) + 
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Number of Nodules by Specificity") +
  xlab("Specificity")+
  ylab("Number of Nodules")


##density of nodules by treatment?


lm_nod <- lm(nod5c$tukey_nod~nod5c$date)
plot(lm_nod)
hist(residuals(lm_nod))
residuals(lm_nod)
plot(nod5c$tukey_nod, pch = 16, col = "black")


nod_lm1 <- lm(nodules ~ combination, data=nod4) 
nod_resid<- resid(nod_lm1)
#We now plot the residual against the observed values of the variable waiting.
plot(nod4$combination, nod_resid, 
     ylab="Residuals", xlab="Waiting Time", 
     main="Old Faithful Eruptions") 
abline(0, 0)                  # the horizon




