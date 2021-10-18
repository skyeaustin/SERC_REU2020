#############################################
#############################################
#############################################
############ Summer 2020 Biomass ############
######## Data Exploration & Analysis ########
#############################################
#############################################
#############################################


#load packages
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
setwd("C:/Users/hrusk/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis") #amy's laptop
setwd("C:/Users/hruskaa/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis") #amy's SERC desktop
setwd("~/Documents/Data")#Rachael's working directory

#Skye Choose file#
biom1 <- read.csv("C:\\Users\\Airsi\\Dropbox (Smithsonian)\\SERC_REU_2020\\Experiment_Data_and_R_Code\\Data\\Data_For_Analysis\\plant_biomass_all.csv") #skye's desktop
biom1 <- read.csv("/Users/saus/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis/plant_biomass_all.csv") #skye's mac

#Amy choose file#
biom1 <- read.csv("plant_biomass_all.csv") ### amy's computers ###


#################################
####### Data manipulation #######
#################################

## remove weird columns ##
biom1.5 <- select(biom1, -starts_with("X"))

## species combination ##
biom2<- biom1.5 %>% 
  mutate(combination = substring(pot_id, 7, 10))

## hetero v. con specific ##
biom3 <- biom2%>% 
  mutate(mono_hetero = if_else(biom2$combination %in% c("EpEV", "CnEv", "CnEp"), "hetero", "mono"))
 
## nitrogen treatment ##

biom4 <- biom3 %>% 
  mutate(nitrogen = substring(pot_id, 5,5))

## make total biomass column ##
biom5 <- biom4 %>% 
  mutate(total_biomass = above_biomass_g+below_biomass_g)


## as if something is numeric, character, factor, integer ## 
is.numeric(biom5$above_biomass_g)

## define columns as numeric ##
biom5$above_biomass_g <- as.numeric(as.character(biom5$above_biomass_g))
biom5$below_biomass_g <- as.numeric(as.character(biom5$below_biomass_g))
biom5$total_biomass <- as.numeric(as.character(biom5$total_biomass))

## remove dead plants ##
biom6 <- na.omit(biom5)

## create yard column ##
biom6$yard = substring(biom6$pot_id, 12,14)

## normality tests ## 
ggqqplot(biom6$above_biomass_g)
ggqqplot(biom6$below_biomass_g)
ggqqplot(biom6$total_biomass)

shapiro.test(biom6$above_biomass_g)
shapiro.test(biom6$below_biomass_g)
shapiro.test(biom6$total_biomass)

#transform, since non-normal
biom6$tukey_ab = transformTukey(biom6$above_biomass_g)
biom6$tukey_bb = transformTukey(biom6$below_biomass_g)
biom6$tukey_tb = transformTukey(biom6$total_biomass)


# store transformTukey lambda values #
tukey_ab_lambda <- transformTukey(biom6$tukey_ab, returnLambda = TRUE)
tukey_bb_lambda <- transformTukey(biom6$tukey_bb, returnLambda = TRUE)
tukey_tb_lambda <- transformTukey(biom6$total_biomass, returnLambda = TRUE)


# normality after transformTukey #
ggqqplot(biom6$tukey_ab)
ggqqplot(biom6$tukey_bb)
ggqqplot(biom6$tukey_tb)

shapiro.test(biom6$tukey_ab) #p==< 2.2e-16==raw, tukey==0.5536
shapiro.test(biom6$tukey_bb) #p==< 2.2e-16==raw, tukey==0.0011
shapiro.test(biom6$tukey_tb) #p==< 2.2e-16==raw, tukey==0.168

# Skye t.tests and aov's#
t.test(biom6$tukey_tb~biom6$nitrogen) #p==0.4472==raw, tukey==0.5909
t.test(biom6$tukey_ab~biom6$nitrogen)#p==0.9636==raw, tukey==0.9036
t.test(biom6$tukey_bb~biom6$nitrogen)#p==0.1601==raw, tukey==0.5897
t.test(biom6$above_biomass_g~biom6$mono_hetero)#p==0.626==raw, tukey==0.6666
t.test(biom6$below_biomass_g~biom6$mono_hetero)#p==0.09197==raw, tukey==0.1055
t.test(biom6$total_biomass~biom6$mono_hetero)#p==0.5674==raw, tukey==0.7648
ac <- aov(biom6$tukey_ab~biom6$combination) #p==1.49e-05==raw, tukey==4.51e-07
bc <- aov(biom6$tukey_bb~biom6$combination) #p==7.93e-05==raw, tukey==2.38e-06
tc <- aov(biom6$tukey_tb~biom6$combination) #p==0.00234==raw, tukey==0.0103
as <- aov(biom6$tukey_ab~biom6$plant_species) #p==6.31e-12, tukey==4.96e-11
bs <- aov(biom6$tukey_bb~biom6$plant_species) #p==3.85e-11, tukey==6.64e-14
ts <- aov(biom6$tukey_tb~biom6$plant_species) #p==3.5e-08, tukey==4.25e-06
#ANOVA's
biomaov <- aov(biom6$total_biomass ~ biom6$nitrogen*biom6$plant_species*biom6$combination*biom6$mono_hetero*biom6$yard)
    #spp, yard, n*spp, spp*yard, trending or significant
tukeybiomaov <- biomaov <- aov(biom6$tukey_tb ~ biom6$nitrogen*biom6$plant_species*biom6$combination*biom6$mono_hetero*biom6$yard)
    #spp, yard, n*spp, spp*yard, trending or significant

#take out yard?
biomaov1 <- aov(biom6$total_biomass ~ biom6$nitrogen*biom6$plant_species*biom6$combination*biom6$mono_hetero)
    #spp, n*spp, spp*combo trending or significant
tukeybiomaov1 <- biomaov <- aov(biom6$tukey_tb ~ biom6$nitrogen*biom6$plant_species*biom6$combination*biom6$mono_hetero)
    #spp significant

#take out species
biomaov2 <- aov(biom6$total_biomass ~ biom6$nitrogen*biom6$combination*biom6$mono_hetero)
    #combo significant
tukeybiomaov2 <- biomaov <- aov(biom6$tukey_tb ~ biom6$nitrogen*biom6$combination*biom6$mono_hetero)
    #combo significant, n*combo trending

#take out combo
biomaov3 <- aov(biom6$total_biomass ~ biom6$nitrogen*biom6$mono_hetero)
    #nothing
tukeybiomaov3 <- biomaov <- aov(biom6$tukey_tb ~ biom6$nitrogen*biom6$mono_hetero)
    #nothing


#

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
biomLM_spp <- lm(biom6$total_biomass ~ biom6$plant_species)
summary(biomLM_spp)
par(mfrow = c(2,2))
plot(biomLM_spp)
plot(residuals(biomLM_spp))

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
# amys_branch

###################################
###### Amy's models/analyses ######
###################################

library(nlme) #for mixed linear and generalized linear models
library(lme4) #for mixed linear and generalized linear models
library(devtools) #simplify r commands

#######################################################
#### Total biomass ANOVA with transform Tukey data ####
#######################################################

######## transformed total biomass no random or nested effects ##########
#model1 <- lm(tukey_tb ~ yard*nitrogen*mono_hetero*plant_species, data = biom6)
summary(model1)
anova(model1)

######## transformed total biomass with pot as random effect #########
#model2 <- lmer(tukey_tb ~ yard*nitrogen*mono_hetero*plant_species +(1|pot_id), data = biom6)
summary(model2)
anova(model2)
qqmath(ranef(model2)) #looking at pot_id as random effect

######## transformed total biomass with yard as random effect #########
model3 <- lmer(tukey_tb ~ nitrogen*plant_species*mono_hetero + (1|yard), data = biom6)
summary(model3)
anova(model3)
qqmat(ranef(model3))

######## transformed total biomass with pot_id and yard as random effect #########
#model4 <- lmer(tukey_tb ~ nitrogen*plant_species*mono_hetero + (1|yard) + (1|pot_id), data = biom6)
summary(model4)
anova(model4)
qqmath(ranef(model4))


######## transformed aboveground biomass no random or nested effects ##########
model5 <- lm(tukey_ab ~ yard*nitrogen*mono_hetero*plant_species, data = biom6)
summary(model5)
anova(model5)

######## transformed aboveground biomass pot and yard as random effect #########
model6 <- lmer(tukey_ab ~ nitrogen*mono_hetero*plant_species +  (1|yard), data = biom6)
summary(model6)
anova(model6)
anova.lme(model6)

######## transformed belowground biomass no random or nested effects ##########
model7 <- lm(tukey_bb ~ yard*nitrogen*mono_hetero*plant_species, data = biom6)
summary(model7)
anova(model7)

######## transformed belowground biomass pot as random effect #########
model8 <- lmer(tukey_bb ~ nitrogen*mono_hetero*plant_species + (1|yard), data = biom6)
summary(model8)
anova(model8)



########### Biomass Bar Graphs Based on Analyses ###########

##bar graph summary statistics function from Kim 
#barGraphStats(data=, variable="", byFactorNames=c(""))
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  
 

### figure - belowground biomass vs. species by nitrogen ###

##yard_tb <- 
ggplot(data = barGraphStats(data = biom6, variable = "tukey_tb", byFactorNames = c("yard", "plant_species")), aes(x=plant_species, y=mean, fill=yard)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Tukey Transformed Total Biomass") + xlab("Species")

ggplot(data = barGraphStats(data = biom6, variable = "total_biomass", byFactorNames = c("nitrogen", "plant_species")), aes(x=plant_species, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Total Biomass") + xlab("Species")

ggplot(data = barGraphStats(data = biom6, variable = "total_biomass", byFactorNames = c("nitrogen", "plant_species")), aes(x=plant_species, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Total Biomass") + xlab("Nitrogen")

ggplot(data = barGraphStats(data = biom6, variable = "tukey_bb", byFactorNames = c("nitrogen", "plant_species")), aes(x=plant_species, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Tukey Transformed Belowground Biomass") + xlab("Nitrogen")

ggplot(data = barGraphStats(data = biom6, variable = "tukey_tb", byFactorNames = c("nitrogen", "mono_hetero", "plant_species")), aes(x=mono_he, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Tukey Transformed Total Biomass") + xlab("Combination")+
  facet_wrap(~plant_species)

ggplot(data = barGraphStats(data = biom6, variable = "tukey_bb", byFactorNames = c("nitrogen", "mono_hetero", "plant_species")), aes(x=mono_hetero, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Tukey Transformed Below Biomass") + xlab("combination")+
  facet_wrap(~plant_species)

