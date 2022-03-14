#############################################
#############################################
#############################################
############ Summer 2020 Nitrogen ###########
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
library(ggplot2)
library(dplyr)
library(moments)
library(lmerTest)

setwd("C:/Users/hrusk/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis") #amy's laptop

labels <- read.csv("experimental_design.csv") #spreadsheet for pot specific information

labels2 <- labels %>%
  mutate(pot = substring(pot_id, 1, 3)) %>% #creating "pot" as a column to merge with N datasets
  mutate(yard = substring(pot_id, 12, 14)) #creating "yard" column
#######################################################################
leaves <- read.csv("RRBplate14and18leaves.csv")

leaves$ID <- as.character(as.factor(leaves$ID)) #defining as character for mutations

leaves2 <- leaves %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) #removing excess spaces

leaves2$ID <- as.character(as.factor(leaves2$ID))#redefining column as character, because reasons

leaves2$ID <- str_pad(leaves2$ID, 4, side = "left", pad = "0") #making all IDs the same length

leaves2 <- leaves2 %>%
  filter(ID != "blank") %>% #removing rows that were blanks and standards
  filter(ID != "cond") %>% #removing rows that were blanks and standards
  filter(ID != "STD4") %>% #removing rows that were blanks and standards
  select(c("ID", "C_Result", "H_Result", "N_Result", "Message")) %>% #selecting a subset of columns
  mutate(plant_num = substring(ID, 4)) %>% #creating plant_num column
  mutate(pot = substring (ID, 1, 3)) #creating pot column for merging

leaves3 <- merge(leaves2, labels2, by = "pot") #merging CHN data with experimental design dataframe

#more defining columns....
leaves3$plant_species <- as.character(as.factor(leaves3$plant_species))
leaves3$nitrogen <- as.character(as.factor(leaves3$nitrogen))
leaves3$mono_hetero <- as.character(as.factor(leaves3$mono_hetero))

##############################################################
roots <- read.csv("root_CHN_14March2022.csv") #root CHN results
roots$ID <- as.character(as.factor(roots$ID)) #defining as character for mutations

roots2 <- roots %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) #removing excess spaces

roots2$ID <- as.character(as.factor(roots2$ID))#redefining column as character, because reasons

roots2$ID <- str_pad(roots2$ID, 4, side = "left", pad = "0") #making all IDs the same length

roots2 <- roots2 %>%
  filter(ID != "blank") %>% #removing rows that were blanks and standards
  filter(ID != "cond") %>% #removing rows that were blanks and standards
  filter(ID != "STD4") %>% #removing rows that were blanks and standards
  select(c("ID", "C_Result", "H_Result", "N_Result", "Message")) %>% #selecting a subset of columns
  mutate(plant_num = substring(ID, 4)) %>% #creating plant_num column
  mutate(pot = substring (ID, 1, 3)) #creating pot column for merging

roots3 <- merge(roots2, labels2, by = "pot") #merging CHN data with experimental design dataframe

#more defining columns....
roots3$plant_species <- as.character(as.factor(roots3$plant_species))
roots3$nitrogen <- as.character(as.factor(roots3$nitrogen))
roots3$mono_hetero <- as.character(as.factor(roots3$mono_hetero))

#### data exploration ###

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


ggplot(data = barGraphStats(data = roots3, variable = "N_Result", byFactorNames = c("nitrogen", "plant_species")), aes(x=plant_species, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Plant Species")
  
ggplot(data = barGraphStats(data = roots3, variable = "N_Result", byFactorNames = c("nitrogen", "combination")), aes(x=combination, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("combination")

ggplot(data = barGraphStats(data = roots3, variable = "N_Result", byFactorNames = c("nitrogen", "mono_hetero", "plant_species")), aes(x=mono_hetero, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Combination") +
  facet_wrap(~plant_species)
 
ggplot(data = barGraphStats(data = leaves3, variable = "N_Result", byFactorNames = c("nitrogen", "combination")), aes(x=combination, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("combination")

ggplot(data = barGraphStats(data = leaves3, variable = "N_Result", byFactorNames = c("nitrogen", "mono_hetero", "plant_species")), aes(x=mono_hetero, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Combination") +
  facet_wrap(~plant_species)

ggplot(data = barGraphStats(data = leaves3, variable = "N_Result", byFactorNames = c("nitrogen", "plant_species")), aes(x=plant_species, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Plant Species")
