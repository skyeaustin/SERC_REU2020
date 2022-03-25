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
library(ggpubr) # for customizing plots made with ggplot2, QQplots
library(RColorBrewer) # color palettes for graphing
library(rcompanion) # for normality tests and transformations
library(nlme) # for mixed linear and generalized linear models
library(lme4) # for mixed linear and generalized linear models
library(devtools) # simplify r commands
library(moments) # for distribution testing
library(lmerTest) # for testing models 

setwd("C:/Users/hrusk/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis") #amy's laptop

labels <- read.csv("experimental_design.csv") #spreadsheet for pot specific information

labels2 <- labels %>%
  mutate(pot = substring(pot_id, 1, 3)) %>% #creating "pot" as a column to merge with N datasets
  mutate(yard = substring(pot_id, 12, 14)) %>% #creating "yard" column
  mutate(pot_plant = paste(pot, plant_num, sep = "_"))
#######################################################################
leaves <- read.csv("leave_CNH_22March2020.csv")

leaves$ID <- as.character(as.factor(leaves$ID)) #defining as character for mutations

leaves2 <- leaves %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) #removing excess spaces

leaves2$ID <- as.character(as.factor(leaves2$ID))#redefining column as character, because reasons

leaves2$ID <- str_pad(leaves2$ID, 4, side = "left", pad = "0") #making all IDs the same length

leaves2 <- leaves2 %>%
  filter(ID != "blank") %>% #removing rows that were blanks and standards
  filter(ID != "cond") %>% #removing rows that were blanks and standards
  filter(ID != "STD4") %>% #removing rows that were blanks and standards
  filter(ID != "SDT4") %>%
  select(c("ID", "C_Result", "H_Result", "N_Result", "Message")) %>% #selecting a subset of columns
  mutate(material = paste("leaves")) %>%
  mutate(plant_num = substring(ID, 4)) %>% #creating plant_num column
  mutate(pot = substring (ID, 1, 3)) %>% #creating pot column for merging
  mutate(pot_plant = paste(pot, plant_num, sep = "_"))%>%
  arrange(pot_plant)

leaves3 <- merge(leaves2, labels2, by = "pot_plant") #merging CHN data with experimental design dataframe

#more defining columns....
leaves3$plant_species <- as.character(as.factor(leaves3$plant_species))
leaves3$nitrogen <- as.character(as.factor(leaves3$nitrogen))
leaves3$mono_hetero <- as.character(as.factor(leaves3$mono_hetero))

##############################################################
roots <- read.csv("root_CHN_22March2022.csv") #root CHN results
roots$ID <- as.character(as.factor(roots$ID)) #defining as character for mutations

roots2 <- roots %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) #removing excess spaces

roots2$ID <- as.character(as.factor(roots2$ID))#redefining column as character, because reasons

roots2$ID <- str_pad(roots2$ID, 4, side = "left", pad = "0") #making all IDs the same length

roots2 <- roots2 %>%
  filter(ID != "blank") %>% #removing rows that were blanks and standards
  filter(ID != "cond") %>% #removing rows that were blanks and standards
  filter(ID != "STD4") %>% #removing rows that were blanks and standards
  filter(!(ID == "0222" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "0721" & Message != "RRBreruns"))%>% #removing duplicates
  filter(!(ID == "0421" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "0781" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "1152" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "1111" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "1112" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "1122" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "1142" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "0941" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "0942" & Message != "RRBreruns")) %>% #removing duplicatez
  filter(!(ID == "0012" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "0051" & Message != "RRBreruns")) %>% #removing duplicates
  filter(!(ID == "0272" & Message != "RRBreruns")) %>% #removing duplicates
  select(c("ID", "C_Result", "H_Result", "N_Result", "Message")) %>% #selecting a subset of columns
  mutate(material = paste("roots")) %>%
  mutate(plant_num = substring(ID, 4)) %>% #creating plant_num column
  mutate(pot = substring (ID, 1, 3)) %>% 
  mutate(pot_plant = paste(pot, plant_num, sep = "_"))%>% #creating pot column for merging
  arrange(pot_plant)

roots3 <- merge(roots2, labels2, by = "pot_plant") #merging CHN data with experimental design dataframe

#roots4 <- roots3 %>%
#  filter(N_Result < 10)

#more defining columns....
roots3$plant_species <- as.character(as.factor(roots3$plant_species))
roots3$nitrogen <- as.character(as.factor(roots3$nitrogen))
roots3$mono_hetero <- as.character(as.factor(roots3$mono_hetero))
leaves3$plant_species <- as.character(as.factor(roots3$plant_species))
leaves3$nitrogen <- as.character(as.factor(roots3$nitrogen))
leaves3$mono_hetero <- as.character(as.factor(roots3$mono_hetero))

#merge roots and leaves
total <- rbind(roots3, leaves3)

#### data exploration ###

#function of calculating means and standard errors
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

##### root graphs #####
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

#####removing outliers###
#ggplot(data = barGraphStats(data = roots4, variable = "N_Result", byFactorNames = c("nitrogen", "plant_species")), aes(x=plant_species, y=mean, fill=nitrogen)) +
#  geom_bar(stat='identity', position=position_dodge()) +
#  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
#  scale_fill_brewer(palette = "Set1") +
#  ylab("Percent N") + xlab("Plant Species")

#ggplot(data = barGraphStats(data = roots4, variable = "N_Result", byFactorNames = c("nitrogen", "combination")), aes(x=combination, y=mean, fill=nitrogen)) +
#  geom_bar(stat='identity', position=position_dodge()) +
#  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
#  scale_fill_brewer(palette = "Set1") +
#  ylab("Percent N") + xlab("combination")

#ggplot(data = barGraphStats(data = roots4, variable = "N_Result", byFactorNames = c("nitrogen", "mono_hetero", "plant_species")), aes(x=mono_hetero, y=mean, fill=nitrogen)) +
#  geom_bar(stat='identity', position=position_dodge()) +
#  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
#  scale_fill_brewer(palette = "Set1") +
#  ylab("Percent N") + xlab("Combination") +
#  facet_wrap(~plant_species)

###### leaves graphs ######
 
ggplot(data = barGraphStats(data = leaves3, variable = "N_Result", byFactorNames = c("nitrogen", "plant_species")), aes(x=plant_species, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Plant Species")

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


ggplot(roots3, aes(x=below_biomass_g, y=N_Result, fill = nitrogen)) + 
  geom_point()



#####leaves vs. roots####

ggplot(data = barGraphStats(data = total, variable = "N_Result", byFactorNames = c("nitrogen", "plant_species", "material")), aes(x=plant_species, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Plant Species") +
  facet_wrap(~material)

ggplot(data = barGraphStats(data = total, variable = "N_Result", byFactorNames = c("nitrogen", "combination", "material")), aes(x=material, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Combination") +
  facet_wrap(~combination)

ggplot(data = barGraphStats(data = total, variable = "N_Result", byFactorNames = c("nitrogen", "mono_hetero", "material")), aes(x=material, y=mean, fill=nitrogen)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  ylab("Percent N") + xlab("Mono/Hetero") +
  facet_wrap(~mono_hetero)

######### Data analysis #############
## normality tests ## 
ggqqplot(roots3$N_Result)
ggqqplot(leaves3$N_Result)

shapiro.test(roots3$N_Result)
shapiro.test(leaves3$N_Result)

roots3$N_result_tukey = transformTukey(roots3$N_Result)
leaves3$N_Result_tukey= transformTukey(leaves3$N_Result)



# store transformTukey lambda values #
tukey_root_lambda <- transformTukey(roots3$N_result_tukey, returnLambda = TRUE)
tukey_leave_lambda <- transformTukey(leaves3$N_Result_tukey, returnLambda = TRUE)



