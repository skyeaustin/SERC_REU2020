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
  mutate(pot = substring(pot_id, 1, 3)) #creating "pot" as a column to merge with N datasets


roots <- read.csv("roots_CHN_14March2022.csv") #root CHN results
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

roots3 <- merge(roots4, labels2, by = "pot") #merging CHN data with experimental design dataframe


