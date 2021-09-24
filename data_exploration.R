#########################################
####### Data & R Exploration ############
#########################################

library(tidyverse)
library(RColorBrewer)
library(ggpubr)

setwd("~/Documents/Data") #working directory

biom1 <- read.csv("plant_biomass_all.csv") ### amy's computers ###
biom1 <- read.csv("/Users/saus/Dropbox (Smithsonian)/SERC_REU_2020/Experiment_Data_and_R_Code/Data/Data_For_Analysis/plant_biomass_all.csv") #skye's mac

# remove weird columns ##
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




## bar graph summary statistics function from Kim ##
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
  
g1 <- ggplot(data = barGraphStats(data = biom6, variable = "total_biomass", byFactorNames = c("nitrogen")), aes(x=nitrogen, y=mean, fill=nitrogen)) +
    geom_bar(stat='identity', position=position_dodge(), width=0.8) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
    annotate("text", x = 1, y = 2.7 , label = "a", size = 8)+
    annotate("text", x = 2, y = 2.7 , label = "a", size = 8)+
    annotate("text", x = 3, y = 1.75, label = "b", size = 8) +
    scale_fill_brewer(palette = "Set1") + theme(legend.position = "none") + xlab("Nitrogen") + ylab("Total Biomass")
  
g1 + theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 24),axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), legend.position = "none")

g2 <- ggplot(data = barGraphStats(data = biom6, variable = "total_biomass", byFactorNames = c("nitrogen", "combination")), aes(x=nitrogen, y=mean, fill=combination)) +
  geom_bar(stat='identity', position=position_dodge(), width=0.8) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") + xlab("Nitrogen") + ylab("Total Biomass")

g2 + theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 24),axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), legend.position = "none")



#manova: get all the sheets together and test w/all y vars
