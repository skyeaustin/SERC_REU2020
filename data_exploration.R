#########################################
####### Data & R Exploration ############
#########################################

library(tidyverse)
library(RColorBrewer)
library(ggpubr)

## bar graph summary statistics function from Kim ##
barGraphStats(data=, variable="", byFactorNames=c(""))
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
  
g1 <- ggplot(data = barGraphStats(data = damage2, variable = "Percent_damage", byFactorNames = c("Area_type")), aes(x=Area_type, y=mean, fill=Area_type)) +
    geom_bar(stat='identity', position=position_dodge(), width=0.8) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
    annotate("text", x = 1, y = 2.7 , label = "a", size = 8)+
    annotate("text", x = 2, y = 2.7 , label = "a", size = 8)+
    annotate("text", x = 3, y = 1.75, label = "b", size = 8) +
    scale_fill_manual(values = EBpalette1) + theme(legend.position = "none") + xlab("Area Type") + ylab("Percent Damage")
  
g1 + theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 24),axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), legend.position = "none")