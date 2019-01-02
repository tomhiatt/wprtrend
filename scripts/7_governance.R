# setwd("~/Dropbox/Current work space/STB_Mywork_2017/RD")
# setwd("~/Dropbox/_Collaborations/wprtrend")

library(WDI) # extract world bank data
library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(gridExtra)
library(ggrepel)
library(scales)
library(xlsx)
library(treemap)

# iso2 and iso3 global list with WHO regions
g_iso2 <- read.xlsx("iso2_global_whoregion.xlsx", sheetName="clist_iso2")
g_iso2 <- g_iso2[-219,]
g_iso3 <- read.xlsx("iso2_global_whoregion.xlsx", sheetName="clist_iso3")

# upload data
gov <- read.csv("./data/governance/world_governace_indicators.csv")

# data process
gov <- melt(gov, id.vars = c("country", "iso3", "variable"))
names(gov)[4] <- "year"
gov$year <- substring(gov$year, 2,5)

gov <- merge(gov, g_iso3, by="iso3")


# reigonal dataframe
gov_wpr <- gov %>% filter(who_region=="WPR")
gov_afr <- gov %>% filter(who_region=="AFR")
gov_amr <- gov %>% filter(who_region=="AMR")
gov_emr <- gov %>% filter(who_region=="EMR")
gov_eur <- gov %>% filter(who_region=="EUR")
gov_sea <- gov %>% filter(who_region=="SEA")

gov_wpr.ag <- aggregate(value ~ who_region + year + variable, data = gov_wpr, mean, na.rm = TRUE)
gov_afr.ag <- aggregate(value ~ who_region + year + variable, data = gov_afr, mean, na.rm = TRUE)
gov_amr.ag <- aggregate(value ~ who_region + year + variable, data = gov_amr, mean, na.rm = TRUE)
gov_emr.ag <- aggregate(value ~ who_region + year + variable, data = gov_emr, mean, na.rm = TRUE)
gov_eur.ag <- aggregate(value ~ who_region + year + variable, data = gov_eur, mean, na.rm = TRUE)
gov_sea.ag <- aggregate(value ~ who_region + year + variable, data = gov_sea, mean, na.rm = TRUE)

gov_region.ag <- rbind(gov_wpr.ag,
                       gov_afr.ag,
                       gov_amr.ag,
                       gov_emr.ag,
                       gov_eur.ag,
                       gov_sea.ag)


gov_region.ag$variable <- factor(gov_region.ag$variable,labels=c("Control of Corruption", "Government Effectiveness", "Political Stability", "Rule of Law", "Voice and Accountability"))

gov_region.ag$year <- as.integer(gov_region.ag$year)


p <- ggplot(gov_region.ag, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_grid(variable ~ who_region)
p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.25, size=0.4, linetype=0)

p <- p + labs(title= "Governance indicators by WHO region, 1996-2017", y = "Index score", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="Governance_indicators_region.pdf", width=8,height=10) # write PDF
p
dev.off()

# by country
gov_wpr$year <- as.integer(gov_wpr$year)
gov_wpr$variable <- factor(gov_wpr$variable,labels=c("Control of Corruption", "Government Effectiveness", "Political Stability", "Rule of Law", "Voice and Accountability"))

gov_wpr.s <- gov_wpr %>% filter(!country=="Niue")
  
p <- ggplot(gov_wpr.s, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_wrap( ~ country) #, scales="free"
p <- p + labs(title= "Governance indicators in countries in WPR, 1996-2017", y = "Index score", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=2,byrow=TRUE))
p

pdf(file="Governance_indicators_country_WPR.pdf", width=10,height=13) # write PDF
p
dev.off()







