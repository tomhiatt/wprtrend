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

# fix value for WB data 
st <- 1990
en <- 2017

# upload data
df <- read.csv("./data/SDG_UHC/SDG_UHC_data_wpr.csv")
var <- names(df)

# process data
names(df) <- c("ino", "id", "order.no", "country", "iso3", "area", "region.code", "region", "core.member", "monitor.domain", "ind.domain", "health.topic", "ind.type", "sdg.no", "sdg.t", "sdg.t.value", "disag.type", "disag", "ind", "value", "Cimin", "Cimax", "survey.st", "sruvey.end", "year", "source", "whs.year", "rescaled.value", "footnote.no", "note", "footnote", "link", "data.update", "date.access")
var <- names(df)

df <- df %>% select(year, country, area, iso3, ind, value)
ind.list <- unique(df$ind)

df$variable <- NULL
df$variable[df$ind=="Maternal mortality ratio (per 100 000 live births)"] <- "mmr"
# df[df$ind=="Motorcycle helmet wearing rate (%)",] 

### stop here to look at SDG data


############################################
# Immunization coverage 
############################################

# DPT
ind.name <- "dpt" 
code <- "SH.IMM.IDPT"
ind.lab <- "DPT (% of children ages 12-23 months)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

dpt <- temp
rm(temp)

# Hep B
ind.name <- "hep.b" 
code <- "SH.IMM.HEPB"
ind.lab <- "HepB3 (% of one-year-old children)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

hep.b <- temp
rm(temp)

# Measles
ind.name <- "measles" 
code <- "SH.IMM.MEAS"
ind.lab <- "Measles (% of children ages 12-23 months)"
  
temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

measles <- temp
rm(temp)

# combine datasets
immu <- rbind(dpt, hep.b, measles)

# drop countreis without data
immu.s <- immu %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 

# Immunization coverage by country in WPR

p <- ggplot(immu.s, aes(x=year, y=value, colour=lab))
p <- p + geom_line() + facet_wrap( ~ country) #, scales="free"

p <- p + labs(title= "Immunization coverage in countries in WPR, 1990-2017", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=2,byrow=TRUE))
p

pdf(file="Immunization coverage_country_WPR.pdf", width=9,height=11) # write PDF
p
dev.off()

############################################
# MCH coverage indicators
############################################
st <- 1990
en <- 2017

# Skilled birth attendant
ind.name <- "sba" 
code <- "SH.STA.BRTC.ZS"
ind.lab <- "Births attended by skilled health staff (% of total)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

sba <- temp
rm(temp)

# Contraceptive
ind.name <- "con" 
code <- "SP.DYN.CONU.ZS"
ind.lab <- "Contraceptive prevalence, any methods (% of women ages 15-49)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

con <- temp
rm(temp)

# Prenatal care
ind.name <- "prenatal" 
code <- "SH.STA.ANVC.ZS"
ind.lab <- "Pregnant women receiving prenatal care (%)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

prenatal <- temp
rm(temp)



# combine datasets
mca <- rbind(sba, con, prenatal)

# drop countreis without data
mca <- mca %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 


p <- ggplot(mca, aes(x=year, y=value, colour=lab))
p <- p + geom_point(size=0.7, alpha=0.5)
p <- p + ylim(0,100)
p <- p + geom_line() + facet_wrap( ~ country) #, scales="free"
p <- p + geom_smooth(aes(x=year, y=value, fill=lab),  method = "lm",  alpha=0.25, size=0.4, linetype=0)
p <- p + labs(title= "MCA indicators in countries in WPR, 1990-2017", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=2,byrow=TRUE))
p

pdf(file="MCA_indicators_country_WPR.pdf", width=9, height=10) # write PDF
p
dev.off()











############################################
# Health Financing 
############################################
st <- 2000
en <- 2015

# Total Health Expenditure
ind.name <- "h.spend" 
code <- "SH.XPD.CHEX.GD.ZS"
ind.lab <- "Current health expenditure, total (% of GDP)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

h.spend <- temp
rm(temp)

# Total Health Expenditure
ind.name <- "g.h.spend" 
code <- "SH.XPD.GHED.GE.ZS"
ind.lab <- "Government expenditure on health (% of total government expenditure)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

g.h.spend <- temp
rm(temp)

# combine datasets
spend <- rbind(h.spend, g.h.spend)

# drop countries without data
spend <- spend %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 

p <- ggplot(spend, aes(x=year, y=value, colour=lab))
p <- p + geom_point(size=0.7, alpha=0.5)
p <- p + geom_line() + facet_wrap( ~ country) #, scales="free"
p <- p + ylim(0, 25)
p <- p + geom_smooth(aes(x=year, y=value, fill=lab),  method = "lm",  alpha=0.20, size=0.4, linetype=0)
p <- p + labs(title= "Health expenditure in countries in WPR, 2000-2015", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=1,byrow=TRUE))
p

pdf(file="Health expenditure_country_WPR.pdf", width=9,height=11) # write PDF
p
dev.off()


########

# Health Expenditure per capita
st <- 2000
en <- 2015

ind.name <- "h.spend.capita" 
code <- "SH.XPD.CHEX.PC.CD"
ind.lab <- "Current health expenditure per capita (current US$)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

h.spend.capita <- temp
rm(temp)

# OOP per capita
ind.name <- "oop.capita" 
code <- "SH.XPD.OOPC.PC.CD"
ind.lab <- "Out-of-pocket expenditure per capita (current US$)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

oop.capita <- temp
rm(temp)

# combine datasets
spend.capita <- rbind(h.spend.capita, oop.capita)

# drop countries without data
spend.capita <- spend.capita %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 

p <- ggplot(spend.capita, aes(x=year, y=value, colour=lab))
# p <- p + geom_point(size=0.7, alpha=0.5)
p <- p + geom_line() + facet_wrap( ~ country, scales="free") #, scales="free"
p <- p + geom_smooth(aes(x=year, y=value, fill=lab),  method = "lm",  alpha=0.20, size=0.4, linetype=0)
p <- p + labs(title= "Health expenditure per capita in countries in WPR, 2000-2015", y = "Current US$", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=1,byrow=TRUE))
p

pdf(file="Health expenditure_capita_country_WPR.pdf", width=9, height=10) # write PDF
p
dev.off()




