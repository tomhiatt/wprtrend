

# HIV prevaelnce
st <- 2000
en <- 2017

ind.name <- "hiv" 
code <- "SH.DYN.AIDS.ZS"
ind.lab <- "Prevalence of HIV, total (% of population ages 15-49)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

hiv <- temp
rm(temp)


# combine datasets
#spend.capita <- rbind(h.spend.capita, oop.capita)

# drop countries without data
hiv <- hiv %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 

p <- ggplot(hiv, aes(x=year, y=value, colour=lab))
p <- p + geom_point(size=0.7, alpha=0.5)
p <- p + geom_line() + facet_wrap( ~ country) #, scales="free"
p <- p + geom_smooth(aes(x=year, y=value, fill=lab),  method = "lm",  alpha=0.20, size=0.4, linetype=0)
p <- p + labs(title= "HIV prevalence in countries in WPR, 2000-2017", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=1,byrow=TRUE))
p



# Malaria incidence
st <- 2000
en <- 2015

ind.name <- "mal" 
code <- "SH.MLR.INCD.P3"
ind.lab <- "Incidence of malaria (per 1,000 population at risk)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

mal <- temp
rm(temp)

mal <- mal %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 

p <- ggplot(mal, aes(x=year, y=value, colour=lab))
p <- p + geom_point(size=0.7, alpha=0.5)
p <- p + geom_line() + facet_wrap( ~ country) #, scales="free"
p <- p + geom_smooth(aes(x=year, y=value, fill=lab),  method = "lm",  alpha=0.20, size=0.4, linetype=0)
p <- p + labs(title= "Malaria incidence in countries in WPR, 2000-2017", y = "Cases per 1,000 pop", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=1,byrow=TRUE))
p









