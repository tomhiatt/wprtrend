
# CO2 emissions (metric tons per capita)


st <- 1990
en <- 2014

ind.name <- "co2.capita" 
code <- "EN.ATM.CO2E.PC"
ind.lab <- "CO2 emissions (metric tons per capita)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

co2.capita <- temp
rm(temp)


#########

# N.ATM.CO2E.LF.KT

# CO2 emissions from liquid fuel consumption (kt)


co2.capita <- co2.capita %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 

p <- ggplot(co2.capita, aes(x=year, y=value, colour=lab))
p <- p + geom_point(size=0.7, alpha=0.5)
p <- p + geom_line() + facet_wrap( ~ country) #, scales="free"
p <- p + geom_smooth(aes(x=year, y=value, fill=lab),  method = "lm",  alpha=0.20, size=0.4, linetype=0)
p <- p + labs(title= "CO2 emissions in countries in WPR, 1990-2014", y = "metric tons per capita", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=1,byrow=TRUE))
p

pdf(file="CO2 emissions in countries in WPR, 1990-2017.pdf", width=9, height=10) # write PDF
p
dev.off()


 # barplot using 2014 data

co2.capita.14  <- co2.capita %>% filter(year==2014)

p <- ggplot(co2.capita.14, aes(x=reorder(country,-value), y=value, fill=income))
p <- p + geom_bar(stat="identity", alpha=0.7) 
p <- p + labs(title= "CO2 emissions per capita in WPR countries, 2014", x = "", y="metric tons per capita", fill="Income classification") 
p <- p + theme(#legend.title= element_text(size=9),
  plot.title = element_text(size = 10),
  axis.title=element_text(size=7),
  axis.text.x = element_text(size = 7, angle=40, hjust = 1, vjust = 1),
  legend.position = "bottom")
p

pdf(file="CO2 emissions per capita in WPR countries, 2014_bar.pdf", width=8,height=5.5) # write PDF
p
dev.off()


#############

ind.name <- "co2.fuel" 
code <- "EN.ATM.CO2E.GF.ZS"
ind.lab <- "CO2 emissions from gaseous fuel consumption (% of total)"

temp <- WDI(indicator = code, start = st, end = en, extra = TRUE, cache = NULL)
temp <- temp %>% select(-c(capital,longitude, latitude, lending, region))
names(temp) <- c("iso2", "country", "value", "year", "iso3", "income")
temp$variable <- ind.name
temp$lab <- ind.lab
temp <- merge(temp, g_iso3, by="iso3")
temp <- temp %>% filter(who_region=="WPR")

co2.fuel <- temp
rm(temp)


co2.fuel <- co2.fuel %>% filter(!iso3=="ASM" & !iso3=="PYF" & !iso3=="GUM" &  !iso3=="MAC" & !iso3=="NCL" & !iso3=="MNP" & !iso3=="HKG") 

p <- ggplot(co2.fuel, aes(x=year, y=value, colour=lab))
p <- p + geom_point(size=0.7, alpha=0.5)
p <- p + geom_line() + facet_wrap( ~ country, scales="free") #, scales="free"
p <- p + geom_smooth(aes(x=year, y=value, fill=lab),  method = "lm",  alpha=0.20, size=0.4, linetype=0)
p <- p + labs(title= "CO2 emissions from gaseous fuel consumption in countries in WPR, 1990-2014", y = "% of total", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10),
               axis.text.x = element_text(size=7)) + guides(fill=FALSE, color =guide_legend(nrow=1,byrow=TRUE))
p

pdf(file="CO2 emissions from gaseous fuel consumption, 1990-2017.pdf", width=9, height=10) # write PDF
p
dev.off()

# barplot using 2014 data

co2.fuel.14  <- co2.fuel %>% filter(year==2014)

p <- ggplot(co2.fuel.14, aes(x=reorder(country,-value), y=value, fill=income))
p <- p + geom_bar(stat="identity", alpha=0.7) 
p <- p + labs(title= "CO2 emissions from gaseous fuel consumption in WPR countries, 2014", x = "", y="% of total", fill="Income classification") 
p <- p + theme(#legend.title= element_text(size=9),
  plot.title = element_text(size = 10),
  axis.title=element_text(size=7),
  axis.text.x = element_text(size = 7, angle=40, hjust = 1, vjust = 1),
  legend.position = "bottom")
p

pdf(file="CO2 emissions from gaseous fuel consumption in WPR countries, 2014_bar.pdf", width=8,height=5.5) # write PDF
p
dev.off()



