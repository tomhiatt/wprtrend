setwd("~/Dropbox/Current work space/STB_Mywork_2017/RD")

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


clist <- read.csv("country_iso.csv")
wpr_iso2 <- clist$iso2
wpr_iso3 <- clist$iso3

# iso2 and iso3 global list with WHO regions
g_iso2 <- read.xlsx("iso2_global_whoregion.xlsx", sheetName="clist_iso2")
g_iso2 <- g_iso2[-219,]
g_iso3 <- read.xlsx("iso2_global_whoregion.xlsx", sheetName="clist_iso3")

# EMR AMR AFR EUR WPR SEA
afr_iso2 <- g_iso2 %>% filter(who_region=="AFR")
afr_iso2 <- afr_iso2[,1]
amr_iso2 <- g_iso2 %>% filter(who_region=="AMR")
amr_iso2 <- amr_iso2[,1]
emr_iso2 <- g_iso2 %>% filter(who_region=="EMR")
emr_iso2 <- emr_iso2[,1]
eur_iso2 <- g_iso2 %>% filter(who_region=="EUR")
eur_iso2 <- eur_iso2[,1]
sea_iso2 <- g_iso2 %>% filter(who_region=="SEA")
sea_iso2 <- sea_iso2[,1]

#c_name <- "MN" # specify country 
st <- 2000
en <- 2017
mycols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF") 

# obtain ggplot default colours for manual use
require(latticeExtra)
mycols <- dput(ggplot2like(n = 10, h.start = 0, l = 65)$superpose.line $col)
mycols # "#F8766D" "#7CAE00" "#00BFC4" "#C77CFF" 


# other indicators 
# GNI
# Human health development index
# Gini 
# SDG index 

# Life expectancy
life <- WDI(country = wpr_iso2, indicator = "SP.DYN.LE00.IN", start = st, end = en, extra = TRUE, cache = NULL)
life <- life %>% select(-c(capital,longitude, latitude,lending , region))
names(life)[3] <- "life"
head(life)


# economic development
# GDP per capita (constant 2010 USD)
gdp <- WDI(country = wpr_iso2, indicator = "NY.GDP.PCAP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[2] <- "gdp"
head(gdp)

# GDP growth # No data for CK, NU, TK, WF
gdp.g <- WDI(country = wpr_iso2, indicator = "NY.GDP.MKTP.KD.ZG", start = st, end = en, extra = TRUE, cache = NULL)
gdp.g <- gdp.g %>% select(-c(capital,longitude, latitude,lending , region))
gdp.g <- gdp.g %>% select(-c(country, iso3c, income))
names(gdp.g)[2] <- "gdp.g"
head(gdp.g)


# Poverty rate - Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)
pov <- WDI(country = wpr_iso2, indicator = "SI.POV.DDAY", start = st, end = en, extra = TRUE, cache = NULL)
pov <- pov %>% select(-c(capital,longitude, latitude,lending , region))
pov <- pov %>% select(-c(country, iso3c, income))
names(pov)[2] <- "poverty.rate"
head(pov)

# Poverty rate2 - Poverty headcount ratio at $3.10 a day (2011 PPP) (% of population)
# pov2 <- WDI(country = wpr_iso2, indicator = "SI.POV.2DAY", start = st, end = en, extra = TRUE, cache = NULL)
# pov2 <- pov2 %>% select(-c(capital,longitude, latitude,lending , region))
# pov2 <- pov2 %>% select(-c(country, iso3c, income))
# names(pov2)[2] <- "poverty.rate2"
# head(pov2)

# Population - Population ages 65 and above (% of total)
pop65 <- WDI(country = wpr_iso2, indicator = "SP.POP.65UP.TO.ZS", start = st, end = en, extra = TRUE, cache = NULL)
pop65 <- pop65 %>% select(-c(capital,longitude, latitude,lending , region))
pop65 <- pop65 %>% select(-c(country, iso3c, income))
names(pop65)[2] <- "pop65"
head(pop65)

# Population - Population ages 65 and above (% of total)
pop65 <- WDI(country = wpr_iso2, indicator = "SP.POP.65UP.TO.ZS", start = st, end = en, extra = TRUE, cache = NULL)
pop65 <- pop65 %>% select(-c(capital,longitude, latitude,lending , region))
names(pop65)[3] <- "pop65"


# GINI SI.POV.GINI
gini <- WDI(country = wpr_iso2, indicator = "SI.POV.GINI", start = st, end = en, extra = TRUE, cache = NULL)
gini <- gini %>% select(-c(capital,longitude, latitude,lending , region))
gini <- gini %>% select(-c(country, iso3c, income))
names(gini)[2] <- "gini"
head(gini)


# Unemployment SL.UEM.TOTL.NE.ZS
unemp <- WDI(country = wpr_iso2, indicator = "SL.UEM.TOTL.NE.ZS", start = st, end = en, extra = TRUE, cache = NULL)
unemp <- unemp %>% select(-c(capital,longitude, latitude,lending , region))
unemp <- unemp %>% select(-c(country, iso3c, income))
names(unemp)[2] <- "unemployment"
head(unemp)

# Health expenditure per capita, PPP (constant 2011 international $) - SH.XPD.PCAP.PP.KD
hexp <- WDI(country = wpr_iso2, indicator = "SH.XPD.TOTL.ZS", start = st, end = en, extra = TRUE, cache = NULL)
hexp <- hexp %>% select(-c(capital,longitude, latitude,lending , region))
hexp <- hexp %>% select(-c(country, iso3c, income))
names(hexp)[2] <- "health.exp"
head(hexp)


# Life expectancy (Male)
life.m <- WDI(country = wpr_iso2, indicator = "SP.DYN.LE00.MA.IN", start = st, end = en, extra = TRUE, cache = NULL)
life.m <- life.m %>% select(-c(capital,longitude, latitude,lending , region))
names(life.m)[3] <- "life.m"
head(life.m)

# Life expectancy (Female)
life.f <- WDI(country = wpr_iso2, indicator = "SP.DYN.LE00.FE.IN", start = st, end = en, extra = TRUE, cache = NULL)
life.f <- life.f %>% select(-c(capital,longitude, latitude,lending , region))
names(life.f)[3] <- "life.f"
head(life.f)

# health life expectancy
hlife <- read.csv("health_life_expectancy.csv")
hlife <- hlife %>% filter(GHO..DISPLAY.=="Healthy life expectancy (HALE) at birth (years)") %>% select(c(YEAR..CODE., REGION..CODE., REGION..DISPLAY., COUNTRY..CODE. , COUNTRY..DISPLAY., SEX..CODE., SEX..DISPLAY., Numeric)) 
names(hlife) <- c("year", "region", "region.name", "iso3", "country", "sex.code", "sex", "hlife")
hlife <- hlife %>% filter(sex.code=="BTSX") %>% filter(region=="WPR")
hlife <- hlife %>% select(-c(region, region.name, sex.code, sex))
head(hlife)

## check range for each indicator
range(gdp$gdp, na.rm=T)
range(life$life, na.rm=T)
range(gdp.g$gdp.g, na.rm=T)
range(pov$pov, na.rm=T)
range(gini$gini, na.rm=T)
range(unemp$unemp, na.rm=T)
range(hlife$hlife, na.rm=T)

# merge all WB data
df <- merge(life, gdp, by = c("year", "iso2c"), all.x = TRUE)
#df <- merge(df, gdp.g, by = c("year", "iso2c"), all.x =TRUE)
#df <- merge(df, pov, by = c("year", "iso2c"), all.x =TRUE)
names(df)[5] <- "iso3"
head(df)

# merge health life expectancy from WHO data
df <- merge(df, hlife, by=c("iso3", "year"), all = TRUE)
names(df) [3] <- "iso2"
names(df)[4] <- "short.country"
names(df)[8] <- "country"
df <- df %>% select(-country)
df <- df[,c(2,3,1,4,6,5,8,7)]
names(df)[6] <- "Life expectancy at birth (years)"
names(df)[7] <- "Healthy life expectancy at birth (years)"
names(df)[8] <- "GDP/capita (Constant 2010 US$)"

# conver to long format
dfl <- melt(df, id.vars = c("year", "iso2","iso3","short.country", "income"))

# add proper WHO style country name to all rows
dfl <- merge(dfl, clist[,-2], by ="iso3")
dfl <- dfl %>% select("year", "iso2", "iso3", "short.country", "country", "income", "variable", "value") # re-order variables

# dfl$variable <- factor(dfl$varibale, levels = c("life", "hlife", "gdp", "gdp.g"))
head(dfl)

# save(dfl, df, clist, file="key_indicators.Rda")


## projected life expectancy by 5 years 
life.p <- read.csv("UNdata_Export_20181019_055246508.csv")
names(life.p) <- c("country", "year5", "variant", "value")
life.p$year <- substring(life.p$year5, 1, 4)
#life.p$year <- factor(life.p$year, levels=c( "1950" ,"1955" ,"1960", "1965", "1970" ,"1975" ,"1980" ,"1985" ,"1990","1995", "2000", "2005", "2010", "2015",  "2020", "2025" ,"2030", "2035", "2040", "2045", "2050", "2055", "2060", "2065", "2070", "2075" ,"2080", "2085", "2090", "2095"))
life.p <- life.p %>% filter(year>2015)

#rev(unique(life.p$year))
head(life.p)
life.p <- life.p[,-c(2, 3)]
life.p.s <- dcast(life.p, country ~ year) 
life.p.s$region <- NA
life.p.s$region[c(5,14,33, 38, 50,51,52, 57,83,87,98,118,122,125,143,147,155, 159,167,170,171,175,179,185,187,190,196,207,215,219,244,245,251,262,264,265)] <- "WPR"
life.p.s <- life.p.s %>% filter(region=="WPR")

life.p.s$iso3 <- NA
life.p.s$iso3 <- as.character(wpr_iso3)
# life.p.s[,c("country", "iso3")] # perfect match!
#head(life.p.s)
life.pl <- melt(life.p.s, id.vars = c("country", "region", "iso3"))
names(life.pl)[4]  <- "year"


# try adding projected life expectancy in dlf
df2 <- merge(df, life.pl, by=c("iso3", "year"), all=TRUE)
names(df2)[11] <- "Projected life expectancy (years)"
df2 <- df2[,-c(9,10)]
dfl2 <- melt(df2, id.vars = c("year", "iso2","iso3","short.country", "income"))
dfl2 <- merge(dfl2, clist[,-2], by ="iso3")
dfl2 <- dfl2 %>% select("year", "iso2", "iso3", "short.country", "country", "income", "variable", "value") 
dfl2 <- dfl2 %>% filter(year<2055)
head(dfl2)

miss.val <- read.csv("missing.value.csv")
miss.val <- melt(miss.val, id.vars = "iso3")
names(miss.val)[2] <- "year"
miss.val$year <- substring(miss.val$year, 2, 5)
miss.val$value <- as.numeric(miss.val$value)
miss.val$var1 <- "Life expectancy at birth (years)"
miss.val$var2 <- "Healthy life expectancy at birth (years)"
miss.val$var3 <- "GDP/capita (Constant 2010 US$)"
miss.val$var4 <- "Projected life expectancy (years)"
miss.val2 <- melt(miss.val, id.vars = c("iso3", "year", "value"))
miss.val2$iso2 <- NA
miss.val2$short.country<- NA
miss.val2$country <- NA
miss.val2$income <- NA
miss.val2 <- miss.val2[,-4]
names(miss.val2)[4] <- "variable"
miss.val2 <- miss.val2 %>% select(year, iso2, iso3, short.country, country, income, variable, value)

dfl2 <- data.frame(rbind(dfl2, miss.val2))
dfl2 <- merge(dfl2, clist[,-2], by ="iso3")
dfl2 <- dfl2[,-5]
names(dfl2)[8] <- "country"
dfl2 <- dfl2 %>% select("year", "iso2", "iso3", "short.country", "country", "income", "variable", "value") # re-order variables


# Life expectancy at birth (years)
# Healthy life expectancy at birth (years)
# GDP/capita (Constant 2010 US$)
# Projected life expectancy (years)    

# graph by country
# try one country 
i <- "CHN"
dfs <- dfl %>% filter(iso3==i)
p <- ggplot(data=dfs, aes(x=year, y=value)) 
p <- p + geom_line(aes(x=year, y=value), stat="identity") 
 p <- p + geom_smooth(aes(x=year, y=value), colour="transparent", method="loess", fill="grey20", alpha=0.15)
p <- p + geom_smooth(aes(x=year, y=value), colour=mycols[3], method = "lm", fill=mycols[3], alpha=0.3)
p <- p + facet_wrap( ~ variable, scales = "free", ncol=3)
p <- p + geom_point(size=0.8) 
p <- p + theme(axis.title.x=element_blank(),
               axis.title.y=element_blank())
p <- p + ggtitle(paste(dfs$country[1]))
p

# pdf(file="indicators_Japan.pdf", width = 10, height =3)
# p 
# dev.off()

# another plot making the same scale range for life and hlife
i <- "CHN"
dfs <- dfl %>% filter(iso3==i) %>% filter(!variable=="GDP/capita (Constant 2010 US$)")
p <- ggplot(data=dfs, aes(x=year, y=value)) 
p <- p + geom_line(aes(x=year, y=value), stat="identity") # + ylim(40, 85)
# p <- p + geom_smooth(aes(x=year, y=value), colour="transparent", method="loess", fill="grey20", alpha=0.15)
p <- p + geom_smooth(aes(x=year, y=value), colour=mycols[3], method = "lm", fill=mycols[3], alpha=0.3)
p <- p + facet_wrap( ~ variable, ncol=2) #scales = "free"
p <- p + geom_point(size=0.7) 
p <- p + theme(axis.title.x=element_blank(),
               axis.title.y=element_blank())
p <- p + ggtitle(paste(dfs$country[1]))
p

## GDP plot
dfs <- dfl %>% filter(iso3==i) %>% filter(variable=="GDP/capita (Constant 2010 US$)")
p2 <- ggplot(data=dfs, aes(x=year, y=value)) 
p2 <- p2 + geom_line(aes(x=year, y=value), stat="identity") # + ylim(40, 85)
# p <- p + geom_smooth(aes(x=year, y=value), colour="transparent", method="loess", fill="grey20", alpha=0.15)
p2 <- p2 + geom_smooth(aes(x=year, y=value), colour=mycols[1], method = "lm", fill=mycols[1], alpha=0.3)
p2 <- p2 + facet_wrap( ~ variable, ncol=1) #scales = "free"
p2 <- p2 + geom_point(size=0.7) 
p2 <- p2 + theme(axis.title.x=element_blank(),
               axis.title.y=element_blank())
p2 <- p2 + ggtitle(paste(""))
p2

grid.arrange(p, p2, ncol=2, widths=c(2, 1))

# another plot making the same scale range for life and hlife
dfl2$year <- as.numeric(dfl2$year)
i <- "CHN"
dfs2 <- dfl2 %>% filter(iso3==i) %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
#p <- p + geom_line(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
# + ylim(40, 85)
# p <- p + geom_smooth(aes(x=year, y=value), colour="transparent", method="loess", fill="grey20", alpha=0.15)
p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)

#p <- p + geom_smooth(aes(x=year, y=value), colour=mycols[3], method = "lm", fill=mycols[3], alpha=0.3)
# p <- p + facet_wrap( ~ variable, ncol=2) #scales = "free"
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2034, by = 2))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(#axis.title.x=element_blank(),
               #axis.title.y=element_blank(),
               legend.position="top", legend.title = element_blank(),
               axis.text.x =  element_text(size = 8, angle=90),
               axis.title.y =  element_text(size = 10),
               axis.ticks.x=element_blank())
p <- p + guides(fill=guide_legend(ncol=2))
p

##### adding projected GDP per capita
gdp.ppp <-read.csv("./IMF_data/GDP per capita IMF.csv") 
gdp.ppp <- gdp.ppp[,-c(2:21)]
gdp.ppp <- gdp.ppp[c(9, 25,30,36,59, 75, 86, 90,91,95,103,106,110,114,116,122,125,132,134,137,148,155,158,175,180,188,190),]
gdp.ppp$iso3<- c("AUS", "BRN", "KHM", "CHN", "FJI", "HKG", "JPN", "KIR", "KOR", "LAO", "MAC", "MYS", "MHL", "FSM", "MNG", "NRU", "NZL", "PLW", "PNG", "PHL", "WSM", "SGP", "SLB", "TON", "TUV", "VUT", "VNM")

gdp.ppp <- melt(gdp.ppp, id.vars = c("country", "iso3"))
gdp.ppp$value[gdp.ppp$value=="no data"] <- NA
gdp.ppp$value <- as.numeric(gdp.ppp$value)
names(gdp.ppp)[3] <- "year"
gdp.ppp$year <- substring(gdp.ppp$year, 2,5)
gdp.ppp %>% filter(year=="2017")
gdp.ppp$variable <- ifelse(gdp.ppp$year>2017, "Projected", "Past")  

# c("ASM", "COK","PYF", "GUM", "NCL", "NIU", "MNP", "TKL", "WLF") #missing
mis.gdf <- read.csv("./IMF_data/missing_gdf.csv")
mis.gdf <- melt(mis.gdf, id.vars = "iso3")
names(mis.gdf)[2] <- "year"
mis.gdf$year <- substring(mis.gdf$year, 2,5)
mis.gdf$year <- as.numeric(mis.gdf$year)
mis.gdf$var1 <- "Projected"
mis.gdf$var2 <- "Past"
mis.gdf <- melt(mis.gdf, id.vars = c("iso3", "year", "value"))
mis.gdf <- mis.gdf[,-4]
mis.gdf$value <- as.numeric(mis.gdf$value)
mis.gdf$country <- NA
names(mis.gdf)[4] <- "variable" 
mis.gdf <- mis.gdf[,c(5, 1,2,3,4)]
gdp.ppp <- data.frame(rbind(gdp.ppp,mis.gdf))
gdp.ppp <- merge(gdp.ppp, clist[,-2], by="iso3")
gdp.ppp <- gdp.ppp[,-2]
names(gdp.ppp)[5] <- "country"
gdp.ppp <- gdp.ppp[,c(1,2,5,4,3)]
gdp.ppp$year <- as.numeric(gdp.ppp$year)

gdp.ppp$variable[gdp.ppp$variable=="Past"] <- "GDP/capita (Past, USD)"
gdp.ppp$variable[gdp.ppp$variable=="Projected"] <- "GDP/capita (Projected, USD)"
  
dfs <- gdp.ppp %>% filter(iso3==i) 
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") # + ylim(40, 85)
# p <- p + geom_smooth(aes(x=year, y=value), colour="transparent", method="loess", fill="grey20", alpha=0.15)
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
#p2 <- p2 + facet_wrap( ~ variable, ncol=1) #scales = "free"
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 2))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(#axis.title.x=element_blank(),
                 #axis.title.y=element_blank(),
                 legend.position="top", legend.title = element_blank(),
                 axis.text.x =  element_text(size = 8, angle=90),
                 axis.title.y =  element_text(size = 10),
                 axis.ticks.x=element_blank())
p2 <- p2 + guides(colour=guide_legend(nrow=2))
p2

grid.arrange(p, p2, ncol=2, widths=c(1.3, 1))


##################################
## loop ploting
##################################
i <- "JPN"

#i <- dfl$iso3[2000]
#iso3_list <- unique(clist$iso3)

i <- dfl$country[1500]
countryname <- unique(dfl$country)
for(i in countryname){
  dfs <- dfl %>% filter(country==i) %>% filter(!variable=="GDP/capita (Constant 2010 US$)")
  p <- ggplot(data=dfs, aes(x=year, y=value)) 
  p <- p + geom_line(aes(x=year, y=value), stat="identity") # + ylim(40, 85)
  # p <- p + geom_smooth(aes(x=year, y=value), colour="transparent", method="loess", fill="grey20", alpha=0.15)
  p <- p + geom_smooth(aes(x=year, y=value), colour=mycols[3], method = "lm", fill=mycols[3], alpha=0.3)
  p <- p + facet_wrap( ~ variable, ncol=2) #scales = "free"
  p <- p + geom_point(size=0.7) 
  p <- p + theme(axis.title.x=element_blank(),
                 axis.title.y=element_blank())
  p <- p + ggtitle(paste(dfs$country[1]))
  p
  ## GDP plot
  dfs <- dfl %>% filter(country==i) %>% filter(variable=="GDP/capita (Constant 2010 US$)")
  p2 <- ggplot(data=dfs, aes(x=year, y=value)) 
  p2 <- p2 + geom_line(aes(x=year, y=value), stat="identity") # + ylim(40, 85)
  # p <- p + geom_smooth(aes(x=year, y=value), colour="transparent", method="loess", fill="grey20", alpha=0.15)
  p2 <- p2 + geom_smooth(aes(x=year, y=value), colour=mycols[1], method = "lm", fill=mycols[1], alpha=0.3)
  p2 <- p2 + facet_wrap( ~ variable, ncol=1) #scales = "free"
  p2 <- p2 + geom_point(size=0.7) 
  p2 <- p2 + theme(axis.title.x=element_blank(),
                   axis.title.y=element_blank())
  p2 <- p2 + ggtitle(paste(""))
  p2
  
  mypath <- file.path("./figure", paste("indicator_",dfs$income, "_", dfs$country,".pdf",sep=""))
   pdf(file=mypath, width=9, height=3) # width=10,height=14
  # mypath <- file.path("./figure", paste("indicator_summary_",dfs$country,".pdf",sep=""))
  #pdf(file=paste("indicator_summary_",dfs$country,".pdf",sep=""), width=10, height=3) # width=10,height=14
  
  print(grid.arrange(p, p2, ncol=2, widths=c(1.9, 1)))
  dev.off()
}



## Loop
######## combined life, hlife and projected life in one plot, gdp per capita (projected and past)
i <- dfl2$country[1500]
countryname <- unique(dfl$country)
for(i in countryname){

  dfs2 <- dfl2 %>% filter(country==i) %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
  p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
  p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
  p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2034, by = 2))
  p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
  p <- p + theme(
    legend.position="top", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8, angle=90),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
  p <- p + guides(fill=guide_legend(ncol=2))

  dfs <- gdp.ppp %>% filter(country==i) 
  p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
  p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
  p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
  p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 2))
  p2 <- p2 + labs(title ="", x="", y="USD")
  p2 <- p2 + theme(
    legend.position="top", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8, angle=90),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
  p2 <- p2 + guides(colour=guide_legend(nrow=2))
  
  mypath <- file.path("./figure", paste("indicator_combined",dfs$income, "_", dfs$country,".pdf",sep=""))
  pdf(file=mypath, width=9, height=4.5) # width=10,height=14
  # mypath <- file.path("./figure", paste("indicator_summary_",dfs$country,".pdf",sep=""))
  #pdf(file=paste("indicator_summary_",dfs$country,".pdf",sep=""), width=10, height=3) # width=10,height=14
  
  print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
  dev.off()
}



## additonal codes for countries without Health life expectancy 
i <- dfl2$country[1500]
countryname <- unique(dfl$country)
for(i in countryname){
  dfs2 <- dfl2 %>% filter(country==i) %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(!variable=="Healthy life expectancy at birth (years)") %>% filter(year<2036)
  p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
  p <- p + scale_colour_manual(values = c(mycols[1], mycols[7]))
  p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
  p <- p + scale_fill_manual(values = c(mycols[1], mycols[7]))
  p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2034, by = 2))
  p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
  p <- p + theme(
    legend.position="top", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8, angle=90),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
  p <- p + guides(colour=guide_legend(ncol=1))
  
  dfs <- gdp.ppp %>% filter(country==i) 
  p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
  p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
  p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
  p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 2))
  p2 <- p2 + labs(title ="", x="", y="USD")
  p2 <- p2 + theme(
    legend.position="top", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8, angle=90),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
  p2 <- p2 + guides(colour=guide_legend(nrow=2))
  
  mypath <- file.path("./figure/without_HLE", paste("indicator_combined",dfs$income, "_", dfs$country,"_without_HLE.pdf",sep=""))
  pdf(file=mypath, width=9, height=4.5) # width=10,height=14
  # mypath <- file.path("./figure", paste("indicator_summary_",dfs$country,".pdf",sep=""))
  #pdf(file=paste("indicator_summary_",dfs$country,".pdf",sep=""), width=10, height=3) # width=10,height=14
  
  print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
  dev.off()
}


## only for Palau
dfs2 <- dfl2 %>% filter(country=="Palau") %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(!variable=="Healthy life expectancy at birth (years)") %>%  filter(!variable=="Projected life expectancy (years)") %>% filter(year<2036)
p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
p <- p + scale_colour_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
p <- p + scale_fill_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2034, by = 2))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p <- p + guides(colour=guide_legend(ncol=1))

dfs <- gdp.ppp %>% filter(country==i) 
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 2))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p2 <- p2 + guides(colour=guide_legend(nrow=2))
  
mypath <- file.path("./figure", paste("indicator_combined",dfs$income, "_", "Palau_without_HLEP.pdf",sep=""))
pdf(file=mypath, width=9, height=4.5) 
print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
dev.off()

# separte plots for Marchall Nauru and Tuvalu where life expectancy not available
countryname_selected <- c("Tuvalu", "Nauru", "Marshall Islands")
for(i in countryname_selected){
dfs2 <- dfl2 %>% filter(country==i) %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
#p <- p + scale_colour_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
#p <- p + scale_fill_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2034, by = 2))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p <- p + guides(colour=guide_legend(ncol=2))

dfs <- gdp.ppp %>% filter(country==i) 
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 2))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p2 <- p2 + guides(colour=guide_legend(nrow=2))

mypath <- file.path("./figure", paste("indicator_combined",dfs$income, "_", dfs$country, "_without_HLEP.pdf",sep=""))
pdf(file=mypath, width=9, height=4.5) 
print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
dev.off()
}
## American Samoa and Northern Mariana
dfs2 <- dfl2 %>% filter(country=="American Samoa") %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
#p <- p + scale_colour_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
#p <- p + scale_fill_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2034, by = 2))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p <- p + guides(colour=guide_legend(ncol=2))

dfs <- gdp.ppp %>% filter(country=="American Samoa") %>% filter(year<2023)
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 2))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p2 <- p2 + guides(colour=guide_legend(nrow=2))

mypath <- file.path("./figure", paste("indicator_combined",dfs$income, "_"," American Samoa_without_HLEP.pdf",sep=""))
pdf(file=mypath, width=9, height=4.5) 
print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
dev.off()

## Northern Mariana Islands
dfs2 <- dfl2 %>% filter(country=="Northern Mariana Islands") %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
#p <- p + scale_colour_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
#p <- p + scale_fill_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2034, by = 2))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p <- p + guides(colour=guide_legend(ncol=2))

dfs <- gdp.ppp %>% filter(country=="Northern Mariana Islands") %>% filter(year<2023)
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 2))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(
  legend.position="top", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8, angle=90),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
p2 <- p2 + guides(colour=guide_legend(nrow=2))

mypath <- file.path("./figure", paste("indicator_combined",dfs$income, "_","Northern Mariana Islands_without_HLEP.pdf",sep=""))
pdf(file=mypath, width=9, height=4.5) 
print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
dev.off()




# scatter plot for country comparison

































############### copied from Mongolia epi review code


# can be used
fuel <- WDI(country = c_name, indicator = "EG.CFT.ACCS.ZS", start = st,
            end = en, extra = FALSE, cache = NULL)

pov <- WDI(country = c_name, indicator = "SI.POV.DDAY", start = st,
           end = en, extra = FALSE, cache = NULL)


income.20 <- WDI(country = c_name, indicator = "SI.DST.FRST.20", start = st,
                 end = en, extra = FALSE, cache = NULL)



# can be used
social.ins <- WDI(country = c_name, indicator = "per_si_allsi.cov_pop_tot", start = st,
                  end = en, extra = FALSE, cache = NULL)

social.ins.poor <- WDI(country = c_name, indicator = "per_si_allsi.cov_q1_tot", start = st,
                       end = en, extra = FALSE, cache = NULL)

gini <- WDI(country = c_name, indicator = "SI.POV.GINI", start = st,
            end = en, extra = FALSE, cache = NULL)

smoke.male <- WDI(country = c_name, indicator = "SH.PRV.SMOK.MA", start = st,
                  end = en, extra = FALSE, cache = NULL)

smoke.female <- WDI(country = c_name, indicator = "SH.PRV.SMOK.FE", start = st,
                    end = en, extra = FALSE, cache = NULL)

dm <- WDI(country = c_name, indicator = "SH.STA.DIAB.ZS", start = st,
          end = en, extra = FALSE, cache = NULL)

gdp <- WDI(country = c_name, indicator = "SL.GDP.PCAP.EM.KD", start = st,
           end = en, extra = FALSE, cache = NULL)

slum <- WDI(country = c_name, indicator = "EN.POP.SLUM.UR.ZS", start = st,
            end = en, extra = FALSE, cache = NULL)


hiv <- WDI(country = c_name, indicator = "SH.DYN.AIDS.ZS", start = st,
           end = en, extra = FALSE, cache = NULL)


undernourish <- WDI(country = c_name, indicator = "SN.ITK.DEFC.ZS", start = st,
                    end = en, extra = FALSE, cache = NULL)


cancer <- WDI(country = c_name, indicator = "SH.DYN.NCOM.ZS", start = st,
              end = en, extra = FALSE, cache = NULL)

unemployment <- WDI(country = c_name, indicator = "SL.UEM.TOTL.NE.ZS", start = st,
                    end = en, extra = FALSE, cache = NULL)

######## key health indicator

u5m <- WDI(country = c_name, indicator = "SH.DYN.MORT", start = st2,
           end = en, extra = FALSE, cache = NULL)

infant <- WDI(country = c_name, indicator = "SP.DYN.IMRT.IN", start = st2,
              end = en, extra = FALSE, cache = NULL)


life.exp <- WDI(country = c_name, indicator = "SP.DYN.LE00.IN", start = st2,
                end = en, extra = FALSE, cache = NULL)


# <- WDI(country = c_name, indicator = "per_si_allsi.cov_pop_tot", start = st,
#       end = en, extra = FALSE, cache = NULL)


## temp
##


## create dataframe
df <- data.frame(gdp[,c(4,3)], 
                 pov[,3], 
                 income.20[,3],
                 unemployment[,3],
                 #         social.ins[,3],
                 #          social.ins.poor[,3],
                 #           gini[,3],
                 smoke.male[,3],
                 smoke.female[,3],
                 fuel[,3],
                 dm[,3],
                 slum[,3],
                 hiv[,3],
                 undernourish[,3],
                 cancer[,3])


names(df) <- c("year","GDP/capita (cst 2011)", "Below poverty line (%)", "Income share by loest 20%", "Unemployment (%)", "Smoking, Male (% adult)", "Smoking, Female (% adult)", "Clean fuel for cooking (%)", "Diabetes (% 20-79)", "Living in slum (% urban pop)", "HIV prevalence (%)", "Undernourished (%)", "Death: CVD/cancer/DM/CRD (% 30-70)")

# covnert to long format
dfl <- melt(df, id.vars = "year")

# plot
p <- ggplot(data=dfl, aes(x=year, y=value)) 
p <- p + geom_line(aes(x=year, y=value, group=variable), stat="identity") + facet_wrap(~variable,  scales = "free", ncol = 3)
p <- p + geom_point(size=1) + theme(strip.text = element_text(size=7))
p 

pdf(file="Trend of risk factors in Mongolia, 2000-2017.pdf", width = 7, height =8)
p
dev.off()

## 
df2 <- data.frame(u5m[,c(4,3)],
                  infant[,3],
                  life.exp[,3])

names(df2) <- c("year", "Under 5 mortality (per 1000)", "Infant mortality (per 1000)", "Life expectancy at birth (year)")

dfl2 <- melt(df2, id.vars = "year")

p <- ggplot(data=dfl2, aes(x=year, y=value)) 
p <- p + geom_line(aes(x=year, y=value, group=variable), stat="identity") + facet_wrap(~variable,  scales = "free", ncol = 3)
p <- p + geom_point(size=1) + theme(strip.text = element_text(size=8))
p 

pdf(file="Trend of key health indicators, 1990-2017.pdf", width = 8, height =3)
p
dev.off()


########### population change 

f04 <- WDI(country = c_name, indicator = "SP.POP.0004.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m04 <- WDI(country = c_name, indicator = "SP.POP.0004.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f59 <- WDI(country = c_name, indicator = "SP.POP.0509.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m59 <- WDI(country = c_name, indicator = "SP.POP.0509.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f1014 <- WDI(country = c_name, indicator = "SP.POP.1014.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m1014 <- WDI(country = c_name, indicator = "SP.POP.1014.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f1519 <- WDI(country = c_name, indicator = "SP.POP.1519.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m1519 <- WDI(country = c_name, indicator = "SP.POP.1519.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f2024 <- WDI(country = c_name, indicator = "SP.POP.2024.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m2024 <- WDI(country = c_name, indicator = "SP.POP.2024.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f2529 <- WDI(country = c_name, indicator = "SP.POP.2529.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m2529 <- WDI(country = c_name, indicator = "SP.POP.2529.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f3034 <- WDI(country = c_name, indicator = "SP.POP.3034.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m3034 <- WDI(country = c_name, indicator = "SP.POP.3034.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f3539 <- WDI(country = c_name, indicator = "SP.POP.3539.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m3539 <- WDI(country = c_name, indicator = "SP.POP.3539.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f4044 <- WDI(country = c_name, indicator = "SP.POP.4044.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m4044 <- WDI(country = c_name, indicator = "SP.POP.4044.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f4549 <- WDI(country = c_name, indicator = "SP.POP.4549.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m4549 <- WDI(country = c_name, indicator = "SP.POP.4549.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f5054 <- WDI(country = c_name, indicator = "SP.POP.5054.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m5054 <- WDI(country = c_name, indicator = "SP.POP.5054.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f5559 <- WDI(country = c_name, indicator = "SP.POP.5559.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m5559 <- WDI(country = c_name, indicator = "SP.POP.5559.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f6064 <- WDI(country = c_name, indicator = "SP.POP.6064.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m6064 <- WDI(country = c_name, indicator = "SP.POP.6064.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f6569 <- WDI(country = c_name, indicator = "SP.POP.6569.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m6569 <- WDI(country = c_name, indicator = "SP.POP.6569.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f7074 <- WDI(country = c_name, indicator = "SP.POP.7074.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m7074 <- WDI(country = c_name, indicator = "SP.POP.7074.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f7579 <- WDI(country = c_name, indicator = "SP.POP.7579.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m7579 <- WDI(country = c_name, indicator = "SP.POP.7579.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)
f80 <- WDI(country = c_name, indicator = "SP.POP.80UP.FE.5Y", start = st, end = en, extra = FALSE, cache = NULL)
m80 <- WDI(country = c_name, indicator = "SP.POP.80UP.MA.5Y", start = st, end = en, extra = FALSE, cache = NULL)


pop.f <- data.frame(f04[,c(4,3)],
                    f59[,3],
                    f1014[,3],
                    f1519[,3],
                    f2024[,3],
                    f2529[,3],
                    f3034[,3],
                    f3539[,3],
                    f4044[,3],
                    f4549[,3],
                    f5054[,3],
                    f5559[,3],
                    f6064[,3],
                    f6569[,3],
                    f7074[,3],
                    f7579[,3],
                    f80[,3]
)

names(pop.f) <- c("year","0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", ">80")
pop.f$sex <- "Female"

pop.fl <- melt(pop.f, id.vars = c("year", "sex"))

pop.m <- data.frame(m04[,c(4,3)],
                    m59[,3],
                    m1014[,3],
                    m1519[,3],
                    m2024[,3],
                    m2529[,3],
                    m3034[,3],
                    m3539[,3],
                    m4044[,3],
                    m4549[,3],
                    m5054[,3],
                    m5559[,3],
                    m6064[,3],
                    m6569[,3],
                    m7074[,3],
                    m7579[,3],
                    m80[,3]
)

names(pop.m) <- c("year","0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", ">80")
pop.m$sex <- "Male"

pop.ml <- melt(pop.m, id.vars = c("year", "sex"))

pop.change <- rbind(pop.fl, pop.ml)

pop.change$sex <- factor(pop.change$sex, levels=c("Male", "Female"))

## plot

p <- ggplot(data=pop.change, aes(x=year, y=value)) 
p <- p + geom_line(aes(x=year, y=value, colour=variable), stat="identity") + facet_wrap(~sex,  scales = "free", ncol = 2)
p <- p + theme(strip.text = element_text(size=10), 
               legend.title = element_blank()) + ylab("Percentage (%)") #geom_point(size=1) + 
p 

pdf(file="Change in population structure, 2000-2017.pdf", width = 10, height =7)
p
dev.off()


