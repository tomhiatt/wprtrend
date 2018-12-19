# iso2 by WHO-region 
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


st <- 1990
en <- 2017
div=1000000000000 # Trillion

gdp_all <- WDI(indicator = "NY.GDP.MKTP.KD", start = 1980, end = en, extra = TRUE, cache = NULL)
gdp_all <- gdp_all %>% filter(country=="World")
gdp_all$value <- gdp_all$NY.GDP.MKTP.KD/div

# global trend in GDP
p <- ggplot(gdp_all , aes(x = year, y = value))
p <- p + geom_point(alpha=0.3, colour="blue")
p <- p + geom_smooth(aes(x=year, y= value),  method = "lm",  alpha=0.1, size=0.4, fill="blue")
p <- p + labs(title= "Global trend of GDP (constant 2010 US$), 1990-2017", y = "GDP in Trillion USD", x="") 
p <- p + theme(legend.title=element_blank()) + scale_y_continuous(label=comma)
# p <- p + scale_y_log10(breaks=pretty_breaks())
p

pdf(file="gdp_global.pdf", width=9,height=6) # write PDF
p
dev.off()


# WHO-region wise 
# WPR
gdp <- WDI(country = wpr_iso2, indicator = "NY.GDP.MKTP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
#gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[3] <- "gdp"
gdp_wpr <- gdp
# AFR
gdp <- WDI(country = afr_iso2, indicator = "NY.GDP.MKTP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[2] <- "gdp"
gdp_afr <- gdp
# AMR
gdp <- WDI(country = amr_iso2, indicator = "NY.GDP.MKTP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[2] <- "gdp"
gdp_amr <- gdp
# EMR
gdp <- WDI(country = emr_iso2, indicator = "NY.GDP.MKTP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[2] <- "gdp"
gdp_emr <- gdp
# EUR
gdp <- WDI(country = eur_iso2, indicator = "NY.GDP.MKTP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[2] <- "gdp"
gdp_eur <- gdp
# SEA
gdp <- WDI(country = sea_iso2, indicator = "NY.GDP.MKTP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[2] <- "gdp"
gdp_sea <- gdp

gdp_wpr_y <- aggregate(gdp ~ year, gdp_wpr, sum) 
gdp_afr_y <- aggregate(gdp ~ year, gdp_afr, sum)
gdp_amr_y <- aggregate(gdp ~ year, gdp_amr, sum)
gdp_emr_y <- aggregate(gdp ~ year, gdp_emr, sum)
gdp_eur_y <- aggregate(gdp ~ year, gdp_eur, sum)
gdp_sea_y <- aggregate(gdp ~ year, gdp_sea, sum)

gdp_region <- cbind(gdp_wpr_y,
      gdp_afr_y[,2],
      gdp_amr_y[,2],
      gdp_emr_y[,2],
      gdp_eur_y[,2],
      gdp_sea_y[,2])
      
names(gdp_region) <- c("year","WPR", "AFR", "AMR", "EMR", "EUR", "SEA")

# gap <- gdp_region[gdp_region$year==2017,-1]-gdp_region[gdp_region$year==1995,-1]
# gap/gdp_region[gdp_region$year==1995,-1]*100
# 
# gap <- gdp_region[gdp_region$year==2000,-1]-gdp_region[gdp_region$year==1995,-1]
# a <- gap/gdp_region[gdp_region$year==2000,-1]*100
# 
# gap <- gdp_region[gdp_region$year==2005,-1]-gdp_region[gdp_region$year==2000,-1]
# b <- gap/gdp_region[gdp_region$year==2005,-1]*100
# 
# gap <- gdp_region[gdp_region$year==2010,-1]-gdp_region[gdp_region$year==2005,-1]
# c <- gap/gdp_region[gdp_region$year==2010,-1]*100
# 
# gap <- gdp_region[gdp_region$year==2015,-1]-gdp_region[gdp_region$year==2010,-1]
# d <- gap/gdp_region[gdp_region$year==2015,-1]*100

global_gdp <- data.frame(rowSums(gdp_region[,c(2:6)]))
rownames(global_gdp) <- gdp_region[,1]
class(global_gdp)

plot(as.matrix(global_gdp))


########
gap <- gdp_region[gdp_region$year==1997,-1]-gdp_region[gdp_region$year==1992,-1]
a <- gap/gdp_region[gdp_region$year==1997,-1]*100

gap <- gdp_region[gdp_region$year==2002,-1]-gdp_region[gdp_region$year==1997,-1]
b <- gap/gdp_region[gdp_region$year==2002,-1]*100

gap <- gdp_region[gdp_region$year==2007,-1]-gdp_region[gdp_region$year==2002,-1]
c <- gap/gdp_region[gdp_region$year==2007,-1]*100

gap <- gdp_region[gdp_region$year==2012,-1]-gdp_region[gdp_region$year==2007,-1]
d <- gap/gdp_region[gdp_region$year==2012,-1]*100

gap <- gdp_region[gdp_region$year==2017,-1]-gdp_region[gdp_region$year==2012,-1]
e <- gap/gdp_region[gdp_region$year==2017,-1]*100


trend <- as.matrix(rbind(a,b,c,d, e))
#rownames(trend) <- c("1995-2000", "2000-2005", "2005-2010", "2010-2015")
rownames(trend) <- c("1992-1997", "1997-2002", "2002-2007", "2007-2012", "2012-2017")


pdf("GDP_growth_rate_region.pdf", width =8, height=5)
  
barplot(trend, beside=T, las=1, ylab="GDP growth rate (%)", cex.names = 0.9, cex.axis = 0.9, cex.lab=0.9, main="")
legend(3, 32.5, legend=rownames(trend), bty="n", cex=0.8,  horiz=T, fill=gray.colors(5), xpd=T)
title("GDP growth rate by WHO regions", line=+2.4)

dev.off()

trend_long <- melt(trend)
names(trend_long) <- c("year_cat", "who_region", "value")

p <- ggplot(trend_long, aes(x=who_region, y=value, fill=year_cat))
p <- p + geom_bar(stat="identity", alpha=0.7, position=position_dodge(width=1)) 
p

## gdp plot by region
gdp_region_long <- melt(gdp_region, id.vars = "year")
gdp_region_long$variable <- factor(gdp_region_long$variable , levels=c("AMR", "EUR", "WPR", "SEA", "EMR", "AFR"))

div=1000000000 # Billion
gdp_region_long$value <- gdp_region_long$value/div

p <- ggplot(gdp_region_long, aes(x = year, y = value, colour=variable))
p <- p + geom_point(alpha=0.3)
p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.16, size=0.4)
# p <- p + geom_line()
# p <- p + scale_y_log10(breaks=pretty_breaks())
p <- p + labs(title= "GDP (constant 2010 US$) by WHO region, 1990-2017", y = "GDP in Billion USD", x="") 
p <- p + theme(legend.title=element_blank()) + scale_y_continuous(label=comma)
p

pdf(file="gdp_who_region.pdf", width=9,height=6) # write PDF
p
dev.off()


# GDP time trend by country in WPR
# names(gdp_wpr)[1] <- "iso2"
# gdp_wpr <- merge(gdp_wpr, clist, by="iso2")
# 
# # by country
# names(gdp_wpr)[1] <- "iso2"
# gdp_wpr <- merge(gdp_wpr, clist, by="iso2")
names(gdp_wpr)[1] <- "iso2"
names(gdp_wpr)[5] <- "iso3"
gdp_wpr$gdp_bil <- gdp_wpr$gdp/div


p <- ggplot(gdp_wpr, aes(x = year, y = gdp_bil, colour=iso3, fill =iso3))
p <- p + geom_point(alpha=0.3)
p <- p + geom_smooth(aes(x=year, y= gdp_bil, fill=iso3),  method = "lm",  alpha=0.16, size=0.4)
p <- p + labs(title= "GDP (constant 2010 US$) by country in WPR, 1990-2017", y = "GDP in Billion USD", x="") 
p <- p + theme(legend.title=element_blank()) + scale_y_continuous(label=comma)
# p <- p + scale_y_log10(breaks=pretty_breaks())
p

pdf(file="gdp_country_WPR.pdf", width=10,height=6) # write PDF
p
dev.off()



gdp_wpr %>% filter(iso3=="CHN") # 1
gdp_wpr %>% filter(iso3=="JPN") # 2
gdp_wpr %>% filter(iso3=="AUS") # 3
gdp_wpr %>% filter(iso3=="KOR") # 4

gdp_wpr_17 <- gdp_wpr %>% filter(year==2017)
gdp_wpr_17 <- gdp_wpr_17[order(gdp_wpr_17$gdp_bil, decreasing=T),]


p <- ggplot(gdp_wpr_17, aes(x=reorder(iso3, -gdp_bil), y=gdp_bil, fill=income))
p <- p +  geom_bar(stat="identity", alpha=0.7)  #+coord_flip()
p <- p + labs(title= "GDP (constant 2010 US$) by country in WPR, 2017", y = "GDP in Billion USD", x="") 
#p <- p + scale_y_log10(breaks=pretty_breaks())
p <- p + theme(legend.title=element_blank(),
               axis.text.x = element_text(size = 7, angle=35, hjust = 1, vjust = 1)) 
p

pdf(file="gdp_country_WPR_2017.pdf", width=10,height=6) # write PDF
p
dev.off()

# GDP per capita in WPR
gdp <- WDI(country = wpr_iso2, indicator = "NY.GDP.PCAP.KD", start = st, end = en, extra = TRUE, cache = NULL)
gdp <- gdp %>% select(-c(capital,longitude, latitude,lending , region))
gdp <- gdp %>% select(-c(country, iso3c, income))
names(gdp)[2] <- "gdp"
head(gdp)
ppp <- gdp

ppp17 <- ppp %>% filter(year==2017)
names(ppp17)[1] <- "iso2"
gdp_ppp_wpr <- merge(gdp_wpr_17, ppp17, by="iso2")


p <- ggplot(gdp_ppp_wpr, aes(x = gdp_bil, y = gdp.y, label = country, colour=income)) #, size=pop # you can change label 
p <- p + geom_point(alpha=0.6)
p <- p + geom_text_repel(show.legend = FALSE, size=2.2)
p <- p + scale_x_log10(breaks=pretty_breaks())
p <- p + scale_y_log10(breaks=pretty_breaks())
#p <- p + geom_smooth(aes(x = gdp_bil, y = gdp.y, fill=income),  method = "lm",  alpha=0.16, size=0.4)

p <- p + labs(title= "GDP vs GDP per capita, countreis in WPR, 2017", y = "GDP per capita (USD)", x="GDP in Billion USD", size="", col="")
p <- p +  guides(fill=FALSE)
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9)) 
#p <- p + geom_abline(intercept=0, slope=1, colour="grey60")
p

pdf(file="gdp_vs_gdp per capita_country_WPR_2017.pdf", width=8.5,height=6) # write PDF
p
dev.off()


##  tree map trial
# p <- ggplot(gdp_wpr_17, aes(area=gdp_bil, fill=gdp_bil, label=country))
# p <- p + geom_treemap()
# 
# treemap(gdp_wpr_17,
#         index="country",
#         vSize="gdp_bil",
#         vColor=NULL,
#         type="index",
#         format.legend = list(scientific = FALSE, big.mark = " "))






######### Unemployment rate using ILO estiamte

unemp <- read.csv("./IMF_data/Unemployment rate_IMF_FM.csv")
unemp <- unemp[,-7]

unemp2 <- unemp %>% filter(age=="25+") %>% filter(area =="Africa" | area=="Arab States" | area=="Asia and the Pacific" | area=="Europe and Central Asia" | area=="Latin America and the Caribbean")

p <- ggplot(unemp2 , aes(x = year, y = rate, colour=area, fill =area))
p <- p + geom_point(alpha=0.3)
p <- p + geom_smooth(aes(x=year, y= rate, fill=area),  method = "lm",  alpha=0.16, size=0.4)
p <- p + labs(title= "Unemployment rate (ILO estimate), 2000-2022", y = "Rate (%)", x="") 
p <- p + theme(legend.title=element_blank()) # + scale_y_continuous(label=comma)
# p <- p + scale_y_log10(breaks=pretty_breaks())
p


pdf(file="unemployment rate by ILO region.pdf", width=10 ,height=5.5) # write PDF
p
dev.off()

## unemployment rate by country in WPR
names(unemp)[1] <- "country"
unemp_wpr <- merge(clist, unemp,by = "country")
unemp_wpr <- unemp_wpr %>% filter(age=="25+")
  
p <- ggplot(unemp_wpr , aes(x = year, y = rate, colour=country, fill =country))
#p <- p + geom_point(alpha=0.3)
p <- p + geom_line(alpha=0.8)
# p <- p + geom_smooth(aes(x=year, y= rate, fill=country),  method = "lm",  alpha=0.16, size=0.4)
p <- p + labs(title= "Unemployment rate (ILO estimate) in countries in WPR, 2000-2022", y = "Rate (%)", x="") 
p <- p + theme(legend.position = "none") # + scale_y_continuous(label=comma)
#p <- p + scale_y_log10(breaks=pretty_breaks())
p <- p +  geom_text(data = unemp_wpr[unemp_wpr$year == 2022,], aes(label = iso3), size=2.5, hjust = -0.4)
p

pdf(file="unemployment rate in countries in WPR.pdf", width=8 ,height=5.5) # write PDF
p
dev.off()



# Income share by highest 20%


# SI.POV.GINI

income20 <- WDI(country = wpr_iso2, indicator = "SI.DST.05TH.20", start = st, end = en, extra = TRUE, cache = NULL)
income20 <- income20 %>% select(-c(capital,longitude, latitude,lending , region))
income20 <- income20 %>% select(-c(country, iso3c, income))
names(income20)[2] <- "income20"
income20_wpr <- income20
# # AFR
# income20 <- WDI(country = afr_iso2, indicator = "SI.DST.05TH.20", start = st, end = en, extra = TRUE, cache = NULL)
# income20 <- income20 %>% select(-c(capital,longitude, latitude,lending , region))
# income20 <- income20 %>% select(-c(country, iso3c, income))
# names(income20)[2] <- "income20"
# income20_afr <- income20
# # AMR
# income20 <- WDI(country = amr_iso2, indicator = "SI.DST.05TH.20", start = st, end = en, extra = TRUE, cache = NULL)
# income20 <- income20 %>% select(-c(capital,longitude, latitude,lending , region))
# income20 <- income20 %>% select(-c(country, iso3c, income))
# names(income20)[2] <- "income20"
# income20_amr <- income20
# # EMR
# income20 <- WDI(country = emr_iso2, indicator = "SI.DST.05TH.20", start = st, end = en, extra = TRUE, cache = NULL)
# income20 <- income20 %>% select(-c(capital,longitude, latitude,lending , region))
# income20 <- income20 %>% select(-c(country, iso3c, income))
# names(income20)[2] <- "income20"
# income20_emr <- income20
# # EUR
# income20 <- WDI(country = eur_iso2, indicator = "SI.DST.05TH.20", start = st, end = en, extra = TRUE, cache = NULL)
# income20 <- income20 %>% select(-c(capital,longitude, latitude,lending , region))
# income20 <- income20 %>% select(-c(country, iso3c, income))
# names(income20)[2] <- "income20"
# income20_eur <- income20
# # SEA
# income20 <- WDI(country = sea_iso2, indicator = "SI.DST.05TH.20", start = st, end = en, extra = TRUE, cache = NULL)
# income20 <- income20 %>% select(-c(capital,longitude, latitude,lending , region))
# income20 <- income20 %>% select(-c(country, iso3c, income))
# names(income20)[2] <- "income20"
# income20_sea <- income20


names(income20_wpr )[1] <- "iso2"
income20_wpr <- merge(income20_wpr, clist, by = "iso2")

p <- ggplot(income20_wpr , aes(x = year, y = income20, colour=iso3, fill =iso3))
p <- p + geom_point(alpha=0.3)
#p <- p + geom_line(alpha=0.8)
 p <- p + geom_smooth(aes(x=year, y= income20, fill=iso3),  method = "lm",  alpha=0.16, size=0.4)
p <- p + labs(title= "", y = "Percentage (%)", x="") 
#p <- p + theme(legend.position = "none") # + scale_y_continuous(label=comma)
#p <- p + scale_y_log10(breaks=pretty_breaks())
#p <- p +  geom_text(data = unemp_wpr[unemp_wpr$year == 2022,], aes(label = iso3), size=2.5, hjust = -0.4)
p

## Gini


gini <- read.csv(file="./OurWorldinData/gini-index-around-2015-vs-gini-index-around-2000.csv")


names(gini) <- c("country","iso3", "year", "gini2015", "gini2000", "pop")
gini <- merge(gini, g_iso3, by = "iso3")

gini2015 <- gini %>% filter(year==2015)

p <- ggplot(gini2015, aes(x = gini2000, y = gini2015, label = country, colour=who_region)) #, size=pop # you can change label 
p <- p + geom_point(alpha=0.6)
p <- p + geom_text_repel(show.legend = FALSE, size=2.2)
# p <- p + geom_smooth(aes(x=gini2000, y=gini2015, fill=who_region),  method = "lm",  alpha=0.16, size=0.4)
p <- p + labs(title= "Gini index around 2015 vs Gini index around 2000", y = "Gini index around 2015", x="Gini index around 2000", size="", col="WHO regions")
p <- p +  guides(fill=FALSE)
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9)) 
p <- p + geom_abline(intercept=0, slope=1, colour="grey60")
p

pdf(file="Gini 2000 vs 2015 by WHO region_country.pdf", width=8 ,height=6) # write PDF
p
dev.off()


##################################################
## Urbanization
##################################################

ub <- read.csv("./UN_data/urban_population_projection.csv")
un_code <- read.csv("./UN_data/un_code_iso3.csv")
ub.g <- ub %>% filter(Country.code == 900) # Global

# plot global

ub.g.l <- melt(ub.g, id.vars="Country.code")
ub.g.l$variable <- substring(ub.g.l$variable, 2, 5)
ub.g.l$variable <- as.integer(ub.g.l$variable)

p <- ggplot(ub.g.l, aes(x = variable, y = value))
p <- p + geom_point(alpha=0.3, colour="blue")
#p <- p + geom_line(alpha=0.8)
p <- p + geom_smooth(aes(x=variable, y= value),  method = "lm",  alpha=0.18, size=0.4)
p <- p + labs(title= "Percentage of global population in Urban areas, 1950-2050", y = "Percentage (%)", x="") 

p

pdf(file="Percentage of population in urban areas_global.pdf", width=7 ,height=5.5) # write PDF
p
dev.off()


# plot reggiona
ub.r <- ub %>% filter(Country.code == 903 | # Africa
                      Country.code == 935 | # Asia
                      Country.code == 908 | # Europe
                      Country.code == 904 | # LATIN AMERICA AND THE CARIBBEAN
                      Country.code == 905 | # NORTHERN AMERICA
                      Country.code == 909) # Oceania

ub.r$region <- c("Africa", "Asia", "Europe", "Latin America and the Caribbean", "Northern America", "Oceania")

ub.r <- ub.r[,-1]
ub.r.l <- melt(ub.r[, ], id.vars = "region")
ub.r.l$variable <- substring(ub.r.l$variable, 2, 5)

ub.r.l$variable <- as.integer(ub.r.l$variable)

p <- ggplot(ub.r.l, aes(x = variable, y = value, colour=region))
p <- p + geom_point(alpha=0.3)
p <- p + geom_line(alpha=0.8)
# p <- p + geom_smooth(aes(x=variable, y= value, fill=region),  method = "lm",  alpha=0.18, size=0.4)

p <- p + labs(title= "Percentage of population in Urban areas by region, 1950-2050", y = "Percentage (%)", x="") 
# p <- p + theme(legend.position = "none") # + scale_y_continuous(label=comma)
# p <- p + scale_y_log10(breaks=pretty_breaks())
# p <- p +  geom_text(data = unemp_wpr[unemp_wpr$year == 2022,], aes(label = iso3), size=2.5, hjust = -0.4)
p

pdf(file="Percentage of population in urban areas by region.pdf", width=9 ,height=5.5) # write PDF
p
dev.off()

# plot country

names(ub)[1] <- "un_code"
un_code<- un_code[,-c(5,6)]

ub <- merge(ub, un_code, by="un_code")
ub <- merge(ub, g_iso3, by="iso3")
ub_w <- ub %>% filter(who_region=="WPR")
ub_w_l <- melt(ub_w, id.vars = c("iso3", "un_code", "country", "iso2", "who_region"))
ub_w_l$variable <- substring(ub_w_l$variable, 2,5)
ub_w_l$variable <- as.integer(ub_w_l$variable)

p <- ggplot(ub_w_l, aes(x = variable, y = value))
p <- p + geom_point(alpha=0.3, colour="blue", size=0.7) + facet_wrap(~ country, ncol = 5)
# p <- p + geom_line(alpha=0.8) 
p <- p + geom_smooth(aes(x=variable, y= value),  method = "lm",  alpha=0.18, size=0.4)
p <- p + labs(title= "Percentage of population in Urban areas by country in WPR, 1950-2050", y = "Percentage (%)", x="") 
p

pdf(file="Percentage of population in urban areas by country in WPR.pdf", width=10 ,height=15) # write PDF
p
dev.off()









######################################################
######### sample code
# line plot
p <- ggplot(df16, aes(x = life, y = hlife, label = country, colour=income, size=gdp))
p <- p + geom_point(alpha=0.6)
p <- p + geom_text_repel(show.legend = FALSE, size=3)
p <- p + geom_smooth(aes(x=life, y=hlife, fill=income),  method = "lm",  alpha=0.16, size=0.4)
p <- p + geom_hline(yintercept = mean(df16$hlife, na.rm=T), linetype=2, color="#787C7D") # grey50
p <- p + geom_vline(xintercept = mean(df16$life, na.rm=T), linetype=2, color="#787C7D") # grey50
p <- p + labs(title= "Relationship between life expectancy, healthy life expactancy and GDP per capita in WPR countries, 2016", y = "Healthy life expectancy at birth (years)", x="Life expectancy at birth (years)", size="GDP per capita (USD)", col="Income classification") 
p <- p + ylim(55, 77) + xlim(62,86) + guides(fill=FALSE)
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9))
p <- p + annotate("text", x=76.2, y=55, label= "Average", size=3, colour="#787C7D")
p <- p + annotate("text", x=62, y=66, label= "Average", size=3, colour="#787C7D")
p

## bar plot
p <- ggplot(df16, aes(x=reorder(country,-hlife), y=hlife, fill=income))
p <- p + geom_bar(stat="identity", alpha=0.7) 
p <- p + labs(title= "Healthy life expectancy at birth in WPR countries, 2016", x = "", y="Healthy life expectancy at birth (years)", fill="Income classification") 
p2 <- p + theme(#legend.title= element_text(size=9),
  plot.title = element_text(size = 10),
  axis.title=element_text(size=7),
  axis.text.x = element_text(size = 5.5, angle=35, hjust = 1, vjust = 1),
  legend.position = "none")














