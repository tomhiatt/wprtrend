# setwd("~/Dropbox/Current work space/STB_Mywork_2017/RD")

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
library(countrycode)
library(tidyr)
library(stringr)

options(stringsAsFactors = FALSE)
# ------------------------------------------
# Telecom data
# ------------------------------------------
  
# ---- ITU summary indicators ----
## Key ICT indicators for the ITU/BDT regions (totals and penetration rates)
## 2018 is estimated. 

## variables
ituc <- data.frame(
  name = c(
    "Fixed-telephone subscriptions", 
    "Mobile-cellular telephone subscriptions",
    "Active mobile-broadband subscriptions",
    "Fixed broadband subscriptions",
    "Households with a computer",
    "Households with Internet access at home",
    "Individuals using the Internet"), 
  code = c(
    "fixd.tel", 
    "mob.tel",
    "mob.brnd",
    "fixd.brnd",
    "hh.comp",
    "hh.internet",
    "indiv.internet"
  )
)

itua <- read.xlsx("data/Telecom/ITU_Key_2005-2018_ICT_data_with LDCs_rev27Nov2018.xls", 1, startRow = 56, rowIndex = c(56, 57:63, 64, 66:72, 74:80, 82:87, 91:98, 100:106, 108:113), colIndex = c(1, 16:29)) %>% rename(Region=NA.) %>% 
  mutate(var=c(
    rep(ituc$code[1],7),
    rep(ituc$code[2],7),
    rep(ituc$code[3],7),
    rep(ituc$code[4],7),
    rep(ituc$code[5],7),    
    rep(ituc$code[6],7),    
    rep(ituc$code[7],7)
  ))
  
   ### Test to see if coding is right 
itud <- itua %>% filter(is.na(X2018.)) %>% select(name=Region, code=var)
if (!identical(itud, ituc)) print("All is NOT well!")

itub <- itua %>% gather(yr, value, -Region, -var) %>% filter(!is.na(Region)) %>% transmute(Region, year=as.numeric(str_extract(yr, "[0-9]+")), var, value=as.numeric(value)) %>% filter(!is.na(value)) %>% spread(var, value)

## Plot
itub %>% select(Region, year, ituc$code[1:4]) %>% gather(code, value, -Region, -year) %>% inner_join(ituc) %>% ggplot(aes(year, value, color=Region, group=Region)) + facet_wrap(~name, scales = "free_y") + geom_line(size=1) + labs(x="", y="Per 100 inhabitants", title="Key ICT indicators for the ITU/BDT regions, 2005-2018", subtitle="* values for 2018 are estimated. \nCIS=Commonwealth of Independent States (formed when the former Soviet Union dissolved).")

itub %>% select(Region, year, ituc$code[5:7]) %>% gather(code, value, -Region, -year) %>% inner_join(ituc) %>% 
  ggplot(aes(year, value, color=Region, group=Region) ) + facet_wrap(~name, scales = "free_y", nrow=2) + geom_line(size=1) + labs(x="", y="%")


# ---- Indicators by country ----
# Fixed broadband data and telephone and mobile and internet
dta1 <- read.xlsx("data/Telecom/Fixed_broadband_2000-2017.xls", 1, startRow = 2, colIndex = c(1, 21:38)) %>% rename(country=NA.) 
names(dta1) <- str_replace(names(dta1), "X", "fixd.brnd:")

dta2 <- read.xlsx("data/Telecom/Fixed_tel_2000-2017.xls", 1, startRow = 2, colIndex = c(1, 21:38)) %>% rename(country=NA.) 
names(dta2) <- str_replace(names(dta2), "X", "fixd.tel:")

dta3 <- read.xlsx("data/Telecom/Mobile_cellular_2000-2017.xls", 1, startRow = 2, colIndex = c(1, 21:38)) %>% rename(country=NA.) 
names(dta3) <- str_replace(names(dta3), "X", "mob.tel:")

dta4 <- read.xlsx("data/Telecom/Individuals_Internet_2000-2017.xls", 1, startRow = 2, colIndex = c(1:19)) %>% rename(country=NA.) 
names(dta4) <- str_replace(names(dta4), "X", "indiv.internet:")

dta <- dta1 %>% left_join(dta2) %>% left_join(dta3) %>% left_join(dta4)

# Add in iso code
dta$iso3 <- countrycode(dta$country, 'country.name.en', 'iso3c')

## Fix countries countrycode can't handle
dta[is.na(dta$iso3),"country"]
dta[dta$country=="Central African Rep.", "iso3"] <- "CAF"
dta[dta$country=="Eswatini", "iso3"] <- "SWZ"
dta[dta$country=="Micronesia", "iso3"] <- "FSM"
dta[dta$country=="Neth. Antilles", "iso3"] <- "ANT"
dta[dta$country=="Ascension", "iso3"] <- "ASC"

# Add in WHO regions and remove non-WHO
# http://intranet.who.int/dev/refmartviewer/CustomViews/VIEW_CountryList
country.list <- read.csv("data/CountryList.csv")

dtb <- country.list %>% select(Title, iso3=ISO_Alpha3_Code_CODE, WHO_Region, WB_Income_Group) %>% inner_join(dta) %>% select(-country, country=Title) %>% filter(WHO_Region!="")


# Reshape and calculate vars
dtc <- dtb %>% 
  gather(yr, value, -country, -iso3, -WHO_Region, -WB_Income_Group) %>%
  separate(yr, c("var", "year"), sep=":", convert=TRUE) %>% left_join(ituc, by = c("var"="code")) %>% rename(Indicator=name)


# Calculate by country in groups

dtc %>% filter(WHO_Region=="WPR") %>% filter(WB_Income_Group=="HIC") %>%  
  ggplot(aes(year, value, color=Indicator)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Subscriptions per 100 inhabitants or % of Individuals", title="Key ICT indicators, high-income countries, 2000-2017") + geom_point(size=1)

dtc %>% filter(WHO_Region=="WPR") %>% filter(WB_Income_Group=="LMC") %>%  
  ggplot(aes(year, value, color=Indicator)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Subscriptions per 100 inhabitants or % of Individuals", title="Key ICT indicators, lower middle-income countries, 2000-2017") + geom_point(size=1)

dtc %>% filter(WHO_Region=="WPR") %>% filter(WB_Income_Group=="UMC") %>%  
  ggplot(aes(year, value, color=Indicator)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Subscriptions per 100 inhabitants or % of Individuals", title="Key ICT indicators, upper middle-income countries, 2000-2017") + geom_point(size=1)


# Mobile phone only
dtc %>% filter(WHO_Region=="WPR") %>% filter(var=="mob.tel") %>%  
  ggplot(aes(year, value, group=country)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Per 100 inhabitants", title="Mobile-cellular telephone subscriptions, 2000-2017") + geom_point(size=1)


#### Previous code ####

# Fixed broadband data
dta <- read.xlsx("data/Telecom/Fixed_broadband_2000-2017.xls", 1, startRow = 2, colIndex = c(1:19,21:38)) %>% rename(country=NA.) 

# Add in iso code
dta$iso3 <- countrycode(dta$country, 'country.name.en', 'iso3c')

## Fix countries countrycode can't handle
dta[is.na(dta$iso3),"country"]
dta[dta$country=="Central African Rep.", "iso3"] <- "CAF"
dta[dta$country=="Eswatini", "iso3"] <- "SWZ"
dta[dta$country=="Micronesia", "iso3"] <- "FSM"
dta[dta$country=="Neth. Antilles", "iso3"] <- "ANT"
dta[dta$country=="Ascension", "iso3"] <- "ASC"

# Add in WHO regions and remove non-WHO
# http://intranet.who.int/dev/refmartviewer/CustomViews/VIEW_CountryList
country.list <- read.csv("data/CountryList.csv")

dtb <- country.list %>% select(Title, iso3=ISO_Alpha3_Code_CODE, WHO_Region) %>% inner_join(dta) %>% select(-country, country=Title) %>% filter(WHO_Region!="")


# Reshape and calculate vars
dtc <- dtb %>% 
  gather(yr, value, -country, -iso3, -WHO_Region) %>% 
  mutate(var=if_else(str_detect(yr, "\\.1"), "fxbrnd.100", "fxbrnd"), year=as.numeric(str_extract(yr,"[0-9]+")), value=as.numeric(str_remove_all(value, "\'"))) %>% 
  select(-yr) %>% 
  spread(var, value) %>% 
  ## Calculate population used
  mutate(pop=fxbrnd/fxbrnd.100*100)

# Calculate globally and by region
# # missing data are excluded from numerator and denominators.
dtc %>% group_by(year, WHO_Region) %>% summarise(fxbrnd=sum(fxbrnd, na.rm=T), pop=sum(pop, na.rm=T)) %>% mutate(fxbrnd.100=fxbrnd/pop*100) -> reg.1
reg.1 %>% summarise(fxbrnd=sum(fxbrnd, na.rm=T), pop=sum(pop, na.rm=T)) %>% mutate(fxbrnd.100=fxbrnd/pop*100) -> reg.2
reg.1 %>% bind_rows(reg.2) %>% 
  ggplot(aes(year, fxbrnd.100, color=WHO_Region)) + geom_line()

dtc %>% filter(WHO_Region=="WPR", !is.na(fxbrnd.100)) %>% left_join(country.list, by = c("iso3" = "ISO_Alpha3_Code_CODE")) %>% filter(WB_Income_Group=="HIC") %>%  
  ggplot(aes(year, fxbrnd.100, group=country)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Per 100 inhabitants", title="Fixed-broadband subscriptions, high-income countries, 2000-2017") + geom_point(size=1)

dtc %>% filter(WHO_Region=="WPR", !is.na(fxbrnd.100)) %>% left_join(country.list, by = c("iso3" = "ISO_Alpha3_Code_CODE")) %>% filter(WB_Income_Group=="LMC") %>%  
  ggplot(aes(year, fxbrnd.100, group=country)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Per 100 inhabitants", title="Fixed-broadband subscriptions, Lower middle-income countries, 2000-2017") + geom_point(size=1)

dtc %>% filter(WHO_Region=="WPR", !is.na(fxbrnd.100)) %>% left_join(country.list, by = c("iso3" = "ISO_Alpha3_Code_CODE")) %>% filter(WB_Income_Group=="UMC") %>%  
  ggplot(aes(year, fxbrnd.100, group=country)) + facet_wrap(~country) + geom_line(size=1) + labs(x="", y="Per 100 inhabitants", title="Fixed-broadband subscriptions, Upper middle-income countries, 2000-2017") + geom_point(size=1)




