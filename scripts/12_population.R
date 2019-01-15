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
# Population data
# ------------------------------------------
  
# ---- bring in data ----

load("D:/Users/hiattt/Dropbox/_Collaborations/wprtrend/data/UN_data/pop.rdata")

# WPR population shift
pop %>% filter(g.whoregion=="WPR") %>% group_by(year) %>% summarize(`Under 15`= sum(e.pop.f014, e.pop.m014) , `Over 65`= sum(e.pop.f65, e.pop.m65), fif=sum(e.pop.15plus)) %>% mutate(`From 15 to 64`=fif - `Over 65`) %>% select(-fif) %>% gather(Variable, value, -year) %>% filter(year %in%  seq(1950, 2050, 5)) %>% mutate(Variable=factor(Variable, levels = c("Over 65", "From 15 to 64",  "Under 15"))) %>% 
    ggplot(aes(year, value/1e9, fill=Variable)) + geom_bar(position = "stack", stat = "identity") + labs(title="Population estimates and projections, WPR, 1950-2050", x="", y="Number of people (billions)", fill="")

# Percent over 65 by WHO region
pop %>% group_by(g.whoregion, year) %>% summarize(`Under 15`= sum(e.pop.f014, e.pop.m014) , `Over 65`= sum(e.pop.f65, e.pop.m65), fif=sum(e.pop.15plus), e.pop.num=sum(e.pop.num)) %>% mutate(`From 15 to 64`=fif - `Over 65`, pct65=`Over 65`/e.pop.num) %>% 
  ggplot(aes(year, pct65, color=g.whoregion)) + geom_line(size=1) + labs(title="Estimated and projected proportion of population age 65 or over, 1950-2050", x="", y="Percent of population 65 or over", color="WHO region")+ scale_y_continuous(labels = percent)


# Population shift by country
top12 <- pop %>% filter(g.whoregion=="WPR", year==2019) %>% top_n(15, e.pop.num) %>% select(country)

pop %>% filter(country %in% top12$country) %>% group_by(country, year) %>% summarize(`Under 15`= sum(e.pop.f014, e.pop.m014) , `Over 65`= sum(e.pop.f65, e.pop.m65), fif=sum(e.pop.15plus)) %>% mutate(`From 15 to 64`=fif - `Over 65`) %>% select(-fif) %>% gather(Variable, value, -year, -country) %>% filter(year %in%  seq(1950, 2050, 5)) %>% mutate(Variable=factor(Variable, levels = c("Over 65", "From 15 to 64",  "Under 15"))) %>% 
  ggplot(aes(year, value/1e6, fill=Variable)) + geom_bar(position = "stack", stat = "identity") + labs(title="Population estimates and projections, 15 most populous countries in WPR, 1950-2050", x="", y="Number of people (millions)", fill="") + facet_wrap(~country, ncol = 3, scales = "free_y")


# Population shift by country in the Pacific
pacific_nPNG <- c("ASM", "COK", "FJI", "PYF", "GUM", "KIR", "MHL", "FSM", "NRU",   "NCL", "NIU", "MNP", "PLW", "WSM", "SLB", "TKL", "TON", "TUV",   "VUT", "WLF")

pop %>% filter(iso3 %in% pacific_nPNG) %>% group_by(country, year) %>% summarize(`Under 15`= sum(e.pop.f014, e.pop.m014) , `Over 65`= sum(e.pop.f65, e.pop.m65), fif=sum(e.pop.15plus)) %>% mutate(`From 15 to 64`=fif - `Over 65`) %>% select(-fif) %>% gather(Variable, value, -year, -country) %>% filter(year %in%  seq(1950, 2050, 5)) %>% mutate(Variable=factor(Variable, levels = c("Over 65", "From 15 to 64",  "Under 15"))) %>% 
  ggplot(aes(year, value/1e3, fill=Variable)) + geom_bar(position = "stack", stat = "identity") + labs(title="Population estimates and projections, Pacific Island countries, 1950-2050", x="", y="Number of people (thousands)", fill="") + facet_wrap(~country, ncol = 3, scales = "free_y")

# Percent over 65 by country in PIC and elsewhere
# Percent over 65 by WHO region


pop1 <- pop %>% filter(g.whoregion=="WPR") %>%  group_by(iso3, country, year) %>% summarize(`Over 65`= sum(e.pop.f65, e.pop.m65), e.pop.num=sum(e.pop.num)) %>% mutate(pct65=`Over 65`/e.pop.num) 

pop2 <- pop1 %>% filter(year==2050, iso3 %in% pacific_nPNG)

pop1 %>% filter(iso3 %in% pacific_nPNG) %>% ggplot(aes(year, pct65, color=country)) + geom_line(size=1) + labs(title="Estimated and projected proportion of population age 65 or over, Pacific, 1950-2050", x="", y="Percent of population 65 or over", color="Country")+ scale_y_continuous(labels = percent) + geom_label_repel(aes(year, pct65, label=country), data = pop2) + theme(legend.position = "none")


pop3 <- pop1 %>% filter(year==2050, !iso3 %in% pacific_nPNG)

pop1 %>% filter(!iso3 %in% pacific_nPNG) %>% ggplot(aes(year, pct65, color=country)) + geom_line(size=1) + labs(title="Estimated and projected proportion of population age 65 or over, non-Pacific, 1950-2050", x="", y="Percent of population 65 or over", color="Country")+ scale_y_continuous(labels = percent) + geom_label_repel(aes(year, pct65, label=country), data = pop3) + theme(legend.position = "none")





# pop %>% filter(g.whoregion=="WPR") %>% transmute(country, year, `Under 15`= sum(e.pop.f014, e.pop.m014) , `Over 65`= sum(e.pop.f65, e.pop.m65), `From 15 to 64`=e.pop.15plus - `Under 15`) %>% gather(Variable, value, -country, -year) %>% group_by(year) %>% 
  




