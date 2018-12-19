

clist <- read.csv("country_iso.csv")
wpr_iso2 <- clist$iso2
wpr_iso3 <- clist$iso3

#c_name <- "MN" # specify country 
st <- 1900
en <- 2017
mycols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF") 


# # Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)
# com <- WDI(country = wpr_iso2, indicator = "SH.DTH.COMM.ZS", start = st, end = en, extra = TRUE, cache = NULL)
# com <- com %>% select(-c(capital,longitude, latitude,lending , region))
# names(com)[3] <- "com"
# head(com)
# 
# 
# ncd <- WDI(country = wpr_iso2, indicator = "SH.DTH.NCOM.ZS", start = st, end = en, extra = TRUE, cache = NULL)
# ncd <- ncd %>% select(-c(capital,longitude, latitude,lending , region))
# names(ncd)[3] <- "ncd"
# head(ncd)
# 


mort <- read.csv("./GBD/total_disease_burden_by_cause.csv") # data from GDB through Our World in Data
names(mort) <- c("country", "iso3", "year", "ncd", "com", "inj") 

mort <- merge(mort, g_iso3, by="iso3")
mort_long <- melt(mort, id.vars = c("iso3", "country", "year", "who_region"))
mort_region_long <- aggregate(value ~ who_region + year+ variable, data = mort_long, sum, na.rm = TRUE)
mort_global_long <- aggregate(value ~ year + variable, data=mort_region_long, sum, na.rm=T)

# Global mortality trend
mort_global <- dcast(mort_global, year ~ variable)
mort_global.p <- data.frame(cbind(mort_global$year,prop.table(as.matrix(mort_global[,c(2:4)]), 1)*100))
names(mort_global.p)[1] <- "year"

mort_global.p_long <- melt(mort_global.p, id="year")

mort_wpr_long <- mort_region_long %>% filter(who_region=="WPR")
mort_afr_long <- mort_region_long %>% filter(who_region=="AFR")
mort_amr_long <- mort_region_long %>% filter(who_region=="AMR")
mort_emr_long <- mort_region_long %>% filter(who_region=="EMR")
mort_eur_long <- mort_region_long %>% filter(who_region=="EUR")
mort_sea_long <- mort_region_long %>% filter(who_region=="SEA")

mort_wpr <- dcast(mort_wpr_long, year + who_region ~ variable)
mort_afr <- dcast(mort_afr_long, year + who_region ~ variable)
mort_amr <- dcast(mort_amr_long, year + who_region ~ variable)
mort_emr <- dcast(mort_emr_long, year + who_region ~ variable)
mort_eur <- dcast(mort_eur_long, year + who_region ~ variable)
mort_sea <- dcast(mort_sea_long, year + who_region ~ variable)

mort_wpr.p <- data.frame(cbind(mort_wpr[,c(1,2)], prop.table(as.matrix(mort_wpr[,c(3:5)]), 1)*100))
mort_afr.p <- data.frame(cbind(mort_afr[,c(1,2)], prop.table(as.matrix(mort_afr[,c(3:5)]), 1)*100))
mort_amr.p <- data.frame(cbind(mort_amr[,c(1,2)], prop.table(as.matrix(mort_amr[,c(3:5)]), 1)*100))
mort_emr.p <- data.frame(cbind(mort_emr[,c(1,2)], prop.table(as.matrix(mort_emr[,c(3:5)]), 1)*100))
mort_eur.p <- data.frame(cbind(mort_eur[,c(1,2)], prop.table(as.matrix(mort_eur[,c(3:5)]), 1)*100))
mort_sea.p <- data.frame(cbind(mort_sea[,c(1,2)], prop.table(as.matrix(mort_sea[,c(3:5)]), 1)*100))

mort_region.p <- rbind(mort_wpr.p,
                       mort_afr.p,
                       mort_amr.p,
                       mort_emr.p,
                       mort_eur.p,
                       mort_sea.p)

mort_region.p_long <- melt(mort_region.p,id.vars = c("year", "who_region"))


# plot global diease burden
mort_global.p_long$variable <- factor(mort_global.p_long$variable, levels=c("ncd","inj","com"),labels=c("Non-communicable disease (NCDs)", "Injuries", "Communicable, maternal, neonatal, and nutritional diseases"))

p <- ggplot(mort_global.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "% of global disease burden by cause, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=8)) + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_burden_global_prop.pdf", width=8,height=6) # write PDF
p
dev.off()


mort_global_long$value2 <- mort_global_long$value/1000000
mort_global_long$variable <- factor(mort_global_long$variable, levels=c("ncd","inj","com"),labels=c("Non-communicable disease (NCDs)", "Injuries", "Communicable, maternal, neonatal, and nutritional diseases"))

p <- ggplot(mort_global_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "Global disease burden by cause, 1990-2016", y = "Number of DALYs per year (million)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=8)) + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_burden_global.pdf", width=8,height=6) # write PDF
p
dev.off()

#"Total disease burden measured as the number of DALYs (Disability-Adjusted Life Years) per year. DALYs are used tomeasure total burden of disease - both from years of life lost and years lived with a disability. One DALY equals one lostyear of healthy life"


# by region
mort_region.p_long$variable <- factor(mort_region.p_long$variable, levels=c("ncd","inj","com"),labels=c("Non-communicable disease (NCDs)", "Injuries", "Communicable, maternal, neonatal, and nutritional diseases"))

p <- ggplot(mort_region.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region)
p <- p + labs(title= "% of disease burden by cause by WHO region, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_burden_region_prop.pdf", width=9,height=8) # write PDF
p
dev.off()

mort_region_long$value2 <- mort_region_long$value/100000
mort_region_long$variable <- factor(mort_region_long$variable, levels=c("ncd","inj","com"),labels=c("Non-communicable disease (NCDs)", "Injuries", "Communicable, maternal, neonatal, and nutritional diseases"))

p <- ggplot(mort_region_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region,scales="free")
p <- p + labs(title= "Disease burden by cause by WHO region, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_burden_region.pdf", width=9,height=8) # write PDF
p
dev.off()




# by country
mort_long_wpr <- mort_long %>% filter(who_region=="WPR")

mort_long_wpr$variable <- factor(mort_long_wpr$variable, levels=c("ncd","inj","com"),labels=c("Non-communicable disease (NCDs)", "Injuries", "Communicable, maternal, neonatal, and nutritional diseases"))


mort_long_wpr$value2 <- mort_long_wpr$value/100000
mort_long_wpr$country <- as.character(mort_long_wpr$country)
mort_long_wpr$country[mort_long_wpr$iso3=="BRN"] <- "Brunei Darussalam"
mort_long_wpr$country[mort_long_wpr$iso3=="LAO"] <- "Lao PDR"
mort_long_wpr$country[mort_long_wpr$iso3=="FSM"] <- "Micronesia (Fed. States of)"
mort_long_wpr$country[mort_long_wpr$iso3=="VNM"] <- "Viet Nam"
mort_long_wpr$country[mort_long_wpr$iso3=="KOR"] <- "Rep. of Korea"


p <- ggplot(mort_long_wpr, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~ country, scales="free")
p <- p + labs(title= "Disease burden by cause by country in WPR, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=9),
               axis.text = element_text(size=6))
p <- p + guides(colour=guide_legend(nrow=2,byrow=TRUE))

pdf(file="disease_burden_country_WPR.pdf", width=10,height=12) # write PDF
p
dev.off()

######### 2016 comparison across country

mort_long_wpr_16 <- mort_long_wpr %>% filter(year==2016)

mort_long_wpr_16 <- mort_long_wpr_16[,-7]
mort_wpr_16 <- dcast(mort_long_wpr_16, country + iso3 ~ variable)


mort_wpr_16.p <- data.frame(cbind(mort_wpr_16, rowSums(mort_wpr_16[,c(3,5)]), prop.table(as.matrix(mort_wpr_16[,c(3:5)]), 1)*100)) 

names(mort_wpr_16.p) <- c("country", "iso3", "ncd", "inj", "com", "com_ncd", "ncd.p", "inj.p", "com.p")

income_wpr <- gdp_wpr_17[,c(2,1, 5,6)]
#write.csv(income_wpr, file = "income_wpr.csv",row.names = F)

mort_wpr_16.p <- merge(mort_wpr_16.p, income_wpr)
mort_wpr_16.p$com_ncd2 <- mort_wpr_16.p$com_ncd/1000000

p <- ggplot(mort_wpr_16.p, aes(x = ncd.p, y = com.p, label = country, colour=income, size=com_ncd2))
p <- p + geom_point(alpha=0.6)
p <- p + geom_text_repel(show.legend = FALSE, size=3)
p <- p + geom_smooth(aes(x=ncd.p, y=com.p, fill=income),  method = "lm",  alpha=0.13, size=0.4, linetype=0)
p <- p + labs(title= "Disease burdedn, NCD vs Communicable and other diseases, 2016", y = "Communicable and other diseases (% of total DALYs)", x="NCDs (% of total DALYs)", size="DALYs per year (million)", col="Income classification") 
p <- p +  guides(fill=FALSE)
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9))
p

pdf(file="disease_burden_scatterplot_WPR.pdf", width=8,height=5) # write PDF
p
dev.off()

# p <- ggplot(mort_wpr_16.p, aes(x = ncd, y = com, label = country, colour=income, size=com_ncd2))
# p <- p + geom_point(alpha=0.6)
# p <- p + geom_text_repel(show.legend = FALSE, size=3)
# p <- p + scale_x_log10(breaks=pretty_breaks())
# p <- p + scale_y_log10(breaks=pretty_breaks())

#################################################
#        NCDs
#################################################

ncds <- read.csv("./GBD/disease-burden-from-ncds.csv") # data from GDB through Our World in Data
names(ncds) <- c("country", "iso3", "year", "car", "can", "res", "dm", "oth", "liv", "men", "neu", "mus", "diges") 

ncds <- merge(ncds, g_iso3, by="iso3")
ncds_long <- melt(ncds, id.vars = c("iso3", "country", "year", "who_region"))
ncds_region_long <- aggregate(value ~ who_region + year+ variable, data = ncds_long, sum, na.rm = TRUE)
ncds_global_long <- aggregate(value ~ year + variable, data=ncds_region_long, sum, na.rm=T)

# Global burden trend
ncds_global <- dcast(ncds_global_long, year ~ variable)
ncds_global.p <- data.frame(cbind(ncds_global$year,prop.table(as.matrix(ncds_global[,c(2:11)]), 1)*100))
names(ncds_global.p)[1] <- "year"

ncds_global.p_long <- melt(ncds_global.p, id="year")

ncds_wpr_long <- ncds_region_long %>% filter(who_region=="WPR")
ncds_afr_long <- ncds_region_long %>% filter(who_region=="AFR")
ncds_amr_long <- ncds_region_long %>% filter(who_region=="AMR")
ncds_emr_long <- ncds_region_long %>% filter(who_region=="EMR")
ncds_eur_long <- ncds_region_long %>% filter(who_region=="EUR")
ncds_sea_long <- ncds_region_long %>% filter(who_region=="SEA")

ncds_wpr <- dcast(ncds_wpr_long, year + who_region ~ variable)
ncds_afr <- dcast(ncds_afr_long, year + who_region ~ variable)
ncds_amr <- dcast(ncds_amr_long, year + who_region ~ variable)
ncds_emr <- dcast(ncds_emr_long, year + who_region ~ variable)
ncds_eur <- dcast(ncds_eur_long, year + who_region ~ variable)
ncds_sea <- dcast(ncds_sea_long, year + who_region ~ variable)

ncds_wpr.p <- data.frame(cbind(ncds_wpr[,c(1,2)], prop.table(as.matrix(ncds_wpr[,c(3:12)]), 1)*100))
ncds_afr.p <- data.frame(cbind(ncds_afr[,c(1,2)], prop.table(as.matrix(ncds_afr[,c(3:12)]), 1)*100))
ncds_amr.p <- data.frame(cbind(ncds_amr[,c(1,2)], prop.table(as.matrix(ncds_amr[,c(3:12)]), 1)*100))
ncds_emr.p <- data.frame(cbind(ncds_emr[,c(1,2)], prop.table(as.matrix(ncds_emr[,c(3:12)]), 1)*100))
ncds_eur.p <- data.frame(cbind(ncds_eur[,c(1,2)], prop.table(as.matrix(ncds_eur[,c(3:12)]), 1)*100))
ncds_sea.p <- data.frame(cbind(ncds_sea[,c(1,2)], prop.table(as.matrix(ncds_sea[,c(3:12)]), 1)*100))

ncds_region.p <- rbind(ncds_wpr.p,
                       ncds_afr.p,
                       ncds_amr.p,
                       ncds_emr.p,
                       ncds_eur.p,
                       ncds_sea.p)

ncds_region.p_long <- melt(ncds_region.p,id.vars = c("year", "who_region"))



# plot global NCDs burden

ncds_global.p_long$variable <- factor(ncds_global.p_long$variable, labels=c("Cardiovascular diseases", "Cancers", "Respiratory diseases", "Diabetes & endocrine diseases", "Other NCDs", "Liver disease", "Mental & substance use disorders", "Neurological disorders", "Musculoskeletal disorders", "Digestive diseases"))


p <- ggplot(ncds_global.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "% of global burden from NCDs by cause, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10)) #+ guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="disease_NCDs_global_prop.pdf", width=11,height=6) # write PDF
p
dev.off()


ncds_global_long$value2 <- ncds_global_long$value/1000000

ncds_global_long$variable <- factor(ncds_global_long$variable, labels=c("Cardiovascular diseases", "Cancers", "Respiratory diseases", "Diabetes & endocrine diseases", "Other NCDs", "Liver disease", "Mental & substance use disorders", "Neurological disorders", "Musculoskeletal disorders", "Digestive diseases"))


p <- ggplot(ncds_global_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "Global burden from NCDs by cause, 1990-2016", y = "Number of DALYs per year (million)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10))# + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_NCDs_burden_global.pdf", width=11,height=6) # write PDF
p
dev.off()


# by region
ncds_region.p_long$variable <- factor(ncds_region.p_long$variable, labels=c("Cardiovascular diseases", "Cancers", "Respiratory diseases", "Diabetes & endocrine diseases", "Other NCDs", "Liver disease", "Mental & substance use disorders", "Neurological disorders", "Musculoskeletal disorders", "Digestive diseases"))

p <- ggplot(ncds_region.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region)
p <- p + labs(title= "% of burden from NCDs by cause by WHO region, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="NCDs_burden_region_prop.pdf", width=9,height=8) # write PDF
p
dev.off()

ncds_region_long$value2 <- ncds_region_long$value/100000
ncds_region_long$variable <- factor(ncds_region_long$variable, labels=c("Cardiovascular diseases", "Cancers", "Respiratory diseases", "Diabetes & endocrine diseases", "Other NCDs", "Liver disease", "Mental & substance use disorders", "Neurological disorders", "Musculoskeletal disorders", "Digestive diseases"))

p <- ggplot(ncds_region_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region,scales="free")
p <- p + labs(title= "NCDs burden by cause by WHO region, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="NCDs_burden_region.pdf", width=9,height=8) # write PDF
p
dev.off()


# by country
ncds_long_wpr <- ncds_long %>% filter(who_region=="WPR")

ncds_long_wpr$variable <- factor(ncds_long_wpr$variable, labels=c("Cardiovascular diseases", "Cancers", "Respiratory diseases", "Diabetes & endocrine diseases", "Other NCDs", "Liver disease", "Mental & substance use disorders", "Neurological disorders", "Musculoskeletal disorders", "Digestive diseases"))


ncds_long_wpr$value2 <- ncds_long_wpr$value/100000
ncds_long_wpr$country <- as.character(ncds_long_wpr$country)
ncds_long_wpr$country[ncds_long_wpr$iso3=="BRN"] <- "Brunei Darussalam"
ncds_long_wpr$country[ncds_long_wpr$iso3=="LAO"] <- "Lao PDR"
ncds_long_wpr$country[ncds_long_wpr$iso3=="FSM"] <- "Micronesia (Fed. States of)"
ncds_long_wpr$country[ncds_long_wpr$iso3=="VNM"] <- "Viet Nam"
ncds_long_wpr$country[ncds_long_wpr$iso3=="KOR"] <- "Rep. of Korea"


p <- ggplot(ncds_long_wpr, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~ country, scales="free")
p <- p + labs(title= "NCDs burden by cause by country in WPR, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=9),
               axis.text = element_text(size=6))
p <- p + guides(colour=guide_legend(nrow=3,byrow=TRUE))

pdf(file="NCDs_burden_country_WPR.pdf", width=10,height=13) # write PDF
p
dev.off()

#################################################
#        Communicable and other diseases
#################################################


coms <- read.csv("./GBD/disease-burden-from-communicable-diseases.csv") # data from GDB through Our World in Data
names(coms) <- c("country", "iso3", "year", "mat", "neo", "nut", "mal", "dia", "hiv", "tb", "oth") 

coms <- merge(coms, g_iso3, by="iso3")
coms_long <- melt(coms, id.vars = c("iso3", "country", "year", "who_region"))
coms_region_long <- aggregate(value ~ who_region + year+ variable, data = coms_long, sum, na.rm = TRUE)
coms_global_long <- aggregate(value ~ year + variable, data=coms_region_long, sum, na.rm=T)

# Global burden trend
coms_global <- dcast(coms_global_long, year ~ variable)
coms_global.p <- data.frame(cbind(coms_global$year,prop.table(as.matrix(coms_global[,c(2:9)]), 1)*100))
names(coms_global.p)[1] <- "year"

coms_global.p_long <- melt(coms_global.p, id="year")

coms_wpr_long <- coms_region_long %>% filter(who_region=="WPR")
coms_afr_long <- coms_region_long %>% filter(who_region=="AFR")
coms_amr_long <- coms_region_long %>% filter(who_region=="AMR")
coms_emr_long <- coms_region_long %>% filter(who_region=="EMR")
coms_eur_long <- coms_region_long %>% filter(who_region=="EUR")
coms_sea_long <- coms_region_long %>% filter(who_region=="SEA")

coms_wpr <- dcast(coms_wpr_long, year + who_region ~ variable)
coms_afr <- dcast(coms_afr_long, year + who_region ~ variable)
coms_amr <- dcast(coms_amr_long, year + who_region ~ variable)
coms_emr <- dcast(coms_emr_long, year + who_region ~ variable)
coms_eur <- dcast(coms_eur_long, year + who_region ~ variable)
coms_sea <- dcast(coms_sea_long, year + who_region ~ variable)

coms_wpr.p <- data.frame(cbind(coms_wpr[,c(1,2)], prop.table(as.matrix(coms_wpr[,c(3:10)]), 1)*100))
coms_afr.p <- data.frame(cbind(coms_afr[,c(1,2)], prop.table(as.matrix(coms_afr[,c(3:10)]), 1)*100))
coms_amr.p <- data.frame(cbind(coms_amr[,c(1,2)], prop.table(as.matrix(coms_amr[,c(3:10)]), 1)*100))
coms_emr.p <- data.frame(cbind(coms_emr[,c(1,2)], prop.table(as.matrix(coms_emr[,c(3:10)]), 1)*100))
coms_eur.p <- data.frame(cbind(coms_eur[,c(1,2)], prop.table(as.matrix(coms_eur[,c(3:10)]), 1)*100))
coms_sea.p <- data.frame(cbind(coms_sea[,c(1,2)], prop.table(as.matrix(coms_sea[,c(3:10)]), 1)*100))

coms_region.p <- rbind(coms_wpr.p,
                       coms_afr.p,
                       coms_amr.p,
                       coms_emr.p,
                       coms_eur.p,
                       coms_sea.p)

coms_region.p_long <- melt(coms_region.p,id.vars = c("year", "who_region"))



# plot global Coms burden

coms_global.p_long$variable <- factor(coms_global.p_long$variable, labels=c("Maternal disorders", "Neonatal disorders", "Nutritional deficiencies", "Malaria & NTDs", "Diarrhea, lower respiraotry & infectious diseases", "HIV/AIDS", "Tuberculosis", "Other communicable disease"))


p <- ggplot(coms_global.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "% of global burden from communicable, maternal, neonatal, and nutritional diseases, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10)) #+ guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="disease_coms_global_prop.pdf", width=11,height=6) # write PDF
p
dev.off()


coms_global_long$value2 <- coms_global_long$value/1000000

coms_global_long$variable <- factor(coms_global_long$variable, labels=c("Maternal disorders", "Neonatal disorders", "Nutritional deficiencies", "Malaria & NTDs", "Diarrhea, lower respiraotry & infectious diseases", "HIV/AIDS", "Tuberculosis", "Other communicable disease"))


p <- ggplot(coms_global_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "Global burden from communicable, maternal, neonatal, and nutritional diseases, 1990-2016", y = "Number of DALYs per year (million)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10))# + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_coms_burden_global.pdf", width=11,height=6) # write PDF
p
dev.off()

# by region
coms_region.p_long$variable <- factor(coms_region.p_long$variable, labels=c("Maternal disorders", "Neonatal disorders", "Nutritional deficiencies", "Malaria & NTDs", "Diarrhea, lower respiraotry & infectious diseases", "HIV/AIDS", "Tuberculosis", "Other communicable disease"))

p <- ggplot(coms_region.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region)
p <- p + labs(title= "% of burden from communicable, maternal, neonatal, and nutritional diseases, by WHO region, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="coms_burden_region_prop.pdf", width=9,height=8) # write PDF
p
dev.off()

coms_region_long$value2 <- coms_region_long$value/100000
coms_region_long$variable <- factor(coms_region_long$variable, labels=c("Maternal disorders", "Neonatal disorders", "Nutritional deficiencies", "Malaria & NTDs", "Diarrhea, lower respiraotry & infectious diseases", "HIV/AIDS", "Tuberculosis", "Other communicable disease"))

p <- ggplot(coms_region_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region,scales="free")
p <- p + labs(title= "Burden from communicable, maternal, neonatal, and nutritional diseases, by WHO region, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="coms_burden_region.pdf", width=9,height=8) # write PDF
p
dev.off()

# by country
coms_long_wpr <- coms_long %>% filter(who_region=="WPR")

coms_long_wpr$variable <- factor(coms_long_wpr$variable, labels=c("Maternal disorders", "Neonatal disorders", "Nutritional deficiencies", "Malaria & NTDs", "Diarrhea, lower respiraotry & infectious diseases", "HIV/AIDS", "Tuberculosis", "Other communicable disease"))


coms_long_wpr$value2 <- coms_long_wpr$value/100000
coms_long_wpr$country <- as.character(coms_long_wpr$country)
coms_long_wpr$country[coms_long_wpr$iso3=="BRN"] <- "Brunei Darussalam"
coms_long_wpr$country[coms_long_wpr$iso3=="LAO"] <- "Lao PDR"
coms_long_wpr$country[coms_long_wpr$iso3=="FSM"] <- "Micronesia (Fed. States of)"
coms_long_wpr$country[coms_long_wpr$iso3=="VNM"] <- "Viet Nam"
coms_long_wpr$country[coms_long_wpr$iso3=="KOR"] <- "Rep. of Korea"


p <- ggplot(coms_long_wpr, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~ country, scales="free")
p <- p + labs(title= "Burden from communicable, maternal, neonatal, and nutritional diseases, by country in WPR, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=9),
               axis.text = element_text(size=6))
p <- p + guides(colour=guide_legend(nrow=3,byrow=TRUE))

pdf(file="coms_burden_country_WPR.pdf", width=10,height=13) # write PDF
p
dev.off()


















