#################################################
#        Total disease burden by age group
#################################################

tot <- read.csv("./GBD/disease-burden-by-age.csv") # data from GDB through Our World in Data
names(tot) <- c("country", "iso3", "year", "u5", "_514", "_1549", "_5069", "_70") 

tot <- merge(tot, g_iso3, by="iso3")
tot_long <- melt(tot, id.vars = c("iso3", "country", "year", "who_region"))
tot_region_long <- aggregate(value ~ who_region + year+ variable, data = tot_long, sum, na.rm = TRUE)
tot_global_long <- aggregate(value ~ year + variable, data=tot_region_long, sum, na.rm=T)

# Global burden trend
tot_global <- dcast(tot_global_long, year ~ variable)
tot_global.p <- data.frame(cbind(tot_global$year,prop.table(as.matrix(tot_global[,c(2:6)]), 1)*100))
names(tot_global.p)[1] <- "year"

tot_global.p_long <- melt(tot_global.p, id="year")

tot_wpr_long <- tot_region_long %>% filter(who_region=="WPR")
tot_afr_long <- tot_region_long %>% filter(who_region=="AFR")
tot_amr_long <- tot_region_long %>% filter(who_region=="AMR")
tot_emr_long <- tot_region_long %>% filter(who_region=="EMR")
tot_eur_long <- tot_region_long %>% filter(who_region=="EUR")
tot_sea_long <- tot_region_long %>% filter(who_region=="SEA")

tot_wpr <- dcast(tot_wpr_long, year + who_region ~ variable)
tot_afr <- dcast(tot_afr_long, year + who_region ~ variable)
tot_amr <- dcast(tot_amr_long, year + who_region ~ variable)
tot_emr <- dcast(tot_emr_long, year + who_region ~ variable)
tot_eur <- dcast(tot_eur_long, year + who_region ~ variable)
tot_sea <- dcast(tot_sea_long, year + who_region ~ variable)

tot_wpr.p <- data.frame(cbind(tot_wpr[,c(1,2)], prop.table(as.matrix(tot_wpr[,c(3:7)]), 1)*100))
tot_afr.p <- data.frame(cbind(tot_afr[,c(1,2)], prop.table(as.matrix(tot_afr[,c(3:7)]), 1)*100))
tot_amr.p <- data.frame(cbind(tot_amr[,c(1,2)], prop.table(as.matrix(tot_amr[,c(3:7)]), 1)*100))
tot_emr.p <- data.frame(cbind(tot_emr[,c(1,2)], prop.table(as.matrix(tot_emr[,c(3:7)]), 1)*100))
tot_eur.p <- data.frame(cbind(tot_eur[,c(1,2)], prop.table(as.matrix(tot_eur[,c(3:7)]), 1)*100))
tot_sea.p <- data.frame(cbind(tot_sea[,c(1,2)], prop.table(as.matrix(tot_sea[,c(3:7)]), 1)*100))

tot_region.p <- rbind(tot_wpr.p,
                       tot_afr.p,
                       tot_amr.p,
                       tot_emr.p,
                       tot_eur.p,
                       tot_sea.p)

tot_region.p_long <- melt(tot_region.p,id.vars = c("year", "who_region"))


# plot global tot burden

tot_global.p_long$variable <- factor(tot_global.p_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


p <- ggplot(tot_global.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "% of global disease burden by age, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10)) #+ guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="disease_tot_age_global_prop.pdf", width=10,height=6) # write PDF
p
dev.off()


tot_global_long$value2 <- tot_global_long$value/1000000

tot_global_long$variable <- factor(tot_global_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


p <- ggplot(tot_global_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "Global disease burden by age, 1990-2016", y = "Number of DALYs per year (million)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10))# + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_tot_age_burden_global.pdf", width=10,height=6) # write PDF
p
dev.off()

# by region
tot_region.p_long$variable <- factor(tot_region.p_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))

p <- ggplot(tot_region.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region)
p <- p + labs(title= "% of disease burden by age, by WHO region, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=1,byrow=TRUE))
p 

pdf(file="tot_age_burden_region_prop.pdf", width=9,height=8) # write PDF
p
dev.off()

tot_region_long$value2 <- tot_region_long$value/100000
tot_region_long$variable <- factor(tot_region_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))

p <- ggplot(tot_region_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region,scales="free")
p <- p + labs(title= "Disease burden by age, by WHO region, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=1,byrow=TRUE))
p 

pdf(file="tot_age_burden_region.pdf", width=9,height=8) # write PDF
p
dev.off()

# by country
tot_long_wpr <- tot_long %>% filter(who_region=="WPR")

tot_long_wpr$variable <- factor(tot_long_wpr$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


tot_long_wpr$value2 <- tot_long_wpr$value/100000
tot_long_wpr$country <- as.character(tot_long_wpr$country)
tot_long_wpr$country[tot_long_wpr$iso3=="BRN"] <- "Brunei Darussalam"
tot_long_wpr$country[tot_long_wpr$iso3=="LAO"] <- "Lao PDR"
tot_long_wpr$country[tot_long_wpr$iso3=="FSM"] <- "Micronesia (Fed. States of)"
tot_long_wpr$country[tot_long_wpr$iso3=="VNM"] <- "Viet Nam"
tot_long_wpr$country[tot_long_wpr$iso3=="KOR"] <- "Rep. of Korea"


p <- ggplot(tot_long_wpr, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~ country, scales="free")
p <- p + labs(title= "Disease burden by age, by country in WPR, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=9),
               axis.text = element_text(size=6))
p <- p + guides(colour=guide_legend(nrow=1,byrow=TRUE))

pdf(file="tot_age_burden_country_WPR.pdf", width=10,height=13) # write PDF
p
dev.off()

#################################################
#        NCDs burden by age group
#################################################

ncds <- read.csv("./GBD/disease-burden-from-ncds-by-age.csv") # data from GDB through Our World in Data
names(ncds) <- c("country", "iso3", "year", "u5", "_514", "_1549", "_5069", "_70") 

ncds <- merge(ncds, g_iso3, by="iso3")
ncds_long <- melt(ncds, id.vars = c("iso3", "country", "year", "who_region"))
ncds_region_long <- aggregate(value ~ who_region + year+ variable, data = ncds_long, sum, na.rm = TRUE)
ncds_global_long <- aggregate(value ~ year + variable, data=ncds_region_long, sum, na.rm=T)

# Global burden trend
ncds_global <- dcast(ncds_global_long, year ~ variable)
ncds_global.p <- data.frame(cbind(ncds_global$year,prop.table(as.matrix(ncds_global[,c(2:6)]), 1)*100))
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

ncds_wpr.p <- data.frame(cbind(ncds_wpr[,c(1,2)], prop.table(as.matrix(ncds_wpr[,c(3:7)]), 1)*100))
ncds_afr.p <- data.frame(cbind(ncds_afr[,c(1,2)], prop.table(as.matrix(ncds_afr[,c(3:7)]), 1)*100))
ncds_amr.p <- data.frame(cbind(ncds_amr[,c(1,2)], prop.table(as.matrix(ncds_amr[,c(3:7)]), 1)*100))
ncds_emr.p <- data.frame(cbind(ncds_emr[,c(1,2)], prop.table(as.matrix(ncds_emr[,c(3:7)]), 1)*100))
ncds_eur.p <- data.frame(cbind(ncds_eur[,c(1,2)], prop.table(as.matrix(ncds_eur[,c(3:7)]), 1)*100))
ncds_sea.p <- data.frame(cbind(ncds_sea[,c(1,2)], prop.table(as.matrix(ncds_sea[,c(3:7)]), 1)*100))

ncds_region.p <- rbind(ncds_wpr.p,
                      ncds_afr.p,
                      ncds_amr.p,
                      ncds_emr.p,
                      ncds_eur.p,
                      ncds_sea.p)

ncds_region.p_long <- melt(ncds_region.p,id.vars = c("year", "who_region"))


# plot global ncds burden

ncds_global.p_long$variable <- factor(ncds_global.p_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


p <- ggplot(ncds_global.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "% of burden from NCDs by age, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10)) #+ guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="disease_ncds_age_global_prop.pdf", width=10,height=6) # write PDF
p
dev.off()


ncds_global_long$value2 <- ncds_global_long$value/1000000

ncds_global_long$variable <- factor(ncds_global_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


p <- ggplot(ncds_global_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "Global burden from NCDs by age, 1990-2016", y = "Number of DALYs per year (million)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10))# + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_ncds_age_burden_global.pdf", width=10,height=6) # write PDF
p
dev.off()

# by region
ncds_region.p_long$variable <- factor(ncds_region.p_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))

p <- ggplot(ncds_region.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region)
p <- p + labs(title= "% of burden from NCDs by age, by WHO region, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=1,byrow=TRUE))
p 

pdf(file="ncds_age_burden_region_prop.pdf", width=9,height=8) # write PDF
p
dev.off()

ncds_region_long$value2 <- ncds_region_long$value/100000
ncds_region_long$variable <- factor(ncds_region_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))

p <- ggplot(ncds_region_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region,scales="free")
p <- p + labs(title= "NCDs burden by age, by WHO region, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=1,byrow=TRUE))
p 

pdf(file="ncds_age_burden_region.pdf", width=9,height=8) # write PDF
p
dev.off()

# by country
ncds_long_wpr <- ncds_long %>% filter(who_region=="WPR")

ncds_long_wpr$variable <- factor(ncds_long_wpr$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


ncds_long_wpr$value2 <- ncds_long_wpr$value/100000
ncds_long_wpr$country <- as.character(ncds_long_wpr$country)
ncds_long_wpr$country[ncds_long_wpr$iso3=="BRN"] <- "Brunei Darussalam"
ncds_long_wpr$country[ncds_long_wpr$iso3=="LAO"] <- "Lao PDR"
ncds_long_wpr$country[ncds_long_wpr$iso3=="FSM"] <- "Micronesia (Fed. States of)"
ncds_long_wpr$country[ncds_long_wpr$iso3=="VNM"] <- "Viet Nam"
ncds_long_wpr$country[ncds_long_wpr$iso3=="KOR"] <- "Rep. of Korea"


p <- ggplot(ncds_long_wpr, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~ country, scales="free")
p <- p + labs(title= "NCDs burden by age, by country in WPR, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=9),
               axis.text = element_text(size=6))
p <- p + guides(colour=guide_legend(nrow=1,byrow=TRUE))

pdf(file="ncds_age_burden_country_WPR.pdf", width=10,height=13) # write PDF
p
dev.off()

#################################################
#    Communicable diasese burden by age group
#################################################

coms <- read.csv("./GBD/disease-burden-from-communicable-diseases-by-age.csv") # data from GDB through Our World in Data
names(coms) <- c("country", "iso3", "year", "u5", "_514", "_1549", "_5069", "_70") 

coms <- merge(coms, g_iso3, by="iso3")
coms_long <- melt(coms, id.vars = c("iso3", "country", "year", "who_region"))
coms_region_long <- aggregate(value ~ who_region + year+ variable, data = coms_long, sum, na.rm = TRUE)
coms_global_long <- aggregate(value ~ year + variable, data=coms_region_long, sum, na.rm=T)

# Global burden trend
coms_global <- dcast(coms_global_long, year ~ variable)
coms_global.p <- data.frame(cbind(coms_global$year,prop.table(as.matrix(coms_global[,c(2:6)]), 1)*100))
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

coms_wpr.p <- data.frame(cbind(coms_wpr[,c(1,2)], prop.table(as.matrix(coms_wpr[,c(3:7)]), 1)*100))
coms_afr.p <- data.frame(cbind(coms_afr[,c(1,2)], prop.table(as.matrix(coms_afr[,c(3:7)]), 1)*100))
coms_amr.p <- data.frame(cbind(coms_amr[,c(1,2)], prop.table(as.matrix(coms_amr[,c(3:7)]), 1)*100))
coms_emr.p <- data.frame(cbind(coms_emr[,c(1,2)], prop.table(as.matrix(coms_emr[,c(3:7)]), 1)*100))
coms_eur.p <- data.frame(cbind(coms_eur[,c(1,2)], prop.table(as.matrix(coms_eur[,c(3:7)]), 1)*100))
coms_sea.p <- data.frame(cbind(coms_sea[,c(1,2)], prop.table(as.matrix(coms_sea[,c(3:7)]), 1)*100))

coms_region.p <- rbind(coms_wpr.p,
                       coms_afr.p,
                       coms_amr.p,
                       coms_emr.p,
                       coms_eur.p,
                       coms_sea.p)

coms_region.p_long <- melt(coms_region.p,id.vars = c("year", "who_region"))


# plot global coms burden

coms_global.p_long$variable <- factor(coms_global.p_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


p <- ggplot(coms_global.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "% of burden from communicable, maternal, neonatal, and nutritional diseases by age, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10)) #+ guides(colour=guide_legend(nrow=4,byrow=TRUE))
p 

pdf(file="disease_coms_age_global_prop.pdf", width=10,height=6) # write PDF
p
dev.off()


coms_global_long$value2 <- coms_global_long$value/1000000

coms_global_long$variable <- factor(coms_global_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


p <- ggplot(coms_global_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line()
# p <- p + geom_smooth(aes(x=year, y=value, fill=variable),  method = "loess",  alpha=0.16, size=0.4)
p <- p + labs(title= "Global burden from communicable, maternal, neonatal, and nutritional diseases by age, 1990-2016", y = "Number of DALYs per year (million)", x="", col="") 
p <- p + theme(legend.position="right",
               legend.text = element_text(size=10))# + guides(colour=guide_legend(nrow=2,byrow=TRUE))
p 

pdf(file="disease_coms_age_burden_global.pdf", width=10,height=6) # write PDF
p
dev.off()

# by region
coms_region.p_long$variable <- factor(coms_region.p_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))

p <- ggplot(coms_region.p_long, aes(x=year, y=value, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region)
p <- p + labs(title= "% of burden from communicable, maternal, neonatal, and nutritional diseases by age, by WHO region, 1990-2016", y = "Percentage (%)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=1,byrow=TRUE))
p 

pdf(file="coms_age_burden_region_prop.pdf", width=9,height=8) # write PDF
p
dev.off()

coms_region_long$value2 <- coms_region_long$value/100000
coms_region_long$variable <- factor(coms_region_long$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))

p <- ggplot(coms_region_long, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~who_region,scales="free")
p <- p + labs(title= "Burden from communicable, maternal, neonatal, and nutritional diseases by age, by WHO region, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=10)) + guides(colour=guide_legend(nrow=1,byrow=TRUE))
p 

pdf(file="coms_age_burden_region.pdf", width=9,height=8) # write PDF
p
dev.off()

# by country
coms_long_wpr <- coms_long %>% filter(who_region=="WPR")

coms_long_wpr$variable <- factor(coms_long_wpr$variable, labels=c("Under-5s", "5-14 years old", "15-49 years old", "50-69 years old", "70+ years old"))


coms_long_wpr$value2 <- coms_long_wpr$value/100000
coms_long_wpr$country <- as.character(coms_long_wpr$country)
coms_long_wpr$country[coms_long_wpr$iso3=="BRN"] <- "Brunei Darussalam"
coms_long_wpr$country[coms_long_wpr$iso3=="LAO"] <- "Lao PDR"
coms_long_wpr$country[coms_long_wpr$iso3=="FSM"] <- "Micronesia (Fed. States of)"
coms_long_wpr$country[coms_long_wpr$iso3=="VNM"] <- "Viet Nam"
coms_long_wpr$country[coms_long_wpr$iso3=="KOR"] <- "Rep. of Korea"


p <- ggplot(coms_long_wpr, aes(x=year, y=value2, colour=variable))
p <- p + geom_line() + facet_wrap(~ country, scales="free")
p <- p + labs(title= "Burden from communicable, maternal, neonatal, and nutritional diseases by age, by country in WPR, 1990-2016", y = "Number of DALYs per year (x 100,000)", x="", col="") 
p <- p + theme(legend.position="bottom",
               legend.text = element_text(size=9),
               axis.text = element_text(size=6))
p <- p + guides(colour=guide_legend(nrow=1,byrow=TRUE))

pdf(file="coms_age_burden_country_WPR.pdf", width=10,height=13) # write PDF
p
dev.off()










