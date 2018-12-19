## plot to see correlation between life expectancy and health life expectancy and GDP

dfl16 <- dfl %>% filter(year==2016)
head(dfl16)
df16 <- dcast(dfl16, country + short.country +iso3  + income ~  variable)
names(df16)[5] <- "life"
names(df16)[6] <- "hlife"
names(df16)[7] <- "gdp"

class(df16$income)
df16$income <- factor(df16$income, levels=c("High income", "Upper middle income", "Lower middle income"))
  
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

pdf(file="scatterplot_life_hlife_gdp.pdf", width=10,height=6) # write PDF
p
dev.off()


p <- ggplot(df16, aes(x = gdp, y = life, label = country, colour=income))
p <- p + geom_point() 
p <- p + geom_text_repel(show.legend = FALSE, size=3)
p <- p + geom_smooth(aes(x=gdp, y=life, fill=income),  method = "lm",  alpha=0.16, size=0.4)
p <- p + scale_x_log10(breaks=pretty_breaks())
p <- p + labs(title= "Relationship between GDP per capita and life expectancy in WPR countries, 2016", x = "GDP per capita (USD) - log scale", y="Life expectancy at birth (years)", col="Income classification") 
p <- p + guides(fill=FALSE)
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9),
               axis.text.x = element_text(size = 8, angle=30, hjust = 1, vjust = 0.9))
p

pdf(file="scatterplot_gdp_life.pdf", width=10,height=6) # write PDF
p
dev.off()



## calculate gap between life and hlife
df16$gap.life <-df16$life - df16$hlife

## plot 
p <- ggplot(df16, aes(x = life, y = gap.life, label = country, colour=income))
p <- p + geom_point() 
p <- p + geom_text_repel(show.legend = FALSE, size=3)
p <- p + geom_hline(yintercept = mean(df16$gap.life, na.rm=T), linetype=2, color="#787C7D") # grey50
p <- p + geom_vline(xintercept = mean(df16$life, na.rm=T), linetype=2, color="#787C7D") # grey50
#p <- p + geom_smooth(aes(x=life, y=gap.life, fill=income),  method = "lm",  alpha=0.16, size=0.4)
p <- p + labs(title= "Relationship between life expectancy and equivalent of healthy years lost in WPR countries, 2016", x = "Life expectancy at birth (years)", y="Equivalent of healthy years lost (years)", col="Income classification") 
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9))
p <- p + annotate("text", x=76, y=6.6, label= "Average", size=3, colour="#787C7D")
p <- p + annotate("text", x=64.5, y=8.4, label= "Average", size=3, colour="#787C7D")

p

pdf(file="scatterplot_life_gap.life.pdf", width=10,height=6) # write PDF
p
dev.off()


##
p <- ggplot(df16, aes(x=reorder(country,-life), y=life, fill=income))
p <- p + geom_bar(stat="identity", alpha=0.7) 
p <- p + labs(title= "Life expectancy at birth in WPR countries, 2016", x = "", y="Life expectancy at birth (years)", fill="Income classification") 
p1 <- p + theme(#legend.title= element_text(size=9),
               plot.title = element_text(size = 10),
               axis.title=element_text(size=7),
               axis.text.x = element_text(size = 5.5, angle=35, hjust = 1, vjust = 1),
               legend.position = "none")
p1

p <- ggplot(df16, aes(x=reorder(country,-hlife), y=hlife, fill=income))
p <- p + geom_bar(stat="identity", alpha=0.7) 
p <- p + labs(title= "Healthy life expectancy at birth in WPR countries, 2016", x = "", y="Healthy life expectancy at birth (years)", fill="Income classification") 
p2 <- p + theme(#legend.title= element_text(size=9),
               plot.title = element_text(size = 10),
               axis.title=element_text(size=7),
               axis.text.x = element_text(size = 5.5, angle=35, hjust = 1, vjust = 1),
               legend.position = "none")
p2

p <- ggplot(df16, aes(x=reorder(country, -gap.life), y=gap.life, fill=income))
p <- p + geom_bar(stat="identity", alpha=0.7) 
p <- p + ylim(0,10)
p <- p + labs(title= "Equivalent of healthy years lost in WPR countries, 2016", x = "", y="Equivalent of healthy years lost (years)", fill="Income classification") 
p3 <- p + theme(#legend.title= element_text(size=9),
                plot.title = element_text(size = 10),
                axis.title=element_text(size=7),
                axis.text.x = element_text(size = 5.5, angle=35, hjust = 1, vjust = 1),
                legend.position = "none")
p3

p <- ggplot(df16, aes(x=reorder(country, -gdp), y=gdp, fill=income))
p <- p + geom_bar(stat="identity", alpha=0.7) 
p <- p + labs(title= "GDP per capita in WPR countries, 2016", x = "", y="GDP per capita (USD)", fill="Income classification") 
p4 <- p + theme(#legend.title= element_text(size=9),
                plot.title = element_text(size = 10),
                axis.title=element_text(size=7),
                axis.text.x = element_text(size = 5.5, angle=35, hjust = 1, vjust = 1),
                legend.position = "none")
p4

pdf(file="barplot_all.pdf", width=10,height=7) # write PDF
print(grid.arrange(p1, p2, p3, p4, ncol=2))
dev.off()

##
# p <- ggplot(df16, aes(x=reorder(country, -gdp), y=gdp, fill=income))
# p <- p + geom_bar(stat="identity") 
# p <- p + labs(title= "GDP per capita in WPR countries, 2016", x = "", y="GDP per capita (USD)", fill="Income classification") 
# p <- p + theme(#legend.title= element_text(size=9),
#   plot.title = element_text(size = 10),
#   axis.title=element_text(size=7),
#   axis.text.x = element_text(size = 5, angle=30, hjust = 1, vjust = 1),
#   legend.position = "top")
# p
# pdf(file="bar_gdp_legend_extract.pdf", width=9,height=6) # write PDF
# p
# dev.off()


## table
life.m16 <- life.m %>% filter(year==2016)
life.f16 <- life.f %>% filter(year==2016)
pop65.16 <- pop65 %>% filter(year==2016)

names(life.m16)[5] <- "iso3"
names(life.f16)[5] <- "iso3"
names(pop65.16)[5] <- "iso3"

tab16 <- merge(df16, life.m16[,c(5,3)], by="iso3", all=T)
tab16 <- merge(tab16, life.f16[,c(5,3)], by="iso3", all=T)
tab16 <- merge(tab16, pop65.16[,c(5,3)], by="iso3", all=T)

# reorder variable
tab16 <- tab16[,c(2, 4, 5, 9,10,6,8, 11,7)]
# remove "income" from the text
tab16$income <- as.character(tab16$income)
tab16$income <- gsub(" income","",tab16$income)
# reorder country name
tab16 <- tab16[order(tab16$country),]

write.csv(tab16, "data_table_2018.csv",row.names = F)



### additonal scatterplot
# 
# p <- ggplot(tab16, aes(x = hlife, y = pop65, label = country, colour=income, size=gdp))
# p <- p + geom_point(alpha=0.6)
# p <- p + geom_text_repel(show.legend = FALSE, size=3)
# p <- p + geom_smooth(aes(x=hlife, y=pop65, fill=income),  method = "lm",  alpha=0.16, size=0.4)
# p



p <- ggplot(tab16, aes(x = pop65, y = hlife, label = country, colour=income, size=gdp))
p <- p + geom_point(alpha=0.6)
p <- p + geom_text_repel(show.legend = FALSE, size=3)
p <- p + geom_smooth(aes(x=pop65, y=hlife, fill=income),  method = "lm",  alpha=0.16, size=0.4)
p <- p + labs(title= "Relationship between elderly population and healthy life expectancy in WPR countries, 2016", y = "Healthy life expectancy at birth (years)", x="Population ages 65 and above (% of total)", size="GDP per capita (USD)", col="Income classification")
p <- p +  guides(fill=FALSE)
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9))
p

pdf(file="scatterplot_pop65_hlife_gdp.pdf", width=10,height=6) # write PDF
p
dev.off()


p <- ggplot(tab16, aes(x = pop65, y = gap.life, label = country, colour=income, size=gdp))
p <- p + geom_point(alpha=0.6)
p <- p + geom_text_repel(show.legend = FALSE, size=3)
p <- p + labs(title= "Relationship between size of elderly population and equivalent of healthy years lost in WPR countries, 2016", y = "Equivalent of healthy years lost (years)", x="Population ages 65 and above (% of total)", size="GDP per capita (USD)", col="Income classification")
p <- p + theme(legend.title= element_text(size=9),
               plot.title = element_text(size = 11.5),
               axis.title=element_text(size=9))
p

pdf(file="scatterplot_pop65_gap.life_gdp.pdf", width=10,height=6) # write PDF
p
dev.off()






##
p <- ggplot(tab16, aes(x = pop65, y = hlife, label = country, colour=income, size=gdp))
p <- p + geom_point(alpha=0.6)
p <- p + geom_text_repel(show.legend = FALSE, size=3)
p <- p + geom_smooth(aes(x=pop65, y=hlife, fill=income),  method = "lm",  alpha=0.16, size=0.4)
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

pdf(file="scatterplot_life_hlife_gdp.pdf", width=10,height=6) # write PDF
p
dev.off()




