## Plot without legend 


## Loop
######## combined life, hlife and projected life in one plot, gdp per capita (projected and past)
i <- dfl2$country[1500]
countryname <- unique(dfl$country)
for(i in countryname){
  
  dfs2 <- dfl2 %>% filter(country==i) %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
  p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
  p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
  p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2035, by = 5))
  p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
  p <- p + theme(
    legend.position="none", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
#  p <- p + guides(fill=guide_legend(ncol=2))
  
  dfs <- gdp.ppp %>% filter(country==i) 
  p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
  p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
  p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
  p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 5))
  p2 <- p2 + labs(title ="", x="", y="USD")
  p2 <- p2 + theme(
    legend.position="none", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
#  p2 <- p2 + guides(colour=guide_legend(nrow=2))
  
  mypath <- file.path("./figure/without_legend/full", paste("indicator_combined",dfs$income, "_", dfs$country,".pdf",sep=""))
  pdf(file=mypath, width=9, height=3.5) # width=10,height=14
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
  p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2035, by = 5))
  p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
  p <- p + theme(
    legend.position="none", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
#  p <- p + guides(colour=guide_legend(ncol=1))
  
  dfs <- gdp.ppp %>% filter(country==i) %>% filter(year<2023)
  p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
  p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
  p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
  p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 5))
  p2 <- p2 + labs(title ="", x="", y="USD")
  p2 <- p2 + theme(
    legend.position="none", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
#  p2 <- p2 + guides(colour=guide_legend(nrow=2))
  
  mypath <- file.path("./figure/without_legend/without_HLE", paste("indicator_combined",dfs$income, "_", dfs$country,"_without_HLE.pdf",sep=""))
  pdf(file=mypath, width=9, height=3.5) # width=10,height=14
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
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2035, by = 5))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(
  legend.position="none", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
#p <- p + guides(colour=guide_legend(ncol=1))

dfs <- gdp.ppp %>% filter(country==i) 
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 5))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(
  legend.position="none", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
#p2 <- p2 + guides(colour=guide_legend(nrow=2))

mypath <- file.path("./figure/without_legend", paste("indicator_combined",dfs$income, "_", "Palau_without_HLEP.pdf",sep=""))
pdf(file=mypath, width=9, height=3.5) 
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
  p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2035, by = 5))
  p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
  p <- p + theme(
    legend.position="none", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
#  p <- p + guides(colour=guide_legend(ncol=2))
  
  dfs <- gdp.ppp %>% filter(country==i) 
  p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
  p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
  p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
  p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 5))
  p2 <- p2 + labs(title ="", x="", y="USD")
  p2 <- p2 + theme(
    legend.position="none", legend.title = element_blank(),
    axis.text.x =  element_text(size = 8),
    axis.title.y =  element_text(size = 10),
    axis.ticks.x=element_blank())
#  p2 <- p2 + guides(colour=guide_legend(nrow=2))
  
  mypath <- file.path("./figure/without_HLE", paste("indicator_combined",dfs$income, "_", dfs$country, "_without_HLE.pdf",sep=""))
  pdf(file=mypath, width=9, height=3.5) 
  print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
  dev.off()
}
## American Samoa and Northern Mariana
dfs2 <- dfl2 %>% filter(country=="American Samoa") %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
#p <- p + scale_colour_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
#p <- p + scale_fill_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2035, by = 5))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(
  legend.position="none", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
#p <- p + guides(colour=guide_legend(ncol=2))

dfs <- gdp.ppp %>% filter(country=="American Samoa") %>% filter(year<2023)
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 5))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(
  legend.position="none", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
# p2 <- p2 + guides(colour=guide_legend(nrow=2))

mypath <- file.path("./figure/without_legend", paste("indicator_combined",dfs$income, "_"," American Samoa_without_HLEP.pdf",sep=""))
pdf(file=mypath, width=9, height=3.5) 
print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
dev.off()

## Northern Mariana Islands
dfs2 <- dfl2 %>% filter(country=="Northern Mariana Islands") %>% filter(!variable=="GDP/capita (Constant 2010 US$)") %>% filter(year<2036)
p <- ggplot(data=dfs2, aes(x=year, y=value, group=variable, colour=variable)) 
#p <- p + scale_colour_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_smooth(aes(x=year, y=value,  fill=variable), method = "lm", alpha=0.3, size=0.4)
#p <- p + scale_fill_manual(values = c(mycols[1], mycols[7]))
p <- p + geom_point(size=1)  + scale_x_continuous(breaks= seq(2000, 2035, by = 5))
p <- p + labs(title =paste(dfs2$country[1]), x="", y="Life expectancy at birth (in years)")
p <- p + theme(
  legend.position="none", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
# p <- p + guides(colour=guide_legend(ncol=2))

dfs <- gdp.ppp %>% filter(country=="Northern Mariana Islands") %>% filter(year<2023)
p2 <- ggplot(data=dfs, aes(x=year, y=value,group=variable, colour=variable)) 
p2 <- p2 + geom_point(aes(x=year, y=value, group=variable, colour=variable), stat="identity") 
p2 <- p2 + geom_smooth(aes(x=year, y=value, fill=variable),  method = "lm",  alpha=0.3, size=0.4)
p2 <- p2 + geom_point(size=0.5) + scale_x_continuous(breaks= seq(2000, 2023, by = 5))
p2 <- p2 + labs(title ="", x="", y="USD")
p2 <- p2 + theme(
  legend.position="none", legend.title = element_blank(),
  axis.text.x =  element_text(size = 8),
  axis.title.y =  element_text(size = 10),
  axis.ticks.x=element_blank())
# p2 <- p2 + guides(colour=guide_legend(nrow=2))

mypath <- file.path("./figure/without_legend", paste("indicator_combined",dfs$income, "_","Northern Mariana Islands_without_HLEP.pdf",sep=""))
pdf(file=mypath, width=9, height=3.5) 
print(grid.arrange(p, p2, ncol=2, widths=c(1.3, 1)))
dev.off()
