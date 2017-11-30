#Data on the Rocks
#Science Funding
#April 2017


#REQUIRED PACKAGES
library(readxl) #package to import xls files directly
library(pastecs) #summary stats function stat.desc
library(ggplot2)
library(lubridate) #data manipulation with date
library(dplyr) #data wrangling

#CUSTOM FUNCTIONS
Sp.Desc <- function(data)
{
  data.num <- data.frame(Dummy=rep(NA,dim(data)[1])) #Make a dummy data.frame
  data.cat <- data.frame(Dummy=rep(NA,dim(data)[1]))
  #separate categorical from numerical data
  for(i in 1:dim(data)[2]){
    if(!is.na(stat.desc(data[i])["mean",])){#if R can compute a mean then add to data.num
      data.num <- cbind(data.num, data[i])
    }
    else{
      data.cat <- cbind(data.cat, data[i])
    }
  }
  #Delete dummy variable
  data.num$Dummy <- NULL
  data.cat$Dummy <- NULL
  
  #Print Numerical results
  if(dim(data.num)[2]>0) {
    print(t(stat.desc(data.num))[,-c(2,3,6,7,11,14)])
    cat(noquote(""), sep="\n\n")
  }
  
  #Print categorical results
  if(dim(data.cat)[2]>0) {
    for(j in 1:dim(data.cat)[2]){
      cat(noquote(names(data.cat[j])))
      print(table(data.cat[j]))
    }
  }
}

DoR.Theme <- function(axis.text.size=18, axis.title.size=18, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}


#DATA IMPORT----

#Inflation Data
monthly_cpi <- read.csv("C:/Users/sbuja/Documents/Data on the Rocks/Science Funding/CPIAUCSL.csv", header = TRUE)
monthly_cpi$Year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(Year) %>% summarize(cpi = mean(CPIAUCSL))
yearly_cpi <- yearly_cpi[-71,] #delete 2017 since only 1 month of data
#only care about post1994
yearly_cpi <- subset(yearly_cpi, Year >=1994)

#average yearly CPI
yearly_cpi$cpi.lag <- lag(yearly_cpi$cpi)
yearly_cpi$cpi.mult  <- yearly_cpi$cpi/yearly_cpi$cpi.lag
inf.med <- median(yearly_cpi$cpi/yearly_cpi$cpi.lag, na.rm=T)
inf.med
#median CPI multiplier = 1.023178

#project out to 2017 and 2018
yearly_cpi <- rbind(yearly_cpi, 
                    c(2017,yearly_cpi$cpi[23]*inf.med,NA,NA),
                    c(2018,yearly_cpi$cpi[23]*inf.med^2,NA,NA))

ggplot(yearly_cpi,aes(x=Year,y=cpi)) + geom_line()

#adjustment factor for 2016 dollars
yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$Year == 2016]
View(yearly_cpi)


#NIH Funding Data
NIH <- read_excel("C:/Users/sbuja/Documents/Data on the Rocks/Science Funding/NIH.Budget.xlsx")
View(NIH)
Sp.Desc(NIH)

#merge CPI data
NIH <- merge(NIH, yearly_cpi)

NIH$Budget.InfAdj <- NIH$Budget / NIH$adj_factor
NIH$ARRA.Inc.InfAdj <- NIH$ARRA.Inc / NIH$adj_factor
NIH$Request.InfAdj <- NIH$Request / NIH$adj_factor
NIH$Trump.InfAdj <- NIH$Trump / NIH$adj_factor

#GRAPHING DATA----
colours <- c("#228B22", "#e67300", "#B01D03")
NIH.plot <- ggplot(data=NIH, aes(x=Year)) + 
  geom_line(aes(y=Budget.InfAdj), colour=colours[1], size=2) + 
  annotate("text", label="NIH Operating\nBudget", x=1995, y=34, colour=colours[1], size=8, fontface="bold", hjust=0, vjust=0) + 
  #annotate("text", label="Requested", x=2017, y=34, colour=colours[1], size=6, fontface="bold", hjust=1, vjust=0, alpha=.5) + 
  geom_line(aes(y=Request.InfAdj), colour=colours[1], size=2, alpha=.5) + 
  geom_line(aes(y=ARRA.Inc.InfAdj), colour=colours[2], size=2) + 
  annotate("text", label="American Recovery\nand Reinvestment Act", x=2009.5, y=41, colour=colours[2], size=6, fontface="bold", hjust=0.5, vjust=0) + 
  geom_path(aes(y=Trump.InfAdj), arrow=arrow(length=unit(0.8,"cm")), colour=colours[3], size=3) + 
  annotate("text", label="Trump's Proposal", x=2017, y=23, colour=colours[3], size=8, fontface="bold", hjust=1, vjust=0) + 
  scale_x_continuous("Fiscal Year", limits=c(1994,2020), breaks=c(seq(1994,2018,4)), expand = c(0,0)) +
  scale_y_continuous("Billion Dollars (in 2016 USD)", limits=c(10,45), breaks=seq(15,45,5), expand = c(0,0)) +
  ggtitle("Biomedical Research Funding") +
  DoR.Theme()
NIH.plot

ggsave(NIH.plot, filename="NIH.plot.png", width = 8, height=7, dpi=500)

