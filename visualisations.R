setwd("C:/Users/roych/OneDrive/Documents/SHEFFIELD")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(mapproj)
library(ggmap)
library(gridExtra) 

#GDP per capita across thw world,2018

mapdata=map_data("world")
View(mapdata)
GDP=read_csv("Gross.csv")
mapdata=left_join(mapdata,GDP,by="region")
mapdata1<-mapdata%>%filter(!is.na(mapdata$GDP))
map1=ggplot(mapdata1,aes(x=long,y=lat,group=group))+
geom_polygon(aes(fill=GDP),color="black")

map2=map1+scale_fill_gradient(name="GDP per capita",low="orange",high="blue",na.value="grey")+
theme(axis.text.x=element_blank(),
axis.text.y=element_blank(),
axis.ticks=element_blank(),
axis.title.y=element_blank(),
axis.title.x=element_blank(),
rect=element_blank())
map2




## GDP GROWTH RATE

GDP<-read_csv("GrossDP.csv")

GDP<-GDP[,-c(5:52,65)]
GDP<-GDP%>%
  pivot_longer(-c("Country","Country Code","Indicator Code","Indicator Name"),
                names_to="date",
                values_to="GDP")%>%
  select(-c("Country Code","Indicator Code","Indicator Name"))%>%
  ungroup()

# plot
p1=GDP%>%filter(Country=="Pakistan")
p2=GDP%>%filter(Country=="India")
p3=GDP%>%filter(Country=="Bangladesh")
p4=GDP%>%filter(Country=="Sri Lanka")

plot(p1$date,p1$GDP,ylim=c(0,12),xlab="Date",ylab="GDP growth(annual%)",main="GDP growth rate for South Asian Countries",type="l",col="red",lwd=2,sub="Source: The World Bank")
lines(p2$date,p2$GDP,col="blue",lwd=2)
lines(p3$date,p3$GDP,col="green",lwd=2)
lines(p4$date,p4$GDP,col="orange",lwd=2)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1) 
legend(x = "topright",          
       legend = c("Pakistan", "India","Bangladesh","Sri Lanka"),            
       col = c("red","blue","green","orange"),          
       lwd = 2) 



##Income inequality

IE<-read_csv("IE1.csv")
I=IE%>%filter(Country%in% c("Pakistan", "India","Sri Lanka","Bangladesh"))

IE%>%filter(Country%in% c("Pakistan", "India","Sri Lanka","Bangladesh"))%>%
ggplot(aes(x = "", y = Income, fill = Country)) +
  geom_col(color = "black") +
  labs(caption ="Source:World Bank (2020a). World Development Indicators database.")+
  geom_text(aes(label = Income),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  ggtitle("Income share held by richest 10%")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#ebf2ff"),
        plot.background = element_rect(fill = "#ebf2ff"),
        legend.background = element_rect(fill = "#ebf2ff"))+ggeasy::easy_center_title()
easy_plot_title_size(20)

#Gender Inequality

FUE<-read.csv("Female_labour2.csv") 
FUE<-FUE[,-c(2:62,64:65)] 
FUE<-FUE%>%
filter(Country %in% c("Pakistan", "India","Bangladesh","Sri Lanka"))


MUE<-read.csv("Male_labour2.csv") 
MUE<-MUE[,-c(2:62,64)]
MUE<-MUE%>%
filter(Country %in% c("Pakistan", " India","Bangladesh","Sri Lanka"))
data<-merge(FUE,MUE)

d1 <-MUE%>%filter(Country=="Pakistan") 
d2<-FUE%>%filter(Country=="Pakistan") 
p1 <-merge(d1 ,d2)
dfm1<- melt(data[ ,c('Country','male','female')],id.vars = 1 )

p3<-ggplot(dfm1,aes(x =Country,y = value)) +
geom_bar(aes(fill = variable),stat = "identity",position = "dodge",width=0.6)+
ggtitle( "Unemployment by gender,2018") +labs(x="",y=" Unemployment(% of labor force)(modeled ILO estimate)",caption="Source: International  Labour  Organization, ILOSTAT database")+
ggeasy::easy_center_title()+
theme(text=element_text(size=15))+ guides(fill=guide_legend("Gender"))


#Women Empowerment


FE<- read.csv( "FE.csv") 

FE<- FE%>%
filter(Country%in%c("Pakistan", "India","Bangladesh" ,"Sri Lanka"))
FE<-FE[,2:3]

p4<-ggplot(FE,aes(x = "", y = Female_employment, fill = Country)) + 
 geom_col(color = "black")+
 labs(caption ="Source: International Labour Organization, ILOSTAT database.") + 
 geom_text(aes(label=Female_employment),
 position = position_stack(vjust = 0.5)) + 
coord_polar(theta = "y") +
scale_fill_brewer() +
ggtitle("Female share of employment in senior and middle mangernent(%)")+ 
theme(axis.text = element_blank(),
axis.ticks = element_blank(),
axis.title = element_blank(), 
panel.grid = element_blank(),
panel.background=element_rect(fill = "#ebf2ff"), 
plot.background = element_rect(fill = " #ebf2ff") ,
legend.background = element_rect(fill = "#ebf2ff"))+ggeasy::easy_center_title() +
theme(text = element_text(size = 12))

#Terrorisrn
GT<-read.csv("GlobalTerrorism.csv")
p5 <-ggplot(GT,aes(x =iyear,y = success)) +
geom_bar(aes(fill = country),stat = "identity",position = "dodge")+
ggtitle("Terrorist	attacks,2017-2019") +labs(x=" " ,y="No.	of	successful	terrorist attack",caption="Source:Global Terrorism Database")+
ggeasy::easy_center_title()

p5

# Load necessary packages
library(ggplot2)

# Assuming 'GT' is your dataset

# Filter data for the countries and years of interest
GT_filtered <- subset(GT, iyear >= 2017 & iyear <= 2019 &
                          country_txt %in% c("Bangladesh", "India", "Pakistan", "Sri Lanka"))

# Create the multiple bar plot
p <- ggplot(GT_filtered, aes(x = as.factor(iyear), fill = country_txt)) +
  geom_bar(position = "dodge") +
  labs(title = "Terrorist Attacks (2017-2019)",
       x = "Year",
       y = "Number of Attacks",
       fill = "Country") +
  scale_fill_manual(values = c("Bangladesh" = "blue", "India" = "red", "Pakistan" = "green", "Sri Lanka" = "purple")) +
  theme_minimal()

# Print the plot
p


 