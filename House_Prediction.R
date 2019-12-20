# Install the relevant libraries - do this one time
install.packages("lubridate")
install.packages("ggplot2")
install.packages("data.table")
install.packages("ggrepel")
install.packages("dplyr")
install.packages("data.table")
install.packages("tidyverse")


# Load the relevant libraries - do this every time
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)


#A) Load the Melbourne housing market dataset

house_market= read.csv("Latest_Full_Imputed.csv", stringsAsFactors = FALSE)

data(house_market)
View(house_market)

# Look at the data sets

dim(house_market)

head(house_market)

attach(house_market)

# Create some color variables for graphing later

col1 = "#011f4b"

col2 = "#6497b1"

col3 = "#b3cde0"

col4 = "#CC0000"

col5 = "#FFFF00"


#Price Filter
boxplot(house_market$Price)
summary(house_market$Price)

q1 <- house_market %>% 
  filter(Price<=695000)

q3 <- house_market %>% 
  filter(Price>=1150000)

boxplot(q3$Price)
summary(q3$Price)



#Type House
house <- house_market %>% 
  filter(Type=='h')

unit <- house_market %>% 
  filter(Type=='u')

Townhouse <- house_market %>% 
  filter(Type=='t')

summary(Townhouse$Price)




#Distance
st <- house_market %>% 
  filter(Distance>=0 & Distance<=10)

th <- house_market %>% 
  filter(Distance>=11 & Distance<=20)

twenty <- house_market %>% 
  filter(Distance>=21 & Distance<=30)

thirty <- house_market %>% 
  filter(Distance>=31 & Distance<=40)

fourty <- house_market %>% 
  filter(Distance>=41)




#Load the library
library("ggmap")

#Set your API Key
ggmap::register_google(key = "AIzaSyCgZWPFV2EPu4ysIw9YVI8StOEoTtJ7zjw")

##1) Create a map with all of the crime locations plotted.

p <- ggmap(get_googlemap(center = c(lon = 144.9633333333333, lat = -37.814166666666665),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))+ 
             geom_point(aes(x = Longtitude, y = Lattitude,  colour = Distance, data = house_market, size = 0.5)) + 
  theme(legend.position="bottom")

## q1 and q3

p <- ggmap(get_googlemap(center = c(lon = 144.9633333333333, lat = -37.814166666666665),
                         zoom = 11, scale = 2,
                         maptype ='terrain')) + geom_point(aes(x = Longtitude, y = Lattitude),
             data = q1, size = 0.75, colour = "#006400")+
  geom_point(aes(x = Longtitude, y = Lattitude),
             data = q3, size = 0.75, colour = "red")


##q1 only
p <- ggmap(get_googlemap(center = c(lon = 144.9633333333333, lat = -37.814166666666665),
                         zoom = 11, scale = 2,
                         maptype ='terrain')) + geom_point(aes(x = q1$Longtitude, y = q1$Lattitude),
               data = q1, size = 0.75, colour = "#006400") +
  geom_point(aes(x = 144.9633333333333, y = -37.814166666666665, stroke = 2), colour=col5, data = n, size =2.5)



##q3 only
p <- ggmap(get_googlemap(center = c(lon = 144.9633333333333, lat = -37.814166666666665),
                         zoom = 11, scale = 2,
                         maptype ='terrain')) + geom_point(aes(x = Longtitude, y = Lattitude),
               data = q3, size = 0.75, colour = "red") +
  geom_point(aes(x = 144.9633333333333, y = -37.814166666666665, stroke = 2), colour=col5, data = n, size =2.5)


