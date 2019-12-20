library(data.table)
library(dplyr)
library(ggplot2)
library(ggiraphExtra)
library(tidyr)
library(ggmap)
library(ggplot2)
library(glue)
library(digest)
melData = read.csv("Latest_Full_Imputed.csv")
q1<-read.csv("MyDataq1.csv")
q3 <- read.csv("MyDataq3.csv")
str(melData)

n="CBD"
n <-as.data.frame(n)

melData_topPrice <- melData[with(melData,order(-Price)),] ##Order
melData_bottomPrice <- melData[with(melData,order(Price)),]

melData_topPrice20 <- melData_topPrice[1:20,]
melData_bottomPrice20<- melData_bottomPrice[1:20,]

key = "AIzaSyCgZWPFV2EPu4ysIw9YVI8StOEoTtJ7zjw"
register_google(key = key, write = TRUE)


p<-get_googlemap(center = c(lon = 144.9633179, lat = -37.81399),
              zoom = 11, size = c(1280, 1280), scale = 2, messaging = FALSE, urlonly = FALSE, filename = NULL,
              color = c("color", "bw"), force = FALSE, where = tempdir(),
              archiving = FALSE, ext = "com", inject = "")

ggmap(p)
Melbourne <- geocode("Melbourne, Australia")

ggmap(Melbourne)

MelbMap <- get_map(Melbourne)

  
try<-ggmap(p) +
  geom_point(aes(x = melData_topPrice20K$Longtitude, y = melData_topPrice20K$Lattitude),
             data = melData_topPrice20K, size = 1, colour = "black")

meanMel<-mean(melData$Price)
boxplot(melData$Price)
summary(melData$Price)

melDataMean <- melData %>% filter(Price >= meanMel)

melDataThirdQ <- melData %>% filter(Price >= 1150000)
melDataFirstQ <- melData %>% filter(Price <= 695000)
str(melDataThirdQ)

Visualize_Distance<-ggmap(p) +
  geom_point(aes(x = melDataThirdQ$Longtitude, y = melDataThirdQ$Lattitude),
             data = melDataThirdQ, size = 0.75, colour = "red")+
  geom_point(aes(x = 144.9633179 , y = -37.81399 ),
             data = melDataFirstQ, size = 5, colour = "black") +
  geom_label(aes(x=144.9633179 , y = -37.83399, label=n),
             data=n,
             family = 'Times', 
             size = 3) 

Visualize_Distance_2<-ggmap(p) +
  geom_point(aes(x = melDataFirstQ$Longtitude, y = melDataFirstQ$Lattitude),
             data = melDataFirstQ, size = 0.75, colour = "#006400")+
  geom_point(aes(x = 144.9633179 , y = -37.81399 ),
             data = melDataFirstQ, size = 5, colour = "red") +
  geom_label(aes(x=144.9633179 , y = -37.83399, label=n),
             data=n,
             family = 'Times', 
             size = 3) 

groupMelb <-melDataThirdQ %>% group_by(Suburb)

freqMelQ<-data.frame(table(melDataThirdQ$Suburb))
freqMelQ <- freqMelQ %>% filter(Freq > 0 )
frqMelQ_Top <- freqMelQ[with(freqMelQ,order(-Freq)),]
frqMelQ_Bot <- freqMelQ[with(freqMelQ,order(Freq)),]
frqmelQ_Top5 <- frqMelQ_Top[1:5,]
frqmelQ_Bot5 <- frqMelQ_Bot[1:5,]

freqMelQ_2 <- data.frame(table(melDataFirstQ$Suburb))
freqMelQ_2 <- freqMelQ_2 %>% filter(Freq > 0 )
frqMelQ_2_Top <- freqMelQ_2[with(freqMelQ_2,order(-Freq)),]
frqMelQ_2_Bot <- freqMelQ_2[with(freqMelQ_2,order(Freq)),]
frqmelQ_2_Top5 <- frqMelQ_2_Top[1:5,]
frqmelQ_2_Bot5 <- frqMelQ_2_Bot[1:5,]

melDataThirdQ_NEW <- melDataThirdQ %>% filter(Suburb == frqmelQ_Top5$Var1) ## get multiple subset?
melDataThirdQ_NEW2 <- melDataFirstQ %>% filter(Suburb == frqmelQ_2_Top5$Var1) ## get multiple subset?

Visualize<-ggmap(p) +
  geom_point(aes(x = melDataThirdQ_NEW2$Longtitude, y = melDataThirdQ_NEW2$Lattitude),
             data = melDataThirdQ_NEW2, size = 0.75, colour = "#006400")+
  geom_point(aes(x = melDataThirdQ_NEW$Longtitude, y = melDataThirdQ_NEW$Lattitude),
             data = melDataThirdQ_NEW, size = 0.75, colour = "red")

write.csv(melDataThirdQ, "Third Quartile Price Range.csv", row.names = FALSE)
write.csv(melDataFirstQ, "First Quartile Price Range.csv", row.names = FALSE)


common <- intersect(melDataThirdQ$Suburb, melDataFirstQ$Suburb) 
common<-data.frame(common)
class(common)
common<-common %>% rename(Destination = common)
test <- merge(melDataFirstQ, melDataThirdQ, by="Suburb")
data4 <- test %>% group_by(Suburb) %>%
  filter(row_number()==1)

Overlap_region <- select(data4,c(1,15,16))


Overlap_map<-ggmap(p) +
  geom_point(aes(x = Overlap_region$Longtitude.x, y = Overlap_region$Lattitude.x),
             data = Overlap_region, size = 2.8, colour = "black")+
  geom_point(aes(x = 144.9633179 , y = -37.81399 ),
             data = melDataFirstQ, size = 5, colour = "RED") +
geom_label(aes(x=144.9633179 , y = -37.83399, label=n),
  data=n,
  family = 'Times', 
  size = 3)

Zharif_Map<-ggmap(p) +
  geom_point(aes(x = q1$Longtitude, y = q1$Lattitude),
             data = q1, size = 0.75, colour = "purple")+
  geom_point(aes(x = 144.9633179 , y = -37.81399 ),
             data = melDataFirstQ, size = 5, colour = "black") +
  geom_label(aes(x=144.9633179 , y = -37.83399, label=n),
             data=n,
             family = 'Times', 
             size = 3) 

Zharif_Map_2<-ggmap(p) +
  geom_point(aes(x = q3$Longtitude, y = q3$Lattitude),
             data = q3, size = 0.75, colour = "blue")+
  geom_point(aes(x = 144.9633179 , y = -37.81399 ),
             data = melDataFirstQ, size = 5, colour = "black") +
  geom_label(aes(x=144.9633179 , y = -37.83399, label=n),
             data=n,
             family = 'Times', 
             size = 3) 

