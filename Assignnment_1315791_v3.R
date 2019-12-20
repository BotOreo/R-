library(data.table)
library(dplyr)
library(ggplot2)
library(ggiraphExtra)
library(tidyr)
data2 <- read.csv("Muhamad Arif Lutfi - unhcr-resettlement-residing-usa-csv-1.csv")
data2 <- data2[-1,]
sum(is.na(data2))

unk_data <- data2[grep("unknown",data2$Origin),] 
length(unk_data$Origin) # this is for row, sebab dah specify column (boleh guna nrow)
length(unk_data) # this is for column sebab takde specify mana column
ncol(unk_data) # basic untuk dapatkan number off col
nrow(unk_data) # basic untuk dapatkan number of row
dim(unk_data) # dapat (row x col)

asterisk<-data2[grepl("\\*",data2$Value),] # to isolate regular expression (*)
length(asterisk$Value)

data3 <- (sapply(data2,sub,pattern='\\*',replacement=NA)) ## Ganti asterisk value kepada N/A

asterisk_NA 
write.csv(data3, "Arep_3-12-2019.csv", row.names = FALSE)
#asterisk_NA <- as.data.frame(asterisk_NA)#all column with NA on value col

#Value_NA <- as.data.frame(asterisk_NA$Value)#value column je

data4 <- read.csv("Arep_3-12-2019.csv") #data now with NA
#omitting NA for mean prediction
clean_value <- na.omit(data4$Value) # clean_price ni yg tak de na
#clean_price <- as.data.frame(clean_price[-1,])
#clean_price
#clean<-unlist(clean_price) # unlist the data type

#clean_1 <- as.data.frame(as.numeric(levels(clean))[clean])

#clean_price <- as.character(clean_price)
#clean_price <- as.numeric(clean_price) #tak boleh buat ni, nanti clean_price jadi NA

#getting the clean Mean
#mean_price <- lapply(clean_1, mean, na.rm = TRUE)
mean_price<- mean(clean_value)

#imputing NA with mean
#Value_NA <- as.data.frame(Value_NA[-1,])
#unl_value <- unlist(Value_NA)
#Value_NA <- as.data.frame(as.numeric(levels(unl_value))[unl_value])
#Value_NA[is.na(Value_NA)] <- mean_price

#sum(is.na(Value_NA))# no more na in values column

data4$Value[is.na(data4$Value)] <- mean_price

View(data4$Value)

write.csv(data4, "Data.csv", row.names = FALSE)

data_frameFULL <- data.frame(data4)
data_frameFULL$Origin <- as.character(data_frameFULL$Origin)
data_frameFULL$Origin[data_frameFULL$Origin == "Iran (Islamic Rep. of)"] <- "Iran"

data_frameFULL<-data_frameFULL %>% 
  rename(Asylum_Country = Country...territory.of.asylum.residence) ## change column name

data_framePART = subset(data_frameFULL, Origin == "Iran") ##get a subset

data_frameFULL1 <- data_frameFULL %>% filter(Origin == "Cuba" | Origin == "Iran" | Origin == "Afghanistan") ## get multiple subset?
data_frameFULL1_Russia <- data_frameFULL %>% filter(Origin == "Russian Federation" | Origin == "Iran" | Origin == "Afghanistan") ## get multiple subset?

##---To plot multiple histogram in one graph---#
## The data needs to be in long format, like in data_frameFULL1
first_graph<-ggplot(data = data_frameFULL1_Russia , aes(x = Year, y = Value, fill = Origin)) +
  geom_col(position = position_dodge())

second_graph<-ggplot(data = data_frameFULL1 , aes(x = Year, y = Value, fill = Origin, color=Origin)) +
  geom_point()+geom_smooth(method=lm, aes(fill=Origin),se=FALSE)

third_graph<-ggplot(data = data_frameFULL1_Russia , aes(x = Year, y = Value, fill = Origin, color=Origin)) +
  geom_point()+geom_smooth(method=lm, aes(fill=Origin))
##---To plot multiple histogram in one graph---#

Afghan_Data <- data_frameFULL[data_frameFULL$Origin == "Afghanistan",-1] ##Pecahkan? Why lol?
Cuba_Data <- data_frameFULL[data_frameFULL$Origin == "Cuba",-1]
Iran_Data <- data_frameFULL[data_frameFULL$Origin == "Iran",-1]
Russian_Data <- data_frameFULL[data_frameFULL$Origin == "Iran",-1]
data.frame(data_frameFULL)
#data_Transpose <-data.frame(t(data_frameFULL[-1]))
Merge_FULL <- merge(Afghan_Data, Iran_Data, by.x ="Year", by.y = "Year", sort = TRUE)
Merge_FULL <- merge(Merge_FULL, Cuba_Data, by.x="Year", by.y = "Year", sort =TRUE)
Merge_FULL_Russian <- merge(Merge_FULL, Russian_Data, by.x="Year", by.y = "Year", sort =TRUE)

Merge_FULL<-Merge_FULL %>% ## change column name
  rename(
    Afghan = Value.x,
    Iran = Value.y,
    Cuba =  Value
  )

Merge_FULL_Russian <-Merge_FULL_Russian %>% ## change column name
  rename(
    Afghan = Value.x,
    Iran = Value.y,
    Russia =  Value
  )
Merge_PART <- Merge_FULL %>% select(1,3,5,7) ## Select the wanted columns
Merge_PART_Russia <- Merge_FULL_Russian %>% select(1,3,5,7) ## Select the wanted columns


MLR <- lm(Value~Year+Origin, data=data_frameFULL1)
MLR_First<-ggPredict(MLR, colorAsFactor = TRUE, interactive=TRUE)

str(Merge_PART)
Merge_PART$Year<-as.numeric(Merge_PART$Year)

data_frameFULL1 <- data_frameFULL1[-1]

table1 <- table(data_frameFULL1$Origin, data_frameFULL1$Year)
table1


table2<-spread(data_frameFULL1, Year, Value) #untuk group same row in data into one
table4<-table2%>%filter(table2$Origin=="Afghanistan") ##to select row

table3<-as.data.frame(t(table2)) ## To transpose data (column to row, row to column)
##---------Untuk tukar first row jadi column name---_##
colnames(table3) <- as.character(unlist(table3[1,]))
table3 = table3[-1, ]
##---------Untuk tukar first row jadi column name---_##

class(table3)
str(table3)
table3$Afghanistan<-as.character(table3$Afghanistan)
table3$Cuba<-as.character(table3$Cuba)
table3$Iran<-as.character(table3$Iran)

table3$Afghanistan[is.na(table3$Afghanistan)] <- 0
table3$Iran[is.na(table3$Iran)] <- 0
table3$Cuba[is.na(table3$Cuba)] <- 0

table5<-as.data.frame(t(table3))
colnames(table5)
table5$Origin <- row.names(table5) ## Add another column called "Origin" in table5
table5<-table5[,c(37,1:36)] ##put the last column to first
table5<-table5[-1]#Delete first column

##----convert multiple column to numeric---------##
cols.num <- c(1:36) ## Set which column, 1:4 means from 1 to 4 and 1,4 means 1 and 4 only
table5[cols.num] <- sapply(table5[cols.num],as.character) ## factor need to be character first before become numeric
table5[cols.num] <- sapply(table5[cols.num],as.numeric)
sapply(table5, class)
##----convert multiple column to numeric---------##
hist(table5)
str(table5)
