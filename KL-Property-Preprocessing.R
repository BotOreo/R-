library(data.table)
dataKL <- read.csv("data_kaggle.csv")
str(dataKL)
colLocation<-sum(is.na(dataKL$Location))
colPrice<-sum(is.na(dataKL$Price)) #There is whitespaces that does not consider as null

data1 <- dataKL

data1[data1 == ""] <- NA # Change all blankspace "" into NA

data2 <- data1
data3 <- data1
data2 <- na.omit(data1) # Kalau nak buang semua NA, regardless of column

data3<-data3[complete.cases(data3[ , 2,7:8]),] # Kalau nak buang row yang betul betul takdak NA yang ditetapkan

str(data3)
data3$Price <- as.character(data3$Price)
str(data3)
data4 <- data3
try<-gsub('RM ','',data3$Price)
try <- gsub(',','',try)
data4$Price <- as.numeric(try)

write.csv(data3,"Dataset.csv")

colnames(data4)[colnames(data4)=="Price"] <- "Price in RM" #Change column name
str(data4)

data5<-data4
data4$Rooms

typeFreq<-data.table(table(data4$Rooms))

##eval(parse(text='3+19-2'))

toBeRemoved<-which(data4$Rooms =="20 Above" | data4$Rooms == "Studio") ## drop or remove "20 above" and "Studio" kalau perlu. Kalau tak, tak payah
data5<-as.data.frame(data4[-toBeRemoved,])

levels(data5$Rooms)[match("6+",levels(data5$Rooms))] <- "6" ## replace the 6+ to 6+0
levels(data5$Rooms)[match("10+",levels(data5$Rooms))] <- "10"
levels(data5$Rooms)[match("12+",levels(data5$Rooms))] <- "12"
levels(data5$Rooms)[match("13+",levels(data5$Rooms))] <- "13"
levels(data5$Rooms)[match("15+",levels(data5$Rooms))] <- "15"
levels(data5$Rooms)[match("7+",levels(data5$Rooms))] <- "7"
levels(data5$Rooms)[match("8+",levels(data5$Rooms))] <- "8"
levels(data5$Rooms)[match("9+",levels(data5$Rooms))] <- "9"
levels(data5$Rooms)[match("Studio",levels(data5$Rooms))] <- "1"
levels(data5$Rooms)[match("20 Above",levels(data5$Rooms))] <- "20"



typeFreq_2<-data.table(table(data5$Rooms))
str(data5)

data5$finalRooms <- sapply(as.character(data5$Rooms), function(x) eval(parse(text = x)))


