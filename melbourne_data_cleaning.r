library("mice")
library("dplyr")

#importing data
#melb <- read.csv("melbourne.csv")
melb <- read.csv("E:/Zaim/Final_year_1/ML/Project/MELBOURNE_HOUSE_PRICES_LESS.csv")

#checking data
head(melb)

#checking for missing values
md.pattern(melb)

#price has missing value
#taking out price column
price <- melb$Price


#omitting NA for mean prediction
clean_price <- na.omit(price)

#getting the clean Mean
mean_price<- mean(clean_price)

#imputing NA with mean
price[is.na(price)] <- mean_price

#removing price column
new_melb <- melb[,-5]

#replacing new price column
new_melb$Price <- price

#checking for missing values
md.pattern(new_melb)
is.na(new_melb)

#the cleaning is done
#the missing value is imputed with the mean
