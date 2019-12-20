library(dplyr)
data <- read.csv("Melbourne_Less_imputed.csv")
data
null_val <- is.na(data)
sum(null_val)
summary(null_val)

View(data)

unique_ID <- data$Unique
unique_ID
View(unique_ID)

data2 <- read.csv("Z.csv")
View(data2)

data2$Unique_ID <- unique_ID
View(data2)

write.csv(data2, "Melbourne_Imputed.csv")
