library (e1071)
library(VIM)
library(mice)


happiness<-read.csv("2017.csv",TRUE,",")

#Limits the maximum view of the data to only 10000
options(max.print=100000)

md.pattern(happiness)
#Checking missing value in the mydata dataset
missing_plot=aggr(happiness,col=c('green','red'),
                  numbers=TRUE, sortVars=TRUE, labels=names(happiness),
                  cex.axis=7,gap=3,
                  ylab=c("Histogram of missing data", "Pattern"))

#Returns first parts of the dataset
head(happiness)
#showing the structure
str(happiness)
#View statistical summary of data
summary(happiness)

repeating_seq = rep.int(seq_len(nrow(happiness)), happiness$Country)

happiness_dataset = happiness[repeating_seq,]

happiness_dataset$Country = NULL