library (e1071)
library(VIM)
library(ggplot2)
library(dplyr)
library(data.table)
library(mice)


mydata<-read.csv("Student_Exp2019.csv",TRUE,",")
std_exp<- select(mydata, Your.Matric.Number, Your.age., Your.gender, 
                 Choose.Your.Kulliyyah, Level.of.Study, Total.amount.of.income..RM.per.month.,
                 Total.amount.of.expenditure..RM.per.month.)
std_exp<-rename(std_exp, Matric_no=Your.Matric.Number, Age=Your.age., Sex=Your.gender, 
               Kulliyyah=Choose.Your.Kulliyyah, Level=Level.of.Study,
               inc_mon=Total.amount.of.income..RM.per.month.,exp_mon=Total.amount.of.expenditure..RM.per.month.)
md.pattern(std_exp)
std_exp

#Checking missing value in the mydata dataset
missing_plot=aggr(std_exp,col=c('yellow','blue'),
                  numbers=TRUE, sortVars=TRUE, labels=names(std_exp),
                  cex.axis=0.9,gap=3,
                  ylab=c("Histogram of missing data", "Pattern"))

#Returns first parts of the dataset
head(std_exp)
#showing the structure
str(std_exp)
#View statistical summary of data
summary(std_exp)

duplicated(std_exp$Matric_no)
unique(std_exp$Matric_no)

#Delete rows with N/A values and assign it to another dataframe
new_std_exp <- na.omit(std_exp)
#re-numbered the rows accordingly
rownames(new_std_exp)<-NULL
#Drop Matrci number since we don't need that
new_std_exp$Matric_no=NULL

#Classify : Target class = Kulliyyah
Naive_Bayes_Model = naiveBayes(Sex ~., data = new_std_exp)
Naive_Bayes_Model
NB_Predictions=predict(Naive_Bayes_Model,new_std_exp)

print("Confusion Matrix : ")
table1 = table(NB_Predictions,new_std_exp$Sex)
table1


print("Misclassification Rate : ")
misc_rate = 1 - sum(diag(table1))/sum(table1)
misc_rate

print("Accuracy : ")
1-misc_rate



levels(std_exp$Sex)







