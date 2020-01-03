library('Metrics')

library('randomForest')

library('ggplot2')

library('ggthemes')

library('dplyr')

#set random seed

set.seed(101)

data<-read.csv("stock_data.csv")


dim(data)

data$Y<-as.factor(data$Y)

data$Time<-NULL

#dividing the dataset into train and test

train<-data[1:2000,]

test<-data[2001:3000,]

#applying Random Forest

model_rf<-randomForest(Y ~ ., data = train)



preds<-predict(model_rf,test[,-101])

table(preds)


auc(preds,test$Y)

#Feauture importance-------------------------------
  
importance(model_rf)

#---------------------------#

model_rf<-randomForest(Y ~ X55+X11+X15+X64+X30
                       
                       +X37+X58+X2+X7+X89
                       
                       +X31+X66+X40+X12+X90
                       
                       +X29+X98+X24+X75+X56,
                       
                       data = train)



preds<-predict(model_rf,test[,-101])



table(preds)

auc(preds,test$Y)
