##ARTIFICIAL NEUROL NETWORK###
summary(bank.additional)
bank.additional <-subset(bank.additional[c(1,12,13,16,18,19,20,21)])
summary(bank.additional)
class(bank.additional)                       
colnames(bank.additional)
str(bank.additional)
levels(bank.additional$y)
barplot(table(bank.additional$y))
#min -max normalization converting to numerical
bank.additional$age<-(bank.additional$age - min(bank.additional$age))/(max(bank.additional$age)-min(bank.additional$age))
bank.additional$campaign<-(bank.additional$campaign - min(bank.additional$campaign))/(max(bank.additional$campaign)-min(bank.additional$campaign))
bank.additional$pdays<-(bank.additional$pdays - min(bank.additional$pdays))/(max(bank.additional$pdays)-min(bank.additional$pdays))
bank.additional$emp.var.rate<-(bank.additional$emp.var.rate - min(bank.additional$emp.var.rate))/(max(bank.additional$emp.var.rate)-min(bank.additional$emp.var.rate))
bank.additional$euribor3m<-(bank.additional$euribor3m - min(bank.additional$euribor3m))/(max(bank.additional$euribor3m)-min(bank.additional$euribor3m))
bank.additional$cons.conf.idx<-(bank.additional$cons.conf.idx - min(bank.additional$cons.conf.idx))/(max(bank.additional$cons.conf.idx)-min(bank.additional$cons.conf.idx))
bank.additional$nr.employed<-(bank.additional$nr.employed - min(bank.additional$nr.employed))/(max(bank.additional$nr.employed)-min(bank.additional$nr.employed))
bank.additional$y<-as.numeric(bank.additional$y)
bank.additional$y<-(bank.additional$y-min(bank.additional$y))/(max(bank.additional$y)-min(bank.additional$y))
str(bank.additional)

#Data Partitioning
set.seed(222)
ind<-sample(2,nrow(bank.additional),replace=TRUE,prob=c(0.7,0.3))#using 60 and 40 split
training<-bank.additional[ind==1,]
testing<-bank.additional[ind==2,]

#neural network
library(neuralnet)
set.seed(333)
n <-neuralnet(y~age+campaign+pdays+cons.conf.idx+emp.var.rate+euribor3m+nr.employed,
              training,
              hidden=2,
              err.fct="sse",
              linear.output=FALSE,
              rep = 1,
              algorithm = "rprop+",
              lifesign = "full")

plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = T,
     information = T,
     fill='lightblue')

# Prediction
output <-compute(n,training[,-8]) #last column not included
head(output$net.result)
head(training[8,])#last row all columns

#confusion matrix and miscalculation Error on training data 
output<-compute(n,training[,-8])
p1<-output$net.result
pred1<-ifelse(p1>0.5,1,0)
tab1<-table(pred1,training$y)
tab1
sum(diag(tab1))/sum(tab1) #accuracy
1-sum(diag(tab1))/sum(tab1)# miscalculation

#confusion matrix and miscalculation Error on testing data
output<-compute(n,testing[,-8])
p2<-output$net.result
pred2<-ifelse(p2>0.5,1,0)
tab2<-table(pred2,testing$y)
tab2
sum(diag(tab2))/sum(tab2)# accuracy
1-sum(diag(tab2))/sum(tab2)# miscalculation

