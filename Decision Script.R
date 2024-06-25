# Step 1
# Load marketing data
Mydata <- bank.additional
# Remove variable "duration"
Mydata <- subset(Mydata, select = -c(duration) )
# Split data into training (80%) and validation (20%)
A = sort(sample(nrow(Mydata), nrow(Mydata)*.8)) 
Train<-Mydata[A,]  # train data
Val<-Mydata[-A,] # validation data
# Step 2
# load the library rpart which is Decision trees (DT)
library(rpart)  
# build a DT with outcome y on the train data
complexity=0.001
split_size=200 
depth=5
error_costs=matrix(c(0,2,1,0))
mtree <- rpart(y ~ ., data = Train, method="class",control = rpart.control(cp=complexity, minsplit=split_size, maxdepth=depth), parms=list(loss=error_costs)) 

#plot tree 
plot(mtree)
text(mtree, pretty=FALSE, cex=.6) 
# Step 3 Confusion Matrix
Yt=predict(mtree,Val,type="class") # predict results on the validation data
conf.matrix <- table(Val$y, Yt)  # build a confusion matrix
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix))
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix))
print(conf.matrix)
acc=mean(Val$y==Yt) # accuracy
tp=sum(Val$y=='yes' & Yt=='yes')/sum(Val$y=='yes') # true positive rate
tn=sum(Val$y=='no' & Yt=='no')/sum(Val$y=='no') # true negative rate
fp=sum(Val$y=='no' & Yt=='yes')/sum(Val$y=='no') # false positive rate
fn=sum(Val$y=='yes' & Yt=='no')/sum(Val$y=='yes') # false negative rate
sprintf("Accuracy:%.2f, TP:%.2f, TN:%.2f",acc,tp,tn)# formatted printing
sprintf("Error rates: FP:%.2f, FN:%.2f",fp,fn)
