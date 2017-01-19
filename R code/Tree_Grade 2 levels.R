library(tree)
data<-read.csv('clean.csv')

data1=data[,-c(2)] #remove variable int_rate
attach(data1)

High=ifelse(grade %in% c('A','B'),'Yes','No')
loan1=data.frame(data1,High)
loan=loan1[,-18]

attach(loan)
tree.loan=tree(High~.,loan)
summary(tree.loan) #we get misclassification error rate: 0.29
plot(tree.loan)
text(tree.loan,pretty=0)

set.seed(2)
train=sample(1:nrow(loan),32000)
loan.test=loan[-train,]
High.test=High[-train]

tree.loan=tree(High~.,loan,subset=train)
tree.pred=predict(tree.loan,loan.test,type='class')
pretable=table(tree.pred,High.test)
pretable
accuracy=sum(diag(pretable))/8000
accuracy  #we get accuracy 71.0%


set.seed(1)
cv.loan=cv.tree(tree.loan,FUN=prune.misclass)
cv.loan
par(mfrow=c(1,2))
plot(cv.loan$size,cv.loan$dev,type='b')
plot(cv.loan$k,cv.loan$dev,type='b')

prune.loan=prune.misclass(tree.loan,best=5)
plot(prune.loan)
text(prune.loan,pretty=0)
tree.pred=predict(prune.loan,loan.test,type='class')
pretable=table(tree.pred,High.test)
accuracy=sum(diag(pretable))/8000
accuracy  #we get accuracy 71.0%

library(randomForest)
set.seed(3)
rf.loan=randomForest(High~.,data=loan,subset=train,mtry=9,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,High.test)
accuracy=sum(diag(pretable))/8000
accuracy #we get 86.4%

rf.loan=randomForest(High~.,data=loan,subset=train,mtry=17,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,High.test)
accuracy=sum(diag(pretable))/8000
accuracy #we get 87.7%

rf.loan=randomForest(High~.,data=loan,subset=train,mtry=21,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,High.test)
accuracy=sum(diag(pretable))/8000
accuracypre #we get 88.0%

library(gbm)
#set.seed(5)
#boost.loan=gbm(High~.,data=loan[train,],distribution='bernoulli',n.trees=5000,interaction.depth=4)
#summary(boost.loan)


