data0<-read.csv('clean.csv')

data=data0[,-c(2)] #remove int_rate and factors with more than 32 level
attach(data)

default=ifelse(loan_status %in% c('Charged Off','Default',' Issued  Late (16-30 days)','Late (31-120 days)'),'Yes','No')
summary(as.factor(default))
loan=data.frame(data,default)

attach(loan)
library(tree)
tree.loan=tree(default~.-loan_status,loan)
summary(tree.loan) #we get misclassification error rate: 0.0238
plot(tree.loan)
text(tree.loan,pretty=0)

set.seed(2)
train=sample(1:nrow(loan),32000)
loan.test=loan[-train,]
default.test=default[-train]

tree.loan=tree(default~.-loan_status,loan,subset=train)
tree.pred=predict(tree.loan,loan.test,type='class')
pretable=table(tree.pred,default.test)
pretable
accuracy=sum(diag(pretable))/8000
accuracy  #we get accuracy 97.7%
184/(184+275)
 #core error rate 40.09%:default that we do not predict right

set.seed(1)
cv.loan=cv.tree(tree.loan,FUN=prune.misclass)
cv.loan
par(mfrow=c(1,2))
plot(cv.loan$size,cv.loan$dev,type='b') #we get best size=6
plot(cv.loan$k,cv.loan$dev,type='b')

prune.loan=prune.misclass(tree.loan,best=4)
plot(prune.loan)
text(prune.loan,pretty=0)
tree.pred=predict(prune.loan,loan.test,type='class')
pretable=table(tree.pred,default.test)
pretable
accuracy=sum(diag(pretable))/8000
accuracy  #we get accuracy 97.4%
160/(160+299) #core error rate 40.1%:default that we do not predict right

library(randomForest)
set.seed(3)
rf.loan=randomForest(default~.-loan_status,data=loan,subset=train,mtry=10,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,default.test)
pretable
accuracy=sum(diag(pretable))/8000
accuracy #we get accuracy 98.4%
333/(333+126) #core error rate 27.45%:default that we do not predict right

rf.loan=randomForest(default~.-loan_status,data=loan,subset=train,mtry=18,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,default.test)
pretable
accuracy=sum(diag(pretable))/8000
accuracy  #we get accuracy 98.40%
333/(333+126) #core error rate 27.45%:default that we do not predict right

