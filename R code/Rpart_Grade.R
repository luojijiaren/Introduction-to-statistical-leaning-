loan1<-read.csv('clean.csv')
#loan4=loan[,-c(1:6,8,9,14,18,19,22,29:35,39:42)]  #remove factors with levels larger than 32.
#loan4=loan[,-c(1:6,8,9,14,18,19,22,39,42)]
loan=loan1[,-c(2)] #remove int_rate
attach(loan)
library(rpart)
set.seed(2)
train=sample(1:nrow(loan),20000)
loan.test=loan[-train,]
grade.test=grade[-train]
cfit=rpart(grade~.,loan,subset=train,method='class',control=rpart.control(cp=0))
print(cfit)
#par(mar=rep(0.1,4))
plot(cfit)
text(cfit,pretty=0)
#library(rpart.plot)
#rpart.plot(cfit,split.cex=1.2)
rpart.pred=predict(cfit,loan.test,type='class')
pretable=table(rpart.pred,grade.test)
pretable
accuracy=sum(diag(pretable))/20000
accuracy                           #get 52.73%


#(temp=with(train,table(cut(revol_util,c(0,48.05,399)),cut(total_rev_hi_lim,c(0,38370,69700)),exclude=NULL)))

#(temp=with(train,table(cut(revol_util,c(0,48.05,399)),exclude=NULL)))
printcp(cfit) #when cp= 0.00028 , nsplit=75,we get minimun xerror0.6381.we don't need to prune
cfit2=prune(cfit,cp= 0.00028 )
plot(cfit2)
text(cfit2,pretty=0)
rpart.pred=predict(cfit2,loan.test,type='class')
pretable=table(rpart.pred,grade.test)
pretable
accuracy=sum(diag(pretable))/20000
accuracy      #get 53.2%


library(randomForest)
set.seed(3)
rf.loan=randomForest(grade~.,data=loan,subset=train,mtry=9,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,grade.test)
accuracy=sum(diag(pretable))/20000
pretable
accuracy    #we get 59.30%
importance(rf.loan)



