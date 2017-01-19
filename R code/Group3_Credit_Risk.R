## DS502 Statistical Method For Data Science
## Group 3: Fangling Zhang, Huayi Zhang,Jiexuan Sun, Jun Dao, Ziqi Lin
## Final Project: Credit Risk Statistical Learning

##### clean the data #####

subset <- read.csv("C:/Users/zlin3/Downloads/subset.csv",stringsAsFactors=T)
# remove loan_amnt (it is same as funded_amnt), emp_title (it has 20391 levels), issue_d, 
# pymnt_plan(all are same), title
# policy_code(all are same), verification_status_joint (it has 39970 NULL)
data=subset[,-c(1,9,14,16,18,44,46)]
# modify earliest_cr_line
# only include year
f=data.frame(do.call('rbind', strsplit(as.character(df$earliest_cr_line),'-',fixed=TRUE)))
data=cbind(data,f[,2]) # keep year
colnames(data)[44]='earliest_cr_line'
data=data[,-17] # remove the old earliest_cr_line
write.csv(data, file='clean_subset.csv', row.names=FALSE)


loan<-read.csv('clean_subset.csv')
library(caret)
#center and scale values
preProcValues=preProcess(loan[,-5],methond=c('center','scale')) 
scaleddata=predict(preProcValues,loan[,-5]) #for next steps, fill up null values
#remove column with 0 varience
newdata=scaleddata[,-nearZeroVar(scaleddata)]
#for next steps, we need to use numeric variables, so remove factor variables
newdata1=newdata[,-c(3,5:9,10:13,22,29,31)]
# remove high cor
descrCorr=cor(newdata1)
newdata2=newdata1[,-findCorrelation(descrCorr)]

#remove linear combos
combolnfo=findLinearCombos(newdata2) 
combolnfo   #after removing high correlation,we do not find any linear combos

data3=data.frame(newdata2,newdata[,c(3,5:9,10:13,22,29,31)])
data4=data3[,-c(20,27,29)]

write.csv(data4,file='clean.csv',row.names=FALSE) # write into a csv file



##### Data Overview #####

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(maps)
library(lattice)
library(plotly)
library(data.table)
# using the subset.csv
data <- read.csv("C:/Users/zlin3/Downloads/clean_subset.csv")
# pie chart - loan status
loan_status_new=data%>%
  group_by(loan_status) %>%
  summarise(n=length(loan_status))
loan_status_new=as.data.frame(loan_status_new)

plot_ly(loan_status_new,
        type = "pie", 
        labels = ~loan_status, 
        values = ~n, 
        hole = 0.5,
        marker = list(colors = brewer.pal(7, "Pastel2"),
                      line = list(width = 1, color = "rgb(52, 110, 165)")),
        sort = F,
        direction = "counterclockwise",
        rotation = 90,
        textinfo = "label+percent",
        textfont = list(size = 14),
        opacity = 1,
        textposition = "outside") %>%
  layout(title = 'LOAN STATUS',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# pie chart - grade
grade_new=data%>%
  group_by(grade) %>%
  summarise(n=length(grade))
grade_new=as.data.frame(grade_new)

plot_ly(grade_new,
        type = "pie", 
        labels = ~grade, 
        values = ~n, 
        hole = 0.5,
        marker = list(colors = brewer.pal(7, "Pastel2"),
                      line = list(width = 1, color = "rgb(52, 110, 165)")),
        sort = F,
        direction = "counterclockwise",
        rotation = 90,
        textinfo = "label+percent",
        textfont = list(size = 14),
        opacity = 1,
        textposition = "outside") %>%
  layout(title = 'Grade STATUS',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# default percentage of each state
l <- list(color = toRGB("steelblue"), width = 1)
g = list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))
s_new=data%>%
  group_by(addr_state) %>%
  summarise(n=length(addr_state))
de = subset(data,is_bad=='2')
sd_new=de%>%
  group_by(addr_state) %>%
  summarise(n=length(addr_state))
s_new=as.data.frame(s_new)
sd_new=as.data.frame(sd_new)
for (i in 1:50){
  for (j in 1:46){
    if (s_new[i,1]==sd_new[j,1]){
      tmp=s_new[i,2]
      s_new[i,2]=sd_new[j,2]/tmp
    }
  }
}
s_new[c(13,21,28,29),2]=c(0,0,0,0) 

#plot_geo(s_new, locationmode = 'USA-states') %>%
#  add_trace(
#    z = ~n, locations = ~addr_state,
#    color = ~n, colors = 'Purples'
#  ) %>%
#  colorbar(title = "Millions USD") %>%
#  layout(
#    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
#    geo = g
#  )

# plotting the map:
#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}
s_new$addr_state=stateFromLower(s_new$addr_state)
colnames(s_new)=c("region","n")
states <- map_data("state")
map.df <- merge(states,s_new,by="region", all.s_new=T)

ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=n))+
  geom_path()+
  scale_fill_gradientn(colours=rev(heat.colors(9)),na.value="grey90")+
  coord_map()

plot(grade,funded_amnt,color = 3, xlab = 'Grade', ylab = 'Funded Amount')
# grouped by number of grade (indicated by color)
qplot(funded_amnt, data=data, geom="density", fill=grade, alpha=I(.5), 
      main="Distribution of Funded Amount", xlab="Funded Amount",
      ylab="Density")



##### LASSO select varialbe #####

## LASSO - Default ##

default = read.csv("DefaultData.csv")
default = na.omit(default)

names(default)

x = model.matrix(is_bad~., default)[,-1]
y = default$is_bad

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

# Least Squares

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)

# perform Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:167,]
lasso.coef[lasso.coef != 0]

## LASSO - Grade ##

default = read.csv("clean_subset.csv")
names(default)
default = default[,-c(4,7)]

default$grade = as.integer(default$grade)

x = model.matrix(grade~., default)[,-1]
y = default$grade

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:167,]
lasso.coef[lasso.coef != 0]




##### default classification #####

library(randomForest) 
library(ggplot2) 
library(glmnet)
library(tree)
library(ROCR)

# import the dataset
data <- read.csv("C:/Users/zlin3/Downloads/clean_subset.csv")
# When we classified default or not, we consider "Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off"
# as bad behavior. But "Fully Paid" is hard to decide. Thus, if the "Fully Paid" account has paid total_rec_late_fee or
# collection_recovery_fee will be considered as bad behavior.

# paid total_rec_late_fee
data$late_fee <- ifelse(data$total_rec_late_fee==0,0,1) 
data$late_fee=as.factor(data$late_fee)
# paid collection_recovery_fee
data$rec_fee <- ifelse(data$collection_recovery_fee==0,0,1) 
data$rec_fee=as.factor(data$rec_fee)
# default account
d=subset(data,loan_status=='Fully Paid')
summary(d[,c(44,45)]) # all rec_ff is 0, which means all fully paid accounts don't have collection_recovery_fee
data=data[,-45] # remove collection_recovery_fee
bad_indicators <- c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off") 
data$is_bad <- ifelse(data$loan_status %in% bad_indicators,1,ifelse(data$loan_status=="", NA,0)) 
data$is_bad=as.factor(data$is_bad)
for (i in 1:40000){
  if(data$late_fee[i]=='1'){  # paid late_fee is considered as Default
    data$is_bad[i]='1'}
}
# remove total_rec_late_fee, grade, loan_status, late_fee
data=data[,-c(6,12,30,44)]
table(data$is_bad) 

# normalize
normalize=function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
s=seq(1,41,by=1)
c=c(3,6,7,8,10,11,12,21,30,32,33,35,40,41) # categorical data
i=setdiff(s,c)
for (j in 1:27){
  tmp=i[j]
  data[,tmp]=normalize(data[,tmp])
}
# write into csv
write.csv(data,file='default_nor.csv',row.names=FALSE)

# training set and test set
set.seed(1)
idx <- runif(nrow(data)) > 0.80 
newtrain <- data[idx==FALSE,] 
newtest <- data[idx==TRUE,] 

## logistic regression ##

# logistic regression (according to the lasso result from Huayi Zhang)
fit=glm(is_bad ~ funded_amnt + funded_amnt_inv + int_rate + installment + out_prncp
        + total_rec_prncp + total_rec_int + recoveries + last_pymnt_d, data=newtrain, family=binomial)
summary(fit)
glm.pro=predict(fit,newdata=newtest,type="respons")

# find out the best threshold
td=seq(0.05,0.50,by=0.01)
sen=rep(0,46) 
ac=rep(0,46)
tt=rep(0,46)
for (i in 1:46){
  tmp=td[i]
  glm.pred=rep("0",nrow(newtest))
  glm.pred[glm.pro>tmp]="1"
  t=table(glm.pred,newtest$is_bad)
  sen[i]=t[4]/(t[4]+t[3]) # sensitivity (larger is better)
  ac[i]=t[4]/(t[4]+t[2]) # precision (larger is better)
  tt[i]=sen[i]+ac[i] # sum of sensitivity and precision (larger is better)
}
plot(td,sen,type='l',col='red',xlab="",ylab="")
par(new=TRUE)
plot(td,ac,type='l',col='blue',axes=FALSE,xlab="",ylab="")
par(new=TRUE)
plot(td,tt,type='l') 
points(td[which.max(tt)],tt[which.max(tt)],pch=20,col='red',lwd=7) # the largest one is the threshold = 0.4

glm.pred=rep("0",nrow(newtest))
glm.pred[glm.pro>0.4]="1"
# confusion matrix
table(glm.pred,newtest$is_bad)
# ROC curve
pred=predict(fit,newtest,type='response')
roc.pred=prediction(pred,newtest$is_bad)
plot(performance(roc.pred,'tpr','fpr'))
auc=performance(roc.pred,'auc')
auc=unlist(slot(auc,"y.values"))
round(auc,digit=2)  # AUC = 0.92

## Random Forest ##

rf.loan=randomForest(is_bad~.,train,mtry=14,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,default.test)
pretable
accuracy=sum(diag(pretable))/nrow(loan.test)
accuracy #we get accuracy 97.43%
103/(103+204) # sensitivity = 33.55%

rf.loan1=randomForest(is_bad~.,train,importance=TRUE) # using default mtry
yhat.rf1=predict(rf.loan1,newdata=loan.test)
pretable=table(yhat.rf1,default.test)
pretable
accuracy=sum(diag(pretable))/nrow(loan.test)
accuracy  #we get accuracy 97.44% 
103/(103+204) # sensitivity = 33.55%

## Tree ##
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



##### Grade Classification #####

## KNN ##

# data information
data = read.csv("clean_subset.csv")

# normalize the data
normalize = function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

data$funded_amnt_n = normalize(data$funded_amnt)
data$funded_amnt_inv_n = normalize(data$funded_amnt_inv)
data$installment_n = normalize(data$installment)
data$annual_inc_n = normalize(data$annual_inc)
data$dti_n = normalize(data$dti)
data$delinq_2yrs_n = normalize(data$delinq_2yrs)
data$inq_last_6mths_n = normalize(data$inq_last_6mths)
data$open_acc_n = normalize(data$open_acc)
data$pub_rec_n = normalize(data$pub_rec)
data$revol_bal_n = normalize(data$revol_bal)
data$revol_util_n = normalize(data$revol_util)
data$total_acc_n = normalize(data$total_acc)
data$out_prncp_n = normalize(data$out_prncp)
data$out_prncp_inv_n = normalize(data$out_prncp_inv)
data$total_pymnt_n = normalize(data$total_pymnt)
data$total_pymnt_inv_n = normalize(data$total_pymnt_inv)
data$total_rec_prncp_n  = normalize(data$total_rec_prncp)
data$total_rec_late_fee_n = normalize(data$total_rec_late_fee)
data$recoveries_n = normalize(data$recoveries)
data$collection_recovery_fee_n = normalize(data$collection_recovery_fee)
data$last_pymnt_amnt_n = normalize(data$last_pymnt_amnt)
data$collections_12_mths_ex_med_n = normalize(data$collections_12_mths_ex_med)
data$acc_now_delinq_n = normalize(data$acc_now_delinq)
data$tot_coll_amt_n = normalize(data$tot_coll_amt)
data$tot_cur_bal_n = normalize(data$tot_cur_bal)
data$earliest_cr_line_n = normalize(data$earliest_cr_line)
data$total_rec_int_n = normalize(data$total_rec_int)
data$total_rev_hi_lim_n = normalize(data$total_rev_hi_lim)

attach(data)
library(class)
set.seed(1)

# split the data
test = sample(1:dim(data)[1],dim(data)[1]/5)
train = -test

data.train = data[train, ]
data.test = data[test, ]

grade.test = grade[test]
grade.train = grade[train]

# perform knn
# varialbes without normalized
train.X = cbind(funded_amnt,funded_amnt_inv,term,installment,emp_length,home_ownership,annual_inc,verification_status,loan_status,purpose,addr_state,dti,delinq_2yrs,inq_last_6mths,open_acc,pub_rec,revol_bal,revol_util,total_acc,initial_list_status,out_prncp,out_prncp_inv,total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_int,total_rec_late_fee,recoveries,collection_recovery_fee,last_pymnt_d,last_pymnt_amnt,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med,application_type,acc_now_delinq,tot_coll_amt,tot_cur_bal,total_rev_hi_lim,earliest_cr_line)[train,]
test.X = cbind(funded_amnt,funded_amnt_inv,term,installment,emp_length,home_ownership,annual_inc,verification_status,loan_status,purpose,addr_state,dti,delinq_2yrs,inq_last_6mths,open_acc,pub_rec,revol_bal,revol_util,total_acc,initial_list_status,out_prncp,out_prncp_inv,total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_int,total_rec_late_fee,recoveries,collection_recovery_fee,last_pymnt_d,last_pymnt_amnt,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med,application_type,acc_now_delinq,tot_coll_amt,tot_cur_bal,total_rev_hi_lim,earliest_cr_line)[-train,]

# all normalized variables: k = 1, test error = 0.731625
train.X = cbind(funded_amnt_n,int_rate,funded_amnt_inv_n,term,installment_n,emp_length,home_ownership,annual_inc_n,verification_status,loan_status,purpose,addr_state,dti_n,delinq_2yrs_n,inq_last_6mths_n,open_acc_n,pub_rec_n,revol_bal_n,revol_util_n,total_acc_n,initial_list_status,out_prncp_n,out_prncp_inv_n,total_pymnt_n,total_pymnt_inv_n,total_rec_prncp_n,total_rec_int_n,total_rec_late_fee_n,recoveries_n,collection_recovery_fee_n,last_pymnt_d,last_pymnt_amnt_n,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med_n,application_type,acc_now_delinq_n,tot_coll_amt_n,tot_cur_bal_n,total_rev_hi_lim_n,earliest_cr_line_n)[train,]
test.X = cbind(funded_amnt_n,int_rate,funded_amnt_inv_n,term,installment_n,emp_length,home_ownership,annual_inc_n,verification_status,loan_status,purpose,addr_state,dti_n,delinq_2yrs_n,inq_last_6mths_n,open_acc_n,pub_rec_n,revol_bal_n,revol_util_n,total_acc_n,initial_list_status,out_prncp_n,out_prncp_inv_n,total_pymnt_n,total_pymnt_inv_n,total_rec_prncp_n,total_rec_int_n,total_rec_late_fee_n,recoveries_n,collection_recovery_fee_n,last_pymnt_d,last_pymnt_amnt_n,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med_n,application_type,acc_now_delinq_n,tot_coll_amt_n,tot_cur_bal_n,total_rev_hi_lim_n,earliest_cr_line_n)[-train,]

# use variables lasso selected 23 variables
train.X = cbind(funded_amnt_inv_n,installment_n,emp_length,home_ownership,verification_status,loan_status,purpose,addr_state,inq_last_6mths_n,pub_rec_n,revol_util_n,initial_list_status,last_credit_pull_d,funded_amnt_n,term,annual_inc_n,delinq_2yrs_n,open_acc_n,revol_bal_n,total_acc_n,total_rec_prncp_n,recoveries_n,next_pymnt_d)[train,]
test.X = cbind(funded_amnt_inv_n,installment_n,emp_length,home_ownership,verification_status,loan_status,purpose,addr_state,inq_last_6mths_n,pub_rec_n,revol_util_n,initial_list_status,last_credit_pull_d,funded_amnt_n,term,annual_inc_n,delinq_2yrs_n,open_acc_n,revol_bal_n,total_acc_n,total_rec_prncp_n,recoveries_n,next_pymnt_d)[-train,]

# test these 11 variables from lasso
train.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,dti_n,pub_rec_n,initial_list_status,delinq_2yrs_n,inq_last_6mths_n)[train,]
test.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,dti_n,pub_rec_n,initial_list_status,delinq_2yrs_n,inq_last_6mths_n)[-train,]

accuracy_rate = rep(0,100)
k = 1:100
for(x in k){
  pred.knn = knn(train.X, test.X, grade.train, k = x)
  accuracy_rate[x] = mean(pred.knn == grade.test)
}
accuracy_rate
plot(k,accuracy_rate,type = 'b')
which.max(accuracy_rate)
points(which.max(accuracy_rate),accuracy_rate[32],col = "red",cex = 1.5, pch =20)
accuracy_rate[32]

pred.knn = knn(train.X, test.X, grade.train, k =32)
table(pred.knn, grade.test)
mean(pred.knn == grade.test)

# put the grade into two catergories
good_grade = c("A","B")
bad_grad = c("C","D","E","F","G")
data$grade_group = ifelse(data$grade %in% good_grade,"Good","Bad")

# perform knn on two different grade
grade_group.test = grade_group[test]
grade_group.train = grade_group[train]

accuracy_rate = rep(0,100)
k = 1:100
for(x in k){
  pred.knn = knn(train.X, test.X, grade_group.train, k = x)
  accuracy_rate[x] = mean(pred.knn == grade_group.test)
}
accuracy_rate
plot(k,accuracy_rate,type = 'b')
which.max(accuracy_rate)
points(which.max(accuracy_rate),accuracy_rate[35],col = "red",cex = 1.5, pch =20)
accuracy_rate[35]

pred.knn = knn(train.X, test.X, grade_group.train, k =35)
table(pred.knn, grade_group.test)
mean(pred.knn == grade_group.test)

## LDA ##

datasetClean<-read.csv('clean.csv')
set.seed(1)
train <- sample(seq_len(nrow(datasetClean)), size=30000)
datasetClean.train <- datasetClean[train, ]
test = -train
datasetClean.test <- datasetClean[test, ]
# '- total_rev_hi_lim - last_pymnt_amnt' for collinarity...
fit.lda <- lda(grade ~ . - total_rev_hi_lim - last_pymnt_amnt - grade - next_pymnt_d - int_rate, data = datasetClean.train)
pred.lda <- predict(fit.lda, datasetClean.test)
table(pred.lda$class, datasetClean.test$grade)

## Rpart ##

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

## Random Forest ##

library(randomForest)
set.seed(3)
rf.loan=randomForest(grade~.,data=loan,subset=train,mtry=9,importance=TRUE)
yhat.rf=predict(rf.loan,newdata=loan.test)
pretable=table(yhat.rf,grade.test)
accuracy=sum(diag(pretable))/20000
pretable
accuracy    #we get 59.30%
importance(rf.loan)

## Tree - 2 levels Grade##

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


## Random Forest - 2 levels grade ##
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






