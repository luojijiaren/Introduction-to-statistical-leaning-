# data information
data = read.csv("clean_subset.csv")
dim(data)

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
train.X = cbind(funded_amnt_n,funded_amnt_inv_n,term,installment_n,emp_length,home_ownership,annual_inc_n,verification_status,loan_status,purpose,addr_state,dti_n,delinq_2yrs_n,inq_last_6mths_n,open_acc_n,pub_rec_n,revol_bal_n,revol_util_n,total_acc_n,initial_list_status,out_prncp_n,out_prncp_inv_n,total_pymnt_n,total_pymnt_inv_n,total_rec_prncp_n,total_rec_int_n,total_rec_late_fee_n,recoveries_n,collection_recovery_fee_n,last_pymnt_d,last_pymnt_amnt_n,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med_n,application_type,acc_now_delinq_n,tot_coll_amt_n,tot_cur_bal_n,total_rev_hi_lim_n,earliest_cr_line_n)[train,]
test.X = cbind(funded_amnt_n,funded_amnt_inv_n,term,installment_n,emp_length,home_ownership,annual_inc_n,verification_status,loan_status,purpose,addr_state,dti_n,delinq_2yrs_n,inq_last_6mths_n,open_acc_n,pub_rec_n,revol_bal_n,revol_util_n,total_acc_n,initial_list_status,out_prncp_n,out_prncp_inv_n,total_pymnt_n,total_pymnt_inv_n,total_rec_prncp_n,total_rec_int_n,total_rec_late_fee_n,recoveries_n,collection_recovery_fee_n,last_pymnt_d,last_pymnt_amnt_n,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med_n,application_type,acc_now_delinq_n,tot_coll_amt_n,tot_cur_bal_n,total_rev_hi_lim_n,earliest_cr_line_n)[-train,]

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

# put the grade into two catergories
good_grade = c("A","B")
bad_grad = c("C","D","E","F","G")
data$grade_group = ifelse(data$grade %in% good_grade,"Good","Bad")
# perform knn on two different grade
grade_group.test = grade_group[test]
grade_group.train = grade_group[train]

# all variables: k = 1, test error = 0.731625
train.X = cbind(funded_amnt_n,int_rate,funded_amnt_inv_n,term,installment_n,emp_length,home_ownership,annual_inc_n,verification_status,loan_status,purpose,addr_state,dti_n,delinq_2yrs_n,inq_last_6mths_n,open_acc_n,pub_rec_n,revol_bal_n,revol_util_n,total_acc_n,initial_list_status,out_prncp_n,out_prncp_inv_n,total_pymnt_n,total_pymnt_inv_n,total_rec_prncp_n,total_rec_int_n,total_rec_late_fee_n,recoveries_n,collection_recovery_fee_n,last_pymnt_d,last_pymnt_amnt_n,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med_n,application_type,acc_now_delinq_n,tot_coll_amt_n,tot_cur_bal_n,total_rev_hi_lim_n,earliest_cr_line_n)[train,]
test.X = cbind(funded_amnt_n,int_rate,funded_amnt_inv_n,term,installment_n,emp_length,home_ownership,annual_inc_n,verification_status,loan_status,purpose,addr_state,dti_n,delinq_2yrs_n,inq_last_6mths_n,open_acc_n,pub_rec_n,revol_bal_n,revol_util_n,total_acc_n,initial_list_status,out_prncp_n,out_prncp_inv_n,total_pymnt_n,total_pymnt_inv_n,total_rec_prncp_n,total_rec_int_n,total_rec_late_fee_n,recoveries_n,collection_recovery_fee_n,last_pymnt_d,last_pymnt_amnt_n,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med_n,application_type,acc_now_delinq_n,tot_coll_amt_n,tot_cur_bal_n,total_rev_hi_lim_n,earliest_cr_line_n)[-train,]

pred.knn = knn(train.X, test.X, grade_group.train, k =97)
table(pred.knn, grade_group.test)
mean(pred.knn == grade_group.test)
library(gmodels)
CrossTable(pred.knn,grade_group.test)

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

# for random test
pred.knn = knn(train.X, test.X, grade.train, k =32)
table(pred.knn, grade.test)
mean(pred.knn == grade.test)

CrossTable(pred.knn,grade.test)

# k = 1, test error = 0.696625
train.X = cbind(home_ownership,purpose,term,total_rec_int_n,total_rev_hi_lim_n,annual_inc_n)[train,]
test.X = cbind(home_ownership,purpose,term,total_rec_int_n,total_rev_hi_lim_n,annual_inc_n)[-train,]

# more error than above one # k =1, test error = 0.704625
train.X = cbind(home_ownership,purpose,term,total_rec_int_n,total_rev_hi_lim_n)[train,] 
test.X = cbind(home_ownership,purpose,term,total_rec_int_n,total_rev_hi_lim_n)[-train,]

train.X = cbind(home_ownership,purpose,term)[train,] 
test.X = cbind(home_ownership,purpose,term)[-train,]

# small lasso
train.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,dti_n,delinq_2yrs_n,inq_last_6mths_n)[train,]
test.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,dti_n,delinq_2yrs_n,inq_last_6mths_n)[-train,]

# test this one from small lasso
train.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,dti_n,pub_rec_n,initial_list_status,delinq_2yrs_n,inq_last_6mths_n)[train,]
test.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,dti_n,pub_rec_n,initial_list_status,delinq_2yrs_n,inq_last_6mths_n)[-train,]

train.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,initial_list_status,inq_last_6mths_n)[train,]
test.X = cbind(home_ownership,purpose,term,emp_length,verification_status,loan_status,initial_list_status,inq_last_6mths_n)[-train,]


# use variables lasso selected 23 variables
train.X = cbind(funded_amnt_inv_n,installment_n,emp_length,home_ownership,verification_status,loan_status,purpose,addr_state,inq_last_6mths_n,pub_rec_n,revol_util_n,initial_list_status,last_credit_pull_d,funded_amnt_n,term,annual_inc_n,delinq_2yrs_n,open_acc_n,revol_bal_n,total_acc_n,total_rec_prncp_n,recoveries_n,next_pymnt_d)[train,]
test.X = cbind(funded_amnt_inv_n,installment_n,emp_length,home_ownership,verification_status,loan_status,purpose,addr_state,inq_last_6mths_n,pub_rec_n,revol_util_n,initial_list_status,last_credit_pull_d,funded_amnt_n,term,annual_inc_n,delinq_2yrs_n,open_acc_n,revol_bal_n,total_acc_n,total_rec_prncp_n,recoveries_n,next_pymnt_d)[-train,]

# explore
library(DescTools)
Desc(data$grade, main = "Grade distribution", plotit = 1)
Desc(data$home_ownership, main = "home_ownership distribution", plotit = 1)

library(ggplot2)
box_plane2 = ggplot(data, aes(term,loan_amnt))
box_plane2 + geom_boxplot(aes(fill = term)) +
  labs(title = "Loan amount by term",
       x = "Term",
       y = "Loan amount")

box_plane2 = ggplot(data, aes(grade,term))
box_plane2 + geom_boxplot(aes(fill = grade)) +
  labs(title = "Gade by term",
       x = "Grade",
       y = "Term")


box_plane3 = ggplot(data, aes(grade,annual_inc))
box_plane3 + geom_boxplot(aes(fill = grade)) +
  labs(title = "Annual income by Grade",
       x = "Grade",
       y = "Annual income")

data$home_ownership = as.factor(data$home_ownership)
box_plane = ggplot(data, aes(grade,int_rate))
box_plane + geom_boxplot(aes(fill = grade)) +
  labs(title = "Interest rate by grade",
       x = "Grade",
       y = "Interest rate")


box_plane = ggplot(data, aes(home_ownership,grade))
box_plane + geom_boxplot(aes(fill = home_ownership)) +
  labs(title = "home_ownership by grade",
       x = "Home_ownership",
       y = "Grade")
plot(data$int_rate ~ data$term)
plot(data$int_rate ~ data$purpose)
plot(data$int_rate ~ data$home_ownership)
plot(data$annual_inc ~ data$int_rate)

prettyNum(12345.678,big.mark=",",scientific=FALSE)


good_status = c("Fully Paid")
on_going_status = c("Current",
                    "Issued")
data$status_group = ifelse(data$loan_status %in% good_status,"Good",
                           ifelse(data$loan_status %in% on_going_status,"On going","Bad"))
data$status_group = factor(data$status_group)
Desc(data$status_group, main = "Status group distribution", plotit = 1)
plot(data$loan_amnt ~ data$status_group)
plot(annual_inc ~ grade)
