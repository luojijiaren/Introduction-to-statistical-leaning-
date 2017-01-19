loan<-read.csv('C:/Users/fzhang2/Downloads/loan.csv')
dim(loan)  #We get 887379 rows and 74 columns.
index=rep(0,74)
for (i in 1:74){
  if (sum (is.na(loan[,i]))>88738) { #remove variables with NA percentage larger than 10%
      index[i]=i
  }
}

data1 = loan[,index==0]
dim (data1)
data2=data1[,-c(1,2,19,20,23)] # remove id, memberid, url, desc, zip_code.
data3=data2[complete.cases(data2),] # remove rows with NA.

dim(data3) #we get 816722 rows and 50 columns.
set.seed(1)
data4=data3[sample(816721,40000),]
dim(data4)


write.csv(data3, file='lynn_clean_subset.csv', row.names=FALSE)

write.csv(data4, file='lynn_clean_subset2.csv', row.names=FALSE)






