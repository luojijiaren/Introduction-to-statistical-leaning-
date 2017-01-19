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


