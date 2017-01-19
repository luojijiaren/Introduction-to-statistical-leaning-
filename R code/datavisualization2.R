data = read.csv('clean_subset.csv')

names(data)

attach(data)

str(data)

plot(grade,funded_amnt,color = 3, xlab = 'Grade', ylab = 'Funded Amount')

library(ggplot2)

# grouped by number of grade (indicated by color)
qplot(funded_amnt, data=data, geom="density", fill=grade, alpha=I(.5), 
   main="Distribution of Funded Amount", xlab="Funded Amount", 
   ylab="Density")

