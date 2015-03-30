#Author: Joyce Xu and Ivy Yang
#Date: 3/26/2015
#Version: version 1


library(plyr)
library(ggplot2)
#setwd("D:/MISM PITTS/Capstone/DATA")
data = read.csv(file = "Data_Pro_WeightedSurvey_Infection.csv")

#Group the data into 5 group in total, Use quantile to cut to make sure each interval have the same number of entries
groups = function(x){
  qvalue = quantile(x,probs=c( .05,.35, .65, .95),na.rm = TRUE)
  x=cut(x, breaks=c(-Inf, qvalue, Inf),include.lowest=TRUE,labels = c("Low_Extreme", "Low", "Medium", "High", "High_Extreme"))
  x
} 

#Aply the function to each column
newdata = sapply(data[,2:37], FUN = groups)
newdata = as.data.frame(newdata)
#Rename the column
colnames(newdata)=paste(colnames(newdata),"_group", sep="")
#combine the grouped column with the original data
finaldata = cbind(data,newdata)      
#write to CSV file
write.csv(finaldata, "group_data_5_entry.csv", row.names=F)
       
#Group the data into 4 group in total
groups4 = function(x){
  qvalue = quantile(x,probs=c( .05,.5, .95),na.rm = TRUE)
  x=cut(x, breaks=c(-Inf, qvalue, Inf),include.lowest=TRUE,labels = c("Low_Extreme", "Low", "High", "High_Extreme"))
  x
} 

newdata = sapply(data[,2:37], FUN = groups4)
newdata = as.data.frame(newdata)
colnames(newdata)=paste(colnames(newdata),"_group", sep="")
finaldata = cbind(data,newdata)      

write.csv(finaldata, "group_data_4_entry.csv", row.names=F)

#Group the data into 7 group in total
#groups7 = function(x){
#  qvalue = quantile(x,probs=c( .05, .23, .41, .59, .77, .95),na.rm = TRUE)
#  x=cut(x, breaks=c(-Inf, qvalue, Inf),include.lowest=TRUE,labels = c("Low_Extreme", "Lowest","Lower","Medium","Higher","Highest","High_Extreme"))
#  x
#} 

#newdata = sapply(data[,2:37], FUN = groups7)
#newdata = as.data.frame(newdata)
#colnames(newdata)=paste(colnames(newdata),"_group", sep="")
#finaldata = cbind(data,newdata)      

#write.csv(finaldata, "group_data_7_entry.csv", row.names=F)


#Plot a distribution plot to verify the group
pdf("Histogram_group_data.pdf")

for (i in 1:ncol(newdata) ) {
  #hist(newdata[,i],label = TRUE, main = paste("Histogram of",names(newdata)[i]),xlab = names(newdata)[i])
  print(ggplot(data=newdata, aes(x=newdata[,i]))+ geom_bar()+labs(x=names(newdata)[i], title=paste("Histogram of",names(newdata)[i])))
}
dev.off()


data.test = read.csv(file = "group_data_7_interval.csv")
pdf("Histogram_group_interval_data.pdf")

for (i in 49:ncol(data.test) ) {
  #hist(newdata[,i],label = TRUE, main = paste("Histogram of",names(newdata)[i]),xlab = names(newdata)[i])
  print(ggplot(data=data.test, aes(x=data.test[,i]))+ geom_bar()+labs(x=names(data.test)[i], title=paste("Histogram of",names(data.test)[i])))
}
dev.off()