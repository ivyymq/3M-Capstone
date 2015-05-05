# title: Model_BiCluster
# autuor: Ivy Yang & Joyce Xu
# version: 1.0
# output: .csv file & .txt file



require(biclust)
library(plyr)

# # set work path
setwd("~/Documents/Capstone - Raw Data Backup")

# original value
data = read.csv("Data_Pro_WeightedSurvey_Infection.csv", header = T)
data.rate = data[, c(1:6,8,10,12,14,16,18:37)]
data.rate = na.omit(data.rate)
data.rate = as.matrix(data.rate)

# x <- discretize(data.rate)
cluster = biclust(data.rate, method = BCrepBimax(), number = 5)
cluster
for(i in 1:5){
  drawHeatmap(data.rate, bicResult=cluster,number=i, plotAll = F)
}
plotclust(cluster, data.rate, bicluster=TRUE,legende=T,noC=5,Titel="Plotclust")
predictBimax(cluster, data.rate)



# labled
data2 = read.csv("group_data_4_interval.csv", header = T)
data.rate2 = data2[, c(49:85)]
for(i in 1:37){
  data.rate2[,i] = mapvalues(data.rate2[,i], c("outlier_low","low","high","outlier_high"),c(0,1,2,3))
}
data.rate2 = na.omit(data.rate2)
data.rate2 = as.matrix(sapply(data.rate2, as.numeric))
set.seed(2)
cluster2 = biclust(data.rate2, method = BCXmotifs(), number = 5)
# cluster2 = biclust(data.rate2, method = BCrepBimax(), number = 5)
cluster2
for(i in 1:5){
  drawHeatmap(data.rate2, bicResult=cluster2,number=i, plotAll = F)
}
heatmapBC(data.rate2, cluster2)
plotclust(cluster2, data.rate2, bicluster=TRUE,legende=T,noC=5,Titel="Plotclust")
# predictBimax(cluster2, data.rate2)
bubbleplot(data.rate2,cluster2, showLabels=TRUE)

writeBiclusterResults("BiCluster_BCXmotifs_model.txt", cluster2,"BCXmotifs", append = F,
                      dimnames(data.rate2)[1][[1]], dimnames(data.rate2)[2][[1]])
pre = writeclust(cluster2,row=TRUE,noC=10)
pre = as.data.frame(pre)
write.csv(pre, "BiCluster_BCXmotifs_results.csv", row.names = F)



# imputed label with mean value
data3 = read.csv("group_data_4_interval_imputed_output.csv", header = T)
data.rate3 = data3[, c(49:85)]
for(i in 1:37){
  data.rate3[,i] = mapvalues(data.rate3[,i], c("outlier_low","low","high","outlier_high"),c(0,1,2,3))
}
# data.rate3 = na.omit(data.rate3)
data.rate3 = as.matrix(sapply(data.rate3, as.numeric))
set.seed(3)
cluster3 = biclust(data.rate3, method = BCXmotifs(), number = 5)
# cluster3 = biclust(data.rate3, method = BCrepBimax(), number = 5)
cluster3
for(i in 1:5){
  drawHeatmap(data.rate3, bicResult=cluster3,number=i, plotAll = F)
}
heatmapBC(data.rate3, cluster3)
plotclust(cluster3, data.rate3, bicluster=TRUE,legende=T,noC=5,Titel="Plotclust")
# predictBimax(cluster3, data.rate3)
bubbleplot(data.rate3,cluster3, showLabels=TRUE)

writeBiclusterResults("BiCluster_BCXmotifs_Imputed_Mean_model.txt", append = F, cluster3,"BCXmotifs", 
                      dimnames(data.rate3)[1][[1]], dimnames(data.rate3)[2][[1]])
pre = writeclust(cluster3,row=TRUE,noC=10)
pre = as.data.frame(pre)
write.csv(pre, "BiCluster_BCXmotifs_Imputed_Mean_results.csv", row.names = F)