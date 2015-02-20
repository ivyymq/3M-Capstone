# Author:Joyce Xu
# Version: Version 4
# Date: 2/15/2015

hospital.data = read.csv(file = "Data_Pro_numeric.csv", header = T) # reade file
# output statistic summary
hospital.summary = summary(hospital.data)[,2:151] 
write.csv(hospital.summary, "Hospital_Statistic_Summary.csv", row.names=FALSE)

#Eliminate non-numeric column
hospital.summary$PSI_90_SAFETY_Denominator = NULL
hospital.data$PSI_90_SAFETY_Denominator = NULL

# Output histogram into PDF
pdf("Histogram_Original_data.pdf")
for (i in 2:ncol(hospital.data) ) {
  hist(hospital.data[,i],label = TRUE, main = paste("Histogram of",names(hospital.data)[i]),xlab = names(hospital.data)[i])
}
dev.off()


#Output box plot into PDF to see outlier
pdf("Boxplot.pdf")
for (i in 2:ncol(hospital.data) ) {
  boxplot(hospital.data[,i], main = paste("Boxplot of ", names(hospital.data)[i]))
}
dev.off()

# Output histogram after eliminate the outlier
pdf("Histogram_Eliminate_Outlier.pdf")
for (i in 2:ncol(hospital.data) ) {
  
  hist(hospital.data[,i][which(!hospital.data[,i] %in% boxplot.stats(hospital.data[,i])$out)], label = TRUE, main = paste("Histogram of",names(hospital.data)[i]), xlab = names(hospital.data)[i])
}
dev.off()

# Draw QQ plot to see if follow distribution
pdf("QQplot_Orignal_data.pdf")
for (i in 2:ncol(hospital.data) ) {
  qqnorm(hospital.data[,i], main = paste("QQplot of ", names(hospital.data)[i])) 
  qqline(hospital.data[,i])
}
dev.off()

#Draw QQ plot after eliminating outlier
pdf("QQplot_Eliminate_outlier.pdf")
for (i in 2:ncol(hospital.data) ) {
  qqnorm(hospital.data[,i][which(!hospital.data[,i] %in% boxplot.stats(hospital.data[,i])$out)], main = paste("QQplot of ", names(hospital.data)[i])) 
  qqline(hospital.data[,i])
}
dev.off()


