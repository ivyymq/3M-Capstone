# Author:Joyce Xu
# Version: Version 3
# Date: 2/20/2015

#Read in file
hospital.data = read.csv(file = "Data_Pro_WeightedSurvey_Infection.csv", header = T) # reade file



#Get Correlation Matrics
correlation.value = cor(hospital.data, use="complete.obs")
write.csv(correlation.value, "correlation_infection_survey.csv")  #Output correlation table

#plot heatmap of correlation
library(corrplot)
jpeg("correlation_infection_survey_circle.jpg", width = 1800, height = 1800,quality = 100)
corrplot(correlation.value, method = "circle")
dev.off()

jpeg("correlation_infection_survey_number.jpg", width = 1800, height = 1800, quality = 100)
corrplot(correlation.value, method = "number")
dev.off()






