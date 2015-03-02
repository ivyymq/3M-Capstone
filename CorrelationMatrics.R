# Author:Joyce Xu
# Version: Version 4(Add differrent correlation method)
# Date: 2/20/2015

#Read in file
hospital.data = read.csv(file = "Data_Pro_WeightedSurvey_Infection.csv", header = T) # reade file



#Get Correlation Matrics
correlation.value = cor(hospital.data, use="complete.obs")
correlation.value.kendall = cor(hospital.data, use="complete.obs", method = "kendall")
correlation.value.spearman = cor(hospital.data, use="complete.obs",method = "spearman")

write.csv(correlation.value, "correlation_infection_survey.csv")  #Output correlation table
write.csv(correlation.value.kendall, "correlation_infection_survey.kendall.csv")  #Output correlation table
write.csv(correlation.value.spearman, "correlation_infection_survey.spearman.csv")  #Output correlation table


#plot heatmap of all correlation
library(corrplot)
jpeg("correlation_infection_survey_circle.jpg", width = 1800, height = 1800,quality = 100)
corrplot(correlation.value, method = "circle")
dev.off()

jpeg("correlation_infection_survey_number.jpg", width = 1800, height = 1800, quality = 100)
corrplot(correlation.value, method = "number")
dev.off()


#Get only cross Correlation matrics-survey against infection
correlation.value.cross = cor(hospital.data[, 1:37], hospital.data[,38:48],use="complete.obs")
write.csv(correlation.value.cross, "correlation_cross.csv")
library(corrplot)
jpeg("correlation_infection_cross_circle.jpg", width = 1800, height = 1800,quality = 100)
corrplot(correlation.value.cross, method = "circle")
dev.off()

jpeg("correlation_infection_cross_number.jpg", width = 1800, height = 1800, quality = 100)
corrplot(correlation.value.cross, method = "number")
dev.off()

#Get only inner Correlation matrics of infection 
correlation.value.infection = cor(hospital.data[, 1:37],use="complete.obs")
write.csv(correlation.value.infection, "correlation_infection.csv")
library(corrplot)
jpeg("correlation_infection_circle.jpg", width = 1800, height = 1800,quality = 100)
corrplot(correlation.value.infection, method = "circle")
dev.off()

jpeg("correlation_infection_number.jpg", width = 1800, height = 1800, quality = 100)
corrplot(correlation.value.infection, method = "number")
dev.off()

#Get only inner Correlation matrics of survey 
correlation.value.survey = cor(hospital.data[, 38:48],use="complete.obs")
write.csv(correlation.value.survey, "correlation_survey.csv")
library(corrplot)
jpeg("correlation_survey_circle.jpg", width = 1800, height = 1800,quality = 100)
corrplot(correlation.value.survey, method = "circle")
dev.off()

jpeg("correlation_survey_number.jpg", width = 1800, height = 1800, quality = 100)
corrplot(correlation.value.survey, method = "number")
dev.off()



#Plot kendall correlation
#plot heatmap of all correlation
library(corrplot)
jpeg("correlation_infection_survey_circle.kendall.jpg", width = 1800, height = 1800,quality = 100)
corrplot(correlation.value.kendall, method = "circle")
dev.off()

jpeg("correlation_infection_survey_number.kendall.jpg", width = 1800, height = 1800, quality = 100)

corrplot(correlation.value.kendall, method = "number")
dev.off()

#Plot spearman correlation
#plot heatmap of all correlation
library(corrplot)
jpeg("correlation_infection_survey_circle.spearman.jpg", width = 1800, height = 1800,quality = 100)
corrplot(correlation.value.spearman, method = "circle")
dev.off()

jpeg("correlation_infection_survey_number.spearman.jpg", width = 1800, height = 1800, quality = 100)
corrplot(correlation.value.spearman, method = "number")
dev.off()


