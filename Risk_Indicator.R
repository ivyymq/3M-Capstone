# title: Risk_Indicator
# autuor: Ivy Yang
# version: 1.0
# output: .csv file & .pdf file

# set work path
setwd("~/Documents/Capstone - Raw Data Backup/GPS")
# load data
data = read.csv("Data_Pro_Group_4_Interval_withGPS.csv", header = T)

# get the risk level for each hospital
countRisk = function(x) {
  return(length(which(x == 'high' | x == 'outlier_high')))
}
# get the count of Risk indicators for each hospital
data$Risk_Indicator = apply(data, 1, countRisk)
data$Non_Missing = rep(0, 4815)  
for (i in 1:4815) {
  if (length(which(!is.na(data[i, c(2:38)]))) > 0)
    data$Non_Missing[i] = length(which(is.na(data[i, c(2:38)])))
}
# level the risks
data$Risk_Score = (data$Risk_Indicator+0.1) / (37.1-data$Non_Missing)

data$Risk_Level = c()
for (i in 1:nrow(data)) {
  if (data$Risk_Score[i] >= 0.75)
    data$Risk_Level[i] = 'Very High Risk'
  else if (data$Risk_Score[i] >= 0.5)
    data$Risk_Level[i] = 'High Risk'
  else if (data$Risk_Score[i] >= 0.25)
    data$Risk_Level[i] = 'Low Risk'
  else
    data$Risk_Level[i] = 'Very Low Risk'
}
write.csv(data, 'Data_Pro_Group_4_Interval_withRisk.csv', row.names = F)

# 2 levels
data$Risk_Level = as.numeric(data$Risk_Score >= 0.5)
library(plyr)
data$Risk_Level = mapvalues(data$Risk_Level, c(0,1), c('Low Risk', 'High Risk'))
write.csv(data, 'Data_Pro_Group_2_Interval_withRisk.csv', row.names = F)

# plot gps
library(ggmap)
us.map <- get_map(location = 'united states', zoom = 4, color = "color")

# plot into pdf
cbbPalette <- c("orange","blue")
names(cbbPalette) <- c("Low Risk", "High Risk")
pdf("GPS_Risk_Indicator_3.pdf", width = 20, height = 10)
plot = ggmap(us.map) + 
  geom_point(data = data, mapping = aes(x = Lon, y = Lat, color = Risk_Level), alpha=.8, size=2) +
  scale_color_manual(values = cbbPalette, na.value = "grey50")
print(plot)
dev.off()

# plot into pdf
cbbPalette <- c("orange","blue")
names(cbbPalette) <- c("Low Risk", "High Risk")
pdf("GPS_Risk_Indicator_2_NonMissing.pdf", width = 20, height = 10)
plot = ggmap(us.map) + 
  geom_point(data = data, mapping = aes(x = Lon, y = Lat, color = Risk_Level), alpha=.8, size=1.5) +
  scale_color_manual(values = cbbPalette, na.value = "grey50")
print(plot)
dev.off()

# plot into pdf
cbbPalette <- c("yellow", "orange", "#CC79A7", "blue")
names(cbbPalette) <- c("Very Low Risk", "Low Risk", "High Risk", "Very High Risk")
pdf("GPS_Risk_Indicator_4.pdf", width = 20, height = 10)
plot = ggmap(us.map) + 
  geom_point(data = data, mapping = aes(x = Lon, y = Lat, color = Risk_Level), alpha=.8, size=1.5) +
  scale_color_manual(values = cbbPalette, na.value = "grey50") + facet_wrap("Risk_Level")
print(plot)
dev.off()

pdf("GPS_Risk_Indicator_4_withType.pdf", width = 20, height = 10)
plot = ggmap(us.map) + 
  geom_point(data = data, mapping = aes(x = Lon, y = Lat, color = Risk_Level),
             alpha=.8, size=1.5) +
  scale_color_manual(values = cbbPalette, na.value = "grey50") + facet_wrap("Hospital.Type")
print(plot)
dev.off()