# title: Data_HAI
# author: 3M Capstone Team: Ivy Yang
# version: 1.0
# output: .csv document


# load packages
# install.packages(plyr)
# install.packages("reshape2")
library(plyr)
library(reshape2)

# Healthcare Associated Infections - Hospital.csv
HAI <- read.csv("Healthcare Associated Infections - Hospital.csv", header=T, colClasses = "character")

# get the information for data dictionary
Measure.ID <- c(unique(HAI$Measure.ID))
Measure.Name <- c(unique(HAI$Measure.Name))
Measure.Source <- c(rep("Healthcare Associated Infections - Hospital", length(unique(HAI$Measure.ID))))

# form the data frame of data dictionary
HAI.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# pick useful columns
HAI <- HAI[, c("Provider.ID", "Measure.ID", "Score")]

# sort data by Provider.ID
HAI <- arrange(HAI, Provider.ID)

# transform the data to required format
HAI.pro <- HAI
# decast from one column to multiple columns
colnames(HAI.pro)[3] <- 'value'
HAI.pro <- dcast(HAI.pro, Provider.ID ~ Measure.ID)

# output the sorted raw data for future verification
write.csv(HAI, "Healthcare Associated Infections - Hospital - raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(HAI.pro, "Healthcare Associated Infections - Hospital - pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(HAI.dic, "Healthcare Associated Infections - Hospital - dic.csv", row.names = FALSE)
