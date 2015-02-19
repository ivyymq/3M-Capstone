# title: Data_RCD
# author: 3M Capstone Team: Liu Yang, Ivy Yang
# version: 1.0
# output: .csv document


# load packages
# install.packages(plyr)
# install.packages("reshape2")
library(plyr)
library(reshape2)

# Readmissions Complications and Deaths - Hospital.csv
RCD <- read.csv("Readmissions Complications and Deaths - Hospital.csv", header=T, colClasses = "character")

# get the information for data dictionary
Measure.ID <- c(unique(RCD$Measure.ID))
Measure.Name <- c(unique(RCD$Measure.Name))
Measure.Source <- c(rep("Readmissions Complications and Deaths - Hospital", length(unique(RCD$Measure.ID))))

# form the data frame of data dictionary
RCD.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# pick useful columns
RCD <- RCD[, c("Provider.ID", "Measure.ID", "Denominator", "Score", "Lower.Estimate", "Higher.Estimate")]

# sort data by Provider.ID
RCD <- arrange(RCD, Provider.ID)

# transform the data to required format
RCD.pro <- RCD
# melt elements of each measures
RCD.pro <- melt(data = RCD.pro, na.rm = FALSE, id.vars = c(1:2), measure.vars = c(3:6))
# rename Measure.ID
RCD.pro <- transform(RCD.pro, "Measure.ID" = paste(Measure.ID,"_",variable, sep = ""))
RCD.pro[3] <- NULL
# decast from one column to multiple columns
RCD.pro <- dcast(RCD.pro, Provider.ID ~ Measure.ID)

# output the sorted raw data for future verification
write.csv(RCD, "Readmissions Complications and Deaths - Hospital - raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(RCD.pro, "Readmissions Complications and Deaths - Hospital - pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(RCD.dic, "Readmissions Complications and Deaths - Hospital - dic.csv", row.names = FALSE)
