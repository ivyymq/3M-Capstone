# title: Data_HCAHPS
# author: 3M Capstone Team: Ashwini Bhaskar, Ivy Yang
# version: 1.0
# output: .csv document


# load packages
# install.packages(plyr)
# install.packages("reshape2")
library(plyr)
library(reshape2)

# HCAHPS - Hospital.csv
HCAHPS = read.csv("HCAHPS - Hospital.csv", header=T, colClasses = "character")

# get the information for data dictionary
Measure.ID <- c(unique(HCAHPS$HCAHPS.Measure.ID))
Measure.Name <- c(unique(HCAHPS$HCAHPS.Question))
Measure.Source <- c(rep("HCAHPS - Hospital", length(unique(HCAHPS$HCAHPS.Measure.ID))))

# form the data frame of data dictionary
HCAHPS.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# pick useful columns
HCAHPS <- HCAHPS[, c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")]

# sort data by Provider.ID
HCAHPS <- arrange(HCAHPS, Provider.ID)

# transform the data to required format
HCAHPS.pro <- HCAHPS
# decast from one column to multiple columns
colnames(HCAHPS.pro)[3] <- 'value'
HCAHPS.pro <- dcast(HCAHPS.pro, Provider.ID ~ HCAHPS.Measure.ID)

# output the sorted raw data for future verification
write.csv(HCAHPS, "HCAHPS - Hospital - raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(HCAHPS.pro, "HCAHPS - Hospital - pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(HCAHPS.dic, "HCAHPS - Hospital - dic.csv", row.names = FALSE)
