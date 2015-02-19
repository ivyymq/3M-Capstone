# title: Data_HAC
# author: 3M Capstone Team: Joyce Xu
# version: 1.0
# output: .csv document


# load packages
# install.packages(plyr)
library(plyr)

# HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014.csv
HAC <- read.csv("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014.csv", header=T, colClasses = "character")

# pick useful columns
HAC <- HAC[, c("Provider.ID", "Domain_1_Score", "AHRQ_PSI_90_Score", 
               "Domain_2_Score", "CLABSI_Score", "CAUTI_Score", "Total_HAC_Score")]

# get the information for data dictionary
Measure.ID <- c("Domain_1_Score", "AHRQ_PSI_90_Score", "Domain_2_Score", 
                "CLABSI_Score", "CAUTI_Score", "Total_HAC_Score")
Measure.Name <- c("Domain 1 Score", "PSI-90 Score", "Domain 2 Score", 
                  "CLABSI Score", "CAUTI Score", "Total HAC Score")
Measure.Source <- c(rep("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014", 6))

# form the data frame of data dictionary
HAC.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# sort data by Provider.ID
HAC <- arrange(HAC, Provider.ID)

# transform the data to required format
HAC.pro <- HAC

# output the sorted raw data for future verification
write.csv(HAC, "HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(HAC.pro, "HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(HAC.dic, "HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - dic.csv", row.names = FALSE)
