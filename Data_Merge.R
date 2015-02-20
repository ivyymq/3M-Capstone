# title: Data_Merge
# author: 3M Capstone Team: Ivy Yang
# version: 3.0
# output: .csv document

# install.packages("reshape")
library(reshape)

# load processed data
HAC.pro <- read.csv("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - pro.csv", header = T, colClasses = "character")
HAI.pro <- read.csv("Healthcare Associated Infections - Hospital - pro.csv", header = T, colClasses = "character")
HCAHPS.pro <- read.csv("HCAHPS - Hospital - pro.csv", header = T, colClasses = "character")
RCD.pro <- read.csv("Readmissions Complications and Deaths - Hospital - pro.csv", header = T, colClasses = "character")
# merge processed data
data.pro <- merge_recurse(list(HAC.pro, HAI.pro, HCAHPS.pro, RCD.pro))
data.pro <- arrange(data.pro, Provider.ID)
# output merged processed data as .csv file
write.csv(data.pro, "Data_pro.csv", row.names = FALSE)


# load data dictionary data
HAC.dic <- read.csv("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - dic.csv", header = T, colClasses = "character")
HAI.dic <- read.csv("Healthcare Associated Infections - Hospital - dic.csv", header = T, colClasses = "character")
HCAHPS.dic <- read.csv("HCAHPS - Hospital - dic.csv", header = T, colClasses = "character")
RCD.dic <- read.csv("Readmissions Complications and Deaths - Hospital - dic.csv", header = T, colClasses = "character")
# merge data dictionary
data.dic <- merge_recurse(list(HAC.dic, HAI.dic, HCAHPS.dic, RCD.dic))
# output merged data dictionary as .csv file
write.csv(data.dic, "Data_dic.csv", row.names = FALSE)