# title: Data_Transformation
# author: CMU 3M Team
# version: 1.0
# output: .csv document

# set work path to your work directory
# mainDir = "~/Documents/Capstone - Raw Data Backup"
outputDir = paste(mainDir, "Data Processed", sep = "/")
dir.create(outputDir, showWarnings = FALSE)

# load useful packages
require(plyr)
require(reshape)
require(reshape2)

# 1. transform HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014.csv
# set input path
setwd(mainDir)
# load raw data
data.1 <- read.csv("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014.csv", header=T, colClasses = "character")
# pick useful columns
data.1 <- data.1[, c("Provider.ID", "Domain_1_Score", "AHRQ_PSI_90_Score", "Domain_2_Score", "CLABSI_Score", "CAUTI_Score", "Total_HAC_Score")]

# get the information for data dictionary
Measure.ID <- c("Domain_1_Score", "AHRQ_PSI_90_Score", "Domain_2_Score", "CLABSI_Score", "CAUTI_Score", "Total_HAC_Score")
Measure.Name <- c("Domain 1 Score", "PSI-90 Score", "Domain 2 Score", "CLABSI Score", "CAUTI Score", "Total HAC Score")
Measure.Source <- c(rep("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014", 6))
data.1.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# transform data
# sort data by Provider.ID
data.1 <- arrange(data.1, Provider.ID)
# transform the data to required format
data.1.pro <- data.1

# set output path
setwd(outputDir)
# output the sorted raw data for future data verification
write.csv(data.1, "HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - Raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(data.1.pro, "HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - Pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(data.1.dic, "HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014 - Dic.csv", row.names = FALSE)



# 2. Healthcare Associated Infections - Hospital.csv
# set input path
setwd(mainDir)
# load raw data
data.2 <- read.csv("Healthcare Associated Infections - Hospital.csv", header=T, colClasses = "character")

# get the information for data dictionary
Measure.ID <- c(unique(data.2$Measure.ID))
Measure.Name <- c(unique(data.2$Measure.Name))
Measure.Source <- c(rep("Healthcare Associated Infections - Hospital", length(unique(data.2$Measure.ID))))
data.2.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# transform data  
# pick useful columns
data.2 <- data.2[, c("Provider.ID", "Measure.ID", "Score")]
# sort data by Provider.ID
data.2 <- arrange(data.2, Provider.ID)
# transform the data to required format
data.2.pro <- data.2
# decast from one column to multiple columns
colnames(data.2.pro)[3] <- 'value'
data.2.pro <- dcast(data.2.pro, Provider.ID ~ Measure.ID)

# set output path
setwd(outputDir)
# output the sorted raw data for future verification
write.csv(data.2, "Healthcare Associated Infections - Hospital - Raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(data.2.pro, "Healthcare Associated Infections - Hospital - Pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(data.2.dic, "Healthcare Associated Infections - Hospital - Dic.csv", row.names = FALSE)



# 3. HCAHPS - Hospital.csv
# set input path
setwd(mainDir)
# load raw data
data.3 = read.csv("HCAHPS - Hospital.csv", header=T, colClasses = "character")

# get the information for data dictionary
Measure.ID <- c(unique(data.3$HCAHPS.Measure.ID))
Measure.Name <- c(unique(data.3$HCAHPS.Question))
Measure.Source <- c(rep("HCAHPS - Hospital", length(unique(data.3$HCAHPS.Measure.ID))))
data.3.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# transform data
# pick useful columns
data.3 <- data.3[, c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")]
# sort data by Provider.ID
data.3 <- arrange(data.3, Provider.ID)
# transform the data to required format
data.3.pro <- data.3
# decast from one column to multiple columns
colnames(data.3.pro)[3] <- 'value'
data.3.pro <- dcast(data.3.pro, Provider.ID ~ HCAHPS.Measure.ID)

# set output path
setwd(outputDir)
# output the sorted raw data for future verification
write.csv(data.3, "HCAHPS - Hospital - Raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(data.3.pro, "HCAHPS - Hospital - Pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(data.3.dic, "HCAHPS - Hospital - Dic.csv", row.names = FALSE)



# 4. Readmissions Complications and Deaths - Hospital.csv
# set input path
setwd(mainDir)
# load raw data
data.4 <- read.csv("Readmissions Complications and Deaths - Hospital.csv", header=T, colClasses = "character")

# form data dictionary
Measure.IDs <- c(unique(data.4$Measure.ID))
Measure.Names <- c(unique(data.4$Measure.Name))
Measure.type <- sort(c("Denominator", "Score", "Lower_Estimate", "Higher_Estimate"))
Measure.ID <- c()
for (i in 1:length(Measure.IDs)) {
  for (j in 1:length(Measure.type)) {
    Measure.ID <- c(Measure.ID, paste(Measure.IDs[i], "_", Measure.type[j], sep = ""))
  }
}
Measure.Name <- c()
for (i in 1:length(Measure.Names)) {
  for (j in 1:length(Measure.type)) {
    Measure.Name <- c(Measure.Name, paste(Measure.Names[i], "_", Measure.type[j], sep = ""))
  }
}
Measure.Source <- c(rep("Readmissions Complications and Deaths - Hospital", length(Measure.ID)))
# form the data frame of data dictionary
data.4.dic <- data.frame(Measure.ID, Measure.Name, Measure.Source)

# transform data
# pick useful columns
data.4 <- data.4[, c("Provider.ID", "Measure.ID", "Denominator", "Score", "Lower.Estimate", "Higher.Estimate")]
# sort data by Provider.ID
data.4 <- arrange(data.4, Provider.ID)
# transform the data to required format
data.4.pro <- data.4
# melt elements of each measures
data.4.pro <- melt(data = data.4.pro, na.rm = FALSE, id.vars = c(1:2), measure.vars = c(3:6))
# rename Measure.ID
data.4.pro <- transform(data.4.pro, "Measure.ID" = paste(Measure.ID,"_",variable, sep = ""))
data.4.pro[3] <- NULL
# decast from one column to multiple columns
data.4.pro <- dcast(data.4.pro, Provider.ID ~ Measure.ID)

# set output path
setwd(outputDir)
# output the sorted raw data for future verification
write.csv(data.4, "Readmissions Complications and Deaths - Hospital - Raw.csv", row.names = FALSE)
# output the processed data for future merge
write.csv(data.4.pro, "Readmissions Complications and Deaths - Hospital - Pro.csv", row.names = FALSE)
# output the data dictionary
write.csv(data.4.dic, "Readmissions Complications and Deaths - Hospital - Dic.csv", row.names = FALSE)



# merge processed data
data.pro <- merge_recurse(list(data.1.pro, data.2.pro, data.3.pro, data.4.pro))
data.pro <- arrange(data.pro, Provider.ID)
# set output path
setwd(outputDir)
# output merged processed data as .csv file
write.csv(data.pro, "Data_Pro.csv", row.names = FALSE)



# formatting the data for analysis and output
# format column names
# replace the "." in header to "_"
names(data.pro) = gsub("[.]", "_", names(data.pro))

# convert data types
# Transform function: to transform from character to numeric, eliminal all non character value
toNumeric <- function(x){
  x = as.numeric(gsub("[^0-9.]", "", x))
}
# Transform all column except Provider_ID to nmeric, Transform all non numeric value to NA
cols = c(2:ncol(data.pro)); 
data.pro[,cols] = apply(data.pro[,cols], 2, FUN = toNumeric)

# Output the transferd file as csv format
write.csv(data.pro, "Data_Pro_Numeric.csv", row.names=FALSE)



# create data summary for the data matrix
cal.summary = function(x) {
  missing = length(which(is.na(x)))
  if (missing == length(x)) {
    min = NA
    max = NA
  }
  else {
    min = min(x, na.rm = T)
    max = max(x, na.rm = T)
  }
  mean = round(mean(x, na.rm = T), 3)
  median = round(median(x, na.rm = T), 3)
  summary = c("Minimum" = min, "Maximum" = max, "Mean" = mean, "Median" = median, "Missing" = missing)
  summary
}
data.pro.summary = as.data.frame(t(sapply(data.pro[, 2:151], FUN = cal.summary)))
data.pro.summary$Measure_ID = row.names(data.pro.summary)

# merge data dictionary
data.dic <- merge_recurse(list(data.1.dic, data.2.dic, data.3.dic, data.4.dic))
colnames(data.dic) = c("Measure_ID", "Measure_Name", "Measure_Source")

# merge data summary with dictionary
data.dic = merge(data.dic, data.pro.summary, by = "Measure_ID", all.x = T, sort = F)
# output merged data dictionary as .csv file
write.csv(data.dic, "Data_Dic.csv", row.names = FALSE)