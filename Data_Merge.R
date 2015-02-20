title: Data_Merge
author: 3M Capstone Team: Liu Yang, Ashwini Bhaskar, Joyce Xu, Ivy Yang
version: 1.0
output: .csv document

# install.packages("reshape")
# install.packages("reshape2")
library(reshape)
library(reshape2)

# define data dictionary factors
Measure.ID <- c()
Measure.Name <- c()
Measure.Source <- c()


# HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014.csv
HAC <- read.csv("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014.csv", header=T, colClasses = "character")
# pick useful columns
HAC <- HAC[, c("Provider.ID", "Domain_1_Score", "AHRQ_PSI_90_Score", 
              "Domain_2_Score", "CLABSI_Score", "CAUTI_Score", "Total_HAC_Score")]


# Healthcare Associated Infections - Hospital.csv
HAI <- read.csv("Healthcare Associated Infections - Hospital.csv", header=T, colClasses = "character")
# get the information for data dictionary
Measure.ID <- c(Measure.ID, unique(HAI$Measure.ID))
Measure.Name <- c(Measure.Name, unique(HAI$Measure.Name))
Measure.Source <- c(Measure.Source, rep("Healthcare Associated Infections - Hospital", 
                                        length(unique(HAI$Measure.ID))))
# pick useful columns
HAI <- HAI[, c("Provider.ID", "Measure.ID", "Score")]
# decast
colnames(HAI)[3] <- 'value'
HAI <- dcast(HAI, Provider.ID ~ Measure.ID)


# HCAHPS - Hospital.csv
HCAHPS = read.csv("HCAHPS - Hospital.csv", header=T, colClasses = "character")
# get the information for data dictionary
Measure.ID <- c(Measure.ID, unique(HCAHPS$HCAHPS.Measure.ID))
Measure.Name <- c(Measure.Name, unique(HCAHPS$HCAHPS.Question))
Measure.Source <- c(Measure.Source, rep("HCAHPS - Hospital", 
                                        length(unique(HCAHPS$HCAHPS.Measure.ID))))
# pick useful columns
HCAHPS <- HCAHPS[, c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")]
# decast
colnames(HCAHPS)[3] <- 'value'
HCAHPS <- dcast(HCAHPS, Provider.ID ~ HCAHPS.Measure.ID)


# Readmissions Complications and Deaths - Hospital.csv
RCD <- read.csv("Readmissions Complications and Deaths - Hospital.csv", header=T, colClasses = "character")
# get the information for data dictionary
Measure.ID <- c(Measure.ID, unique(RCD$Measure.ID))
Measure.Name <- c(Measure.Name, unique(RCD$Measure.Name))
Measure.Source <- c(Measure.Source, rep("Readmissions Complications and Deaths - Hospital", 
                                        length(unique(RCD$Measure.ID))))
# pick useful columns
RCD <- RCD[, c("Provider.ID", "Measure.ID", "Denominator", "Score", "Lower.Estimate", "Higher.Estimate")]
# melt
RCD <- melt(data = RCD, na.rm = FALSE, id.vars = c(1:2), measure.vars = c(3:6))
# rename Measure.ID
RCD <- transform(RCD, "Measure.ID" = paste(Measure.ID,"_",variable, sep = ""))
RCD[3] = NULL
# decast
RCD = dcast(RCD, Provider.ID ~ Measure.ID)


# merge
data.merged <- merge_recurse(list(HAC, HAI, HCAHPS, RCD))
# replace "Not Available" with NA
data.merged[data.merged=="Not Available"] = NA


# output merged matrix as .csv file
write.csv(data.merged, "HospitalCompareData.csv", row.names=FALSE)


# build data dictionary
Measure.Description <- data.frame(Measure.ID, Measure.Name, Measure.Source)
# output as .csv file
write.csv(Measure.Description, "Measure Description.csv", row.names=FALSE)