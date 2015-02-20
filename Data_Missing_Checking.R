# title: Data_Missing_Checking
# author: Ivy Yang
# version: 1.0


# define the method to count the 'Not Available' values in each column
missing.count <- function(x, list = FALSE, type){
  if (list == TRUE) {
    counts = c()
    for (j in 2:ncol(x)) {
      df = x[, j]
      count = 0
      for (i in 1:length(df)) {
        if (type == "Not Available") {
          if (!is.na(df[i]) & df[i] == 'Not Available')
            count = count + 1
        } else if (type == "Not Available*") {
          if(!is.na(df[i]) & df[i] == 'Not Available*')
            count = count + 1
        } else if (type == "non-NA") {
          if(!is.na(df[i]))
            count = count + 1
        }
      }
      counts = c(counts, count) 
    }
    counts
  }
  else{
    count = 0
    for(i in 1:length(x)){
      if (type == "Not Available") {
        if (!is.na(x[i]) & x[i] == 'Not Available')
          count = count + 1
      } else if (type == "Not Available*") {
        if(!is.na(x[i]) & x[i] == 'Not Available*')
          count = count + 1
      } else if (type == "non-NA") {
        if(!is.na(x[i]))
          count = count + 1
      }
    }
    count
  }
}



# load processed file
data.pro <- read.csv("Data_pro.csv", header = T, colClasses = "character")

# count 'Not Available' value for each column
data.not.available <- sapply(data.pro[, 2:length(data.pro)], FUN = missing.count, type = "Not Available")
# count 'Not Available *' value for each column
data.not.available.star <- sapply(data.pro[, 2:length(data.pro)], FUN = missing.count, type = "Not Available*")
# count non NA value for each column
data.non.na <- sapply(data.pro[, 2:length(data.pro)], FUN = missing.count, type = "non-NA")



# check HAC file
# load HAC file
HAC <- read.csv("HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL_11_24_2014.csv", header = T, colClasses = 'character')
# pick useful columns
HAC <- HAC[, c("Domain_1_Score", "AHRQ_PSI_90_Score", 
               "Domain_2_Score", "CLABSI_Score", "CAUTI_Score", "Total_HAC_Score")]

# count 'Not Available' value for each column
HAC.not.available <- sapply(HAC, FUN = missing.count, type = "Not Available")
# count 'Not Available*' value for each column
HAC.not.available.star <- sapply(HAC, FUN = missing.count, type = "Not Available*")
# count non NA value for each column
HAC.non.na <- sapply(HAC, FUN = missing.count, type = "non-NA")

# check count of 'Not Available'
all(HAC.not.available == data.not.available[colnames(HAC)])
# check count of 'Not Available*'
all(HAC.not.available.star == data.not.available.star[colnames(HAC)])
# check count of non-NA values
all(HAC.non.na == data.non.na[colnames(HAC)])



# check HAI file
# load HAI file
HAI <- read.csv("Healthcare Associated Infections - Hospital.csv", header = T, colClasses = 'character')
# pick useful columns
HAI <- HAI[, c("Measure.ID", "Score")]
# subset by measures
HAI <- split(HAI, HAI$Measure.ID)

# count 'Not Available' value for each column
HAI.not.available <- sapply(HAI, FUN = missing.count, type = "Not Available", list = TRUE)
# count non NA value for each column
HAI.non.na <- sapply(HAI, FUN = missing.count, type = "non-NA", list = TRUE)

# check count of 'Not Available'
all(HAI.not.available == data.not.available[names(HAI)])
# check count of non-NA values
all(HAI.non.na == data.non.na[names(HAI)])



# check HCAHPS file
# load HCAHPS file
HCAHPS <- read.csv("HCAHPS - Hospital.csv", header = T, colClasses = "character")
# pick useful columns 
HCAHPS <- HCAHPS[, c("HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")]
# subset by measures
HCAHPS <- split(HCAHPS, HCAHPS$HCAHPS.Measure.ID)

# count 'Not Available' value for each column
HCAHPS.not.available <- sapply(HCAHPS, FUN = missing.count, type = "Not Available", list = TRUE)
# count non NA value for each column
HCAHPS.non.na <- sapply(HCAHPS, FUN = missing.count, type = "non-NA", list = TRUE)

# check count of 'Not Available'
all(HCAHPS.not.available == data.not.available[names(HCAHPS)])
# check count of non-NA values
all(HCAHPS.non.na == data.non.na[names(HCAHPS)])



# check RCD file
# load RCD file
RCD <- read.csv("Readmissions Complications and Deaths - Hospital.csv", header = T, colClasses = 'character')
# pick useful columns
RCD <- RCD[, c("Measure.ID", sort(c("Denominator", "Score", "Lower.Estimate", "Higher.Estimate")))]
# subset by measures
library(plyr)
RCD <- split(RCD, RCD$Measure.ID)

# count 'Not Available' value for each column
RCD.not.available <- sapply(RCD, FUN = missing.count, type = "Not Available", list = TRUE)
# count non NA value for each column
RCD.non.na <- sapply(RCD, FUN = missing.count, type = "non-NA", list = TRUE)

# get measure names
RCD.measure.1 <- names(RCD)
RCD.measure.2<- c("Denominator", "Score", "Lower.Estimate", "Higher.Estimate")
RCD.measure <- sort(apply(expand.grid(RCD.measure.1, RCD.measure.2), 1, function(x) paste(x,collapse="_")))
RCD.measure

# check count of 'Not Available'
all(RCD.not.available == data.not.available[RCD.measure])
# check count of non-NA values
all(RCD.non.na == data.non.na[RCD.measure])