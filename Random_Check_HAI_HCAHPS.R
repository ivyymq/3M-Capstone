# title: Random_Check_HAI_HCAHPS
# author: Ivy Yang
# version: 1.1



# define a function to check each measure in the random rows
# inputs:
#   IDs - random IDs of raw data (vector)
#   measures - unique Measure.IDs of raw data (vector)
#   raw - raw data (data frame with three columns(Provider.ID, Measure.ID, values))
#   pro - pro data (data frame with Provider.ID and other measure columns)
# outputs:
#   if check successfully, print "Check Successfully!"
#   if there are errors, print the error list
check <- function(IDs, measures, raw, pro) {
  report <- c()
  # pick providers one by one
  for(id in 1:length(IDs)) {
    # check all measures one by one
    for(measure in 1:length(measures)) {
      # get raw value
      raw.value <- with(raw, raw[Provider.ID == IDs[id] & 
                                   Measure.ID == measures[measure], 3])
      # get processed value
      pro.value <- with(pro, pro[Provider.ID == IDs[id], measures[measure]])
      # see if correct
      if (raw.value != pro.value) {
        report <- c(report, paste("Hospital ",IDs[id], " meausre ", measures[measure], sep = ""))
      }
    }
    print(paste(trunc(id / length(IDs) * 100), "% checked", sep = ''))
  }
  if (length(report) == 0)
    return("Check Successfully!")
  else
    return(report)
}



# load processed matrix
data.pro <- read.csv("Data_pro.csv", header = T, colClasses = "character")

# check HAI
# load Healthcare Associated Infections - Hospital.csv
HAI.raw <- read.csv("Healthcare Associated Infections - Hospital.csv", header = T, colClasses = 'character')
HAI.raw <- HAI.raw[, c("Provider.ID", "Measure.ID", "Score")]
colnames(HAI.raw)[1:2] <- c("Provider.ID", "Measure.ID")

# get IDs from raw data
HAI.IDs <- unique(HAI.raw$Provider.ID)

# get measures from raw data
HAI.measures <- unique(HAI.raw$Measure.ID)

# get processed HAI data
HAI.pro <- data.pro[, c(1, which(colnames(data.pro) %in% HAI.measures))]

# get random IDs
HAI.IDs.random <- HAI.IDs[sample(1:length(HAI.IDs), length(HAI.IDs)/10, replace=F)]

# check HAI file
HAI.check <- check(HAI.IDs.random, HAI.measures, HAI.raw, HAI.pro)
HAI.check

# check HCAHPS
# load HCAHPS - Hospital.csv
HCAHPS.raw <- read.csv("HCAHPS - Hospital.csv", header = T, colClasses = "character")
HCAHPS.raw <- HCAHPS.raw[, c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Answer.Percent")]
colnames(HCAHPS.raw)[1:2] <- c("Provider.ID", "Measure.ID")

# get IDs from raw data
HCAHPS.IDs <- unique(HCAHPS.raw$Provider.ID)

# get measures from raw data
HCAHPS.measures <- unique(HCAHPS.raw$Measure.ID)

# get processed HCAHPS data
HCAHPS.pro <- data.pro[, c(1, which(colnames(data.pro) %in% HCAHPS.measures))]

# generate random IDs
HCAHPS.IDs.random <- HCAHPS.IDs[sample(1:length(HCAHPS.IDs), length(HCAHPS.IDs)/10, replace=F)]

# check HCAHPS file
HCAHPS.check <- check(HCAHPS.IDs.random, HCAHPS.measures, HCAHPS.raw, HCAHPS.pro)
HCAHPS.check
