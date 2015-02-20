
#Author:Joyce Xu
#Version:Version_1
#Date: 2/12/2015

#Data to be transformed to numeric
char.data = read.csv(file = "Data_pro_header.csv", header = T,stringsAsFactors=FALSE)

#Transform function: to transform from character to numeric, eliminal all non character value
toNumeric <- function(x){
  x = as.numeric(gsub("[^0-9.]", "", x))
}

#Transform all column except Provider_ID to nmeric, Transform all non numeric value to NA
cols = c(2:ncol(char.data)); 
char.data[,cols] = apply(char.data[,cols], 2, FUN = toNumeric)

# Output the transferd file as csv format
write.csv(char.data, "Data_Pro_numeric.csv", row.names=FALSE)
