library(readxl)
library(xlsx)
library(dplyr)

# dataset
data <- as.data.frame(read_excel("C:/Users/Mahdi/Documents/R/M/ckd.xlsx"))
data[data=="?"]<-NA

# convert column to numeric , in this dataset 1 to 14 convert to numeric
data[, c(1:14)] <- sapply(data[,  c(1:14)], as.numeric)



# 1   drop all row which have missing value
list_na <- colnames(data)[ apply(data, 2, anyNA) ]
list_na
data_drop <-data %>%
  na.omit()
dim(data_drop)
FinalData = data_drop

