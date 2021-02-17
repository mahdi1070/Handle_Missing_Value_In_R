library(readxl)
library(xlsx)
library(dplyr)


# dataset
# change this path to your dataset path
datasetPath <- "C:/Users/Mahdi/Documents/R/M/ckd.xlsx"  

# sample dataset
data <- as.data.frame(read_excel(datasetPath))

data[data=="?"]<-NA

# convert column to numeric , in this dataset 1 to 14 convert to numeric
data[, c(1:14)] <- sapply(data[,  c(1:14)], as.numeric)




# list_na consist numeric columns .
list_na <- colnames(data)[c(1:14)]
list_na

# calculate median of all numeric columns
median_missing <- apply(data[,colnames(data) %in% list_na],
                        2,
                        median,
                        na.rm =  TRUE)
median_missing


# replace missing value with median 
data_replace <- data %>%
  mutate(replace_median_age  = ifelse(is.na(age), median_missing[1], age),
         replace_median_bp = ifelse(is.na(bp), median_missing[2], bp),
         replace_median_sg = ifelse(is.na(sg), median_missing[3], sg),
         replace_median_al = ifelse(is.na(al), median_missing[4], al),
         replace_median_su = ifelse(is.na(su), median_missing[5], su),
         replace_median_bgr = ifelse(is.na(bgr), median_missing[6], bgr),
         replace_median_bu = ifelse(is.na(bu), median_missing[7], bu),
         replace_median_sc = ifelse(is.na(sc), median_missing[8], sc),
         replace_median_sod = ifelse(is.na(sod), median_missing[9], sod),
         replace_median_pot = ifelse(is.na(pot), median_missing[10], pot),
         replace_median_hemo = ifelse(is.na(hemo), median_missing[11], hemo),
         replace_median_pcv = ifelse(is.na(pcv), median_missing[12], pcv),
         replace_median_wbcc = ifelse(is.na(wbcc), median_missing[13], wbcc),
         replace_median_rbcc = ifelse(is.na(rbcc), median_missing[14], rbcc))


# data2 is a new matrix consist numeric columns without missing value.
data2 <- data_replace[,c(26:39)]


# function for find majority value in columns with type character
helperFunc <- function(x){
  sample(levels(x), sum(is.na(x)), replace = TRUE,
         prob = as.numeric(table(x))/sum(!is.na(x)))
}


# data3 is a new matrix consist character columns with missing value.
data3 <- data_replace[,c(15:24)]

# convert all character in data3 to factor
data3[sapply(data3, is.character)] <- lapply(data3[sapply(data3, is.character)],
                                            as.factor)


# data3 without missing value.
data3[sapply(data3, is.na)]  <- unlist(sapply(data3, helperFunc))



# FinalData is a orgin dataset without missing value.
FinalData <- cbind(data2 , data3 ,  data$`class@data`)
colnames(FinalData)[25] <- "class@data"




