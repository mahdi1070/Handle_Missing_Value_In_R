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

# calculate mean of all numeric columns
average_missing <- apply(data[,colnames(data) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing



# replace missing value with mean 
data_replace <- data %>%
  mutate(replace_mean_age  = ifelse(is.na(age), average_missing[1], age),
         replace_mean_bp = ifelse(is.na(bp), average_missing[2], bp),
         replace_mean_sg = ifelse(is.na(sg), average_missing[3], sg),
         replace_mean_al = ifelse(is.na(al), average_missing[4], al),
         replace_mean_su = ifelse(is.na(su), average_missing[5], su),
         replace_mean_bgr = ifelse(is.na(bgr), average_missing[6], bgr),
         replace_mean_bu = ifelse(is.na(bu), average_missing[7], bu),
         replace_mean_sc = ifelse(is.na(sc), average_missing[8], sc),
         replace_mean_sod = ifelse(is.na(sod), average_missing[9], sod),
         replace_mean_pot = ifelse(is.na(pot), average_missing[10], pot),
         replace_mean_hemo = ifelse(is.na(hemo), average_missing[11], hemo),
         replace_mean_pcv = ifelse(is.na(pcv), average_missing[12], pcv),
         replace_mean_wbcc = ifelse(is.na(wbcc), average_missing[13], wbcc),
         replace_mean_rbcc = ifelse(is.na(rbcc), average_missing[14], rbcc))





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
