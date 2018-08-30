library(tidyverse)

#Loading the data
data <- read.csv("ConsumerElectronics.csv")

#Checking the data

glimpse(data) #1,648,824 observations and 20 attributes

#Checking NA

colSums(is.na(data)) 
#4904 NAs in gmv column
#4904 NAs in cust_id column
#4904 NAs in pincode column

#Checking unique rows in each column

data %>% summarise_all(funs(n_distinct))

#There are 1201090 unique customer_ids
#1501177 unique order_id
