#CAPSTONE PROJECT- MARKET MIX MODELLING

#Loading the required Libraries

library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggcorrplot)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(e1071)
library(ROCR)

options(scipen = 999)


#1-Loading and examining the data
eleckart_df <-read.csv("ConsumerElectronics.csv", stringsAsFactors = F)

glimpse(eleckart_df) #1,648,824 observations and 20 attributes


#2- DATA UNDERSTANDING

colSums(is.na(eleckart_df)) 
#4904 NAs in gmv column
#4904 NAs in cust_id column
#4904 NAs in pincode column

#Checking unique rows in each column

unique_cat <- eleckart_df %>% summarise_all(funs(n_distinct))

View(unique_cat)

#3-DATA CLEANING & DATA MANIPULATION
#removing rows with NA values 

eleckart_df <-na.omit(eleckart_df)
str(eleckart_df) #1643920 Obs. 20 variables


#Renaming the columns
colnames(eleckart_df)[1] <- "fsn_id"
colnames(eleckart_df)[11] <- "payment_type"


length(unique(eleckart_df$fsn_id)) # there are 21216 unique Fsn_id 
length(unique(eleckart_df$sla)) # there are 60 Sla for product delivery to consumer 
eleckart_df$order_date <-date(eleckart_df$order_date)



# Data Sanity check - data from July 2015 to June 2016. 

eleckart_df <- subset(eleckart_df, eleckart_df$order_date > as.Date("2015-06-30") & eleckart_df$order_date < as.Date("2016-07-01"))

# delivery B days & Delivery C days checks - delivery days cannot be -Ve
length(which(eleckart_df$deliverybdays < 0))
length(which(eleckart_df$deliverycdays <0))

# there are too many -ve values - hence not sure about removing them , however these 
# value may not be used in modeling, hence we keep those rows. 

# GMV cannot be more that MRP - GMV is revenue for total number units sold, 
# to compare - we need to multiply MRP with untis sold. Discounts can be applied -
# but price cannot be more than MRP 

length(which(eleckart_df$gmv > eleckart_df$product_mrp*eleckart_df$units)) 
# there are around 38558 incidents where gmv is more than MRP 

#Removing the records where gmv is more than mrp
eleckart_df <- subset(eleckart_df, gmv <= product_mrp*units)


revenue_by_fsi <-aggregate(eleckart_df$gmv, by=list(eleckart_df$fsn_id), sum, na.rm=T)

# Product_ID - CAMDA6RJERF8HS4G is the highest revenue generated product =3,37,130,535 
# product CAMDA6RJERF8HS4G - Camera DSLR 


number_of_prduct_sold_by_fsi <-aggregate(eleckart_df$order_item_id, by=list(eleckart_df$fsn_id), length)

# product Id ACCE6VGHYKYFX4WB is the highest number of products sold. = 36853
# product ACCE6VGHYKYFX4WB - laptop speaker 

aggregate(eleckart_df$gmv, by=list(eleckart_df$product_analytic_sub_category), sum)
aggregate(eleckart_df$gmv, by=list(eleckart_df$product_analytic_category),sum)
aggregate(eleckart_df$gmv, by=list(eleckart_df$product_analytic_super_category),sum)

# creating a year of the week from the order data - as we need to create model based on weeks. 

eleckart_df$week_year <- week(eleckart_df$order_date)

##Getting data from marketing investement and other data sets####

##Preparing the Special Sales Date file
product_list_df <-read.xlsx("Media data and other information.xlsx", sheetName = "Product List", stringsAsFactors =F)
media_investment_df <-read.xlsx("Media data and other information.xlsx", sheetName = "Media Investment", header = F, stringsAsFactors=F)
special_sale_calendar_df <-read.xlsx("Media data and other information.xlsx", sheetName = "Special Sale Calendar")
monthly_npsscore_df <-read.xlsx("Media data and other information.xlsx", sheetName = "Monthly NPS Score")


holiday_date<-c("2015-07-18","2015-07-19","2015-08-15",
                "2015-08-16","2015-08-17","2015-08-28",
                "2015-08-29","2015-08-30","2015-10-15",
                "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10",
                "2015-10-11","2015-10-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26",
                "2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
                "2016-02-20","2016-02-21","2016-02-14","2016-02-15","2016-03-07","2016-03-08",
                "2016-03-09","2016-05-25","2016-05-26","2016-05-27")

dates <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),1)
dates_df <-as.data.frame(dates)


holiday_description <- c("Eid & Rathayatra sale","Eid & Rathayatra sale","Independence Sale", 
                         "Independence Sale","Independence Sale", "Rakshabandhan Sale", 
                        "Rakshabandhan Sale", "Rakshabandhan Sale", "Daussera sale", "Daussera sale",
                        "Daussera sale", "Big Diwali Sale","Big Diwali Sale", "Big Diwali Sale",
                        "Big Diwali Sale","Big Diwali Sale","Big Diwali Sale","Big Diwali Sale",
                        "Big Diwali Sale","Christmas & New Year Sale","Christmas & New Year Sale",
                        "Christmas & New Year Sale","Christmas & New Year Sale","Christmas & New Year Sale",
                        "Christmas & New Year Sale", "Christmas & New Year Sale","Christmas & New Year Sale",
                        "Christmas & New Year Sale", "Republic Day","Republic Day",
                        "Republic Day", "BED", "BED", "FHSD", "FHSD", "Valentine's Day","Valentine's Day","BSD-5",
                        "BSD-5","BSD-5","Pacman","Pacman","Pacman")

Sales_calendar_df <-as.data.frame(holiday_date)
holiday_description <-as.data.frame(holiday_description)

Sales_calendar_df <-cbind(Sales_calendar_df,holiday_description)

Sales_calendar_df$holiday_date <-as.Date(Sales_calendar_df$holiday_date)


Sales_calendar_df<-merge(Sales_calendar_df,dates_df, by.x="holiday_date",by.y="dates",all.y=TRUE)

Sales_calendar_df$holiday_description <-as.character(Sales_calendar_df$holiday_description)

Sales_calendar_df[is.na(Sales_calendar_df)] <-"no special sale"

#Preparing the Product List file

colnames(product_list_df)[1] <-"product"
product_list_df <-product_list_df[-1,]

View(product_list_df)


#Preparing the Media Investment file
media_investment_df <- media_investment_df[-c(1,2),]
colnames(media_investment_df) <- c("year", "month", "total_investment", "tv", "digital", "sponsorship", "content_marketing", "online_marketing", "affiliates", "sem", "radio", "other")
media_investment_df[,-c(1,2)] <-sapply(media_investment_df[,-c(1,2)], as.numeric)

media_investment_df[is.na(media_investment_df)] <-0

media_investment_df[,-c(1,2)] <-round(media_investment_df[,-c(1,2)],2)



str(media_investment_df)


# Let's work on merging marketing investment information with transaction dataset 

media_investment_df$month_year <-paste(media_investment_df$month,media_investment_df$year, sep = "-")


eleckart_df$Year <-as.character(eleckart_df$Year)
eleckart_df$Month <-as.character(eleckart_df$Month)

eleckart_df$month_year <-paste(eleckart_df$Month,eleckart_df$Year, sep = "-")

eleckart_df_merged <- merge(eleckart_df, media_investment_df, by.x = "month_year", by.y = "month_year", all.x=TRUE)

str(eleckart_df)
str(media_investment_df)


length(unique(eleckart_df_merged$product_analytic_vertical))

eleckart_df_merged <-subset(eleckart_df_merged, !(eleckart_df_merged$product_analytic_vertical %in% "\\N"))

length(unique(product_list_df$product))
eleckart_df_merged <- merge(eleckart_df_merged, product_list_df, by.x="product_analytic_vertical",by.y="product",all.x = TRUE)
str(product_list_df)



length(unique(eleckart_df$fsn_id))  
# unique Fsn ids = 20617

length(unique(eleckart_df$sla)) # there are 60 Sla for product delivery to consumer 


aggregate(eleckart_df$gmv, by=list(eleckart_df$sla), sum,na.rm=T)

#Preparing the monthly NPS file
monthly_npsscore_df <-as.data.frame(t(monthly_npsscore_df))

monthly_npsscore_df <-monthly_npsscore_df[-1,]

monthly_npsscore_df <-as.data.frame(monthly_npsscore_df)

colnames(monthly_npsscore_df)[1] <-"nps_score"
monthly_npsscore_df <-na.omit(monthly_npsscore_df)
monthly_npsscore_df$year <- c("2015","2015","2015","2015","2015","2015","2016","2016","2016","2016","2016","2016")
monthly_npsscore_df$month <-c("7","8","9","10","11","12","1","2","3","4","5","6")

monthly_npsscore_df$month_year <-paste(monthly_npsscore_df$month,monthly_npsscore_df$year,sep = "-")

eleckart_df_merged <-merge(eleckart_df_merged, monthly_npsscore_df,by.x = "month_year", by.y = "month_year", all.x=TRUE )
eleckart_df_merged <-eleckart_df_merged[,-c(38,39)]
eleckart_df_merged$nps_score <- as.numeric(eleckart_df_merged$nps_score)

eleckart_df_merged <-merge(eleckart_df_merged, Sales_calendar_df, by.x="order_date", by.y="holiday_date", all.x=TRUE)

str(eleckart_df_merged)
str(Sales_calendar_df)

unique(eleckart_df_merged$product_analytic_sub_category)
unique(eleckart_df_merged$product_analytic_category)
unique(eleckart_df_merged$product_analytic_super_category)

gmv_by_subCat <-aggregate(eleckart_df_merged$gmv, by=list(eleckart_df_merged$product_analytic_sub_category), sum)

eleckart_df_merged$cust_id <- gsub("-", "", eleckart_df_merged$cust_id)

eleckart_df_merged <- eleckart_df_merged[, -c(23,24)]


#============================
# EDA - Exploratory data analysis - looking at final dataset based on different KPIs and variables. 
#=============================

#Here we first created a week column,product_mrp
#Merged the columns related to Media Spending, NPS score, Special Holiday Sale and Product description

glimpse(eleckart_df_merged)


#year with cust_id - Slightly more customers in the year 2016
eleckart_df_merged %>% 
  group_by(Year)%>%
  summarise(total = length(cust_id))%>%mutate(percent = prop.table(total)*100)%>%
  ggplot(aes(reorder(Year,-percent), y = percent, fill = Year))+ geom_col()+
  xlab('Year')+ylab('Percent of cust_ids')+ theme_bw()


#Year Vs order_item id
#We find the percent of orders placed in 2016 slightly higher than 2015

eleckart_df_merged %>% 
  group_by(Year)%>%
  summarise(total = length(order_item_id))%>%mutate(percent = prop.table(total)*100)%>%
  ggplot(aes(reorder(Year,-percent), y = percent, fill = Year))+ geom_col()+
  xlab('Year')+ylab('Percent of Order_item_ids')+theme_bw()


#Month Vs. Order_id

#In the month of sep and oct most orders are placed
#In the month of August there are no orders
eleckart_df_merged %>% 
  group_by(Month)%>%
  summarise(total = length(order_id))%>%mutate(percent = prop.table(total)*100)%>%
  ggplot(aes(reorder(Month,-percent), y = percent, fill = Month))+ geom_col()+
  xlab('Month')+ylab('Percent of order_ids')+theme_bw()



#Month Vs. gmv
#total gmv is highest in the month of october, followed by december
#Negligible total gmv in the month of August

eleckart_df_merged %>% 
  group_by(Month)%>% 
  summarise(total = sum(gmv))%>%
  ggplot(aes(reorder(Month,-total), y = total, fill = Month))+ geom_col()+
  xlab('Month')+ylab('Distribution of gmv w.r.t Months')+theme_bw()



#Month Vs. SLA
#On an average it takes 6 days to deliver a product in the month of July which is highest
#Around 5.5 days in the month of Oct, Dec and Sep

eleckart_df_merged %>% 
  group_by(Month)%>% 
  summarise(AverageSLA = mean(sla))%>% 
  ggplot(aes(reorder(Month,-AverageSLA), y = AverageSLA, fill = Month))+
  geom_col()+xlab('Month')+ylab('Distribution of AverageSLA w.r.t Months')+theme_bw()



#Product Category Vs. gmv
#Most of the revenue is from Camera sub category

eleckart_df_merged %>% 
  group_by(product_analytic_category)%>% 
  summarise(total = sum(gmv))%>% #mutate(percent = prop.table(total)*100)%>%
  ggplot(aes(reorder(product_analytic_category,total), y = total, fill = product_analytic_category))+
  geom_col()+xlab('product_analytic_category')+ylab('Total GMV')+
  coord_flip()+
  theme_bw()+theme(legend.position = "none")


eleckart_df_merged %>% 
  group_by(product_analytic_sub_category)%>% 
  summarise(total = sum(gmv))%>% 
  ggplot(aes(reorder(product_analytic_sub_category,total), y = total, fill = product_analytic_sub_category))+
  geom_col()+coord_flip()+
  xlab('product_analytic_sub_category')+ylab('Total GMV')+
  theme_bw()+theme(legend.position = "none")


#Product category Vs. order id
#Most of the Orders are in EntertainmentSmall category

eleckart_df_merged %>% 
  group_by(product_analytic_category)%>% 
  summarise(total = length(order_id))%>% #mutate(percent = prop.table(total)*100)%>%
  ggplot(aes(reorder(product_analytic_category,-total), y = total, fill = product_analytic_category))+
  geom_col()+xlab('product_analytic_category')+ylab('Distribution of order_id w.r.t product_analytic_category')+
  theme_bw()+theme(legend.position = "none")

#Most of the orders are in speakers sub category, followed by camera accesory and gaming accesory
eleckart_df_merged %>% 
  group_by(product_analytic_sub_category)%>% 
  summarise(total = length(order_id))%>% 
  ggplot(aes(reorder(product_analytic_sub_category,total), y = total, fill = product_analytic_sub_category))+
  geom_col()+coord_flip()+
  xlab('product_analytic_sub_category')+ylab('Distribution of order_id w.r.t product_analytic_sub_category')+
  theme_bw()+theme(legend.position = "none")



#Payment_type Vs. gmv
#Preferred payment type id COD

eleckart_df_merged %>% 
  group_by(payment_type)%>% 
  summarise(total = sum(gmv))%>% 
ggplot(aes(reorder(payment_type,total), y = total, fill = payment_type))+
  geom_col()+xlab('Payment type')+ylab('Total gmv')+
  theme_bw()+theme(legend.position = "none")


# Average gmv is highest during Republic day sale

eleckart_df_merged %>% 
  group_by(holiday_description)%>% 
  summarise(total = mean(gmv))%>% 
  ggplot(aes(reorder(holiday_description,total), y = total, fill = holiday_description))+
  geom_col()+xlab('Holiday')+ylab('Mean gmv')+coord_flip()+
  theme_bw()+theme(legend.position = "none")


# Total gmv- though most of the sales happening during no special sales- out of sales calendar Daussera & Big Diwali
# Sale have highest sales. 
eleckart_df_merged %>% 
  group_by(holiday_description)%>% 
  summarise(total = sum(gmv))%>% 
  ggplot(aes(reorder(holiday_description,total), y = total, fill = holiday_description))+
  geom_col()+xlab('Holiday')+ylab('Total gmv')+coord_flip()+
  theme_bw()+theme(legend.position = "none")


#NPS Score- The average NPS Score is less in the month of October when the average media investments are highest.

eleckart_df_merged %>% group_by(Month) %>% summarise(avgNPS = mean(nps_score), avgInv = mean(total_investment))%>%
  ggplot(aes(Month, avgInv, size = avgNPS)) + geom_point() + theme_bw()



#The variation of average GMV on a weekly basis shows that after every peak in average GMV, a  gradual fall is recorded in the subsequent weeks.
#However, there is a heavy fall in average GMV in week 32.

eleckart_df_merged %>% group_by(week_year)%>% 
  summarise(avgGMV = mean(gmv))%>%
  ggplot(aes(week_year, avgGMV)) + geom_line()+ geom_point()+
  theme_bw()


#Average Investments on a weekly level

eleckart_df_merged %>% group_by(week_year)%>% 
  summarise(avgInv = mean(total_investment))%>%
  ggplot(aes(week_year, avgInv)) + geom_line()+ geom_point()+theme_bw()

#Average NPS on a weekly basis 

eleckart_df_merged %>% group_by(week_year)%>% 
  summarise(avgNPS = mean(nps_score))%>%
  ggplot(aes(week_year, avgNPS)) + geom_line()+ geom_point()+theme_bw()
            


#4- CHECKING THE VARIATION OF NUMERIC VARIABLES W.R.T GMV(TARGET)

num_col <- eleckart_df_merged[,unlist(lapply(eleckart_df_merged,is.numeric))]
num_col <- num_col[,-c(1,2,6)]

data_cor <- cor(num_col)

ggcorrplot(data_cor)

#We find that gmv is highly correlated to product mrp and has a slight negative correlation with sla
#It has a slight negative correlation with advertisement spent on product.

####------------------------------------------------------------------------------------------------------------------

#Data Preparation for Modelling

#CameraData
trainCamera <- subset(eleckart_df_merged, product_analytic_sub_category == "CameraAccessory")

str(trainCamera) #We have 231089 obs. 

#Product vertical

trainCamera %>% group_by(product_analytic_vertical)%>% summarise(total = n())

#Final dataset

trainCamera <- trainCamera[, -c(1:8, 11,12,15:19)]

#bringing the week column in first and gmv at last

trainCamera <- trainCamera[, c(7, (1:ncol(trainCamera))[-7])]

#Converting categorical to dummy

names1 <- c("payment_type", "holiday_description")
trainCamera[names1] <- sapply(trainCamera[names1], factor)
trainCameraFact <- trainCamera[,c(4, 21)]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(trainCameraFact, function(x) data.frame(model.matrix(~x-1,data =trainCameraFact))[,-1]))

trainCamera <- cbind(trainCamera[,-c(4,21)], dummies)


########################################################################
# splitting the data between train and test
set.seed(100)

indices1 = sample.split(trainCamera$gmv, SplitRatio = 0.7)

train1 = trainCamera[indices1,]

test1 = trainCamera[!(indices1),]

######################################################################
#Linear regression

model_1 <-lm(gmv~.,data=train1[,c(-1)])
summary(model_1)

step <- stepAIC(model_1, direction="both")
step


model_2 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                total_investment + tv + digital + sponsorship + content_marketing + 
                online_marketing + affiliates + sem + other + Frequency + 
                nps_score + payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + holiday_description.xno.special.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])


summary(model_2)

vif(model_2)

#Remove Total Investment due to high vif

model_3 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship + content_marketing + 
                online_marketing + affiliates + sem + other + Frequency + 
                nps_score + payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + holiday_description.xno.special.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_3)

vif(model_3)

#Removing affiliates due to high vif and low p-value

model_4 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship + content_marketing + 
                online_marketing + sem + other + Frequency + 
                nps_score + payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + holiday_description.xno.special.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_4)

vif(model_4)

#Removing content marketing due to high vif and low p-value
model_5 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship + 
                online_marketing + sem + other + Frequency + 
                nps_score + payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + holiday_description.xno.special.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_5)

vif(model_5)

#removing SEM due to high vif and p-value

#Removing content marketing due to high vif and low p-value
model_6 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship + 
                online_marketing + other + Frequency + 
                nps_score + payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + holiday_description.xno.special.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_6)

vif(model_6)

#Removing NPS score due to high p-value and vif

model_7 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship + 
                online_marketing + other + Frequency + 
                payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + holiday_description.xno.special.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_7)

vif(model_7)

#Both digital and sponsorship have vif more than 2, but they have low p-value and are highly significant
#Now we will remove the variables based on p-values only
#Online marketing has high p-value, will remove that now

model_8 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship +other + Frequency + 
                payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + holiday_description.xno.special.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_8)

#No special sale has high p-value, will get rid of that

model_9 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship +other + Frequency + 
                payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xFHSD + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_9)

#holiday_description.xFHSD  has high p-value, will get rid of that

model_10 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                tv + digital + sponsorship +other + Frequency + 
                payment_type + holiday_description.xBSD.5 + holiday_description.xDaussera.sale + 
                holiday_description.xPacman, data = train1[, c(-1)])

summary(model_10)


#holiday_description.xBSD.5   has high p-value, will get rid of that

model_11 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                 tv + digital + sponsorship +other + Frequency + 
                 payment_type + holiday_description.xDaussera.sale + 
                 holiday_description.xPacman, data = train1[, c(-1)])

summary(model_11)


#holiday_description.xPacman   has high p-value, will get rid of that

model_12 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                 tv + digital + sponsorship +other + Frequency + 
                 payment_type + holiday_description.xDaussera.sale, data = train1[, c(-1)])

summary(model_12)


#tv  has high p-value, will get rid of that

model_13 <- lm(formula = gmv ~ units + sla + product_mrp + product_procurement_sla + 
                 digital + sponsorship +other + Frequency + 
                 payment_type + holiday_description.xDaussera.sale, data = train1[, c(-1)])

summary(model_13)

#In model_13 we have all significant variables for Camera Accessory category

##################################################
Predict_1 <- predict(model_13,test1[,-c(1,2)])
test1$test_gmv <- Predict_1

r <- cor(test1$gmv,test1$test_gmv)
rsquared <- cor(test1$gmv,test1$test_gmv)^2
rsquared

#Our training model gave an adjusted Rsquared of 0.88 and test data prediction gave an R-squared of 0.85
#The significant variables for camera accessort sub category are- Units, SLA, Product_mrp, Product_procurement_sla,
#digital,sponsorship, other, frequency,payment_type and dussehra sale.
