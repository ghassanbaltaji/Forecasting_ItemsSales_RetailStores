install.packages("tidyverse")
library('tidyverse')
install.packages("lubridate")
library('lubridate')
install.packages("gclus")
library('gclus')
install.packages("openair")
library("openair")
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
#Load the sales data set and check the first 6 rows
sales_Data <- read.csv("sales_data.csv")
head(sales_Data)

#In the data set, there is a variable without name which I consider an unknown
#variable, and it contains many missing values which is not consistent to the complete data set.
#Replacing the missing value with 0 or by the mean value is not relevant, this is 
#because the variable seems to belong to a class set, and I can not estimate its exact value.
#so I gave it a random name 'xx' to load the data set, and then I remove it.
#Not giving it a name in the csv file will collapse the structure of the data set
sales_Data <- select(sales_Data, -c(xx))

#Get more info from the data set 
names(sales_Data)
class(sales_Data)
summary(sales_Data)

#Load the store coordinates
store_master_Data <- read.csv("store_master.csv")

#Remove unknown variables
store_master_Data <- select(store_master_Data, -c(x,y))

#Get info from the store coordinates data set
head(store_master_Data)
names(store_master_Data)

#Plotting the location of every store on a 2d map
#For example, data point of coordinates (55,491191) is the store number 78 which is
#located in the mid-east region 
longitude <- store_master_Data$longitude
latitude <- store_master_Data$latitude
plot(longitude ~ latitude , data=store_master_Data)

#Combine the data sets to form one data set
data <- merge(sales_Data, store_master_Data,by="store")
head(data)
summary(data)

#Grab a feature from latitude and longitude variables
geom_insight = c()
for (i in 1:nrow(data)) {
  if ( (data$latitude[i] == 51) & (data$latitude[i]/data$longitude[i] < 10000) ) {
    geom_insight[i] = "Hot Spot"
  } else if  ( (data$latitude[i] == 52) & (data$latitude[i]/data$longitude[i] < 10000) ) {
    geom_insight[i] = "Hot Spot"
  } else {
    geom_insight[i] = "Normal Spot"
  }  
}
data["geom_insight"] <- geom_insight 

#Add season variable
seasons = function(x){
  if(x %in% 3:5) return("Spring")
  if(x %in% 6:8) return("Summer")
  if(x %in% 9:11) return("Fall")
  if(x %in% c(12,1,2)) return("Winter")
}
data$Season = sapply(month(data$date), seasons)

#Add holiday effect
holiday = function(x){
  if(x %in% c(12)) return("Christmas")
  else return("Not Christmas")
}
data$Holiday = sapply(month(data$date), holiday)

#data info
str(data)
summary(data)

#Scatter Plot/Correlations
ggplot(data = data, aes(x = item, y = item_category)) + geom_point(color = 'red')
ggplot(data = data, aes(x = item, y = unit_price)) + geom_point(color = 'blue')
ggplot(data = data, aes(x = item_category, y = unit_price)) + geom_point(color = 'green')
ggplot(data = data, aes(x = unit_price, y = qty)) + geom_point(color = 'orange')
ggplot(data = data, aes(x = store, y = qty)) + geom_point(color = 'purple')


#Looking at the data set as a whole with its variables, my projection is total quantity/item/day,
#so I will group the total quantities of items per day regardless of the store, so we construct a new data set
#where model building takes place. 
#The data that we are interested in in order to predict the sales quantity of an item per day
myvars<- c("date", "item", "qty", "unit_price")
df1 <- data[myvars]
final_data <- (df1 %>% 
        group_by(date = df1$date, item = df1$item, unit_price = df1$unit_price) %>%
        summarise(qty = sum(qty)))
class(df1$date)
final_data$date <- as.Date(final_data$date)
#Plot a graph of total sales quantities of all items per day
dfGroup <- final_data %>% group_by(date) %>% summarise(qty = sum(qty))
class(dfGroup$date)
dfGroup$date <- as.Date(dfGroup$date)
class(dfGroup$date)
require(ggplot2)
ggplot( data = dfGroup, aes( date, qty)) + geom_line() 

#From the graph, we can observe a similar pattern in the year interval(yearly trend),
#so it can be assumed that each day of the year behaves as a factor variable
#The model contains numeric and categorical variables, so we need to use the dummy coding to
final_data["day_number"] <- yday(final_data$date)
final_data$day_number <- factor(final_data$day_number)
final_data$item <- factor(final_data$item)


#Model building (Multivariate Linear Regression Model)
regression_model <- lm(qty ~ item + unit_price + day_number
                       , data = final_data)

options(max.print=1000000)
summary(regression_model)

#Total quantity for every item
item_quantity_count <- aggregate(final_data$qty, by=list(Category=final_data$item), FUN=sum)
item_quantity_count
plot(item_quantity_count)
#Choose items to forecast depending on the average of quantity sales per items
#We try to avoid outliers as they will not be in accordance with the model

#Items: 73, 78, 114, 225, 234, 401, 434, 469, 502, 589

#item 73 in the chosen date (2019/04/01 -> 2019/04/30) : day 91 -> day 120
#Plot quantity vs time graph for item 73
final_data_item_73 <- final_data %>% filter(item == 73)

plot_73 <- final_data_item_73 %>% 
        group_by(date = final_data_item_73$date) %>%
        summarise(qty = qty)
ggplot( data = plot_73, aes(date, qty)) + geom_line() 


predicted_value_item_73 = c()
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + -6.78386 + (-12.50837 * 32) 
predicted_value_item_73[1] = round(-292.58728 + common_factor)
predicted_value_item_73[2] = round(-95.28736 + common_factor)
predicted_value_item_73[3] = round(-168.89492 + common_factor)
predicted_value_item_73[4] = round(-120.84692 + common_factor)
predicted_value_item_73[5] = round(-125.36357 + common_factor)
predicted_value_item_73[6] = round(-61.27437+ common_factor)
predicted_value_item_73[7] = round(-103.75892 + common_factor)
predicted_value_item_73[8] = round(-34.95120 + common_factor)
predicted_value_item_73[9] = round(-23.96419 + common_factor)
predicted_value_item_73[10] = round(-140.63805+ common_factor)
predicted_value_item_73[11] = round(-104.56445 + common_factor)
predicted_value_item_73[12] = round(-69.96048 + common_factor)
predicted_value_item_73[13] = round(-92.45942  + common_factor)
predicted_value_item_73[14] = round(-211.74954+ common_factor)
predicted_value_item_73[15] = round(-234.57320   + common_factor)
predicted_value_item_73[16] = round(-41.51517+ common_factor)
predicted_value_item_73[17] = round(2.35805 + common_factor)
predicted_value_item_73[18] = round(89.30169 + common_factor)
predicted_value_item_73[19] = round(106.81296 + common_factor)
predicted_value_item_73[20] = round(67.32630  + common_factor)
predicted_value_item_73[21] = round(-94.24954  + common_factor)
predicted_value_item_73[22] = round(-181.65704 + common_factor)
predicted_value_item_73[23] = round(-79.30423 + common_factor)
predicted_value_item_73[24] = round(-70.43561 + common_factor)
predicted_value_item_73[25] = round(-3.19010 + common_factor)
predicted_value_item_73[26] = round(-25.90156 + common_factor)
predicted_value_item_73[27] = round(-33.74083+ common_factor)
predicted_value_item_73[28] = round(-145.97187  + common_factor)
predicted_value_item_73[29] = round(-149.26038 + common_factor)
predicted_value_item_73[30] = round(-113.32447 + common_factor)

predicted_value_item_73


forecast_date <- c("2019/04/01","2019/04/02","2019/04/03", "2019/04/04", "2019/04/05", "2019/04/06",
                   "2019/04/07","2019/04/08","2019/04/09","2019/04/10","2019/04/11","2019/04/12",
                   "2019/04/13","2019/04/14","2019/04/15","2019/04/16","2019/04/17","2019/04/18",
                   "2019/04/19","2019/04/20","2019/04/21","2019/04/22","2019/04/23","2019/04/24",
                   "2019/04/25","2019/04/26","2019/04/27","2019/04/28","2019/04/29","2019/04/30")

#table result
table_item_73 <- data.frame(forecast_date, predicted_value_item_73)
table_item_73

#Item 78
#Extract data of item 78 
final_data_item_78 <- final_data %>% filter(item == 78)
summary(final_data_item_78)
#Plot quantity vs time graph for item 78
plot_78 <- final_data_item_78 %>% 
  group_by(date = final_data_item_78$date) %>%
  summarise(qty = qty)
ggplot( data = plot_78, aes(date, qty)) + geom_line() 

predicted_value_item_78 = c()
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + -560.24128 + (-12.50837 * 12) 
predicted_value_item_78[1] = round(-292.58728 + common_factor)
predicted_value_item_78[2] = round(-95.28736 + common_factor)
predicted_value_item_78[3] = round(-168.89492 + common_factor)
predicted_value_item_78[4] = round(-120.84692 + common_factor)
predicted_value_item_78[5] = round(-125.36357 + common_factor)
predicted_value_item_78[6] = round(-61.27437+ common_factor)
predicted_value_item_78[7] = round(-103.75892 + common_factor)
predicted_value_item_78[8] = round(-34.95120 + common_factor)
predicted_value_item_78[9] = round(-23.96419 + common_factor)
predicted_value_item_78[10] = round(-140.63805+ common_factor)
predicted_value_item_78[11] = round(-104.56445 + common_factor)
predicted_value_item_78[12] = round(-69.96048 + common_factor)
predicted_value_item_78[13] = round(-92.45942  + common_factor)
predicted_value_item_78[14] = round(-211.74954+ common_factor)
predicted_value_item_78[15] = round(-234.57320   + common_factor)
predicted_value_item_78[16] = round(-41.51517+ common_factor)
predicted_value_item_78[17] = round(2.35805 + common_factor)
predicted_value_item_78[18] = round(89.30169 + common_factor)
predicted_value_item_78[19] = round(106.81296 + common_factor)
predicted_value_item_78[20] = round(67.32630  + common_factor)
predicted_value_item_78[21] = round(-94.24954  + common_factor)
predicted_value_item_78[22] = round(-181.65704 + common_factor)
predicted_value_item_78[23] = round(-79.30423 + common_factor)
predicted_value_item_78[24] = round(-70.43561 + common_factor)
predicted_value_item_78[25] = round(-3.19010 + common_factor)
predicted_value_item_78[26] = round(-25.90156 + common_factor)
predicted_value_item_78[27] = round(-33.74083+ common_factor)
predicted_value_item_78[28] = round(-145.97187  + common_factor)
predicted_value_item_78[29] = round(-149.26038 + common_factor)
predicted_value_item_78[30] = round(-113.32447 + common_factor)

predicted_value_item_78

#table result
table_item_78 <- data.frame(forecast_date, predicted_value_item_78)
table_item_78

#Item 114
#Extract data of item 114 
final_data_item_114 <- final_data %>% filter(item == 114)
summary(final_data_item_114)

#Plot quantity vs time graph for item 114
plot_114 <- final_data_item_114 %>% 
  group_by(date = final_data_item_114$date) %>%
  summarise(qty = qty)
ggplot( data = plot_114, aes(date, qty)) + geom_line() 



predicted_value_item_114 = c() 
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + -230.05774 + (-12.50837 * 28.91) 
predicted_value_item_114[1] = round(-292.58728 + common_factor)
predicted_value_item_114[2] = round(-95.28736 + common_factor)
predicted_value_item_114[3] = round(-168.89492 + common_factor)
predicted_value_item_114[4] = round(-120.84692 + common_factor)
predicted_value_item_114[5] = round(-125.36357 + common_factor)
predicted_value_item_114[6] = round(-61.27437+ common_factor)
predicted_value_item_114[7] = round(-103.75892 + common_factor)
predicted_value_item_114[8] = round(-34.95120 + common_factor)
predicted_value_item_114[9] = round(-23.96419 + common_factor)
predicted_value_item_114[10] = round(-140.63805+ common_factor)
predicted_value_item_114[11] = round(-104.56445 + common_factor)
predicted_value_item_114[12] = round(-69.96048 + common_factor)
predicted_value_item_114[13] = round(-92.45942  + common_factor)
predicted_value_item_114[14] = round(-211.74954+ common_factor)
predicted_value_item_114[15] = round(-234.57320   + common_factor)
predicted_value_item_114[16] = round(-41.51517+ common_factor)
predicted_value_item_114[17] = round(2.35805 + common_factor)
predicted_value_item_114[18] = round(89.30169 + common_factor)
predicted_value_item_114[19] = round(106.81296 + common_factor)
predicted_value_item_114[20] = round(67.32630  + common_factor)
predicted_value_item_114[21] = round(-94.24954  + common_factor)
predicted_value_item_114[22] = round(-181.65704 + common_factor)
predicted_value_item_114[23] = round(-79.30423 + common_factor)
predicted_value_item_114[24] = round(-70.43561 + common_factor)
predicted_value_item_114[25] = round(-3.19010 + common_factor)
predicted_value_item_114[26] = round(-25.90156 + common_factor)
predicted_value_item_114[27] = round(-33.74083+ common_factor)
predicted_value_item_114[28] = round(-145.97187  + common_factor)
predicted_value_item_114[29] = round(-149.26038 + common_factor)
predicted_value_item_114[30] = round(-113.32447 + common_factor)

predicted_value_item_114
table_item_114 <- data.frame(forecast_date, predicted_value_item_114)
table_item_114

#Item 225
#Extract data of item 225 

final_data_item_225 <- final_data %>% filter(item == 225)
summary(final_data_item_225)
#Plot quantity vs time graph for item 225
plot_225 <- final_data_item_225 %>% 
  group_by(date = final_data_item_225$date) %>%
  summarise(qty = qty)
ggplot( data = plot_225, aes(date, qty)) + geom_line() 


predicted_value_item_225 = c() 
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + -434.25588 + (-12.50837 * 24.74) 
predicted_value_item_225[1] = round(-292.58728 + common_factor)
predicted_value_item_225[2] = round(-95.28736 + common_factor)
predicted_value_item_225[3] = round(-168.89492 + common_factor)
predicted_value_item_225[4] = round(-120.84692 + common_factor)
predicted_value_item_225[5] = round(-125.36357 + common_factor)
predicted_value_item_225[6] = round(-61.27437+ common_factor)
predicted_value_item_225[7] = round(-103.75892 + common_factor)
predicted_value_item_225[8] = round(-34.95120 + common_factor)
predicted_value_item_225[9] = round(-23.96419 + common_factor)
predicted_value_item_225[10] = round(-140.63805+ common_factor)
predicted_value_item_225[11] = round(-104.56445 + common_factor)
predicted_value_item_225[12] = round(-69.96048 + common_factor)
predicted_value_item_225[13] = round(-92.45942  + common_factor)
predicted_value_item_225[14] = round(-211.74954+ common_factor)
predicted_value_item_225[15] = round(-234.57320   + common_factor)
predicted_value_item_225[16] = round(-41.51517+ common_factor)
predicted_value_item_225[17] = round(2.35805 + common_factor)
predicted_value_item_225[18] = round(89.30169 + common_factor)
predicted_value_item_225[19] = round(106.81296 + common_factor)
predicted_value_item_225[20] = round(67.32630  + common_factor)
predicted_value_item_225[21] = round(-94.24954  + common_factor)
predicted_value_item_225[22] = round(-181.65704 + common_factor)
predicted_value_item_225[23] = round(-79.30423 + common_factor)
predicted_value_item_225[24] = round(-70.43561 + common_factor)
predicted_value_item_225[25] = round(-3.19010 + common_factor)
predicted_value_item_225[26] = round(-25.90156 + common_factor)
predicted_value_item_225[27] = round(-33.74083+ common_factor)
predicted_value_item_225[28] = round(-145.97187  + common_factor)
predicted_value_item_225[29] = round(-149.26038 + common_factor)
predicted_value_item_225[30] = round(-113.32447 + common_factor)

predicted_value_item_225
table_item_225 <- data.frame(forecast_date, predicted_value_item_225)
table_item_225

#Item 234
#Extract item 234 in the chosen date
final_data_item_234 <- final_data %>% filter(item == 234)
summary(final_data_item_234)
#Plot quantity vs time graph for item 234
plot_234 <- final_data_item_234 %>% 
  group_by(date = final_data_item_234$date) %>%
  summarise(qty = qty)
ggplot( data = plot_234, aes(date, qty)) + geom_line() 



predicted_value_item_234 = c() 
                #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + (-228.85627) + (-12.50837 * 36.22) 
predicted_value_item_234[1] = round(-292.58728 + common_factor)
predicted_value_item_234[2] = round(-95.28736 + common_factor)
predicted_value_item_234[3] = round(-168.89492 + common_factor)
predicted_value_item_234[4] = round(-120.84692 + common_factor)
predicted_value_item_234[5] = round(-125.36357 + common_factor)
predicted_value_item_234[6] = round(-61.27437+ common_factor)
predicted_value_item_234[7] = round(-103.75892 + common_factor)
predicted_value_item_234[8] = round(-34.95120 + common_factor)
predicted_value_item_234[9] = round(-23.96419 + common_factor)
predicted_value_item_234[10] = round(-140.63805+ common_factor)
predicted_value_item_234[11] = round(-104.56445 + common_factor)
predicted_value_item_234[12] = round(-69.96048 + common_factor)
predicted_value_item_234[13] = round(-92.45942  + common_factor)
predicted_value_item_234[14] = round(-211.74954+ common_factor)
predicted_value_item_234[15] = round(-234.57320   + common_factor)
predicted_value_item_234[16] = round(-41.51517+ common_factor)
predicted_value_item_234[17] = round(2.35805 + common_factor)
predicted_value_item_234[18] = round(89.30169 + common_factor)
predicted_value_item_234[19] = round(106.81296 + common_factor)
predicted_value_item_234[20] = round(67.32630  + common_factor)
predicted_value_item_234[21] = round(-94.24954  + common_factor)
predicted_value_item_234[22] = round(-181.65704 + common_factor)
predicted_value_item_234[23] = round(-79.30423 + common_factor)
predicted_value_item_234[24] = round(-70.43561 + common_factor)
predicted_value_item_234[25] = round(-3.19010 + common_factor)
predicted_value_item_234[26] = round(-25.90156 + common_factor)
predicted_value_item_234[27] = round(-33.74083+ common_factor)
predicted_value_item_234[28] = round(-145.97187  + common_factor)
predicted_value_item_234[29] = round(-149.26038 + common_factor)
predicted_value_item_234[30] = round(-113.32447 + common_factor)

predicted_value_item_234
table_item_234 <- data.frame(forecast_date, predicted_value_item_234)
table_item_234

#Item 401
#Extract item 401 in the chosen date
final_data_item_401 <- final_data %>% filter(item == 401)
summary(final_data_item_401)
#Plot quantity vs time graph for item 401
plot_401 <- final_data_item_401 %>% 
  group_by(date = final_data_item_401$date) %>%
  summarise(qty = qty)
ggplot( data = plot_401, aes(date, qty)) + geom_line() 


predicted_value_item_401 = c() 
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + (-194.00291) + (-12.50837 * 12.88) 
predicted_value_item_401[1] = round(-292.58728 + common_factor)
predicted_value_item_401[2] = round(-95.28736 + common_factor)
predicted_value_item_401[3] = round(-168.89492 + common_factor)
predicted_value_item_401[4] = round(-120.84692 + common_factor)
predicted_value_item_401[5] = round(-125.36357 + common_factor)
predicted_value_item_401[6] = round(-61.27437+ common_factor)
predicted_value_item_401[7] = round(-103.75892 + common_factor)
predicted_value_item_401[8] = round(-34.95120 + common_factor)
predicted_value_item_401[9] = round(-23.96419 + common_factor)
predicted_value_item_401[10] = round(-140.63805+ common_factor)
predicted_value_item_401[11] = round(-104.56445 + common_factor)
predicted_value_item_401[12] = round(-69.96048 + common_factor)
predicted_value_item_401[13] = round(-92.45942  + common_factor)
predicted_value_item_401[14] = round(-211.74954+ common_factor)
predicted_value_item_401[15] = round(-234.57320   + common_factor)
predicted_value_item_401[16] = round(-41.51517+ common_factor)
predicted_value_item_401[17] = round(2.35805 + common_factor)
predicted_value_item_401[18] = round(89.30169 + common_factor)
predicted_value_item_401[19] = round(106.81296 + common_factor)
predicted_value_item_401[20] = round(67.32630  + common_factor)
predicted_value_item_401[21] = round(-94.24954  + common_factor)
predicted_value_item_401[22] = round(-181.65704 + common_factor)
predicted_value_item_401[23] = round(-79.30423 + common_factor)
predicted_value_item_401[24] = round(-70.43561 + common_factor)
predicted_value_item_401[25] = round(-3.19010 + common_factor)
predicted_value_item_401[26] = round(-25.90156 + common_factor)
predicted_value_item_401[27] = round(-33.74083+ common_factor)
predicted_value_item_401[28] = round(-145.97187  + common_factor)
predicted_value_item_401[29] = round(-149.26038 + common_factor)
predicted_value_item_401[30] = round(-113.32447 + common_factor)

predicted_value_item_401
table_item_401 <- data.frame(forecast_date, predicted_value_item_401)
table_item_401

#Item 434
#Extract data of item 434
final_data_item_434 <- final_data %>% filter(item == 434)
summary(final_data_item_434)
#Plot quantity vs time graph for item 434
plot_434 <- final_data_item_434 %>% 
  group_by(date = final_data_item_434$date) %>%
  summarise(qty = qty)
ggplot( data = plot_434, aes(date, qty)) + geom_line() 



predicted_value_item_434 = c() 
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + (-447.88708) + (-12.50837 * 24.36) 
predicted_value_item_434[1] = round(-292.58728 + common_factor)
predicted_value_item_434[2] = round(-95.28736 + common_factor)
predicted_value_item_434[3] = round(-168.89492 + common_factor)
predicted_value_item_434[4] = round(-120.84692 + common_factor)
predicted_value_item_434[5] = round(-125.36357 + common_factor)
predicted_value_item_434[6] = round(-61.27437+ common_factor)
predicted_value_item_434[7] = round(-103.75892 + common_factor)
predicted_value_item_434[8] = round(-34.95120 + common_factor)
predicted_value_item_434[9] = round(-23.96419 + common_factor)
predicted_value_item_434[10] = round(-140.63805+ common_factor)
predicted_value_item_434[11] = round(-104.56445 + common_factor)
predicted_value_item_434[12] = round(-69.96048 + common_factor)
predicted_value_item_434[13] = round(-92.45942  + common_factor)
predicted_value_item_434[14] = round(-211.74954+ common_factor)
predicted_value_item_434[15] = round(-234.57320   + common_factor)
predicted_value_item_434[16] = round(-41.51517+ common_factor)
predicted_value_item_434[17] = round(2.35805 + common_factor)
predicted_value_item_434[18] = round(89.30169 + common_factor)
predicted_value_item_434[19] = round(106.81296 + common_factor)
predicted_value_item_434[20] = round(67.32630  + common_factor)
predicted_value_item_434[21] = round(-94.24954  + common_factor)
predicted_value_item_434[22] = round(-181.65704 + common_factor)
predicted_value_item_434[23] = round(-79.30423 + common_factor)
predicted_value_item_434[24] = round(-70.43561 + common_factor)
predicted_value_item_434[25] = round(-3.19010 + common_factor)
predicted_value_item_434[26] = round(-25.90156 + common_factor)
predicted_value_item_434[27] = round(-33.74083+ common_factor)
predicted_value_item_434[28] = round(-145.97187  + common_factor)
predicted_value_item_434[29] = round(-149.26038 + common_factor)
predicted_value_item_434[30] = round(-113.32447 + common_factor)

predicted_value_item_434
table_item_434 <- data.frame(forecast_date, predicted_value_item_434)
table_item_434

#Item 469
#Extract data of item 469
final_data_item_469 <- final_data %>% filter(item == 469)
summary(final_data_item_469)
#Plot quantity vs time graph for item 469
plot_469 <- final_data_item_469 %>% 
  group_by(date = final_data_item_469$date) %>%
  summarise(qty = qty)
ggplot( data = plot_469, aes(date, qty)) + geom_line() 



predicted_value_item_469 = c() 
                  #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + (-439.73178) + (-12.50837 * 14.4) 
predicted_value_item_469[1] = round(-292.58728 + common_factor)
predicted_value_item_469[2] = round(-95.28736 + common_factor)
predicted_value_item_469[3] = round(-168.89492 + common_factor)
predicted_value_item_469[4] = round(-120.84692 + common_factor)
predicted_value_item_469[5] = round(-125.36357 + common_factor)
predicted_value_item_469[6] = round(-61.27437+ common_factor)
predicted_value_item_469[7] = round(-103.75892 + common_factor)
predicted_value_item_469[8] = round(-34.95120 + common_factor)
predicted_value_item_469[9] = round(-23.96419 + common_factor)
predicted_value_item_469[10] = round(-140.63805+ common_factor)
predicted_value_item_469[11] = round(-104.56445 + common_factor)
predicted_value_item_469[12] = round(-69.96048 + common_factor)
predicted_value_item_469[13] = round(-92.45942  + common_factor)
predicted_value_item_469[14] = round(-211.74954+ common_factor)
predicted_value_item_469[15] = round(-234.57320   + common_factor)
predicted_value_item_469[16] = round(-41.51517+ common_factor)
predicted_value_item_469[17] = round(2.35805 + common_factor)
predicted_value_item_469[18] = round(89.30169 + common_factor)
predicted_value_item_469[19] = round(106.81296 + common_factor)
predicted_value_item_469[20] = round(67.32630  + common_factor)
predicted_value_item_469[21] = round(-94.24954  + common_factor)
predicted_value_item_469[22] = round(-181.65704 + common_factor)
predicted_value_item_469[23] = round(-79.30423 + common_factor)
predicted_value_item_469[24] = round(-70.43561 + common_factor)
predicted_value_item_469[25] = round(-3.19010 + common_factor)
predicted_value_item_469[26] = round(-25.90156 + common_factor)
predicted_value_item_469[27] = round(-33.74083+ common_factor)
predicted_value_item_469[28] = round(-145.97187  + common_factor)
predicted_value_item_469[29] = round(-149.26038 + common_factor)
predicted_value_item_469[30] = round(-113.32447 + common_factor)

predicted_value_item_469
table_item_469 <- data.frame(forecast_date, predicted_value_item_469)
table_item_469

#Item 502
#Extract data of item 502
final_data_item_502 <- final_data %>% filter(item == 502)
summary(final_data_item_502)
#Plot quantity vs time graph for item 502
plot_502 <- final_data_item_502 %>% 
  group_by(date = final_data_item_502$date) %>%
  summarise(qty = qty)
ggplot( data = plot_502, aes(date, qty)) + geom_line() 



predicted_value_item_502 = c() 
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + (150.37558) + (-12.50837 * 26.8) 
predicted_value_item_502[1] = round(-292.58728 + common_factor)
predicted_value_item_502[2] = round(-95.28736 + common_factor)
predicted_value_item_502[3] = round(-168.89492 + common_factor)
predicted_value_item_502[4] = round(-120.84692 + common_factor)
predicted_value_item_502[5] = round(-125.36357 + common_factor)
predicted_value_item_502[6] = round(-61.27437+ common_factor)
predicted_value_item_502[7] = round(-103.75892 + common_factor)
predicted_value_item_502[8] = round(-34.95120 + common_factor)
predicted_value_item_502[9] = round(-23.96419 + common_factor)
predicted_value_item_502[10] = round(-140.63805+ common_factor)
predicted_value_item_502[11] = round(-104.56445 + common_factor)
predicted_value_item_502[12] = round(-69.96048 + common_factor)
predicted_value_item_502[13] = round(-92.45942  + common_factor)
predicted_value_item_502[14] = round(-211.74954+ common_factor)
predicted_value_item_502[15] = round(-234.57320   + common_factor)
predicted_value_item_502[16] = round(-41.51517+ common_factor)
predicted_value_item_502[17] = round(2.35805 + common_factor)
predicted_value_item_502[18] = round(89.30169 + common_factor)
predicted_value_item_502[19] = round(106.81296 + common_factor)
predicted_value_item_502[20] = round(67.32630  + common_factor)
predicted_value_item_502[21] = round(-94.24954  + common_factor)
predicted_value_item_502[22] = round(-181.65704 + common_factor)
predicted_value_item_502[23] = round(-79.30423 + common_factor)
predicted_value_item_502[24] = round(-70.43561 + common_factor)
predicted_value_item_502[25] = round(-3.19010 + common_factor)
predicted_value_item_502[26] = round(-25.90156 + common_factor)
predicted_value_item_502[27] = round(-33.74083+ common_factor)
predicted_value_item_502[28] = round(-145.97187  + common_factor)
predicted_value_item_502[29] = round(-149.26038 + common_factor)
predicted_value_item_502[30] = round(-113.32447 + common_factor)


predicted_value_item_502
table_item_502 <- data.frame(forecast_date, predicted_value_item_502)
table_item_502

#Item 589
#Extract data of item 589
final_data_item_589 <- final_data %>% filter(item == 589)
summary(final_data_item_589)
#Plot quantity vs time graph for item 589
plot_589 <- final_data_item_589 %>% 
  group_by(date = final_data_item_589$date) %>%
  summarise(qty = qty)
ggplot( data = plot_589, aes(date, qty)) + geom_line() 



predicted_value_item_589 = c() 
                 #intercept  #itemFactor #priceFactor #average_price
common_factor <- 1370.80222 + (450.30608) + (-12.50837 * 21) 
predicted_value_item_589[1] = round(-292.58728 + common_factor)
predicted_value_item_589[2] = round(-95.28736 + common_factor)
predicted_value_item_589[3] = round(-168.89492 + common_factor)
predicted_value_item_589[4] = round(-120.84692 + common_factor)
predicted_value_item_589[5] = round(-125.36357 + common_factor)
predicted_value_item_589[6] = round(-61.27437+ common_factor)
predicted_value_item_589[7] = round(-103.75892 + common_factor)
predicted_value_item_589[8] = round(-34.95120 + common_factor)
predicted_value_item_589[9] = round(-23.96419 + common_factor)
predicted_value_item_589[10] = round(-140.63805+ common_factor)
predicted_value_item_589[11] = round(-104.56445 + common_factor)
predicted_value_item_589[12] = round(-69.96048 + common_factor)
predicted_value_item_589[13] = round(-92.45942  + common_factor)
predicted_value_item_589[14] = round(-211.74954+ common_factor)
predicted_value_item_589[15] = round(-234.57320   + common_factor)
predicted_value_item_589[16] = round(-41.51517+ common_factor)
predicted_value_item_589[17] = round(2.35805 + common_factor)
predicted_value_item_589[18] = round(89.30169 + common_factor)
predicted_value_item_589[19] = round(106.81296 + common_factor)
predicted_value_item_589[20] = round(67.32630  + common_factor)
predicted_value_item_589[21] = round(-94.24954  + common_factor)
predicted_value_item_589[22] = round(-181.65704 + common_factor)
predicted_value_item_589[23] = round(-79.30423 + common_factor)
predicted_value_item_589[24] = round(-70.43561 + common_factor)
predicted_value_item_589[25] = round(-3.19010 + common_factor)
predicted_value_item_589[26] = round(-25.90156 + common_factor)
predicted_value_item_589[27] = round(-33.74083+ common_factor)
predicted_value_item_589[28] = round(-145.97187  + common_factor)
predicted_value_item_589[29] = round(-149.26038 + common_factor)
predicted_value_item_589[30] = round(-113.32447 + common_factor)

predicted_value_item_589
table_item_589 <- data.frame(forecast_date, predicted_value_item_589)
table_item_589


#Concerning the error,  the adjusted R-squared = 0.8074 which is acceptable in my opinion.
#Adjusted R-squared is the metric we look into when evaluating the performance of the linear regression
#model. 
#As for assessing my own work, we can build the model for February 2019(training) and test our data
#for the month of March(2019/03) and apply the MSE to evaluate our results.
