library(readr) 
library(lubridate)

#Data Prep
#Popup Sales
urlfile2<-'https://raw.githubusercontent.com/riddhi0101/ARS_DataAnalytics/main/Dashboard_rshiny/popup_salesF21.csv'
popupsales<-read_csv(url(urlfile2))
popupsales$price = as.numeric(gsub("\\$", "", popupsales$`Total Price`))

#First Page: 

#Second Page
#Adding day of the week column to data set
urlfile<-'https://raw.githubusercontent.com/riddhi0101/ARS_DataAnalytics/main/Data/clean_entire.csv'
clean_entire <-read_csv(url(urlfile))
date_fact <- as.factor(clean_entire$Date)
new_format<-strptime(date_fact,format="%m/%d/%y")
new_date<-as.Date(new_format,format="%Y-%m-%d")

clean_entire$New_Date <- new_date
clean_entire$Day <- wday(clean_entire$New_Date,label=TRUE)
clean_entire$Price <- as.numeric(round(parse_number(clean_entire$`Total Price`),0))

#Third Page 



