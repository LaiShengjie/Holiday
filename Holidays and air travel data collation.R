rm(list=ls())

library(rvest)
library(purrr)
library(dplyr)
library(lubridate) # working with dates and times
library(stringr) # working with strings
library(data.table) # fread - fastly reading data
library(readxl) # reading excel
library(xml2)
library(reshape) # melt and cast
library(countrycode) # country ISO3 alpha2 and 3 code

## Working directory
wd <- "D:/OneDrive - University of Southampton/Seasonality Project/Scientific_Data_Holidays"

### Public holiday data ------------------------------------------------------------------
setwd(paste(wd, "/Holidays", sep=''))

# Read public holiday data collated manually
holiday <- read.csv('Public holiday/Public holidays_2010_2019_final.csv', stringsAsFactors = F)
holiday$Date <- as.Date(holiday$Date, format = '%d/%m/%Y') 

# Nations/territories in data
adm <- unique(holiday[,c('ADM_name', 'ISO3')]) # 232

# Exclude observance and local holidays
unique(holiday$Type)
holiday <- holiday[holiday$Type != 'Local observance' & 
                     holiday$Type != 'Local holiday' & 
                     holiday$Type != 'Observance',]

# Public holidays and special working days (replacement on weekend)
workday <- holiday[holiday$Type == 'Working day (replacement)',]
workday <- unique(workday[, c('ISO3', 'Date')])
workday$workday <- 1

holiday <- holiday[holiday$Type != 'Working day (replacement)',]
holiday <- unique(holiday[, c('ISO3', 'Date')])
holiday$holiday <- 1

# Time series from 2009 through 2019; Include Year 2009 for school holidays across 2009-2010.
Date <- seq(as.Date('01/01/2009', format = '%d/%m/%Y'), 
            as.Date('31/12/2019', format = '%d/%m/%Y'), by = 'day')
ts <- as.data.frame(Date)
ts$Year <- year(ts$Date)
ts$Month <- month(ts$Date)
ts$Week <- as.integer(format(ts$Date, '%W')) # ISO week numbers
ts$Day <- day(ts$Date)
ts$i <- 1; adm$i <- 1
ts <- merge(adm, ts, by = 'i', all =T)
ts$i <- NULL
ts$weekend <- ifelse(weekdays(ts$Date) == 'Saturday' | weekdays(ts$Date) == 'Sunday', 1, 0 )

# Merge public holidays
ts <- merge(ts, holiday, by = c('ISO3', 'Date'), all.x = T)
ts$holiday[is.na(ts$holiday) == T] <- 0

# Add the New Year's Day as a public holiday for each year and nation/territory
ts$holiday[ts$Month == 1 & ts$Day == 1] <- 1

# Identify special working days at weekend (replacement for special holidays)
ts <- merge(ts, workday, by = c('ISO3', 'Date'), all.x = T)
ts$workday[is.na(ts$workday) == T] <- 0
ts$workday[ts$weekend == 0 & ts$holiday == 0] <- 1


#### School holiday data ------------------------------------------------------------
# Read school holiday data in 2019 and across 2018-2019 and 2019-2020, collated manually
school <- read.csv('School holiday/School holidays_begin to end dates_collated_2019.csv', stringsAsFactors = F)
school$Date_b19 <- as.Date(school$Date_b19, format = '%d/%m/%Y')
school$Date_e19 <- as.Date(school$Date_e19, format = '%d/%m/%Y')
school$Date_b18 <- as.Date(school$Date_b18, format = '%d/%m/%Y')
school$Date_e18 <- as.Date(school$Date_e18, format = '%d/%m/%Y')

### Generate school holiday data from 2009 through 2018
# Adjust the holiday end dates from Monday to Sunday and the beginning dates from Friday to Saturday 
school$week_b <- weekdays(school$Date_b19)
school$week_e <- weekdays(school$Date_e19)
school$Date_b19[is.na(school$week_b) == F & school$week_b == 'Friday'] <- 
  school$Date_b19[is.na(school$week_b) == F & school$week_b == 'Friday'] + 1
school$Date_e19[is.na(school$week_e) == F & school$week_e == 'Monday'] <- 
  school$Date_e19[is.na(school$week_e) == F & school$week_e == 'Monday'] - 1
school$week_b <- school$week_e <- NULL

# 2018
school$Date_b <- school$Date_b19
year(school$Date_b) <- year(school$Date_b) -1
school$Date_b18[is.na(school$Date_b18) == T] <- school$Date_b[is.na(school$Date_b18) == T]

school$Date_e <- school$Date_e19
year(school$Date_e) <- year(school$Date_e) -1
school$Date_e18[is.na(school$Date_e18) == T] <- school$Date_e[is.na(school$Date_e18) == T]
school$Date_b <- school$Date_e <- NULL

Date_b = 'Date_b18'
Date_e = 'Date_e18'

school$week_b <- weekdays(school[,c(Date_b)])
school$week_e <- weekdays(school[,c(Date_e)])
school[is.na(school$week_b) == F & school$week_b == 'Friday', c(Date_b)] <- 
  school[is.na(school$week_b) == F & school$week_b == 'Friday', c(Date_b)] + 1
school[is.na(school$week_b) == F & school$week_b == 'Thursday', c(Date_b)] <- 
  school[is.na(school$week_b) == F & school$week_b == 'Thursday', c(Date_b)] + 2

school[is.na(school$week_e) == F & school$week_e == 'Monday', c(Date_e)] <- 
  school[is.na(school$week_e) == F & school$week_e == 'Monday', c(Date_e)] - 1
school[is.na(school$week_e) == F & school$week_e == 'Tuesday', c(Date_e)] <- 
  school[is.na(school$week_e) == F & school$week_e == 'Tuesday', c(Date_e)] - 2

school$week_b <- school$week_e <- NULL

# Function for adjusting the dates nearing weekends in 2009-2017
newdate <- function(school, Date_b, Date_e){
  school$week_b <- weekdays(school[,c(Date_b)])
  school$week_e <- weekdays(school[,c(Date_e)])
  school[is.na(school$week_b) == F & school$week_b == 'Friday', c(Date_b)] <- 
    school[is.na(school$week_b) == F & school$week_b == 'Friday', c(Date_b)] + 1
  school[is.na(school$week_b) == F & school$week_b == 'Thursday', c(Date_b)] <- 
    school[is.na(school$week_b) == F & school$week_b == 'Thursday', c(Date_b)] + 2
  
  school[is.na(school$week_e) == F & school$week_e == 'Monday', c(Date_e)] <- 
    school[is.na(school$week_e) == F & school$week_e == 'Monday', c(Date_e)] - 1
  school[is.na(school$week_e) == F & school$week_e == 'Tuesday', c(Date_e)] <- 
    school[is.na(school$week_e) == F & school$week_e == 'Tuesday', c(Date_e)] - 2
  
  school$week_b <- school$week_e <- NULL
  return(school)
}

# 2017
school$Date_b17 <- school$Date_b19
year(school$Date_b17) <- year(school$Date_b17) - 2
school$Date_e17 <- school$Date_e19
year(school$Date_e17) <- year(school$Date_e17) - 2
school <- newdate(school, Date_b = 'Date_b17', Date_e = 'Date_e17')

# 2016
school$Date_b16 <- school$Date_b19
year(school$Date_b16) <- year(school$Date_b16) - 3
school$Date_e16 <- school$Date_e19
year(school$Date_e16) <- year(school$Date_e16) - 3
school <- newdate(school, Date_b = 'Date_b16', Date_e = 'Date_e16')

# 2015
school$Date_b15 <- school$Date_b19
year(school$Date_b15) <- year(school$Date_b15) - 4
school$Date_e15 <- school$Date_e19
year(school$Date_e15) <- year(school$Date_e15) - 4
school <- newdate(school, Date_b = 'Date_b15', Date_e = 'Date_e15')

# 2014
school$Date_b14 <- school$Date_b19
year(school$Date_b14) <- year(school$Date_b14) - 5
school$Date_e14 <- school$Date_e19
year(school$Date_e14) <- year(school$Date_e14) - 5
school <- newdate(school, Date_b = 'Date_b14', Date_e = 'Date_e14')

# 2013
school$Date_b13 <- school$Date_b19
year(school$Date_b13) <- year(school$Date_b13) - 6
school$Date_e13 <- school$Date_e19
year(school$Date_e13) <- year(school$Date_e13) - 6
school <- newdate(school, Date_b = 'Date_b13', Date_e = 'Date_e13')

# 2012
school$Date_b12 <- school$Date_b19
year(school$Date_b12) <- year(school$Date_b12) - 7
school$Date_e12 <- school$Date_e19
year(school$Date_e12) <- year(school$Date_e12) - 7
school <- newdate(school, Date_b = 'Date_b12', Date_e = 'Date_e12')

# 2011
school$Date_b11 <- school$Date_b19
year(school$Date_b11) <- year(school$Date_b11) - 8
school$Date_e11 <- school$Date_e19
year(school$Date_e11) <- year(school$Date_e11) - 8
school <- newdate(school, Date_b = 'Date_b11', Date_e = 'Date_e11')

# 2010
school$Date_b10 <- school$Date_b19
year(school$Date_b10) <- year(school$Date_b10) - 9
school$Date_e10 <- school$Date_e19
year(school$Date_e10) <- year(school$Date_e10) - 9
school <- newdate(school, Date_b = 'Date_b10', Date_e = 'Date_e10')

# 2009
school$Date_b09 <- school$Date_b19
year(school$Date_b09) <- year(school$Date_b09) - 10
school$Date_e09 <- school$Date_e19
year(school$Date_e09) <- year(school$Date_e09) - 10
school <- newdate(school, Date_b = 'Date_b09', Date_e = 'Date_e09')

## Adjusting winter breaks in China by using data in Beijing
school$Date_b17[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2017-1-21', format = '%Y-%m-%d')
school$Date_e17[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2017-2-19', format = '%Y-%m-%d')
school$Date_b16[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2016-1-23', format = '%Y-%m-%d')
school$Date_e16[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2016-2-22', format = '%Y-%m-%d')

school$Date_b15[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2015-1-31', format = '%Y-%m-%d')
school$Date_e15[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2015-2-28', format = '%Y-%m-%d')
school$Date_b14[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2014-1-19', format = '%Y-%m-%d')
school$Date_e14[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2014-2-16', format = '%Y-%m-%d')

school$Date_b13[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2013-1-26', format = '%Y-%m-%d')
school$Date_e13[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2013-2-24', format = '%Y-%m-%d')
school$Date_b12[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2012-1-14', format = '%Y-%m-%d')
school$Date_e12[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2012-2-12', format = '%Y-%m-%d')

school$Date_b11[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2011-1-22', format = '%Y-%m-%d')
school$Date_e11[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2011-2-20', format = '%Y-%m-%d')
school$Date_b10[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2010-1-23', format = '%Y-%m-%d')
school$Date_e10[school$ISO3 == 'CHN' & school$Name_short == 'winter holiday'] <- as.Date('2010-2-22', format = '%Y-%m-%d')

# Save data
write.csv(school, 'School holiday/School holidays_2009_2019_final.csv', row.names = F, fileEncoding = 'UTF-8')


### Merge public and school holidays ---------------------------------------------
## Generate school holiday time series
ts$school <- 0

for(i in unique(school$ISO3)){
  for(n in unique(school$Name_short)){
    df <- school[school$ISO3 == i & school$Name_short == n, ]
    for (r in nrow(df)){
      dt <- df[r,]
      for(y in c('09',10:19)){
        begin <- paste('Date_b', y, sep = '')
        end <- paste('Date_e', y, sep = '')
        ts$school[ts$ISO3 == i & ts$Date >= dt[,begin] & ts$Date <= dt[,end]] <- 1
      }
    }
  }
}

# Public and school holidays
ts$hl_sch <- ts$holiday + ts$school
ts$hl_sch <- ifelse(ts$hl_sch >0, 1, 0)

# All breaks: weekend, public holidays, and school holidays; remove special working days on weekend
ts$all_break <- 1 + ts$hl_sch + ts$weekend - ts$workday
ts$all_break <- ifelse(ts$all_break >0, 1, 0)

# Only keep 2010-2019 data
ts <- ts[ts$Date >= as.Date('01/01/2010', format = '%d/%m/%Y'), ]

# Aggregate by month and week
ts.month <- aggregate(cbind(holiday, hl_sch) ~ ISO3 + ADM_name + Year + Month, data = ts, sum)
ts.week <- aggregate(cbind(holiday, hl_sch) ~ ISO3 + ADM_name + Year + Week, data = ts, sum)

# save data
write.csv(ts, 'Day_Public and school holidays_2010_2019.csv', row.names = F)
write.csv(ts.week, 'Week_Public and school holidays_2010_2019.csv', row.names = F)
write.csv(ts.month, 'Month_Public and school holidays_2010_2019.csv', row.names = F)


### Air travel data collation ----------------------------------
setwd(paste(wd, "/Air travel data", sep=''))

### ~~ Official data ####
# Australia
AUS <- read.csv("AUS/AUS Air Travel in thousand.csv", header=T, stringsAsFactors=F)
# Canada
CAN <- read.csv("CAN/CAN Air Travel in thousand.csv", header=T, stringsAsFactors=F)
# China
CHN <- read.csv("CHN/CHN Air Travel in thousand.csv", header=T, stringsAsFactors=F)
# Europe
EU <- read.csv("EU/EU Air Travel in thousand.csv", header=T, stringsAsFactors=F)
EU <- cast(EU, ISO3 + Country + Year + Month ~ TRA_COV, sum)
EU <- EU[, c('ISO3',  "Country", "Year", "Month", "Total" , "Domestic", "International")]
# Nigeria
NGA <- read.csv("Nigeria/Nigeria Air Travel in thousand.csv", header=T, stringsAsFactors=F)
# THA
THA <- read.csv("THA/THA Air Travel in thousand.csv", header=T, stringsAsFactors=F)
# USA
USA <- read.csv("USA/USA Air Travel in thousand.csv", header=T, stringsAsFactors=F)

## Merge official data
air.official <- rbind(AUS, CAN, CHN, EU, NGA, THA, USA)
air.official$Country <- NULL

### ~~ Other data sources ####
# from https://www.anna.aero/databases/
# ISO 3 alpha code
Country <- codelist[, c('country.name.en', 'iso3c')]
colnames(Country) <- c('Country', 'ISO3')

### Europe 
var.name <- c('Country', 'Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
              'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
n=2010
sheet.n <- paste(n, ' pax', sep = '')
eu1 <- read_excel("Other/European-airports-traffic.xlsx", sheet= sheet.n)
eu1$Year <- n
eu1 <- eu1[str_detect(eu1$Airport, 'airports') == F,]
eu1 <- eu1[, var.name]

for(n in 2011:2017){
  sheet.n <- paste(n, ' pax', sep = '')
  eu2 <- read_excel("Other/European-airports-traffic.xlsx", sheet= sheet.n)
  eu2$Year <- n
  eu2 <- eu2[str_detect(eu2$Airport, 'airports') == F,]
  eu2 <- eu2[, var.name]
  eu1 <- rbind(eu1, eu2)
}

eu1 <- eu1[is.na(eu1$Country) == F,]
mon <- as.character(1:12)
colnames(eu1) <- c('Country', 'Year', mon)
eu1 <- as.data.frame(eu1)
eu1 <- melt(data = eu1, id.vars = c( "Country","Year" ), 
            measure.vars =  c( "1","2","3","4","5","6","7",
                               "8","9","10","11","12"))
colnames(eu1) <- c('Country', 'Year', 'Month', 'Total_OS')
eu1$Month <- as.numeric(eu1$Month)
eu1 <- aggregate(Total_OS ~ Country + Year + Month, data = eu1, sum)
# ISO 3 alpha code
eu2 <- merge(eu1, Country, by = 'Country', all.x = T)

eu2$ISO3[eu2$Country == 'Czech Republic'] <- 'CZE'
eu2$ISO3[eu2$Country == 'Kosovo']  <- 'RKS'
unique(eu2$Country[is.na(eu2$ISO3) == T])
eu2 <- eu2[order(eu2$Country, eu2$Year, eu2$Month),]

### America 
n=2010
sheet.n <- paste(n, ' pax', sep = '')
am1 <- read_excel("Other/American-airports-traffic.xlsx", sheet= sheet.n)
am1$Year <- n
am1 <- am1[, var.name]

for(n in 2011:2017){
  sheet.n <- paste(n, ' pax', sep = '')
  am2 <- read_excel("Other/American-airports-traffic.xlsx", sheet= sheet.n)
  am2$Year <- n
  am2 <- am2[, var.name]
  am1 <- rbind(am1, am2)
}

am1 <- am1[is.na(am1$Country) == F,]
am1$Apr <- as.numeric(am1$Apr)
mon <- as.character(1:12)
colnames(am1) <- c('Country', 'Year', mon)
am1 <- as.data.frame(am1)
am1 <- melt(data = am1, id.vars = c( "Country","Year" ), 
            measure.vars =  c( "1","2","3","4","5","6","7",
                               "8","9","10","11","12"))
colnames(am1) <- c('Country', 'Year', 'Month', 'Total_OS')
am1$Month <- as.numeric(am1$Month)
am1 <- aggregate(Total_OS ~ Country + Year + Month, data = am1, sum)
# ISO 3 alpha code
am2 <- merge(am1, Country, by = 'Country', all.x = T)
am2$ISO3[am2$Country == 'USA'] <- 'USA'
unique(am2$Country[is.na(am2$ISO3) == T])
am2 <- am2[order(am2$Country, am2$Year, am2$Month),]

### Other continents 
n=2014
sheet.n <- paste(n, ' pax', sep = '')
asia1 <- read_excel("Other/Other-airports-traffic.xlsx", sheet= sheet.n)
asia1$Year <- n
asia1 <- asia1[, var.name]

for(n in 2015:2017){
  # n = 2015
  sheet.n <- paste(n, ' pax', sep = '')
  asia2 <- read_excel("Other/Other-airports-traffic.xlsx", sheet= sheet.n)
  asia2$Year <- n
  asia2 <- asia2[, var.name]
  asia1 <- rbind(asia1, asia2)
}

asia1 <- asia1[is.na(asia1$Country) == F & asia1$Country != 'Morocco',]
asia1$Jun <- as.numeric(asia1$Jun)
asia1$Jul <- as.numeric(asia1$Jul)
asia1$Aug <- as.numeric(asia1$Aug)
asia1$Sep <- as.numeric(asia1$Sep)

mon <- as.character(1:12)
colnames(asia1) <- c('Country', 'Year', mon)
asia1 <- as.data.frame(asia1)
asia1 <- melt(data = asia1, id.vars = c( "Country","Year" ), 
              measure.vars =  c( "1","2","3","4","5","6","7",
                                 "8","9","10","11","12"))
colnames(asia1) <- c('Country', 'Year', 'Month', 'Total_OS')
asia1$Month <- as.numeric(asia1$Month)
asia1 <- aggregate(Total_OS ~ Country + Year + Month, data = asia1, sum)
# ISO 3 alpha code
asia2 <- merge(asia1, Country, by = 'Country', all.x = T)
asia2$ISO3[asia2$Country == 'Hong Kong'] <- 'HKG'
asia2$ISO3[asia2$Country == 'Macau'] <- 'MAC'
asia2$ISO3[asia2$Country == 'UAE'] <- 'ARE'
unique(asia2$Country[is.na(asia2$ISO3) == T])
asia2 <- asia2[is.na(asia2$ISO3) == F,]
asia2 <- asia2[order(asia2$Country, asia2$Year, asia2$Month),]

### ~~ Merge data ----
other <- rbind(eu2, am2, asia2)
other$Total_OS <- other$Total_OS/1000 # Total number of air passengers from other data sources
other$Country <- NULL
world.air <- merge(air.official, other, by = c('ISO3', 'Year', 'Month'), all = T)
world.air$Total[world.air$Total == 0] <- NA
world.air$Domestic[world.air$Domestic == 0] <- NA
world.air$International[world.air$International == 0] <- NA
world.air$Total_OS[world.air$Total_OS == 0] <- NA
world.air$ISO3[world.air$ISO3 == 'RKS'] <- 'KOS' # Kosovo
world.air <- world.air[order(world.air$ISO3, world.air$Year, world.air$Month),]
write.csv(world.air, 'All Air Travel data in thousand.csv', fileEncoding = 'UTF-8', row.names = F)