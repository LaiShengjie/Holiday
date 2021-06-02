Data sources:
1. School holidays in 2019 summer (June - September): https://school-holidays.net/
2. School terms: https://en.wikipedia.org/wiki/Academic_term
3. https://www.edarabia.com/school-holidays-austria/
4. https://publicholidays.asia/brunei/school-holidays/2019-dates/
5. https://en.wikipedia.org/wiki/Summer_vacation
6. Cook Islands http://www.education.gov.ck/?page_id=586
7. Costa Rica https://publicholidays.co.cr/school-holidays/2019-dates/
8. Mauritius https://publicholidays.mu/school-holidays/2019-dates/
9. Mozambique https://publicholidays.africa/mozambique/school-holidays/2019-dates/
10. EU https://www.schoolholidayseurope.eu/austria/
11. https://en.wikipedia.org/wiki/School_holiday
12. https://holidaycalendar.com/en/calendar/2018/Chile/Regi%C3%B3n+Metropolitana
13. www.Google.com...

Steps to collate data: 
1. For different beginning and end dates of holidays across regions within a country in 2019 (https://school-holidays.net/), median dates were used.
2. As the short holidays or mid-term breaks are commonly overlapped with the public/bank/religous holidays (e.g. Easter/Thanksgiving), here we focus on the longer holidays, e.g. summer/winter holidays, breaks between academic years, with >16 days (2 weeks + weekend) for most countries.
3. Due to the data availability, data in 2019 were first collated and checked manually. Then the dates in 2010-2018 were generated according to the begining and end dates in 2019. If the begining dates in 2010-2018 were Thursday or Friday, they were adjusted to the nearest Saturday, and if the end dates in 2010-2018 were Monday or Tuesday, they were adjusted to the nearest Sunday.
4. All data in China were manually checked and used the school holidays in Beijing. 

Data variables:
'data.frame':	346 obs. of  23 variables:
 $ Country   : chr  "Aruba" "Aruba" "Afghanistan" "Angola" ...
 $ ISO3      : chr  "ABW" "ABW" "AFG" "AGO" ...
 $ Name_short: chr, the names of holidays, e.g.  "autumn holiday" "summer holiday" "summer holiday" "break between school years/terms" ...
 $ Date_b19  : Beginning date in 2019
 $ Date_e19  : End date in 2019 or 2020 (for breaks across years, e.g. winter holidays)
 $ Date_b18  : Beginning date in 2018
 $ Date_e18  : End date in 2018 or 2019 (for breaks across years, e.g. winter holidays)
 $ Date_b17  : Beginning date in 2017
 $ Date_e17  : End date in 2017 or 2018 (for breaks across years, e.g. winter holidays)
 $ Date_b16  : Beginning date in 2016
 $ Date_e16  : End date in 2016 or 2017 (for breaks across years, e.g. winter holidays)
 $ Date_b15  : Beginning date in 2015
 $ Date_e15  : End date in 2015 or 2016 (for breaks across years, e.g. winter holidays)
 $ Date_b14  : Beginning date in 2014
 $ Date_e14  : End date in 2014 or 2015 (for breaks across years, e.g. winter holidays)
 $ Date_b13  : Beginning date in 2013
 $ Date_e13  : End date in 2013 or 2014 (for breaks across years, e.g. winter holidays)
 $ Date_b12  : Beginning date in 2012
 $ Date_e12  : End date in 2012 or 2013 (for breaks across years, e.g. winter holidays)
 $ Date_b11  : Beginning date in 2011
 $ Date_e11  : End date in 2011 or 2012 (for breaks across years, e.g. winter holidays)
 $ Date_b10  : Beginning date in 2010
 $ Date_e10  : End date in 2010 or 2011 (for breaks across years, e.g. winter holidays)