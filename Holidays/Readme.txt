The time series of holidays aggregated by country and by day, week, or month, from 2010 to 2019:
1. Day_Public and school holidays by country_2010_2019.csv
2. Week_Public and school holidays by country_2010_2019.csv
3. Month_Public and school holidays by country_2010_2019.csv

The original data of public holidays and school holidays can be found in the fold 'Orignal holiday data', with README documents detailing the data sources and collation.

I. The variables in the file: Day_Public and school holidays by country_2010_2019.csv
$ ISO3: The three-letter code for each country
$ Date: DD/MM/YYYY
$ Country: The name of the country
$ weekend: 1-YES, 0-NO; Saturday + Sunday
$ holiday: 1-YES,0-NO; Public holiday, aggregated from 'Public holidays by country_2010_2019_final.csv'. The local holidays and special working days in weekends were excluded.
$ school: 1-YES,0-NO; School holiday, aggregated from 'School holidays by country_2010_2019_final.csv'.
$ Year: 
$ Month: 
$ Week: 
$ Day: 
$ hl_sch: 1-YES, 0-NO; Public holiday + school holiday
$ all_break: 1-YES, 0-NO; Public holiday + school holiday + weekend 

II. The variables in the file: Week_Public and school holidays by country_2010_2019.csv
$ ISO3: The three-letter code for each country
$ Country: The name of the country
$ Year: 
$ Week: 
$ hl_sch: the number of days of public and school holidays in a week, aggregated from the 'hl_sch' variable in the 'Day_Public and school holidays by country_2010_2019_final.csv'. 

III. The variables in the file: Month_Public and school holidays by country_2010_2019.csv
$ ISO3: The three-letter code for each country
$ Country: The name of the country
$ Year: 
$ Month: 
$ hl_sch: the number of days of public and school holidays in a month, aggregated from the 'hl_sch' variable in the 'Day_Public and school holidays by country_2010_2019_final.csv'. 
