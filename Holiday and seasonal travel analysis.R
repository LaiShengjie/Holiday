rm(list=ls())

library(rvest)
library(xml2)
library(purrr)
library(plyr); library(dplyr) # for aggregating data
library(lubridate)
library(stringr)
library(data.table) # fread - fastly reading data
library(readxl) # reading excel
library(reshape) # melt and dcast
library(pracma) # detrend
library(ggplot2)
library(rgdal) # read shapefiles
library(sf) 
library(grid) # multipanel plots
library(gridExtra) #multipanel plots

setwd("D:/OneDrive - University of Southampton/Seasonality Project/Scientific_Data_Holidays")

### Holiday Heatmap plots ----------
# data aggregation and plot function
source('heatmap plot_world.r')

## lat/long of nations/territories/state/
adm <- read.csv("Holidays/Nations and territories.csv", header=T, stringsAsFactors=F)
adm <- unique(adm[, c('ISO3', 'lat')]) # 236

# holiday ts data
ts.week <- read.csv('Holidays/Week_Public and school holidays_2010_2019.csv', stringsAsFactors = F)
ts.month <- read.csv('Holidays/Month_Public and school holidays_2010_2019.csv', stringsAsFactors = F)
ts.week <- merge(ts.week, adm, by= 'ISO3')
ts.month <- merge(ts.month, adm, by='ISO3')
dat <- ts.month

### ~~ month -------
pdf('Figures/Heatmaps_Public and School holiday_month_world.pdf', width=12, height=8)
layout(matrix(c(1,2,3,4), nrow=1, ncol=4), widths=c(5,0.85,5,0.85), heights=c(2.5))

# colour
n <- 32
col <- cm.colors(n)[1:n]

mat1 <- dat[order(dat$lat, dat$ISO3, dat$Year, dat$Month),]
mat1 <- mat1[,c('lat', 'Year', 'Month','hl_sch')]
mat1 <- dcast(mat1, Year + Month ~ lat, value.var = 'hl_sch')

mat1$Year <- mat1$Month <- NULL
mat1 <- as.matrix(mat1)
mat1[is.na(mat1)] <- 0

region <- dat[, c('ISO3','lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ISO3
region$y <- region$lat
region$lat <- region$ISO3 <- NULL

## Panel: A
col1 <- c('white', 'lightblue')
mat2 <- mat1
mat2[mat2 >= 1] <- 1

# plot
par(mar=c(4,3,2,1),xpd=F)
panel.month(mat2, region, col1, paste('A Public and school holidays', sep = ''))

# legend
par(mar=c(50,0,2,4.5))
breaks <- 0:2
panel.legend.wk2(mat2, col1, breaks)

## Panel: B
# plot
par(mar=c(4,3,2,1),xpd=F)
panel.month(mat1, region, col, paste('B Public and school holidays', sep = ''))

# legend
par(mar=c(25,0,2,4.5))
breaks <- 0:32
panel.legend.wk1(mat1, col, breaks)

dev.off()

### ~~ week -------- 
## only public holidays
pdf('Figures/Heatmaps_Public holiday_week_world.pdf', width=11, height=5)
layout(matrix(c(1,2,3,4), nrow=1, ncol=4), widths=c(5,1,5,1), heights=c(2.5))

# colour
n <- 8
col <- viridis::viridis(n)[n:1]

for(i in 2010:2019){
  ## week data
  dat <- ts.week[ts.week$Year == i,]
  mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
  mat1 <- mat1[,c('lat','Week','holiday')]
  mat1 <- dcast(mat1, Week ~ lat, value.var = 'holiday')
  mat1$Week <- NULL 
  mat1 <- as.matrix(mat1)
  mat1[is.na(mat1)] <- 0
  wk <- unique(dat$Week)
  
  region <- dat[, c('ISO3','lat')]
  region <- region[order(region$lat, region$ISO3),]
  region <- unique(region)
  region$adm <- region$ISO3
  region$y <- region$lat
  region$lat <- region$ISO3 <- NULL
  
  ## Panel: week: Y/N
  col1 <- c('white', 'lightblue')
  mat2 <- mat1
  mat2[mat2 >= 1] <- 1
  
  # plot
  par(mar=c(5,3,4,2),xpd=F)
  panel(mat2, region, col1, wk, paste('A ', i, sep = ''))
  
  # legend
  par(mar=c(28,0,4,5))
  breaks <- 0:2
  panel.legend.wk2(mat2, col1, breaks)
  
  ## Panel: week, days
  # plot
  par(mar=c(5,3,4,2),xpd=F)
  panel(mat1, region, col, wk, paste('B ', i, sep = ''))
  
  # legend
  par(mar=c(15,0,4,5))
  breaks <- 0:8
  panel.legend.wk1(mat1, col, breaks)
  
}
dev.off()

## public and school holidays
pdf('Figures/Heatmaps_Public and School holiday_week_world.pdf', width=11, height=5)
# windows(width=11, height=8, bg="white")
layout(matrix(c(1,2,3,4), nrow=1, ncol=4), widths=c(5,1,5,1), heights=c(2.5))
# layout.show(3)

# colour
n <- 8
col <- viridis::viridis(n)[n:1]

for(i in 2010:2019){
  ## weekly data
  dat <- ts.week[ts.week$Year == i,]
  mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
  mat1 <- mat1[,c('lat','Week','hl_sch')]
  mat1 <- dcast(mat1, Week ~ lat, value.var = 'hl_sch')
  mat1$Week <- NULL 
  mat1 <- as.matrix(mat1)
  mat1[is.na(mat1)] <- 0
  wk <- unique(dat$Week)
  
  region <- dat[, c('ISO3','lat')]
  region <- region[order(region$lat, region$ISO3),]
  region <- unique(region)
  region$adm <- region$ISO3
  region$y <- region$lat
  region$lat <- region$ISO3 <- NULL
  
  ## Panel: week: Y/N
  col1 <- c('white', 'lightblue')
  mat2 <- mat1
  mat2[mat2 >= 1] <- 1
  
  # plot
  par(mar=c(5,3,4,2),xpd=F)
  panel(mat2, region, col1, wk, paste('A ', i, sep = ''))
  
  # legend
  par(mar=c(28,0,4,5))
  breaks <- 0:2
  panel.legend.wk2(mat2, col1, breaks)
  
  ## Panel: week, days
  # plot
  par(mar=c(5,3,4,2),xpd=F)
  panel(mat1, region, col, wk, paste('B ', i, sep = ''))
  
  # legend
  par(mar=c(15,0,4,5))
  breaks <- 0:8
  panel.legend.wk1(mat1, col, breaks)
  
}
dev.off()

######## One figure with 2010 and 2019 data
pdf('Figures/Heatmaps_Public and School holiday_week_world_2010and2019.pdf', width=11, height=10)
layout(matrix(c(1,2,3,4,5,6,7,8), nrow=2, ncol=4, byrow = T), widths=c(5,1,5,1), heights=c(2.5,2.5))

# colour
n <- 8
col <- viridis::viridis(n)[n:1]

### 2010
i=2010
# only public holiday
## week data
dat <- ts.week[ts.week$Year == i,]
mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
mat1 <- mat1[,c('lat','Week','holiday')]
mat1 <- dcast(mat1, Week ~ lat, value.var = 'holiday')
mat1$Week <- NULL 
mat1 <- as.matrix(mat1)
mat1[is.na(mat1)] <- 0
wk <- unique(dat$Week)

region <- dat[, c('ISO3','lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ISO3
region$y <- region$lat
region$lat <- region$ISO3 <- NULL

## Panel: week, days
# plot
par(mar=c(5,3,4,2),xpd=F)
panel(mat1, region, col, wk, paste('A ', i, sep = ''))

# legend
par(mar=c(15,0,4,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

#### pubic and school holidays
dat <- ts.week[ts.week$Year == i,]
mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
mat1 <- mat1[,c('lat','Week','hl_sch')]
mat1 <- dcast(mat1, Week ~ lat, value.var = 'hl_sch')
mat1$Week <- NULL 
mat1 <- as.matrix(mat1)
mat1[is.na(mat1)] <- 0
wk <- unique(dat$Week)

region <- dat[, c('ISO3','lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ISO3
region$y <- region$lat
region$lat <- region$ISO3 <- NULL

## Panel: week, days
# plot
par(mar=c(5,3,4,2),xpd=F)
panel(mat1, region, col, wk, paste('B ', i, sep = ''))

# legend
par(mar=c(15,0,4,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

#### 2019
i=2019
### only pubic holidays
## week data
dat <- ts.week[ts.week$Year == i,]
mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
mat1 <- mat1[,c('lat','Week','holiday')]
mat1 <- dcast(mat1, Week ~ lat, value.var = 'holiday')
mat1$Week <- NULL 
mat1 <- as.matrix(mat1)
mat1[is.na(mat1)] <- 0
wk <- unique(dat$Week)

region <- dat[, c('ISO3','lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ISO3
region$y <- region$lat
region$lat <- region$ISO3 <- NULL

## Panel: week, days
# plot
par(mar=c(5,3,4,2),xpd=F)
panel(mat1, region, col, wk, paste('C ', i, sep = ''))

# legend
par(mar=c(15,0,4,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

### public and school holidays
dat <- ts.week[ts.week$Year == i,]
mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
mat1 <- mat1[,c('lat','Week','hl_sch')]
mat1 <- dcast(mat1, Week ~ lat, value.var = 'hl_sch')
mat1$Week <- NULL 
mat1 <- as.matrix(mat1)
mat1[is.na(mat1)] <- 0
wk <- unique(dat$Week)

region <- dat[, c('ISO3','lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ISO3
region$y <- region$lat
region$lat <- region$ISO3 <- NULL

## Panel: week, days
# plot
par(mar=c(5,3,4,2),xpd=F)
panel(mat1, region, col, wk, paste('D ', i, sep = ''))

# legend
par(mar=c(15,0,4,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

dev.off()

# Air travel Heatmaps -------
# air travel data
air <- read.csv('Air travel data/All Air Travel data in thousand.csv', stringsAsFactors = F)
air$Total[is.na(air$Total) == T] <-  air$Total_OS[is.na(air$Total) == T]
hol.air <- merge(ts.month, air, by=c('ISO3', 'Year', 'Month'), all.x = T)
hol.air <- hol.air[order(hol.air$lat, hol.air$ISO3, hol.air$Year, hol.air$Month),]

pdf('Figures/Heatmaps_AirTravel_month_world.pdf', width=9, height=10)
# windows(width=9, height=10, bg="white")
layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow = T), widths=c(5.5, 2, 0.9), heights=c(1,1))
# layout.show(6)

# ~~ detrend and standardized monthly proportion/ranks ----
dat <- hol.air
for(i in unique(dat$ISO3)){
  df <- dat[dat$ISO3 == i,]
  # df$Total[is.na(df$Total) == F] <- detrend(df$Total[is.na(df$Total) == F], 'linear')
  
  for(y in unique(dat$Year)){
    df1 <- df[df$ISO3 == i & df$Year == y,]
    
    if(is.na(sum(df1$Total))==T | min(df1$Total) == 0 ){
      dat$Total[dat$ISO3 == i & dat$Year == y] <- NA
    }else{
      # by proporttion
      # dat$Total[dat$ISO3 == i & dat$Year == y] <- dat$Total[dat$ISO3 == i & dat$Year == y]/sum(df1$Total)
      # by rank
      dat$Total[dat$ISO3 == i & dat$Year == y] <- rank(df1$Total,ties.method= "min")
    }
  }
}

dat <- dat[dat$Year != 2019,]
dat <- dat[order(dat$lat, dat$ISO3, dat$Year, dat$Month),]

## For ts plots
mat1 <- dat[,c('lat', 'Year', 'Month','Total')]
mat1.m <- aggregate(Total ~ lat + Month, data = mat1, mean)
mat1.m <- dcast(mat1.m, Month ~ lat, value.var = 'Total', sum)
mat1.m$Month <- NULL

mat1 <- dcast(mat1, Year + Month ~ lat, value.var = 'Total', sum)

mat2 <- dat[,c('lat', 'Year', 'Month','hl_sch')]
mat2 <- dcast(mat2, Year + Month ~ lat, value.var = 'hl_sch')
mat2 <- mat2[,colSums(mat1, na.rm = T)>0] # only keep countries with air travel data
mat2$Year <- NULL
mat2.m <- aggregate( .~ Month, data = mat2, mean)

mat2.m$Month <- mat2$Month <- NULL
mat2 <- as.matrix(mat2)
mat2[is.na(mat2)] <- 0
mat2.m <- as.matrix(mat2.m)
mat2.m[is.na(mat2.m)] <- 0


mat1 <- mat1[,colSums(mat1, na.rm = T)>0] # only keep countries with air travel data
mat1$Year <- mat1$Month <- NULL
mat1 <- as.matrix(mat1)
mat1.m <- as.matrix(mat1.m)

region <- dat[, c('ISO3','lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ISO3
region$y <- region$lat
region$lat <- region$ISO3 <- NULL

## ~~ Panels ---- 
## A - public and school holidays
# colour
n <- 32
col <- cm.colors(n)[1:n]

# plot
par(mar=c(4,3,3,1),xpd=F)
panel.month.air(mat2, region, col, paste('A Holidays', sep = ''))


## B - Holiday seasonality
par(mar=c(4,3,3,1),xpd=F)
panel.month.air.season(mat2.m, region, col, paste('B Seasonality', sep = ''))

# legend
par(mar=c(25,0,3,4.5))
breaks <- 0:(max(mat2, na.rm = T)+1)
panel.legend.wk1(mat2, col, breaks)

## C - air travel seasonality
# colour
n <- 12
# col <- cm.colors(n)[1:n]
col <- heat.colors(n)[n:1]
# col <- c('palegreen', col)

# plot
par(mar=c(4,3,3,1),xpd=F)
panel.month.air(mat1, region, col, paste('C Air passengers', sep = ''))

## D - air travel seasonality
par(mar=c(4,3,3,1),xpd=F)
panel.month.air.season(mat1.m, region, col, paste('D Seasonality', sep = ''))

# legend
par(mar=c(25,0,3,4.5))
breaks <- 1:(max(mat1,na.rm = T)+1)
panel.legend.wk1.air(mat1, col, breaks)

dev.off()

### Maps ----
### ~~ Air travel ----
shp <- readOGR(dsn = 'Shapefiles/ne_50m_admin_0_countries_lakes',
        layer = 'ne_50m_admin_0_countries_lakes',
        stringsAsFactors = F,encoding="UTF-8")

# monthly air travel data
mat1 <- dat[,c('ISO3', 'Year', 'Month','Total')]
mat1.m <- aggregate(Total ~ ISO3 + Month, data = mat1, mean)
mat1.m <- dcast(mat1.m, Month ~ ISO3, value.var = 'Total', sum)
mat1.m$Month <- NULL
mat1 <- dcast(mat1, Year + Month ~ ISO3, value.var = 'Total', sum)
mat1 <- mat1[,colSums(mat1, na.rm = T)>0] # only keep countries with air travel data
mat1$Year <- mat1$Month <- NULL
mat1 <- as.matrix(mat1)
mat1.m <- as.matrix(mat1.m)
mat1.m <- melt(data = mat1.m) 

## Air travel data availability
pdf('Figures/Maps_AirTravel_availability_world.pdf', width=10, height=5, onefile = T)

n <- 12
col <- heat.colors(n)[n:1]

df1 <- mat1.m[mat1.m$X1 == 1,c('X1','X2')]

world <- merge(shp, df1, by.x='ADM0_A3', by.y='X2', all.x = T)
world$id <- row.names(world)
points = fortify(world, region="id")
world.df = join(points, world@data, by="id")

map <- ggplot()  +
  geom_polygon(aes(x=long,y=lat,group=group, fill=X1),colour='grey', show.legend = F,
               size = 0.01, data=world.df) + 
  theme(legend.position = c(0.1,0.2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
        axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 20)) +
  coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 

map

dev.off()

## Air travel by month
pdf('Figures/Maps_AirTravel_month_world.pdf', width=10, height=5, onefile = T)
p <- list()
mon <- unique(mat1.m$X1)
mon.lab <- c('January', 'February', 'March', 'April', 'May', 'June', 
             'July', 'August', 'September', 'October', 'November', 'December')
n <- 12
col <- heat.colors(n)[n:1]
for(i in 1:length(mon)){
  df1 <- mat1.m[mat1.m$X1 == mon[i],]

  world <- merge(shp, df1, by.x='ADM0_A3', by.y='X2', all.x = T)
  world$id <- row.names(world)
  points = fortify(world, region="id")
  world.df = join(points, world@data, by="id")
  
  map <- ggplot()  +
    geom_polygon(aes(x=long,y=lat,group=group, fill=value),colour='grey75',
                 size = 0.01, data=world.df) + 
    scale_fill_gradientn(name='Air passengers (rank)', colours = col) + 
    labs(title = mon.lab[i]) +
    theme(legend.position = c(0.1,0.2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
        axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 20)) +
    coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 
  
  p[[i]] <- map
  print(map)
}
dev.off()

# merge plots
pdf('Figures/Maps_AirTravel_July_DEC_world.pdf', width=10, height=10)
g <- rbind(ggplotGrob(p[[7]]), ggplotGrob(p[[12]]), size = 'last')
grid.draw(g)
dev.off()

## ~~ holidays ----
shp <- readOGR(dsn = 'Shapefiles/ne_50m_admin_0_countries_lakes',
               layer = 'ne_50m_admin_0_countries_lakes',
               stringsAsFactors = F,encoding="UTF-8")

shp.dat <- shp@data
shp.dat$ADMIN[shp.dat$ADM0_A3 == 'PSE']
# Adjust some codes for mapping only
shp$ADM0_A3[shp$ADM0_A3 == 'SOL'] <- 'SOM' # Somaliland, an autonomous region of Somalia [SOM]
shp$ADM0_A3[shp$ADM0_A3 == 'CYN'] <- 'CYP' # Northern Cyprus 
shp$ADM0_A3[shp$ADM0_A3 == 'KAS'] <- 'CHN' 
shp$ADM0_A3[shp$ADM0_A3 == 'SDS'] <- 'SSD' # South Sudan ISO3 code to match with ts data
shp$ADM0_A3[shp$ADM0_A3 == 'PSX'] <- 'PSE' # Palestine

# monthly holiday data
ts.month <- read.csv('Holidays/Month_Public and school holidays_2010_2019.csv', stringsAsFactors = F)
mat1 <- ts.month[,c('ISO3', 'Year', 'Month','hl_sch')]
mat1.m <- aggregate(hl_sch ~ ISO3 + Month, data = mat1, mean)
mat1.m <- dcast(mat1.m, Month ~ ISO3, value.var = 'hl_sch', sum)
mat1.m$Month <- NULL
mat1 <- dcast(mat1, Year + Month ~ ISO3, value.var = 'hl_sch', sum)
mat1$Year <- mat1$Month <- NULL
mat1 <- as.matrix(mat1)
mat1.m <- as.matrix(mat1.m)
mat1.m <- melt(data = mat1.m) 

## Holiday by month
pdf('Figures/Maps_Holidays_month_world.pdf', width=10, height=5, onefile = T)
p <- list()
mon <- unique(mat1.m$X1)
mon.lab <- c('January', 'February', 'March', 'April', 'May', 'June', 
             'July', 'August', 'September', 'October', 'November', 'December')
n <- 32
col <- cm.colors(n)[1:n]
for(i in 1:length(mon)){
  df1 <- mat1.m[mat1.m$X1 == mon[i],]
  
  world <- merge(shp, df1, by.x='ADM0_A3', by.y='X2', all.x = T)
  # world <- merge(shp, df1, by.x='GID_0', by.y='X2', all.x = T)
  world$id <- row.names(world)
  points = fortify(world, region="id")
  world.df = join(points, world@data, by="id")
  
  map <- ggplot()  +
    geom_polygon(aes(x=long,y=lat,group=group, fill=value), show.legend = F,
                 # colour='grey75', size = 0.001, 
                 data=world.df) + 
    scale_fill_gradientn(name='Holidays (days)', colours = col) + 
    labs(title = mon.lab[i]) +
    theme(legend.position = c(0.1,0.2),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
          axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
          axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 20)) +
    coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 
  
  p[[i]] <- map
  print(map)
}

dev.off()

# merge plots
pdf('Figures/Maps_Holidays_month_world_one.pdf', width=15, height=11)
g1 <- cbind(ggplotGrob(p[[1]]),
           ggplotGrob(p[[2]]), 
           ggplotGrob(p[[3]]), size = 'last')
g2 <- cbind(ggplotGrob(p[[4]]),
            ggplotGrob(p[[5]]), 
            ggplotGrob(p[[6]]), size = 'last')
g3 <- cbind(ggplotGrob(p[[7]]),
            ggplotGrob(p[[8]]), 
            ggplotGrob(p[[9]]), size = 'last')
g4 <- cbind(ggplotGrob(p[[10]]),
            ggplotGrob(p[[11]]), 
            ggplotGrob(p[[12]]), size = 'last')
g <- rbind(g1, g2, g3, g4, size = 'last')
grid.draw(g)
dev.off()

### correlation -----
# ~~ detrend and standardized monthly proportion/ranks ----
dat <- hol.air
for(i in unique(dat$ISO3)){
  df <- dat[dat$ISO3 == i,]
  for(y in unique(dat$Year)){
    df1 <- df[df$ISO3 == i & df$Year == y,]
    # Total
    if(is.na(sum(df1$Total))==T | min(df1$Total) == 0 ){
      dat$Total[dat$ISO3 == i & dat$Year == y] <- NA
    }else{
      # by proporttion
      # dat$Total[dat$ISO3 == i & dat$Year == y] <- dat$Total[dat$ISO3 == i & dat$Year == y]/sum(df1$Total)
      # by rank
      dat$Total[dat$ISO3 == i & dat$Year == y] <- rank(df1$Total,ties.method= "min")
    }
    
    # Domestic
    if(is.na(sum(df1$Domestic))==T | min(df1$Domestic) == 0 ){
      dat$Domestic[dat$ISO3 == i & dat$Year == y] <- NA
    }else{
      # by proporttion
      # dat$Domestic[dat$ISO3 == i & dat$Year == y] <- dat$Domestic[dat$ISO3 == i & dat$Year == y]/sum(df1$Domestic)
      # by rank
      dat$Domestic[dat$ISO3 == i & dat$Year == y] <- rank(df1$Domestic,ties.method= "min")
    }
    
    # International
    if(is.na(sum(df1$International))==T | min(df1$International) == 0 ){
      dat$International[dat$ISO3 == i & dat$Year == y] <- NA
    }else{
      # by proporttion
      # dat$International[dat$ISO3 == i & dat$Year == y] <- dat$International[dat$ISO3 == i & dat$Year == y]/sum(df1$International)
      # by rank
      dat$International[dat$ISO3 == i & dat$Year == y] <- rank(df1$International,ties.method= "min")
    }
  }
}

dat$hl_group <- cut(dat$hl_sch, breaks=c(0, 4, 8, 15, 22, Inf),
                    labels=c("0-3","4-7","8-14", '15-21', '22-31'),right = F,order=TRUE)

ggplot(data=dat, aes(x=as.factor(hl_sch), y=Total)) + 
  geom_boxplot() + theme_classic()
ggplot(data=dat, aes(x=as.factor(hl_group), y=Total)) + 
  geom_boxplot() + theme_classic()

### ~~ holiday vs travel plot ----
pdf('Figures/Heatmaps_Holiday vs Air Travel_std.pdf', width=10, height=6)
layout(matrix(c(1,2,3,4), nrow=1, ncol=4, byrow = T), widths=c(3,3,3,0.8), heights=c(2.5))

# col
n <- 20
col <- viridis::magma(n)[n:1]

## holidays vs total travel
df <- dat[is.na(dat$Total) == F,]
cor(df$hl_sch, df$Total)
chisq.test(df$hl_group, df$Total)
dt <- as.data.frame(table(df$hl_group, df$Total))
dt <- dcast(dt, Var1 ~ Var2, value.var = 'Freq')
rownames(dt) <- dt$Var1
dt$Var1 <- NULL
dt <- as.matrix(dt)
dt <- dt/rowSums(dt)
## plot A
par(mar=c(4,6,3,1),xpd=F)
panel.month.air.hol(dt, col, paste('A Total', sep = ''))

## holidays vs domestic travel
df <- dat[is.na(dat$Domestic) == F,]
cor(df$hl_sch, df$Domestic)
chisq.test(df$hl_group, df$Domestic)
dt <- as.data.frame(table(df$hl_group, df$Domestic))
dt <- dcast(dt, Var1 ~ Var2, value.var = 'Freq')
rownames(dt) <- dt$Var1
dt$Var1 <- NULL
dt <- as.matrix(dt)
dt <- dt/rowSums(dt)
## plot B
par(mar=c(4,6,3,1),xpd=F)
panel.month.air.hol(dt, col, paste('B Domestic', sep = ''))

## holidays vs international travel
df <- dat[is.na(dat$International) == F,]
cor(df$hl_sch, df$International)
chisq.test(df$hl_group, df$International)
dt <- as.data.frame(table(df$hl_group, df$International))
dt <- dcast(dt, Var1 ~ Var2, value.var = 'Freq')
rownames(dt) <- dt$Var1
dt$Var1 <- NULL
dt <- as.matrix(dt)
dt <- dt/rowSums(dt)
## plot C
par(mar=c(4,6,3,1),xpd=F)
panel.month.air.hol(dt, col, paste('C International', sep = ''))

# legend
par(mar=c(35,0,3,4.5))
breaks <- 1:(n+1)
panel.legend.air.hol(dt, col, breaks)

dev.off()