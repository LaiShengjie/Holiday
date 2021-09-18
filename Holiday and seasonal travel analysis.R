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

setwd("C:/Users/sl4m18/OneDrive - University of Southampton/Seasonality Project/Scientific_Data_Holidays")

### Multiple plot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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
panel.month(mat2, region, col1, paste('a', sep = ''))

# legend
par(mar=c(50,0,2,4.5))
breaks <- 0:2
panel.legend.wk2(mat2, col1, breaks)

## Panel: B
# plot
par(mar=c(4,3,2,1),xpd=F)
panel.month(mat1, region, col, paste('b', sep = ''))

# legend
par(mar=c(25,0,2,4.5))
breaks <- 0:32
panel.legend.wk1(mat1, col, breaks)

dev.off()

### ~~ week -------- 
## ~~~~ only public holidays ----
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

## ~~~~ public and school holidays ----
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

######## ~~~~ One figure with 2010 and 2019 data ----
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

######## ~~~~ One figure for selected countries with 2010 and 2019 data ----
pdf('Figures/Heatmaps_Public and School holiday_week_world_continent_2010and2019.pdf', width=13, height=10)
layout(matrix(c(1,2,3,4,5,6,7,8), nrow=2, ncol=4, byrow = T), widths=c(5,1,5,1), heights=c(2.5,2.5))

# colour
n <- 8
col <- viridis::viridis(n)[n:1]

### 2010
i=2010
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
par(mar=c(5,8,3,2),xpd=F)
panel.world(mat1, region, col, wk, paste('a ', i, sep = ''))

# legend
par(mar=c(15,0,3,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

#### 2019
i=2019
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
par(mar=c(5,8,3,2),xpd=F)
panel.world(mat1, region, col, wk, paste('b ', i, sep = ''))

# legend
par(mar=c(15,0,3,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

### 2010 selected countries
iso <- read.csv("Holidays/selected countries.csv", header=T, stringsAsFactors=F)
i=2010
#### pubic and school holidays
dat <- ts.week[ts.week$Year == i & ts.week$ISO3 %in% iso$ISO3,]
mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
mat1 <- mat1[,c('lat','Week','hl_sch')]
mat1 <- dcast(mat1, Week ~ lat, value.var = 'hl_sch')
mat1$Week <- NULL 
mat1 <- as.matrix(mat1)
mat1[is.na(mat1)] <- 0
wk <- unique(dat$Week)

region <- iso[, c('ISO3', 'ADM_name', 'lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ADM_name
region$y <- region$lat
region$lat <- region$ISO3 <- region$ADM_name <- NULL

## Panel: week, days
# plot
par(mar=c(5,8,3,2),xpd=F)
panel.iso(mat1, region, col, wk, paste('c ', i, sep = ''))

# legend
par(mar=c(15,0,3,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

### 2019 selected countries
iso <- read.csv("Holidays/selected countries.csv", header=T, stringsAsFactors=F)
i=2019
#### pubic and school holidays
dat <- ts.week[ts.week$Year == i & ts.week$ISO3 %in% iso$ISO3,]
mat1 <- dat[order(dat$lat, dat$ISO3, dat$Week),]
mat1 <- mat1[,c('lat','Week','hl_sch')]
mat1 <- dcast(mat1, Week ~ lat, value.var = 'hl_sch')
mat1$Week <- NULL 
mat1 <- as.matrix(mat1)
mat1[is.na(mat1)] <- 0
wk <- unique(dat$Week)

region <- iso[, c('ISO3', 'ADM_name', 'lat')]
region <- region[order(region$lat, region$ISO3),]
region <- unique(region)
region$adm <- region$ADM_name
region$y <- region$lat
region$lat <- region$ISO3 <- region$ADM_name <- NULL

## Panel: week, days
# plot
par(mar=c(5,8,3,2),xpd=F)
panel.iso(mat1, region, col, wk, paste('d ', i, sep = ''))

# legend
par(mar=c(15,0,3,5))
breaks <- 0:8
panel.legend.wk1(mat1, col, breaks)

dev.off()

#### Air travel Heatmaps -------
## air travel statistic data
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

map.air <- ggplot()  +
  geom_polygon(aes(x=long,y=lat,group=group, fill=X1),colour='grey', show.legend = F,
               size = 0.01, data=world.df) + 
  labs(title = 'a') +
  theme(legend.position = c(0.1,0.2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
        axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 20)) +
  coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 

map.air

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
  
  if(i==1){
    map <- ggplot()  +
      geom_polygon(aes(x=long,y=lat,group=group, fill=value), show.legend = T,
                   # colour='grey75', size = 0.001, 
                   data=world.df) + 
      scale_fill_gradientn(name='Holidays (days)', colours = col, limits=c(1,n)) + 
      labs(title = mon.lab[i]) +
      theme(legend.position = c(0.1,0.3),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
            axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "white"), 
            plot.title = element_text(face = 'bold', hjust = 0.1, size = 20)) +
      coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 
  }else{
    map <- ggplot()  +
      geom_polygon(aes(x=long,y=lat,group=group, fill=value), show.legend = F,
                   # colour='grey75', size = 0.001, 
                   data=world.df) + 
      scale_fill_gradientn(name='Holidays (days)', colours = col) + 
      labs(title = mon.lab[i]) +
      theme(legend.position = c(0.1,0.2),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
            axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "white"), 
            plot.title = element_text(face = 'bold', hjust = 0.1, size = 20)) +
      coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 
  }
  
  p[[i]] <- map
  print(map)
}

dev.off()

# merge plots
pdf('Figures/Maps_Holidays_month_world_one.pdf', width=11, height=14)
g1 <- rbind(ggplotGrob(p[[1]]),
            ggplotGrob(p[[2]]), 
            ggplotGrob(p[[3]]), 
            ggplotGrob(p[[4]]),
            ggplotGrob(p[[5]]), 
            ggplotGrob(p[[6]]), size = 'last')
g2 <- rbind(ggplotGrob(p[[7]]),
            ggplotGrob(p[[8]]), 
            ggplotGrob(p[[9]]),
            ggplotGrob(p[[10]]),
            ggplotGrob(p[[11]]), 
            ggplotGrob(p[[12]]), size = 'last')
g <- cbind(g1, g2, size = 'last')
grid.draw(g)
dev.off()


### correlation using air travel statistics -----
# ~~ detrend and standardized monthly proportion/ranks ----
dat <- hol.air
dat
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

dat$hl_group <- cut(dat$hl_sch, breaks=c(0, 4, 8, 15, Inf),
                    labels=c("0-3","4-7","8-14", '15-31'),right = F,order=TRUE)

### ~~ boxplot ----  
pdf('Figures/Boxplot_Holiday vs Air Travel_std.pdf', width=7, height=5)
dat.air <- dat
Boxplot_air <- ggplot(data=dat.air, aes(x=hl_group, y=Total)) + 
  geom_boxplot() +  scale_y_continuous(breaks=c(1, 2, 3, 4, 5,6, 7,8, 9,10, 11, 12))+
  xlab('Holiday (days)') + ylab('Air travel (rank)') +
  labs(title = paste('a'), colour='') +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank())  +
  theme(legend.position=c(0.2,0.8), legend.background = element_blank())+
  theme( axis.text  = element_text(size=13,colour = 'black'), 
         axis.title = element_text(size = 15),
         title = element_text(size=15),
         legend.title = element_text(size = 12),
         legend.text = element_text(size=12))
Boxplot_air
dev.off()

### ~~ holiday vs travel heatmap ----
pdf('Figures/Heatmaps_Holiday vs Air Travel_std.pdf', width=10, height=6)
layout(matrix(c(1,2,3,4), nrow=1, ncol=4, byrow = T), widths=c(3,3,3,0.8), heights=c(2.5))

# col
n <- 20
col <- viridis::magma(n)[n:1]

## holidays vs total travel
df <- dat.air[is.na(dat.air$Total) == F,]
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
panel.month.air.hol(dt, col, paste('a Total', sep = ''))

## holidays vs domestic travel
df <- dat.air[is.na(dat.air$Domestic) == F,]
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
panel.month.air.hol(dt, col, paste('b Domestic', sep = ''))

## holidays vs international travel
df <- dat.air[is.na(dat.air$International) == F,]
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
panel.month.air.hol(dt, col, paste('c International', sep = ''))

# legend
par(mar=c(35,0,3,4.5))
breaks <- 1:(n+1)
panel.legend.air.hol(dt, col, breaks)

dev.off()


### Worldpop data - correlation using air travel in 2010 -----
## 2010 air travel flow data estimated by Worldpop
airwp <- read.csv('Air travel data/Worldpop_air travel data in thousand in 2010.csv', stringsAsFactors = F)
hol.airwp <- hol.air[,c("ISO3", "Year", "Month", "ADM_name", "holiday", "hl_sch", "lat")]
hol.airwp <- merge(hol.airwp, airwp, by=c('ISO3', 'Year', 'Month'))
hol.airwp <- hol.airwp[order(hol.airwp$lat, hol.airwp$ISO3, hol.airwp$Year, hol.airwp$Month),]

# ~~ detrend and standardized monthly proportion/ranks ----
dat <- hol.airwp
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

dat$hl_group <- cut(dat$hl_sch, breaks=c(0, 4, 8, 15, Inf),
                    labels=c("0-3","4-7","8-14", '15-31'),right = F,order=TRUE)

### ~~ boxplot ----
pdf('Figures/Boxplot_Holiday vs Air Travel 2010_wp_std.pdf', width=7, height=5)
ggplot(data=dat, aes(x=hl_group, y=Total)) + 
  geom_boxplot() +  scale_y_continuous(breaks=c(1, 2, 3, 4, 5,6, 7,8, 9,10, 11, 12))+
  xlab('Holiday (days)') + ylab('Air travel (rank)') +
  # labs(title = paste('A'), x='', y = 'No of exported cases', colour='') +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank())  +
  theme(legend.position=c(0.2,0.8), legend.background = element_blank())+
  theme( axis.text  = element_text(size=13,colour = 'black'), 
         axis.title = element_text(size = 15),
         title = element_text(size=15),
         legend.title = element_text(size = 12),
         legend.text = element_text(size=12))

dev.off()

### OAG international air travel data ------
# no data in pop_data or air travel
# iso.exc <- c('BES', 'BLM', 'CUW', 'MAF', 'SXM')

#### code and pop data ---
# ts
date.df <- data.frame(date = seq(as.Date('2015-01-01'),as.Date('2019-12-31'), by='1 day'))
date.df$Time <- year(date.df$date) *100 + month(date.df$date)

# ISO 3 alpha code
Country <- countrycode::codelist[, c('country.name.en', 'iso3c', 'iso2c')]
colnames(Country) <- c('CountryName', 'ISO3', 'ISO2')
Country <- Country[is.na(Country$ISO3) == F,]
Country$ISO2[Country$ISO3 == 'NAM'] <- 'NAM'

#### OAG country code data
oagcode = fread("Air travel data/country_code_OAG_unique.csv", stringsAsFactors = F)
oagcode <- oagcode[, 1:10]
oagcode$`Country Code`[is.na(oagcode$`Country Code`) == T] <- 'NAM'
colnames(oagcode) <- c("no", "ISO2", "Country", "DOTCountryCode",
                       "Long","Lat","RegionCode", "RegionName",  
                       "EffFrom","EffTo")
oagcode <- merge(oagcode, Country, by.x = 'ISO2', by.y = 'ISO2')
# oagcode <- oagcode[!(oagcode$ISO3 %in% iso.exc),]
# oagcode$ISO3[oagcode$ISO2 == 'ZR'] <- 'COD'
length(unique(oagcode$ISO3))
iso_dup <- oagcode$ISO3[duplicated(oagcode$ISO3) ==T]
oagcode[oagcode$ISO3 %in% iso_dup,]

### OAG air travel data ---
air.oag = fread("Air travel data/OAG_passenger_flow_between_countries_201501_202005.csv", stringsAsFactors = F)
str(air.oag)
air.oag$Dep_Country_Code[air.oag$Dep_Country_Name == 'Namibia'] <- 'NAM'
air.oag$Arr_Country_Code[air.oag$Arr_Country_Name == 'Namibia'] <- 'NAM'
air.oag <- air.oag[air.oag$Time <= 201912,]

air.oag <- air.oag[,c("Dep_Country_Code", "Arr_Country_Code", "Total_Est_Pax", "Time" )]
# air.oag <- air.oag[,c("Dep_Country_Code", "Arr_Country_Code", "Unadjusted_Pax", "Time" )]

colnames(air.oag) <- c("ISO2fr",  "ISO2to", "est",  "Month" )
length(unique(air.oag$ISO2fr))
length(unique(air.oag$ISO2to))

air.oag[air.oag$ISO2fr == 'CN' & air.oag$ISO2to == 'CN',]

yearmonth <- unique(air.oag$Month)
air.oag.ts <- expand.grid(ISO2fr = unique(air.oag$ISO2fr), ISO2to= unique(air.oag$ISO2to), Month = yearmonth)
air.oag.ts <- merge(air.oag.ts, air.oag, by = c("ISO2fr",  "ISO2to",  "Month" ), all.x=T)
air.oag.ts$est[is.na(air.oag.ts$est) == T] <- 0
# air.oag.ts <-air.oag.ts[air.oag.ts$ISO2fr != air.oag.ts$ISO2to,]

air.oag.ts <- merge(air.oag.ts, oagcode[,c('ISO3', 'ISO2')], by.x='ISO2fr', by.y='ISO2')
air.oag.ts$ISO3fr <- air.oag.ts$ISO3
air.oag.ts$ISO3 <- NULL
air.oag.ts <- merge(air.oag.ts, oagcode[,c('ISO3', 'ISO2')],  by.x='ISO2to', by.y='ISO2')
air.oag.ts$ISO3to <- air.oag.ts$ISO3
air.oag.ts$ISO3 <- air.oag.ts$ISO2fr <- air.oag.ts$ISO2to <- NULL
air.oag.ts$Year <- as.integer(substr(air.oag.ts$Month,1,4))
air.oag.ts$Month <- as.integer(substr(air.oag.ts$Month,5,6))

length(unique(air.oag.ts$ISO3fr))
length(unique(air.oag.ts$ISO3to))
air.oag.ts <- data.table(air.oag.ts)
oag.total <- air.oag.ts[, list(Total = sum(est, na.rm = T)), by = 'ISO3fr,Year,Month']
oag.d <- air.oag.ts[air.oag.ts$ISO3fr == air.oag.ts$ISO3to, list(Domestic = sum(est, na.rm = T)), by = 'ISO3fr,Year,Month']
oag.i <- air.oag.ts[air.oag.ts$ISO3fr != air.oag.ts$ISO3to, list(International = sum(est, na.rm = T)), by = 'ISO3fr,Year,Month']
oag.total <- merge(oag.total, oag.d,  by = c('ISO3fr', 'Year', 'Month'))
oag.total <- merge(oag.total, oag.i,  by = c('ISO3fr', 'Year', 'Month'))

#### ~~ Air travel Heatmaps -------
ts.month <- read.csv('Holidays/Month_Public and school holidays_2010_2019.csv', stringsAsFactors = F)
ts.month <- merge(ts.month, adm, by='ISO3')
hol.air.oag <- merge(ts.month, oag.total, by.x=c('ISO3', 'Year', 'Month'),  by.y = c('ISO3fr', 'Year', 'Month'), all.x = T)
hol.air.oag <- hol.air.oag[order(hol.air.oag$lat, hol.air.oag$ISO3, hol.air.oag$Year, hol.air.oag$Month),]

pdf('Figures/Heatmaps_AirTravel_OAG_month_world.pdf', width=9, height=10)
# windows(width=9, height=10, bg="white")
layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow = T), widths=c(5.5, 2, 0.9), heights=c(1,1))
# layout.show(6)

# ~~ detrend and standardized monthly proportion/ranks ----
dat <- hol.air.oag
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
    
    if(is.na(sum(df1$Domestic))==T | min(df1$Domestic) == 0 ){
      dat$Domestic[dat$ISO3 == i & dat$Year == y] <- NA
    }else{
      # by proporttion
      # dat$Domestic[dat$ISO3 == i & dat$Year == y] <- dat$Domestic[dat$ISO3 == i & dat$Year == y]/sum(df1$Domestic)
      # by rank
      dat$Domestic[dat$ISO3 == i & dat$Year == y] <- rank(df1$Domestic,ties.method= "min")
    }
    
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

dat <- dat[dat$Year >= 2015,]
dat <- dat[order(dat$lat, dat$ISO3, dat$Year, dat$Month),]

## For ts plots
mat1 <- dat[,c('lat', 'Year', 'Month','International')]
mat1.m <- aggregate(International ~ lat + Month, data = mat1, mean)
mat1.m <- dcast(mat1.m, Month ~ lat, value.var = 'International', sum)
mat1.m$Month <- NULL

mat1 <- dcast(mat1, Year + Month ~ lat, value.var = 'International', sum)

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
panel.month.air.oag(mat2, region, col, paste('A Holidays', sep = ''))


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
panel.month.air.oag(mat1, region, col, paste('C Air passengers', sep = ''))

## D - air travel seasonality
par(mar=c(4,3,3,1),xpd=F)
panel.month.air.season(mat1.m, region, col, paste('D Seasonality', sep = ''))

# legend
par(mar=c(25,0,3,4.5))
breaks <- 1:(max(mat1,na.rm = T)+1)
panel.legend.wk1.air(mat1, col, breaks)

dev.off()

### ~~ Maps ----
shp <- readOGR(dsn = 'Shapefiles/ne_50m_admin_0_countries_lakes',
               layer = 'ne_50m_admin_0_countries_lakes',
               stringsAsFactors = F,encoding="UTF-8")

# monthly air travel data
mat1 <- dat[,c('ISO3', 'Year', 'Month','International')]
mat1.m <- aggregate(International ~ ISO3 + Month, data = mat1, mean)
mat1.m <- dcast(mat1.m, Month ~ ISO3, value.var = 'International', sum)
mat1.m$Month <- NULL
mat1 <- dcast(mat1, Year + Month ~ ISO3, value.var = 'International', sum)
mat1 <- mat1[,colSums(mat1, na.rm = T)>0] # only keep countries with air travel data
mat1$Year <- mat1$Month <- NULL
mat1 <- as.matrix(mat1)
mat1.m <- as.matrix(mat1.m)
mat1.m <- melt(data = mat1.m) 

## Air travel data availability
pdf('Figures/Maps_AirTravel_OAG_availability_world.pdf', width=10, height=5, onefile = T)

n <- 12
col <- heat.colors(n)[n:1]

df1 <- mat1.m[mat1.m$X1 == 1,c('X1','X2')]

world <- merge(shp, df1, by.x='ADM0_A3', by.y='X2', all.x = T)
world$id <- row.names(world)
points = fortify(world, region="id")
world.df.oag = join(points, world@data, by="id")

map.air.oag <- ggplot()  +
  geom_polygon(aes(x=long,y=lat,group=group, fill=X1),colour='grey', show.legend = F,
               size = 0.01, data=world.df.oag) + 
  labs(title = 'b') +
  theme(legend.position = c(0.1,0.2),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
        axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 20)) +
  coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 

map.air.oag

dev.off()

## Air travel by month
pdf('Figures/Maps_AirTravel_OAG_month_world.pdf', width=10, height=5, onefile = T)
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
  
  if(i==1){
    map <- ggplot()  +
      geom_polygon(aes(x=long,y=lat,group=group, fill=value),show.legend = T,
                   # colour='grey75', size = 0.001, 
                   data=world.df) + 
      scale_fill_gradientn(name='Rank', colours = col, limits=c(1,12)) + 
      # scale_fill_gradientn(name='Outbound air passengers', colours = col, trans="log10") + 
      labs(title = mon.lab[i]) +
      theme(legend.position = c(0.1,0.3),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
            axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 20)) +
      coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 
    
  }else{
    map <- ggplot()  +
      geom_polygon(aes(x=long,y=lat,group=group, fill=value),show.legend = F,
                   # colour='grey75', size = 0.001, 
                   data=world.df) + 
      scale_fill_gradientn(name='Rank', colours = col, limits=c(1,12)) + 
      # scale_fill_gradientn(name='Outbound air passengers', colours = col, trans="log10") + 
      labs(title = mon.lab[i]) +
      theme(legend.position = c(0.1,0.2),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
            axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
            axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 20)) +
      coord_equal(xlim = c(-180, 180) , ylim = c(-55, 90)) 
  }
  
  
  p[[i]] <- map
  print(map)
}
dev.off()

# merge plots
pdf('Figures/Maps_AirTravel_OAG_July_DEC_world.pdf', width=10, height=10)
g <- rbind(ggplotGrob(p[[7]]), ggplotGrob(p[[12]]), size = 'last')
grid.draw(g)
dev.off()

# merge 12 plots
pdf('Figures/Maps_AirTravel_OAG_month_world_one.pdf', width=11, height=14)
g1 <- rbind(ggplotGrob(p[[1]]),
            ggplotGrob(p[[2]]), 
            ggplotGrob(p[[3]]), 
            ggplotGrob(p[[4]]),
            ggplotGrob(p[[5]]), 
            ggplotGrob(p[[6]]), size = 'last')
g2 <- rbind(ggplotGrob(p[[7]]),
            ggplotGrob(p[[8]]), 
            ggplotGrob(p[[9]]),
            ggplotGrob(p[[10]]),
            ggplotGrob(p[[11]]), 
            ggplotGrob(p[[12]]), size = 'last')
g <- cbind(g1, g2, size = 'last')
grid.draw(g)
dev.off()

### ~~ correlation using air travel statistics -----
# ~~~~ detrend and standardized monthly proportion/ranks ----
# dat <- hol.air.oag[hol.air.oag$Year >= 2015,]
# for(i in unique(dat$ISO3)){
#   df <- dat[dat$ISO3 == i,]
#   for(y in unique(dat$Year)){
#     df1 <- df[df$ISO3 == i & df$Year == y,]
#     # Total
#     if(is.na(sum(df1$Total))==T | min(df1$Total) == 0 ){
#       dat$Total[dat$ISO3 == i & dat$Year == y] <- NA
#     }else{
#       # by proporttion
#       # dat$Total[dat$ISO3 == i & dat$Year == y] <- dat$Total[dat$ISO3 == i & dat$Year == y]/sum(df1$Total)
#       # by rank
#       dat$Total[dat$ISO3 == i & dat$Year == y] <- rank(df1$Total,ties.method= "min")
#     }
#     
#     # Domestic
#     if(is.na(sum(df1$Domestic))==T | min(df1$Domestic) == 0 ){
#       dat$Domestic[dat$ISO3 == i & dat$Year == y] <- NA
#     }else{
#       # by proporttion
#       # dat$Domestic[dat$ISO3 == i & dat$Year == y] <- dat$Domestic[dat$ISO3 == i & dat$Year == y]/sum(df1$Domestic)
#       # by rank
#       dat$Domestic[dat$ISO3 == i & dat$Year == y] <- rank(df1$Domestic,ties.method= "min")
#     }
#     
#     # International
#     if(is.na(sum(df1$International))==T | min(df1$International) == 0 ){
#       dat$International[dat$ISO3 == i & dat$Year == y] <- NA
#     }else{
#       # by proporttion
#       # dat$International[dat$ISO3 == i & dat$Year == y] <- dat$International[dat$ISO3 == i & dat$Year == y]/sum(df1$International)
#       # by rank
#       dat$International[dat$ISO3 == i & dat$Year == y] <- rank(df1$International,ties.method= "min")
#     }
#   }
# }

dat$hl_group <- cut(dat$hl_sch, breaks=c(0, 4, 8, 15, Inf),
                    labels=c("0-3","4-7","8-14", '15-31'),right = F,order=TRUE)

### ~~~~ boxplot ----  
Boxplot_air_oag <- ggplot(data=dat, aes(x=hl_group, y=International)) + 
  geom_boxplot() +  scale_y_continuous(breaks=c(1, 2, 3, 4, 5,6, 7,8, 9,10, 11, 12))+
  xlab('Holiday (days)') + ylab('Air travel (rank)') +
  labs(title = paste('b'), colour='') +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank())  +
  theme(legend.position=c(0.2,0.8), legend.background = element_blank())+
  theme( axis.text  = element_text(size=13,colour = 'black'), 
         axis.title = element_text(size = 15),
         title = element_text(size=15),
         legend.title = element_text(size = 12),
         legend.text = element_text(size=12))
Boxplot_air_oag

pdf('Figures/Boxplot_Holiday vs Air Travel_OAG_std.pdf', width=12, height=5)
g <- cbind(ggplotGrob(Boxplot_air), ggplotGrob(Boxplot_air_oag), size = 'last')
grid.draw(g)
dev.off()

### ~~~~ holiday vs travel heatmap ----
pdf('Figures/Heatmaps_Holiday vs Air Travel_OAG_std.pdf', width=13, height=6)
layout(matrix(c(1,2,3,4,5), nrow=1, ncol=5, byrow = T), widths=c(3,3,3,3,0.8), heights=c(2.5))

# col
n <- 20
col <- viridis::magma(n)[n:1]

## holidays vs total travel
df <- dat.air[is.na(dat.air$Total) == F,]
cor(df$hl_sch, df$Total)
chisq.test(df$hl_group, df$Total)
corr <- cor.test(df$hl_sch, df$Total, method = 'spearman')
corr

dt <- as.data.frame(table(df$hl_group, df$Total))
dt <- dcast(dt, Var1 ~ Var2, value.var = 'Freq')
rownames(dt) <- dt$Var1
dt$Var1 <- NULL
dt <- as.matrix(dt)
dt <- dt/rowSums(dt)
min(dt);max(dt)
## plot A
par(mar=c(4,6,3,1),xpd=F)
panel.month.air.hol(dt, col, expression(paste('a  ', rho, '=0.533 p<0.001', sep = '')))

## holidays vs domestic travel
df <- dat.air[is.na(dat.air$Domestic) == F,]
cor(df$hl_sch, df$Domestic)
chisq.test(df$hl_group, df$Domestic)
corr <- cor.test(df$hl_sch, df$Domestic, method = 'spearman')
corr

dt <- as.data.frame(table(df$hl_group, df$Domestic))
dt <- dcast(dt, Var1 ~ Var2, value.var = 'Freq')
rownames(dt) <- dt$Var1
dt$Var1 <- NULL
dt <- as.matrix(dt)
dt <- dt/rowSums(dt)
min(dt);max(dt)

## plot B
par(mar=c(4,6,3,1),xpd=F)
panel.month.air.hol(dt, col, expression(paste('b  ', rho, '=0.309 p<0.001', sep = '')))

## holidays vs international travel
df <- dat.air[is.na(dat.air$International) == F,]
cor(df$hl_sch, df$International)
chisq.test(df$hl_group, df$International)
corr <- cor.test(df$hl_sch, df$International, method = 'spearman')
corr

dt <- as.data.frame(table(df$hl_group, df$International))
dt <- dcast(dt, Var1 ~ Var2, value.var = 'Freq')
rownames(dt) <- dt$Var1
dt$Var1 <- NULL
dt <- as.matrix(dt)
dt <- dt/rowSums(dt)
min(dt);max(dt)

## plot C
par(mar=c(4,6,3,1),xpd=F)
panel.month.air.hol(dt, col, expression(paste('c  ', rho, '=0.612 p<0.001', sep = '')))

## holidays vs OAG international travel
df <- dat[is.na(dat$International) == F,]
cor(df$hl_sch, df$International)
corr <- cor.test(df$hl_sch, df$International, method = 'spearman')
corr

chisq.test(df$hl_group, df$International)
dt <- as.data.frame(table(df$hl_group, df$International))
dt <- dcast(dt, Var1 ~ Var2, value.var = 'Freq')
rownames(dt) <- dt$Var1
dt$Var1 <- NULL
dt <- as.matrix(dt)
dt <- dt/rowSums(dt)
min(dt);max(dt)

## plot D
par(mar=c(4,6,3,1),xpd=F)
panel.month.air.hol(dt, col, expression(paste('d  ', rho, '=0.323 p<0.001', sep = '')))

# legend
par(mar=c(35,0,3,4.5))
breaks <- 1:(n+1)
panel.legend.air.hol(dt, col, breaks)

dev.off()

## ~~ official vs OAG air datasets ----
hol.air.t <- merge(hol.air[is.na(hol.air$Total) == F, c("ISO3","Year","Month", "holiday", "hl_sch", "lat","Total")], 
                   hol.air.oag[is.na(hol.air.oag$Total) == F, c("ISO3","Year","Month","holiday", "hl_sch", "lat","Total")],
                   by = c("ISO3","Year","Month","holiday", "hl_sch", "lat"))
hol.air.t$Total.x <- hol.air.t$Total.x * 1000
hol.air.t <- hol.air.t[hol.air.t$Total.x >0 & hol.air.t$Total.y >0,]
hol.air.t$ratio <- hol.air.t$Total.x/hol.air.t$Total.y
colnames(hol.air.t) <- c("ISO3", "Year", "Month", "holiday", "hl_sch", "lat","x", "y", "ratio")

hol.air.d <- merge(hol.air[is.na(hol.air$Domestic) == F, c("ISO3","Year","Month","holiday", "hl_sch", "lat","Domestic")], 
                   hol.air.oag[is.na(hol.air.oag$Domestic) == F, c("ISO3","Year","Month","holiday", "hl_sch", "lat","Domestic")],
                   by = c("ISO3","Year","Month","holiday", "hl_sch", "lat"))
hol.air.d$Domestic.x <- hol.air.d$Domestic.x * 1000
hol.air.d <- hol.air.d[hol.air.d$Domestic.x >0 & hol.air.d$Domestic.y >0,]
hol.air.d$ratio <- hol.air.d$Domestic.x/hol.air.d$Domestic.y
colnames(hol.air.d) <- c("ISO3", "Year", "Month","holiday", "hl_sch", "lat", "x", "y", "ratio")

hol.air.i <- merge(hol.air[is.na(hol.air$International) == F, c("ISO3","Year","Month","holiday", "hl_sch", "lat","International")], 
                   hol.air.oag[is.na(hol.air.oag$International) == F, c("ISO3","Year","Month","holiday", "hl_sch", "lat","International")],
                   by = c("ISO3","Year","Month","holiday", "hl_sch", "lat"))
hol.air.i$International.x <- hol.air.i$International.x * 1000
hol.air.i <- hol.air.i[hol.air.i$International.x >0 & hol.air.i$International.y >0,]
hol.air.i$ratio <- hol.air.i$International.x/hol.air.i$International.y
colnames(hol.air.i) <- c("ISO3", "Year", "Month","holiday", "hl_sch", "lat", "x", "y", "ratio")

# ~~ detrend and standardized monthly proportion/ranks ---
dat <- hol.air.i
dat$x.rank <- NA
dat$y.rank <- NA
dat$x.prop <- NA
dat$y.prop <- NA

i='USA'
y=2015
for(i in unique(dat$ISO3)){
  df <- dat[dat$ISO3 == i,]
  for(y in unique(dat$Year)){
    df1 <- df[df$ISO3 == i & df$Year == y,]
    # Total
    if(nrow(df1)>=12){
      # by rank
      dat$x.rank[dat$ISO3 == i & dat$Year == y] <- rank(df1$x, ties.method= "min")
      dat$y.rank[dat$ISO3 == i & dat$Year == y] <- rank(df1$y, ties.method= "min")
      
      # by proportion
      dat$x.prop[dat$ISO3 == i & dat$Year == y] <- dat$x[dat$ISO3 == i & dat$Year == y]/sum(df1$x)
      dat$y.prop[dat$ISO3 == i & dat$Year == y] <- dat$y[dat$ISO3 == i & dat$Year == y]/sum(df1$y)
    }
  }
}

dat$hl_group <- cut(dat$hl_sch, breaks=c(0, 4, 8, 15, Inf),
                    labels=c("0-3","4-7","8-14", '15-31'),right = F,order=TRUE)

# scatter plots
regRs=lm((dat$x) ~ (dat$y))
summary(regRs)

ggplot() + 
  labs(title = 'c', x ='Official statistics', y = 'OAG') + 
  geom_smooth(data=dat, mapping = aes(x, y),
              method = 'lm', se = F ,colour='black') +
  geom_point(data=dat, aes(x, y),
             colour='black', fill='grey', alpha=0.5) +
  geom_smooth(data=dat[dat$ISO3=='CHN',], mapping = aes(x, y),
              method = 'lm', se = F ,colour='lightblue') +
  geom_point(data=dat[dat$ISO3=='CHN',], aes(x, y),
             colour='red', fill='pink', alpha=0.5) +
  geom_smooth(data=dat[dat$ISO3=='USA',], aes(x, y),
              method = 'lm', se = F ,colour='lightblue') +
  geom_point(data=dat[dat$ISO3=='USA',], aes(x, y),
             colour='blue', fill='lightblue', alpha=0.5) +
  # annotate("text", x=0.4, y=0.9, label= "p < 0.001", size=5,parse=T) +
  # annotate("text", x=0.4, y=0.8, label= "italic(R) ^ 2 == 0.913", size=5,parse=T) +
  theme_bw() +  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())  +
  theme(legend.position=c(0.8,0.25), legend.background = element_blank())+
  theme( axis.text  = element_text(size=17,colour = 'black'), 
         axis.title = element_text(size = 19),
         title = element_text(size=22),
         legend.title = element_text(size = 19),
         legend.text = element_text(size=17)) 

regRs=lm((dat$x.rank) ~ (dat$y.rank))
summary(regRs)

regRs=lm((dat$x.prop) ~ (dat$y.prop))
summary(regRs)

map.air.vs <- ggplot(data=dat, mapping = aes(x.prop, y.prop)) + 
  labs(title = 'c', x ='Airline passenger statistics', y = 'OAG data') + 
  ylim(0, 0.2) + xlim(0, 0.2)+
  geom_point(colour='black', fill='grey', alpha=0.2) +
  geom_smooth(method = 'lm', se=F, colour='lightgreen', alpha=0.2) +
  annotate("text", x=0.05, y=0.15, label= "p<0.001", size=5,parse=T) +
  annotate("text", x=0.05, y=0.18, label= "italic(R) ^ 2==0.917", size=5,parse=T) +
  theme_bw() +  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())  +
  theme(legend.position=c(0.8,0.25), legend.background = element_blank())+
  theme( axis.text  = element_text(size=14,colour = 'black'), 
         axis.title = element_text(size = 16),
         plot.title = element_text(face = 'bold',size = 20),
         legend.title = element_text(size = 19),
         legend.text = element_text(size=17))
map.air.vs

## merge
pdf('Figures/Maps_AirTravel_official_vs_OAG_world.pdf', width=10, height=5, onefile = T)
multiplot(map.air, map.air.oag, map.air.vs,
          layout = matrix(c(1,3,
                            2,3), nrow = 2, ncol = 2, byrow = T))
dev.off()
