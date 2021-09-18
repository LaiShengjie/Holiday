### Heatmap ####
# Image scale
image.scale <- function(z, zlim, col = col,
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}

# heatmap panel function
panel <- function(mat1, region, col, wk, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  x= (nrow(mat1)+1)/365
  abline(v=c(x*92, x*183, x*275), lty =2,)
 
  axis(1, at=c(1:nrow(mat1)), labels= as.factor(sort(wk)))
  region$order <- seq(0,1,,dim(region)[1])

  ## add North and South
  region$y <- round(region$y,5)
  max.y <- max(region$y)
  min.y <- min(region$y)
  region$order[! region$y %in% c(max.y, min.y)] <- NA
  region$adm[region$y == max.y] <- 'N'
  region$adm[region$y == min.y] <- 'S'
  axis(2, at=region$order, labels=region$adm, las=1, cex.axis=1.3)

  title(xlab='Month and week', cex.lab=1.2)
  mtext(paste(panel.name),side=3,line=0.1, adj=0, cex=1.3)
}

panel.world <- function(mat1, region, col, wk, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  x= (nrow(mat1)+1)/365
  abline(v=c(x*91, x*182, x*274), lty =2,)
  
  v=c((1-3*x), x*32, x*60, x*91, x*121, x*152, x*182, x*213, x*244, x*274, x*305, x*335, (53+3*x))
  axis(1, at=v, labels= c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D', 'J'))

  region$order <- seq(0,1,,dim(region)[1])
  
  ## add North and South
  region$y <- round(region$y,5)
  max.y <- max(region$y)
  min.y <- min(region$y)
  region$order[! region$y %in% c(max.y, min.y)] <- NA
  region$adm[region$y == max.y] <- 'N'
  region$adm[region$y == min.y] <- 'S'
  axis(2, at=region$order, labels=region$adm, las=1, cex.axis=1.3)
  
  title(xlab='Month and week', cex.lab=1.2)
  mtext(paste(panel.name),side=3,line=0.1, adj=0, cex=1.3)
}

# heatmap panel function - selected countries
panel.iso <- function(mat1, region, col, wk, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  x= (nrow(mat1)+1)/365
  abline(v=c(x*91, x*182, x*274), lty =2,)
  
  v=c((1-3*x), x*32, x*60, x*91, x*121, x*152, x*182, x*213, x*244, x*274, x*305, x*335, (53+3*x))
  axis(1, at=v, labels= c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D', 'J'))
  region$order <- seq(0,1,,dim(region)[1])
  
  # ## add North and South
  # region$y <- round(region$y,5)
  # max.y <- max(region$y)
  # min.y <- min(region$y)
  # region$order[! region$y %in% c(max.y, min.y)] <- NA
  # region$adm[region$y == max.y] <- 'N'
  # region$adm[region$y == min.y] <- 'S'
  axis(2, at=region$order, labels=region$adm, las=1, cex.axis=1.3)
  
  title(xlab='Month and week', cex.lab=1.2)
  mtext(paste(panel.name),side=3,line=0.1, adj=0, cex=1.3)
}

# heatmap panel function for monthly time series
panel.month <- function(mat1, region, col, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  v=seq(0.5, 108.5, 12)
  abline(v=v, lty =2,)
  
  axis(1, at=v, labels= as.factor(2010:2019))
  region$order <- seq(0,1,,dim(region)[1])
  
  ## add North and South
  region$y <- round(region$y,5)
  max.y <- max(region$y)
  min.y <- min(region$y)
  region$order[! region$y %in% c(max.y, min.y)] <- NA
  region$adm[region$y == max.y] <- 'N'
  region$adm[region$y == min.y] <- 'S'
  axis(2, at=region$order, labels=region$adm, las=1, cex.axis=1.3)
  
  title(xlab='Year and month', cex.lab=1.2)
  mtext(paste(panel.name),side=3,line=0.1, adj=0, cex=1.3)
}

# heatmap panel function for monthly air travel time series
panel.month.air <- function(mat1, region, col, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  v=seq(0.5, 96.5, 12)
  abline(v=v, lty =2,)
  
  axis(1, at=v, labels= as.factor(2010:2018))
  region$order <- seq(0,1,,dim(region)[1])
  
  ## add North and South
  region$y <- round(region$y,5)
  max.y <- max(region$y)
  min.y <- min(region$y)
  region$order[! region$y %in% c(max.y, min.y)] <- NA
  region$adm[region$y == max.y] <- 'N'
  region$adm[region$y == min.y] <- 'S'
  axis(2, at=region$order, labels=region$adm, las=1, cex.axis=1.3)
  
  title(xlab='Year and month', cex.lab=1.2)
  mtext(paste(panel.name),side=3,line=0.3, adj=0, cex=1.3)
}

# heatmap panel function for air travel - seasonality
panel.month.air.season <- function(mat1, region, col, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  
  axis(1, at=c(0.5, 3.5, 6.5, 9.5, 12.5), labels= c('J', 'A', 'J', 'O', 'J'))
  region$order <- seq(0,1,,dim(region)[1])
  
  ## add North and South
  region$y <- round(region$y,5)
  max.y <- max(region$y)
  min.y <- min(region$y)
  region$order[! region$y %in% c(max.y, min.y)] <- NA
  region$adm[region$y == max.y] <- 'N'
  region$adm[region$y == min.y] <- 'S'
  axis(2, at=region$order, labels=region$adm, las=1, cex.axis=1.3)
  
  title(xlab='Month', cex.lab=1.2)
  mtext(paste(panel.name),side=3,line=0.3, adj=0, cex=1.3)
}

# heatmap panel function for monthly air travel time series
panel.month.air.oag <- function(mat1, region, col, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  v=seq(0.5, 48.5, 12)
  abline(v=v, lty =2,)
  
  axis(1, at=v, labels= as.factor(2015:2019))
  region$order <- seq(0,1,,dim(region)[1])
  
  ## add North and South
  region$y <- round(region$y,5)
  max.y <- max(region$y)
  min.y <- min(region$y)
  region$order[! region$y %in% c(max.y, min.y)] <- NA
  region$adm[region$y == max.y] <- 'N'
  region$adm[region$y == min.y] <- 'S'
  axis(2, at=region$order, labels=region$adm, las=1, cex.axis=1.3)
  
  title(xlab='Year and month', cex.lab=1.2)
  mtext(paste(panel.name),side=3,line=0.3, adj=0, cex=1.3)
}


# heatmap panel function for holiday vs air travel
panel.month.air.hol <- function(mat1, col, panel.name){
  image(x=seq(1,nrow(mat1)), z=mat1, col=col, yaxt="n",xaxt="n",xlab="")
  box()
  
  axis(1, at=c(1:nrow(mat1)), labels= rownames(mat1))
  axis(2, at=seq(0,1, 1/(ncol(mat1)-1)), labels= colnames(mat1))
  
  title(xlab='Holiday (days)', ylab = 'Air travel (rank)', cex.lab=1.2)
  mtext(panel.name,side=3,line=0.3, adj=0, cex=1.3)
}

#### heatmap panel legends
panel.legend.wk1 <- function(mat1, col, breaks){
  image.scale(mat1, col=col, breaks=breaks, horiz=FALSE, yaxt="n", xaxt="n", xlab="", ylab="")
  axis(4,at = c(min(breaks)+0.5, max(breaks)-0.5), 
       labels = c('0 days', paste(max(breaks)-1, ' days', sep = '')), cex.axis=1.1, las=2)
  # axis(4,at = c(1,52),labels = c('High', 'Low'), cex.axis=1.1, las=2) # for ranking plot
    # mtext("Weekly outflow", side=4, line=2.3, cex=0.9)
  box()
}

panel.legend.wk2 <- function(mat1, col, breaks){
  image.scale(mat1, col=col, breaks=breaks, horiz=FALSE, yaxt="n", xaxt="n", xlab="", ylab="")
  axis(4,at = c(min(breaks)+0.5, max(breaks)-0.5), labels = c('NO', 'Yes'), cex.axis=1.1, las=2)
  # axis(4,at = c(1,52),labels = c('High', 'Low'), cex.axis=1.1, las=2) # for ranking plot
  # mtext("Weekly outflow", side=4, line=2.3, cex=0.9)
  box()
}

panel.legend.wk1.air <- function(mat1, col, breaks){
  image.scale(mat1, col=col, breaks=breaks, horiz=FALSE, yaxt="n", xaxt="n", xlab="", ylab="")
  axis(4,at = c(min(breaks)+0.5, max(breaks)-0.5), labels = c('1', '12'), cex.axis=1.1, las=2)
  # axis(4,at = c(1,52),labels = c('High', 'Low'), cex.axis=1.1, las=2) # for ranking plot
  # mtext("Weekly outflow", side=4, line=2.3, cex=0.9)
  box()
}

panel.legend.air.hol <- function(mat1, col, breaks){
  image.scale(mat1, col=col, breaks=breaks, horiz=FALSE, yaxt="n", xaxt="n", xlab="", ylab="")
  axis(4,at = c(min(breaks)+0.5, max(breaks)-0.5), 
       labels = c('0', '0.4'),
       cex.axis=1.1, las=2)
  box()
}
