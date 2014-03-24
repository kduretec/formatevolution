applyRules <- function(data, rules) {
  for (i in 1:nrow(rules)) {
    row <- rules[i,]
    if (nrow(data[data$MIME==row$MIME1 & data$VERSION==row$VERSION1 & !is.na(data$VERSION) & !is.na(data$MIME),])>0) {
      data[data$MIME==row$MIME1 & data$VERSION==row$VERSION1 & !is.na(data$VERSION) & !is.na(data$MIME),]$MIME <- row$MIME2
      data[data$MIME==row$MIME2 & data$VERSION==row$VERSION1 & !is.na(data$VERSION) & !is.na(data$MIME),]$VERSION <- row$VERSION2
    }
  }
  data
}

loadCollectionProfiles <- function(start, end, dataset, rules) {
  #names <- paste(years, dataset, sep="")
  
  data <- data.frame(YEAR=character(), MIME=character(), VERSION=character(), COUNT=character())
  
  for (i in start:end) {
    d <- read.table(paste("data/",i,dataset,sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE)
    d <- applyRules(d,rules)
    d <- aggregate(COUNT~MIME+VERSION, data=d , FUN=sum)
    d["YEAR"] <- i 
    data <- rbind(data,d)
  }
  data
}


plotSeparate <- function(data, toPlot) {
  #reg <- list()
  Yplot <- seq(0,20,length=100)
  reg <- data.frame(MIME=character(), PREDICTIONS=vector());
  for (i in 1:nrow(toPlot)) {
    row <- toPlot[i,]
    mime <- row$MIME
    p <- merge(data,row)
    if (nrow(p)>0){
      plot(p$YEAR, p$PERCENTAGE, xlab="", ylab="", main=paste(row$MIME,row$VERSION), pch=19)
      plot(p$AGE, p$PERCENTAGE, xlab="", ylab="", main=paste(row$MIME,row$VERSION), pch=19)
      
      r <- lm(PERCENTAGE~AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + I(AGE^5) + I(AGE^6), data=p)
      reg <- rbind(reg, data.frame(MIME=mime,PREDICITONS=predict(r, newdata=data.frame(AGE=Yplot))))
      #plot(p$YEAR, p$PERCENTAGE, main=paste(row$MIME,row$VERSION), pch=19, ann=FALSE)
      #plot(p$AGE, p$PERCENTAGE, main=paste(row$MIME,row$VERSION), pch=19, ann=FALSE)
    }
  }
  print(reg)
  reg
}



plotReg <- function(reg) {
  Yplot <- seq(0,20,length=100)
  for (i in 1:length(reg)) {
    #row <- reg[i,]
    lines(Yplot,predict(reg[[i]], newdata=data.frame(AGE=Yplot)),col="blue")
  }
}

clean <- function(releases,data) {
  inc <- rep(FALSE,nrow(data))
  #remove those which appear before release year 
  for (i in 1:nrow(data)) {
    row <- data[i,]
    mime <- row$MIME
    yt <- row$YEAR
    vt <- row$VERSION
    ry <- releases[releases$MEDIATYPE==mime & releases$VERSION==vt,]$YEAR
    if (length(ry)>0) {
      if (yt>=ry) {
        inc[i] <- TRUE
      }
    }
  }
  data <- data[inc,]
}


calcPerc <- function(releases, data, aggr) {
  perc <- data.frame(MIME=character(), VERSION=character(), PERCENTAGE=character(), YEAR=character(), RELEASE=character(), AGE=character(), stringsAsFactors=FALSE)
  
  for (i in 1:nrow(data)) {
    row <- data[i,]
    mime <- row$MIME
    year <- row$YEAR
    version <- row$VERSION
    count <- row$COUNT
    releaseYear <- releases[releases$MIME==mime & releases$VERSION==version,]$YEAR
    if (length(releaseYear)>0) {
      total <- aggr[aggr$YEAR==year,]$COUNT
      percentage <- count/total
      diff = year - releaseYear
      if ( diff >= 0 ) {
        perc <- rbind(perc, data.frame(MIME=mime, VERSION=version, PERCENTAGE=percentage, YEAR=year, RELEASE=releaseYear, AGE=diff))
      }
    }
  }
  perc
}


process <- function(year.versions,year, title) {
  #year.versions <- read.table(file1, header=TRUE, sep="\t", stringsAsFactors=FALSE)
  #dk.year.versions <- read.table(file2, header=TRUE, sep="\t", stringsAsFactors=FALSE)
  releases <- read.table("data/release_years.txt", header=TRUE, sep="\t")

  #exclude NA and take only years after year
  #t <- year.versions[!is.na(uk.year.versions$VERSION),]
  t <- year.versions[!is.na(year.versions$VERSION) & year.versions$YEAR > year,]
  t <- clean(releases,t)
  
  #tDk <- dk.year.versions[!is.na(dk.year.versions$VERSION),]
  #tDk <- clean(releases,tDk,mime)
  
  #aggregate values by years 
  aggr <- aggregate(COUNT~YEAR, FUN=sum, data=t)
  #aggrDk <- aggregate(COUNT~YEAR, FUN=sum, data=tDk)
  
  list <- calcPerc(releases, t, aggr)
  #listDk <- calcPerc(releases, tDk, aggrDk, mime)
  print(list$perc)
  agregPerc <- aggregate(P~Y, FUN=mean, data=list$perc)
  print(agregPerc)
  agregMax <- aggregate(P~YR, data=list$perc, FUN=max)
  agregMaxMer <- merge(list$perc, agregMax)
  print(agregMaxMer)
  #agregPercDk <- aggregate(P~Y, FUN=mean, data=listDk$perc)
  
  regres <- lm(P~Y + I(Y^2) + I(Y^3) + I(Y^4) + I(Y^5) + I(Y^6), data=agregPerc)
  #regresDk <- lm(P~Y + I(Y^2) + I(Y^3) + I(Y^4) + I(Y^5) + I(Y^6), data=agregPercDk)
  
  Yplot <- seq(0,30,length=100)
  
  
  #plots
  plot(agregMaxMer$YR, agregMaxMer$Y, xlab="years", ylab="years after release", main="agregmax")
  plot(list$perc$Y,list$perc$P, xlab="years after release", ylab="percentage", main=title)
  plot(agregPerc$Y, agregPerc$P, xlab="years after release", ylab="percentage", main=paste(title, " (mean)"))
  lines(Yplot,predict(regres, newdata=data.frame(Y=Yplot)),col="blue")
  list$perc
  #lines(Yplot,predict(regresUk, newdata=data.frame(Y=Yplot)),col="blue")
  #plot(listDk$perc$Y,listDk$perc$P, xlab="years after release", ylab="percentage", main=paste(mime,"in DK data set"))
  #plot(agregPercDk$Y, agregPercDk$P, xlab="years after release", ylab="percentage", main=paste(mime,"in DK data set (mean)"))
  #lines(Yplot,predict(regresDk, newdata=data.frame(Y=Yplot)),col="blue")
  
  
  
  
  # library(lattice)
  # print(xyplot(percYears$P~percYears$Y, pch=19, groups=percYears$V, auto.key=TRUE, cex=1.5))
  # 
  # 
  # library(ggplot2)
  # print(ggplot(perc, aes(x=YR, y=P, group=V,fill=V)) + geom_area(position="stack"))
  # 
   #bubble plot 
#    radius <- sqrt(list$perc$P/pi)
#    col <- match(list$perc$V,releases$VERSION)
#    symbols(list$perc$YR, list$perc$Y, circles=radius, inches=0.35, fg="white", bg="red", xlab="Year", ylab="Release")
}