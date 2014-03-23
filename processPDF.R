uk.year.versions <- read.table("data/application-pdf-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
dk.year.versions <- read.table("data/application-pdf-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
releases <- read.table("data/release_years.txt", header=TRUE, sep="\t")
mime <- "application/pdf"
source("utils.R")
#exclude NA
#tUk <- uk.year.versions[!is.na(uk.year.versions$VERSION),]
tUk <- uk.year.versions[!is.na(uk.year.versions$VERSION) & uk.year.versions$YEAR > 2004,]
tUk <- clean(releases,tUk,mime)

tDk <- dk.year.versions[!is.na(dk.year.versions$VERSION),]
tDk <- clean(releases,tDk,mime)

#aggregate values by years 
aggrUk <- aggregate(COUNT~YEAR, FUN=sum, data=tUk)
aggrDk <- aggregate(COUNT~YEAR, FUN=sum, data=tDk)

listUk <- calcPerc(releases, tUk, aggrUk, mime)
listDk <- calcPerc(releases, tDk, aggrDk, mime)

agregPercUk <- aggregate(P~Y, FUN=mean, data=listUk$perc)
agregPercDk <- aggregate(P~Y, FUN=mean, data=listDk$perc)

regresUk <- lm(P~Y + I(Y^2) + I(Y^3) + I(Y^4) + I(Y^5) + I(Y^6), data=agregPercUk)
regresDk <- lm(P~Y + I(Y^2) + I(Y^3) + I(Y^4) + I(Y^5) + I(Y^6), data=agregPercDk)

Yplot <- seq(0,20,length=100)


#plots
plot(listUk$perc$Y,listUk$perc$P, xlab="years after release", ylab="percentage", main="PDF in UK data set")
plot(agregPercUk$Y, agregPercUk$P, xlab="years after release", ylab="percentage", main="PDF in UK data set (mean)")
lines(Yplot,predict(regresUk, newdata=data.frame(Y=Yplot)),col="blue")
plot(listDk$perc$Y,listDk$perc$P, xlab="years after release", ylab="percentage", main="PDF in DK data set")
plot(agregPercDk$Y, agregPercDk$P, xlab="years after release", ylab="percentage", main="PDF in DK data set (mean)")
lines(Yplot,predict(regresDk, newdata=data.frame(Y=Yplot)),col="blue")




# library(lattice)
# print(xyplot(percYears$P~percYears$Y, pch=19, groups=percYears$V, auto.key=TRUE, cex=1.5))
# 
# 
# library(ggplot2)
# print(ggplot(perc, aes(x=YR, y=P, group=V,fill=V)) + geom_area(position="stack"))
# 
# #bubble plot 
# radius <- sqrt(perc$P/pi)
# col <- match(perc$V,releases$VERSION)
# symbols(perc$YR, perc$Y, circles=radius, inches=0.35, fg="white", bg=col, xlab="Year", ylab="Release")