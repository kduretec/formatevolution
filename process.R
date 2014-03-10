year.versions <- read.table("../application-pdf-sb", header=TRUE, sep="\t", stringsAsFactors=FALSE)
releases <- read.table("../release_years.txt", header=TRUE, sep="\t")

#exclude NA
t <- year.versions[!is.na(year.versions$VERSION),]
#t <- year.versions[!is.na(year.versions$VERSION) & year.versions$YEAR > 2004,]

#aggregate values by years 
aggr <- aggregate(COUNT~YEAR, FUN=sum, data=t)

perc <- data.frame(Y=character(), P=character(), stringsAsFactors=FALSE)
for (i in 1:nrow(t)) {
  row <- t[i,]
  year <- row$YEAR
  version <- row$VERSION
  count <- row$COUNT
  releaseYear <- releases[releases$MEDIATYPE=="application/pdf" & releases$VERSION==version,]$YEAR
  if (length(releaseYear)>0) {
    total <- aggr[aggr$YEAR==year,]$COUNT
    percentage <- count/total
    diff = year - releaseYear
    if ( diff > 0 ) {
      perc <- rbind(perc, data.frame(Y=diff, P=percentage))
    }
  }
}

agregPerc <- aggregate(P~Y, FUN=mean, data=perc)