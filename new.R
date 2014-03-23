source("utils.R")

config <- read.table("config.txt", header=FALSE, sep="\t", stringsAsFactors=FALSE)
colnames(config) <- c ("PARAMETER", "VALUE")
releases <- read.table("release_years.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

#load formats and versions to process 
toProcess <- read.table(config[config$PARAMETER=="PROCESS",]$VALUE, header=TRUE, sep="\t", stringsAsFactors=FALSE)
rules <- read.table(config[config$PARAMETER=="CONSOLIDATE",]$VALUE, header=TRUE, sep="\t", stringsAsFactors=FALSE)

data <- loadCollectionProfiles(config[config$PARAMETER=="START",]$VALUE, config[config$PARAMETER=="END",]$VALUE, config[config$PARAMETER=="COLLECTION",]$VALUE, rules)

#filter and aggregate 
 if (config[config$PARAMETER=="INCLUDE",]$VALUE=="not all") {
   data <- merge(data,toProcess, by=c("MIME","VERSION")) 
   aggregData <- aggregate(COUNT~YEAR, FUN=sum, data=data)
 } else {
   aggregData <- aggregate(COUNT~YEAR, FUN=sum, data=data)
   data <- merge(data,toProcess, by=c("MIME","VERSION")) 
 }


results <- calcPerc(releases, data, aggregData)

#aggregate percentages 
aggregPerc <- aggregate(PERCENTAGE~AGE, data=results, FUN=mean)

#plot each version 
plotSeparate(results, toProcess)

#plot everything together
plot(results$AGE, results$PERCENTAGE, xlab="age", ylab="percentage", pch=19)
#plot average
plot(aggregPerc$AGE,aggregPerc$PERCENTAGE, xlab="age", ylab="percentage", main="average", pch=19)


# when was the format version at peak after the release
peak <- aggregate(PERCENTAGE~MIME+VERSION, data=results, FUN=max)
peak <- merge(peak,results)
plot(peak$RELEASE, peak$AGE, xlab="release year", ylab="peak", pch=19)
