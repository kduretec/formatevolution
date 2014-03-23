source("utils.R")
ukPDF <- read.table("data/application-pdf-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
process(ukPDF, 2004, "UK-PDF")

dkPDF <- read.table("data/application-pdf-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
process(dkPDF, 2004, "DK-PDF")
#process("data/image-x-ms-bmp-uk", "data/image-bmp-dk", "image/bmp")
#process("data/application-x-shockwave-flash-uk", "data/application-x-shockwave-flash-uk", "application/x-shockwave-flash")


ukHTML <- read.table("data/text-html-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
process(ukHTML, 1994, "UK-HTML")
dkHTML <- read.table("data/text-html-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
process(dkHTML, 2004, "DK-HTML")


gifUk <- read.table("data/image-gif-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
gifUk <- gifUk[!is.na(gifUk$VERSION),]
gifUk[gifUk$VERSION=="1987a",]$VERSION <- "87a"
gifUk[gifUk$VERSION=="1989a",]$VERSION <- "89a"
pngUk <- read.table("data/image-png-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
bmpUk <- read.table("data/image-x-ms-bmp-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
bmpUk$MIME <- "image/bmp"
jpegUk <- read.table("data/image-jpeg-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
tiffUk <- read.table("data/image-tiff-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)

allImagesUk <- rbind(gifUk,pngUk,bmpUk,jpegUk,tiffUk)
t <- process(allImagesUk,2004, "UK-IMAGES")


gifDk <- read.table("data/image-gif-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
gifDk <- gifDk[!is.na(gifDk$VERSION),]
print(gifDk)
#gifDk[gifDk$VERSION=="1987a",]$VERSION <- "87a"
gifDk[gifDk$VERSION=="1989a",]$VERSION <- "89a"
print(gifDk)
gifDk <- aggregate(COUNT~MIME+YEAR+VERSION, data=gifDk, FUN=sum)
print(gifDk)
pngDk <- read.table("data/image-png-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
bmpDk <- read.table("data/image-bmp-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
jpegDk <- read.table("data/image-jpeg-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
tiffDk <- read.table("data/image-tiff-dk", header=TRUE, sep="\t", stringsAsFactors=FALSE)

allImagesDk <- rbind(gifDk,pngDk,bmpDk,jpegDk,tiffDk)
t <- process(allImagesDk,2004, "DK-IMAGES")


#flashUk <- read.table("data/application-x-shockwave-flash-uk", header=TRUE, sep="\t", stringsAsFactors=FALSE)
#t <- process(flashUk,2004, "UK-FLASH")