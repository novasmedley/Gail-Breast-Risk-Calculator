source(GailRiskCalculator) # if R script is in another location, replace with path to script
library(tools)

#### helper functions, optional
## assuming we have different files for different data obtained in different years 
## (simple method would just require reading in one file containing all data, without missing information)
getCurrentAge <- function (DOB, currentYear) {
  DOB$dob <- lapply(DOB$dob, function(x) as.numeric(x <- substring(x,0,4))) # assumes date in YYYY-MM-DD format
  DOB$dob <- lapply(DOB$dob, function(x) 2015 - x)
  DOB$dob <- unlist(DOB$dob)
  colnames(DOB) <- c('id','age')
  return (DOB)
}
getData <- function (currentAge,status,menarcheFile,biopsyFile,relativesFile,FLBfile,densityFile,raceFile,statusFile) {
  menarcheAge <-    read.table(menarcheFile, header=TRUE,sep=",",stringsAsFactors=FALSE)
  numBiopsy <-      read.table(biopsyFile, header=TRUE,sep=",",stringsAsFactors=FALSE)
  numRelative <-    read.table(relativesFile, header=TRUE,sep=",",stringsAsFactors=FALSE)
  firstLiveBirth <- read.table(FLBfile, header=TRUE,sep=",",stringsAsFactors=FALSE)
  breastDensity <-  read.table(densityFile, header=TRUE,sep=",",stringsAsFactors=FALSE)
  race <-           read.table(raceFile, header=T, sep=",", stringsAsFactors=F)
  status <-         read.table(statusFile, header=FALSE,sep="\t",stringsAsFactors=FALSE) ## status file is tsv and without header
                               
  colnames(firstLiveBirth) <- c('id','FBLage')
  colnames(menarcheAge) <-    c('id','menarcheAge')
  colnames(numBiopsy) <-      c('id','numBiopsy')
  colnames(numRelative) <-    c('id','numRelative')
  colnames(race) <-           c('id','race')
  colnames(status) <-         c('id','status')
  
  gailData <- merge(currentAge,firstLiveBirth,by="id") # combine patients with data across the different risk factor considerations
  gailData <- merge(gailData,menarcheAge, by='id')
  gailData <- merge(gailData,numBiopsy, by='id')
  gailData <- merge(gailData,numRelative, by='id')
  gailData <- merge(gailData,status, by='id')
 
  return(gailDate)
}


## dummy code to use functions: if data is all in one file
## set file path
dataFile <- ""
## do risk calculations
gailData <- read.table(dataFile, header=TRUE,sep=",",stringsAsFactors=FALSE)
colnames(gailData) <- c("age","laterAge","menarcheAge","numBiopsy","FLBage","numRelative","status") # assumes data is in this format in dataFile
absoluteRisk5year <- mapply(GetProbability, gailData$age,later5year,gailData$menarcheAge,gailData$numBiopsy,
                            gailData$FBLage,gailData$numRelative)

## plotting the data
xline <- seq(1:nrow(absoluteRisk5year))
plot(xline,absoluteRisk5year,xlab="women",ylab="Absolute Risk of Breast Cancer within 5 years",
     type="l",lwd=.5,lty=1,col="blue",pch=0)

## some descriptors
nBC <- length(which(gailData$status=="0"))
nNoBC <- length(which(gailData$status=="1"))

