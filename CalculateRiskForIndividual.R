args <- commandArgs(trailingOnly=TRUE);

riskFactorsLocation = args[1]
breastDensityLocation = args[2]
baselineCodeLocation = args[3]
outputDir = args[4]

source(baselineCodeLocation)
library(tools)

#### GET DATA FROM FILES
gailData <-read.table(file_path_as_absolute(riskFactorsLocation), header=TRUE,sep=",",stringsAsFactors=FALSE)
densityData <- read.table(file_path_as_absolute(breastDensityLocation), header=TRUE,sep=",",stringsAsFactors=FALSE)
colnames(gailData) <- c("id","visitType","visitDate.","currentAge","menarcheAge","firstLiveBirthAge","race",
                        "numBiopsy","1stDegreeRelatives","BCever","5yearScore","5yearScoreAvg","10yearScore","
                        10yearScoreAvg", "90yearScore","90yearScoreAvg")
colnames(densityData) <- c("id","densityBirads","daysDiff","sampleAge","mammoAge",
                           "visitDate","createDate","datediff.visitDate.createDate.")

completeDensityData <- densityData[densityData$densityBirads != "",]


#### REMOVE OVERLAPS
data <- merge(gailData,completeDensityData, by.x='id', by.y='id' )

# CONVERT DATA INTO CATEGORICAL DATA, AS DATA.FRAMES TO VISUALIZE DATA
ageCat <- do.call(rbind.data.frame,lapply(data$currentAge, getAgeCategory))
ageMen <- do.call(rbind.data.frame,lapply(data$menarcheAge, getAgeMenCategory))
ageFLB <- do.call(rbind.data.frame,lapply(data$firstLiveBirthAge, getAgeFBLCaetgory))
nBiops <- do.call(rbind.data.frame,lapply(data$numBiopsy, getNBiopsCategory))
nRelat <- do.call(rbind.data.frame,lapply(data$1stDegreeRelatives getNumRelCategory))
# TODO get  race <- do.call(rbind.data.frame,lapply(data$C_races, ))

#### CONVERT INTO FACTORS
ageCat[,1] <- factor(ageCat[,1])
ageMen[,1] <- factor(ageMen[,1])
ageFLB[,1] <- factor(ageFLB[,1])
nBiops[,1] <- factor(nBiops[,1])
nRelat[,1] <- factor(nRelat[,1])
# TODO convert race
breastCancerStatus <- factor(data$BCever)
breastDensity <- factor(data$densityBirads)

#### GET SOME DESCRIPTORS
nBC <- length(which(breastCancerStatus=="0"))
nNoBC <- length(which(breastCancerStatus=="1"))

#### logit 
logitData <- data.frame(breastCancerStatus,ageCat,ageMen,ageFLB,nBiops,nRelat,breastDensity)
colnames(logitData) <- c("breastCancerStatus","ageCat","ageMen","ageFLB","nBiops","nRelat","breastDensity")

nothing <- glm(breastCancerStatus ~ 1,family=binomial)
densityLogit <- glm(breastCancerStatus ~ ageCat + ageMen + ageFLB + nBiops + nRelat + breastDensity, data=logitData,family="binomial")
# forwards = step(nothing, scope=list(lower=formula(nothing),upper=formula(densityLogit)), direction="forward")


