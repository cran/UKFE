#Data update version 13.0.2

library(UKFE)
library(xml2)

OrigWD <- getwd()

#

WD <- "D:/NRFAPeakFlow_v13-0-2/"
setwd(WD)
PoolingWD <- paste(WD, "suitable-for-pooling/", sep = "")
QMEDwd <- paste(WD, "suitable-for-both/", sep = "")

AMFiles <- list.files(path = PoolingWD, pattern = ".am")
setwd(PoolingWD)
AMs <- list()
for(i in 1:length(AMFiles)) {AMs[[i]] <- AMImport(AMFiles[i])}
AMFilesSplit <- strsplit(AMFiles, split = ".am")
id <- NULL
for(i in 1:length(AMs)) {id[i] <- as.character(as.numeric(AMFilesSplit[[i]]))}
#AMs[[1]] <- data.frame(AMs[[1]], rep(id[1], nrow(AMs[[1]])))
for(i in 1:length(AMs)) {AMs[[i]] <- data.frame(AMs[[i]], rep(id[i], nrow(AMs[[i]])))}
for(i in 1:length(AMs)) {colnames(AMs[[i]])[3] <- "id"}
AMSPComp <- rbind(AMs[[1]], AMs[[2]])
for(i in 3:length(AMs)) {AMSPComp <- rbind(AMSPComp, AMs[[i]])}
LStats <- rbind(Lmoms(AMs[[1]][,2])[c(5, 6, 7, 1, 2)], Lmoms(AMs[[2]][,2])[c(5, 6, 7, 1, 2)])
for(i in 3:length(AMs)) {LStats <- rbind(LStats, Lmoms(AMs[[i]][,2])[c(5, 6, 7, 1, 2)])}

setwd(QMEDwd)
XMLFiles <- list.files(path = QMEDwd, pattern = ".xml")
XMLFilesSplit <- strsplit(XMLFiles, split = ".xml")
RwNames <- NULL
for(i in 1:length(XMLFiles)) {RwNames[i] <- as.character(as.numeric(XMLFilesSplit[[i]]))}
CDsList <- list()
for(i in 1:length(XMLFiles)) {CDsList[[i]] <- CDsXML(XMLFiles[i])}
CDsDF <- rbind(CDsList[[1]][,2], CDsList[[2]][,2])
for(i in 3:length(XMLFiles)) {CDsDF <- rbind(CDsDF, CDsList[[i]][,2])}
colnames(CDsDF) <- CDsList[[1]][,1]
rownames(CDsDF) <- RwNames
CDsDF <- as.data.frame(CDsDF)
AMqmedFiles <- list.files(pattern = ".am")
QMEDs <- NULL
for(i in 1:length(AMqmedFiles)) {QMEDs[i] <- median(AMImport(AMqmedFiles[i])[,2])}
QMEDcd <- NULL
for(i in 1:length(CDsList)) {QMEDcd[i] <- QMED(CDsList[[i]])}
QMEDfse <- NULL
for(i in 1:length(AMqmedFiles)) {QMEDfse[i] <- QMEDfseSS(AMImport(AMqmedFiles[i])[,2])}
N <- NULL
for(i in 1:length(AMqmedFiles)) {N[i] <- nrow(AMImport(AMqmedFiles[i]))}
CDsDF <- cbind(CDsDF, QMEDs, QMEDcd, QMEDfse, N)
colnames(CDsDF)[which(colnames(CDsDF) == "QMEDs")] <- "QMED"

MatchSS <- match(id, rownames(CDsDF))
SuitsPool <- CDsDF[MatchSS,]
SuitsPool <- cbind(SuitsPool, LStats)
SPTrunc <- SuitsPool[,c(seq(1,20), 23, 27, 28, 29, 30, 31, 26)]
colnames(SPTrunc)[21] <- "QMED"

#Read in NI grid Refs
IreInd <- which(as.numeric(rownames(SPTrunc)) > 200000)
NIRefs <- read.csv("C:/Users/anthonyhammond/OneDrive - JBA Group/UKFE/NISiteGridRefs.csv", colClasses = c("character", "numeric", "numeric"))
MatchIDs <- match(rownames(SPTrunc)[IreInd], NIRefs$ID)
NIUse <- NIRefs[MatchIDs,]
SPTrunc[IreInd,19:20] <- NIUse[,2:3]


NRFAData <- SPTrunc

#this is to adjust site 44003 which is identical to 44011 - which causes problems
Ind44003 <- which(rownames(NRFAData) == 44003)
NRFAData$AREA[Ind44003] <- 49.1
NRFAData$Easting[Ind44003] <- NRFAData$Easting[Ind44003]+1

save(NRFAData, file = "C:/Users/anthonyhammond/OneDrive - JBA Group/Documents/UKFE/data/NRFAData.rda")

QMEDDF <- CDsDF[,c(seq(1,18), 23, 24, 19, 20, 25, 26, 21, 22)]
colnames(QMEDDF)[21:22] <- c("X", "Y")
colnames(QMEDData)[19] <- "QMED"

IreInd <- which(as.numeric(rownames(QMEDDF)) > 200000)
MatchIDs <- match(rownames(QMEDDF)[IreInd], NIRefs$ID)
NIUse <- NIRefs[MatchIDs,]
QMEDDF[IreInd,21:22] <- NIUse[,2:3]

QMEDData <- QMEDDF

#this is to adjust site 44003 which is identical to 44011 - which causes problems
Ind44003 <- which(rownames(QMEDData) == 44003)
QMEDData$AREA[Ind44003] <- 49.1
QMEDData$X[Ind44003] <- QMEDData$X[Ind44003]+1

save(QMEDData, file = "C:/Users/anthonyhammond/OneDrive - JBA Group/Documents/UKFE/data/QMEDData.rda")

AMSP <- AMSPComp
save(AMSP, file = "C:/Users/anthonyhammond/OneDrive - JBA Group/Documents/UKFE/data/AMSP.rda")

setwd(OrigWD)

