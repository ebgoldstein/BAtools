library(tidyverse)
library(RCurl)
library(gdata)
library(reshape2)

url <- "http://www.nature.com/ngeo/journal/v10/n9/extref/ngeo3026-s1.xlsx"
NGEO <- read.xls(url,skip=3)

colnames(NGEO)[1] <- "Suggest" #Suggest referees? Y/N
colnames(NGEO)[2] <- "CAuthor" #corresponding author gender
colnames(NGEO)[3] <- "SuggRevM" #how many suggested referees are male
colnames(NGEO)[4] <- "SuggRevF" #how many suggested referees are female
colnames(NGEO)[5] <- "SuggRevA" #how many suggested referees are unknown
colnames(NGEO)[6] <- "CorrespondNA" #Corresponding author geography
colnames(NGEO)[7] <- "CorrespondSCA"  #Corresponding author geography
colnames(NGEO)[8] <- "CorrespondEME"  #Corresponding author geography
colnames(NGEO)[9] <- "CorrespondAf"  #Corresponding author geography
colnames(NGEO)[10] <- "CorrespondAs"   #Corresponding author geography
colnames(NGEO)[11] <- "CorrespondAusNZ"   #Corresponding author geography
colnames(NGEO)[12] <- "North Am. Rec"   #Recommeneded referree geography
colnames(NGEO)[13] <- "South+Central Am. Rec" #Recommeneded referree geography
colnames(NGEO)[14] <- "Europe + Middle East Rec" #Recommeneded referree geography
colnames(NGEO)[15] <- "Africa Rec" #Recommeneded referree geography
colnames(NGEO)[16] <- "Asia Rec" #Recommeneded referree geography
colnames(NGEO)[17] <- "Australia + NZ Rec" #Recommeneded referree geography
colnames(NGEO)[18] <- "ActualReqM" #Actual referee, suggested, male
colnames(NGEO)[19] <- "ActualReqF" #Actual referee, suggested, female
colnames(NGEO)[20] <- "ActualAssignM"   #Actual referee, assigned, male
colnames(NGEO)[21] <- "ActualAssignF" #Actual referee, assigned, female

NGEO$CorrespondNA[NGEO$CorrespondNA == 1] <- "North America"
NGEO$CorrespondSCA[NGEO$CorrespondSCA == 1] <- "South+Central America"
NGEO$CorrespondEME[NGEO$CorrespondEME == 1] <- "Europe+Middle East"
NGEO$CorrespondAf[NGEO$CorrespondAf == 1] <- "Africa"
NGEO$CorrespondAs[NGEO$CorrespondAs == 1] <- "Asia"
NGEO$CorrespondAusNZ[NGEO$CorrespondAusNZ == 1] <- "Australia + NZ"

NGEO$CorrespondNA[NGEO$CorrespondNA == 0] <- ""
NGEO$CorrespondSCA[NGEO$CorrespondSCA == 0 ] <- ""
NGEO$CorrespondEME[NGEO$CorrespondEME == 0] <- ""
NGEO$CorrespondAf[NGEO$CorrespondAf == 0] <- ""
NGEO$CorrespondAs[NGEO$CorrespondAs == 0] <- ""
NGEO$CorrespondAusNZ[NGEO$CorrespondAusNZ == 0] <- ""

NGEO<-unite(NGEO,CorrespondNA,CorrespondSCA,CorrespondEME,CorrespondAf,CorrespondAs,CorrespondAusNZ,col="CorrAuth",sep="")

#total suggested reviewers.
NGEO <- mutate(NGEO,Total_Suggested=SuggRevM+SuggRevF+SuggRevA)

#replace all NAs with 0s
NGEO[is.na(NGEO)] <- 0

#change 'a' to 'u'
NGEO$CAuthor <- as.character(NGEO$CAuthor)
NGEO$CAuthor[NGEO$CAuthor == "a"] <- "u"
NGEO$CAuthor <- as.factor(NGEO$CAuthor)

# # of men and women authors from different geography
ggplot(data=NGEO) + geom_bar(mapping = aes(x =CAuthor)) + 
  facet_wrap(~CorrAuth) + xlab('Corresponding Author') + 
  ggtitle('Gender of Corresponding Author, by Geography', subtitle = NULL)

North <- colSums(NGEO[NGEO$CorrAuth=='North America',7:12])
Cent <- colSums(NGEO[NGEO$CorrAuth=='South+Central America',7:12])
EME <- colSums(NGEO[NGEO$CorrAuth=='Europe+Middle East',7:12])
AF <- colSums(NGEO[NGEO$CorrAuth=='Africa',7:12])
AS <- colSums(NGEO[NGEO$CorrAuth=='Asia',7:12])
NZ <- colSums(NGEO[NGEO$CorrAuth=='Australia + NZ',7:12])

refsuggs <- bind_rows(North,Cent,EME,AF,AS,NZ)
CorrAuthGeog <- c("North America","South+Central America","Europe+Middle East","Africa","Asia","Australia + NZ")
CorrAuthGeog <- data.frame(CorrAuthGeog)
refsuggs <- bind_cols(CorrAuthGeog,refsuggs)

NGEO.m <- melt(refsuggs,id.vars="CorrAuthGeog")

#the bar plot of # of corresponding authors that recommended reviewers by geography
ggplot(data =subset(NGEO,Suggest=="Y")) + geom_bar(mapping = aes(x = CorrAuth)) + xlab('Corresponding Authors (who suggested reviewers) by Geography')

# Suggested reviewer geography by submitting author geography
ggplot(NGEO.m,aes(x = CorrAuthGeog,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "fill") + xlab('Corresponding Author Geography') +ylab('Proportion')
