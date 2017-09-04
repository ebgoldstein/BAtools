# Crossref API documents: https://github.com/CrossRef/rest-api-doc
# rOpenSci rcrossref tutorial: https://ropensci.org/tutorials/rcrossref_tutorial.html

library(rcrossref)
library(plyr)
library(rvest)


#get all the dois from CrossRef for ESURFD, which has an eISSN of 2196-6338
ESURFdata <-cr_works(filter = c(issn = "2196-6338"),limit = 1000,cursor='*')
#pull out the dataframe
ESURFDF<-ESURFdata$data
save(ESURFDF,file="ESURFDF.Rda")
#load("~/ESURFDF.Rda")

ESURFDF["C1_first"] <- NA
ESURFDF["C2"] <- NA
ESURFDF["C3"] <- NA
ESURFDF["C4"] <- NA
ESURFDF["C5"] <- NA
ESURFDF["C6"] <- NA
ESURFDF["C7"] <- NA
ESURFDF["C8"] <- NA
ESURFDF["C9"] <- NA
ESURFDF["C10"] <- NA
ESURFDF["C11"] <- NA
ESURFDF["C12"] <- NA
ESURFDF["C13"] <- NA
ESURFDF["C14"] <- NA
ESURFDF["C15"] <- NA
ESURFDF["C16"] <- NA
ESURFDF["C17"] <- NA
ESURFDF["C18"] <- NA
ESURFDF["C19"] <- NA
ESURFDF["C20"] <- NA
ESURFDF["C21"] <- NA

IndexOne<-grep("C1_first", colnames(ESURFDF))

#loop through and do the scraping
for(i in c(1:nrow(ESURFDF))){
  ESURFurl<- read_html(toString(ESURFDF$URL[i]))
  A<- ESURFurl %>%
  html_nodes("a") %>%
  html_attr("href")
  
  #skip if it is still in review (and scraping breaks)
  if (length(A)==1){
  reESURFurl <- read_html(A)
  B <-reESURFurl %>%
    html_nodes(".edt_discuss") %>%
    html_text()
  
  
  #put them in the matrix
  if (length(B)>0) {
    ESURFDF[i,IndexOne:(IndexOne+length(B)-1)] <- B
  }
}
}

save(ESURFDF,file="ESURFDF.Rda")

myfun <- function(x) grepl("Anonymous Referee #1", x)
TRUEanon1 <- colwise(myfun)(ESURFDF)

myfun2 <- function(x) grepl("Anonymous Referee #2", x)
TRUEanon2 <- colwise(myfun2)(ESURFDF)

#only 2 anon reviews calculated. exclude when R3

myfun3 <- function(x) grepl("RC3", x)
myfun4 <- function(x) grepl("Anonymous Referee #3", x)
TRUE3a <- colwise(myfun3)(ESURFDF)
TRUE3b <- colwise(myfun4)(ESURFDF)
TRUE3 <- TRUEanon3a+ TRUEanon3b


Rev1Anon <- rowSums(TRUEanon1)
Rev2Anon <- rowSums(TRUEanon2)
Rev3 <- rowSums(TRUE3)

NoReview <- is.na(ESURFDF$C1_first) # TRUE means no reviews!

ESURFRevDATA<- data.frame(Rev1Anon,Rev2Anon, NoReview,Rev3)
# so need to find rows with papers with reviews, where "NoReview" is false.
# then rows with RevAnon1>1 means rev1 was anon
# then rows with RevAnon2>1 means rev2 was anon
# then rows with Rev3>1 means rev3 occured
numdata<-nrow(subset(ESURFRevDATA,NoReview==FALSE))
bothanon<-nrow(subset(ESURFRevDATA, Rev1Anon>=1 & Rev2Anon>=1 & NoReview==FALSE & Rev3==0))
R1anon<-nrow(subset(ESURFRevDATA, Rev1Anon>=1 & Rev2Anon==0 & NoReview==FALSE & Rev3==0))
R2anon<-nrow(subset(ESURFRevDATA, Rev1Anon==0 & Rev2Anon>=1 & NoReview==FALSE & Rev3==0))
bothnamed<-nrow(subset(ESURFRevDATA, Rev1Anon==0 & Rev2Anon==0 & NoReview==FALSE & Rev3==0))

#matrix view of results
Performance <-
  matrix(c(bothanon, R1anon, R2anon, bothnamed),
         nrow = 2,
         dimnames = list("Reviewer 2" = c("Unsigned", "Signed"),
                         "Reviewer 1" = c("Unsigned", "Signed")))
Performance


