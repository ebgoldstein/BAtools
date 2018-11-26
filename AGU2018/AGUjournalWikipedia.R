#This code:
#1) uses rcrossref to get DOI information for JGRs and GRL. GRL (ISSN 0094-8276).
#2) uses raltmetric to get Altmetric.com data for each DOI

#written by EBG 11/18

#load the needed libraries
library(rcrossref)
library(rAltmetric)
library(tidyverse)

#get all the dois from CrossRef for JGR, which has a print ISSN of 0148-0227 (until 2012)
#from 2013 onward, all 7 JGR sections have their own ISSN
#JGRA  2169-897X
#JGRB 2169-8953
#JGRES 2169-9003
#JGRO 2169-9275
#JGRSE 2169-9313
#JGRSP 2169-9380
#JGRP 2169-9097
# obtained from: https://www.wiley.com/en-us/search?pq=Journal%20of%20Geophysical%20Research%7Crelevance


JGRs <- c("2169-897X","2169-8953","2169-9003","2169-9275","2169-9313","2169-9380","2169-9097","0148-0227")
JGRsN <- c("JGRA","JGRB","JGRES","JGRO","JGRSE","JGRSP","JGRP","JGRcomb")


#loop through the separate sections
for (i in 1:7){
print(i)
print("DOIs")
JGRdata <-
  cr_works(
    filter = c(issn = JGRs[i],from_pub_date='1990-01-01'),
    limit = 1000,
    cursor = '*',
    cursor_max = 100000
  )
#pull out the dataframe
JGRDF <- JGRdata$data
#subset by title NA
JGR <- filter(JGRDF, !is.na(title))

print("Altmetrics")
#then run through rAltmetric to get the Altmetric.com metrics
doilist <- JGR$doi
#using the safe version from K. Ram: https://gist.github.com/karthik/78016fc78d52156561f5f543defb7ec0
safe_altmetrics <- purrr::safely(altmetrics, otherwise = NULL)
alm <- function(x)  safe_altmetrics(doi = x)
requests <- map(doilist, alm) 
# We map the result item from the inner list,
# remove the NULL ones
# then run the altmeric_data on the result objects
results <- requests %>%  
  map("result") %>% 
  compact(.) %>% 
  modify_depth(1, altmetric_data)

AltmetricData <- bind_rows(results) %>% select(doi, contains("cited"))

#join
JGR <- left_join(JGR, AltmetricData, by = "doi")

#rename the file and save it
nam <- paste(JGRsN[i])
save(JGR, file = paste(JGRsN[i],".Rda", sep = ""))

}

# load each sequentially and find year of issued data
getthewikidata <- function(x,y,z) {
  JGRsub <-
    mutate(x, year_issued = format(as.Date(x$created, format = "%Y-%m-%d"), "%Y"))
  #count numbers of articles in a given year
  # number of wiki mentions in a given year
  #put in new DF
  results <- JGRsub %>%
    group_by(year_issued) %>%
    summarise(ms_per_year = n(),
              W_mentions_per_year = sum(!is.na(cited_by_wikipedia_count >= 1)))
  results <- add_column(results, section = y)
  
  z <- rbind(z, results)
  return(z)
}

Journal <- data.frame(year_issued=as.Date(character()),
                 ms_per_year=integer(), 
                 W_mentions_per_year=integer(), 
                 section=character(),
                 stringsAsFactors=FALSE) 
load("JGRA.Rda")
Journal <- getthewikidata(JGR,"A",Journal)
load("JGRB.Rda")
Journal <- getthewikidata(JGR,"B",Journal)
load("JGRES.Rda")
Journal <- getthewikidata(JGR,"ES",Journal)
load("JGRO.Rda")
Journal <- getthewikidata(JGR,"O",Journal)
load("JGRP.Rda")
Journal <- getthewikidata(JGR,"P",Journal)
load("JGRSE.Rda")
Journal <- getthewikidata(JGR,"SE",Journal)
load("JGRSP.Rda")
Journal <- getthewikidata(JGR,"SP",Journal)

save(Journal, file = "JGRcounts.Rda")

# The old records need to be sorted by "issued data", which might have Y, YM, or YMD..
load("JGRcomb.Rda")
JGRcomb <- mutate(JGRcomb, year_issued = format(as.Date(substr(JGRcomb$issued, 1, 4), format = "%Y"), "%Y"))
#next sorrted by the Letter in 'issue'
JGRcomb<- mutate (JGRcomb, sectionL = substr(JGRcomb$issue, 1, 1))
#Select only A through G
target <- c('A','B','C','D','E','F','G')
Test <- filter(JGRcomb, sectionL %in% target)
