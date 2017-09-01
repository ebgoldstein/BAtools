# Crossref API documents: https://github.com/CrossRef/rest-api-doc
# rOpenSci rcrossref tutorial: https://ropensci.org/tutorials/rcrossref_tutorial.html

library(rcrossref)
library(rAltmetric)
library(plyr)

#get all the dois from CrossRef for GRL, which has a print ISSN of 0094-8276
GRLdata <-cr_works(filter = c(issn = "0094-8276"),limit = 1000,cursor='*',cursor_max = 36939)
#pull out the dataframe
GRLDF<-GRLdata$data
#export a list of dois.
doi_list <- paste0("doi","/", GRLDF$DOI)
#then run through rAltmetric (code I have already)
raw_metrics <- llply(doi_list, altmetrics, .progress = 'text')
metric_data <- ldply(raw_metrics, altmetric_data)
merged_data <-merge(x =GRLDF , y = metric_data, by.y = "doi", by.x = "DOI", all.x = TRUE)
write.csv(merged_data, file = "/MERGEDDATA.csv")
#....
