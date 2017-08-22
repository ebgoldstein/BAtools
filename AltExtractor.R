# Some resources:
# http://ropensci.github.io/rAltmetric/
# https://ropensci.org/tutorials/rAltmetric_tutorial.html
# https://api.altmetric.com/docs/call_citations.html

library(rAltmetric)
library(plyr)
doi_data <- read.csv('/LISTOFDOIs.csv', header = TRUE)
doi_list <- paste0("doi","/", doi_data$doi)
raw_metrics <- llply(doi_list, altmetrics, .progress = 'text')
metric_data <- ldply(raw_metrics, altmetric_data)
merged_data <-merge(x =doi_data , y = metric_data, by = "doi", all.x = TRUE)
write.csv(merged_data, file = "/MERGEDDATA.csv")
