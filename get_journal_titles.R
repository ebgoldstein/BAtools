library(stringr)
library(rcrossref)
library(tidyverse)

#take Tom's EarthArXiv output, and make a csv with just DOI's, which is named 'LISTOFDOIs.csv'
#from this, compute the top journals

doi_data <- read.csv('LISTOFDOIs.csv', header = TRUE, stringsAsFactors=FALSE)
doilist <- str_remove(doi_data$doi, "https://doi.org/")
a <- cr_works(dois=doilist, .progress="text")
Journal_Titles <-data.frame(a$data$container.title)
colnames(Journal_Titles) <- "Journal"

write.csv(Journal_Titles, file = "Journal_Titles.csv")

Top_Journals<- Titles %>%
  group_by(Journal) %>%
  tally()

write.csv(Top_Journals, file = "Top_Journals.csv")
