#This code:
#1) uses rcrossref to get DOI information  for GRL (ISSN 0094-8276).
#2) uses raltmetric to get Altmetric.com data for each DOI

#written by EBG 11/18

#load the needed libraries
library(rcrossref)
library(rAltmetric)
library(tidyverse)

#get all the dois from CrossRef for JGR, which has a print ISSN of 0148-0227
JGRdata <-
  cr_works(
    filter = c(issn = "0148-0227",from_pub_date='1990-01-01'),
    limit = 1000,
    cursor = '*',
    cursor_max = 100000
  )
#pull out the dataframe
JGRDF <- JGRdata$data
#save the file
save(JGRDF, file = "JGRDF.Rda")

#subset by title NA
JGR <- filter(JGRDF, !is.na(title))
