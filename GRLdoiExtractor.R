# Crossref API documents: https://github.com/CrossRef/rest-api-doc
# rOpenSci rcrossref tutorial: https://ropensci.org/tutorials/rcrossref_tutorial.html

library("rcrossref")
#get all the dois from CrossRef for GRL, which has a print ISSN of 0094-8276
GRLdata <-cr_works(filter = c(issn = "0094-8276"),limit = 1000,cursor='*',cursor_max = 36939)

#export a list of dois
