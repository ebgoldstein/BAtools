#This code:
#1) uses rcrossref to get DOI information  for GRL (ISSN 0094-8276).
#2) uses raltmetric to get Altmetric.com data for each DOI
#3) uses rvest to get specific wikipedia edit data for each paper that has a wikipedia mention

#written by EBG, starting 8/17

#load the needed libraries
library(rcrossref)
library(rAltmetric)
library(plyr)
library(tidyverse)
library(rvest)

#get all the dois from CrossRef for GRL, which has a print ISSN of 0094-8276
GRLdata <-
  cr_works(
    filter = c(issn = "0094-8276"),
    limit = 1000,
    cursor = '*',
    cursor_max = 36939
  )
#pull out the dataframe
GRLDF <- GRLdata$data
#save the file
save(GRLDF, file = "GRLDF.Rda")
#load("~/GRLDF.Rda")

#export a list of dois.
doi_list <- paste0("doi", "/", GRLDF$DOI)
#then run through rAltmetric to get the Altmetric.com metrics
raw_metrics <- llply(doi_list, altmetrics, .progress = 'text')
metric_data <-
  ldply(raw_metrics, altmetric_data, .progress = "text")
merged_data <-
  merge(
    x = GRLDF ,
    y = metric_data,
    by.y = "doi",
    by.x = "DOI",
    all.x = TRUE
  )
#just an FYI: column 104 is the wikipedia column,
save(merged_data, file = "merged_data.Rda")

##

#load(merged_data.Rda)
#scrape for wikipedia mentions
#make a new smaller dataframe
Wikipapers <-
  merged_data[, c("DOI",
                  "issued",
                  "cited_by_wikipedia_count",
                  "URL",
                  "details_url")]

#select only rows with cited_by_wikipedia >=1
Wikipapers <- subset(Wikipapers, cited_by_wikipedia_count >= 1)
#make some new rows to hold the new data
Wikipapers["W_Pages"] <- NA
Wikipapers["W_Authors"] <- NA
Wikipapers["W_edit_times"] <- NA

#make the dataframe larger by adding rows when a paper has been mentioned more than once in wikipedia
Wikipapers <-
  Wikipapers[rep(row.names(Wikipapers), Wikipapers$cited_by_wikipedia_count), 1:7]

#loop through the papers to populate the dataframe with Wikipedia edit details
i = 1
while (i <= nrow(Wikipapers)) {
  althtml <-
    read_html(paste0(toString(Wikipapers[i, "details_url"]), "/wikipedia"))
  
  #find the # of wikipedia mentions from the altmetric.com data
  num_W_edits <- (Wikipapers[i, "cited_by_wikipedia_count"]) - 1
  
  #find the obsevred # of wikipedia mentions
  TestCompliance <- althtml %>%
    html_nodes("h3") %>%
    html_text()
  
  # if observed wikipedia mentions are larger than the database, add extra rows. (mention data is volatile)
  if (length(TestCompliance) > (num_W_edits + 1)) {
    #print("noncomplaint") #for debugging
    #determine many rows to add rows
    Nrowstoadd <-
      length(TestCompliance) - (Wikipapers[i, "cited_by_wikipedia_count"])
    #add the rows
    j = 1
    while (j <= Nrowstoadd) {
      Wikipapers <-
        rbind(Wikipapers[1:i,], Wikipapers[i,], Wikipapers[-(1:i),])
      #print("new row") #for debugging
      j = j + 1
    }
    #adjust the 'num edits' counter accordingly
    num_W_edits <- num_W_edits + Nrowstoadd
  }
  
  #need an altmetric license or use the API with a research license to get >5 mentions, 
  #with this code, if there are more than 5 mentions, only 5 appear and get filled in and then either:
  # 1) hand curation
  # 2) For GRL (and probably all wiley journals), the work-around is to replace 'www' with 'wiley' in the http address
  
  #scrape for Wikipedia page names
  Wpages <- althtml %>%
    html_nodes("h3") %>%
    html_text()
  Wikipapers[i:(i+length(Wpages)-1), "W_Pages"] <- Wpages
  
  #scrape for the wikipedia edit authors
  Wauths <- althtml %>%
    html_nodes("h4 a:nth-child(1)") %>%
    html_text()
  Wikipapers[i:(i+length(Wauths)-1), "W_Authors"] <- Wauths
  
  #scrape for wikipedia edit times
  Wedits <- althtml %>%
    html_nodes("time") %>%
    html_text()
  Wikipapers[i:(i+length(Wedits)-1), "W_edit_times"] <- Wedits
  
  #increment i
  i = i + 1 + num_W_edits
  print(i) #for debugging
}

save(Wikipapers, file = "Wikipapers.Rda")
#####
#pull GRL index terms for each article, up to 5;
# #http://publications.agu.org/author-resource-center/text-requirements/
# #http://publications.agu.org/author-resource-center/index-terms/
# Sometimes more terms seem to be present, but this might be a mix of index terms and keywords??
# I will select the first 5 
#
# Make new columns for these index terms
Wikipapers["Index_1"] <- NA
Wikipapers["Index_2"] <- NA
Wikipapers["Index_3"] <- NA
Wikipapers["Index_4"] <- NA
Wikipapers["Index_5"] <- NA
# 
IndexOne<-grep("Index_1", colnames(Wikipapers))
#
for (k in 1:nrow(Wikipapers)){
GRL<- read_html(toString(Wikipapers[k,"URL"]))
IndexTerms<-GRL %>%
  html_nodes(".article-info__indexed-terms-data") %>%
  html_text()
#put them in the dataframe in the correct spot
if (length(IndexTerms)>0) {
  if (length(IndexTerms)<=5) {
    Wikipapers[k,IndexOne:IndexOne:(IndexOne+length(IndexTerms)-1)] <- IndexTerms
  }
  else{
    Wikipapers[k,IndexOne:IndexOne:(IndexOne+4)] <- IndexTerms
  }
}
print(k)

######
#add the first publication date from GRL webpage
Wikipapers["firstpubdate"] <- NA
#scrape GRL site for first issued date and put them in the matrix
for (k in 1:nrow(Wikipapers)) {
  GRL <- read_html(toString(Wikipapers[k, "URL"]))
  Wikipapers[k, "firstpubdate"] <- GRL %>%
    html_nodes("#first-published-date") %>%
    html_text()
  print(k) #for debugging
}
save(Wikipapers, file = "Wikipapers.Rda")
  
  
#make some new rows to hold the new data
Wikipapers["WB_Authors"] <- NA
Wikipapers["WB_edit_times"] <- NA
Wikipapers["WB_ver_history"] <- NA

#Notes:
# -some people might not write in doi
# -preprints and alternative titles also confuse matters
#loop through the papers to populate the dataframe with Wikipedia edit details
for (k in 1:nrow(Wikipapers)) {
  #Wikipedia page to search
  article <- Wikipapers[k, "W_Pages"]
  #replace spaces with '+' sign
  article <- gsub(" ", "+", article, fixed = TRUE)
  #DOI to find on the page
  needle <- Wikipapers[k, "DOI"]
  
  beginning <-
    "http://wikipedia.ramselehof.de/wikiblame.php?user_lang=en&lang=en&project=wikipedia&article="
  after <-
    "&skipversions=0&ignorefirst=0&limit=500&offmon=10&offtag=16&offjahr=2017&searchmethod=int&order=desc&force_wikitags=on&user="
  
  site <-  paste0(beginning, article, "&needle=", needle, after)
  
  wikiblame <- read_html(site)
  
  #find edit with Wikiblame
  spedit <- wikiblame %>%
    html_nodes("br~ a+ a") %>%
    html_attr('href')
  
  #this catches where Wikiblame has trouble including 1) when edits are deleted 2) when the page is a redirect 3) other problems???
  if (length(spedit) > 1) {
    # now follow the link to find the editor who added the 'needle':
    lastlink<-tail(spedit,n=1)
    Wikipapers[k, "WB_ver_history"] <-(lastlink)
    wiki <- read_html(lastlink)
    
    #scrape for editor name and add to DF
    Wikipapers[k, "WB_Authors"] <- wiki %>%
      html_nodes("#mw-diff-ntitle2 bdi") %>%
      html_text()
    
    # scrape for date and add to DF
    Wikipapers[k, "WB_edit_times"] <- wiki %>%
      html_nodes("#mw-diff-ntitle1 strong > a") %>%
      html_text()
  }
  print(k) #for debugging
}

#Save
save(Wikipapers,file="Wikipapers.Rda")
GRLwiki <- Wikipapers
#load("~/GRLwiki.Rda")

######
#get the dates in the correct format
#edit times
GRLwiki <- mutate(GRLwiki, W_edit_times= as.Date(W_edit_times, format= "%d %B %Y"))

#GRL first pubed date
GRLwiki <- mutate(GRLwiki, firstpubdate= as.Date(firstpubdate, format= "%d %b %Y"))

######

#create new column 
GRLwiki["WB_edit_DT"] <- NA
GRLwiki <- mutate(GRLwiki, WB_edit_DT= as.Date(WB_edit_DT, format= "%d %b %Y"))

#loop through WB_edit_times column,
for (k in 1:nrow(GRLwiki)) {
  #if its not a NA:
  if (!is.na(GRLwiki$WB_edit_times[k])) {
    #pull out hte string and split it
    DT <- strsplit(GRLwiki$WB_edit_times[k],'[- ]')[[1]]
    #paste together the D,M,Y
    MDY <- paste(DT[5], DT[6], DT[7], sep=" ")
    #convert to date format "2014-12-17"
    #paste into new column
    GRLwiki$WB_edit_DT[k]<- as.Date(MDY, format= "%d %B %Y")
  }
  }

