#This code:
#1) uses rcrossref to get DOI information  for GRL (ISSN 0094-8276).
#2) uses raltmetric to get Altmetric.com data for each DOI
#3) uses rvest to get specific wikipedia edit data for each paper that has a wikipedia mention

#Note: line 76 needs some help

#written by EBG, starting 8/17

#load the needed libraries
library(rcrossref)
library(rAltmetric)
library(plyr)
library(rvest)

#get all the dois from CrossRef for GRL, which has a print ISSN of 0094-8276
GRLdata <-cr_works(filter = c(issn = "0094-8276"),limit = 1000,cursor='*',cursor_max = 36939)
#pull out the dataframe
GRLDF<-GRLdata$data
#save the file
save(GRLDF,file="GRLDF.Rda")
#load("~/GRLDF.Rda")

#export a list of dois.
doi_list <- paste0("doi","/", GRLDF$DOI)
#then run through rAltmetric to get the Altmetric.com metrics
raw_metrics <- llply(doi_list, altmetrics, .progress = 'text')
metric_data <- ldply(raw_metrics, altmetric_data, .progress = "text")
merged_data <-merge(x =GRLDF , y = metric_data, by.y = "doi", by.x = "DOI", all.x = TRUE)
#just an FYI: column 104 is the wikipedia column, 
save(merged_data,file="merged_data.Rda")
#load(merged_data.Rda)

#scrape for wikipedia mentions
#make a new smaller dataframe
Wikipapers <-merged_data[,c("DOI","issued","cited_by_wikipedia_count","URL","details_url")]
#select only rows with cited_by_wikipedia >=1
Wikipapers <- subset(Wikipapers, cited_by_wikipedia_count >=1)
#make some new rows to hold the new data
Wikipapers["W_Pages"] <- NA
Wikipapers["W_Authors"] <- NA
Wikipapers["W_edit_times"] <- NA

#make the dataframe larger by adding rows when a paper has been mentioned more than once in wikipedia
Wikipapers <- Wikipapers[rep(row.names(Wikipapers), Wikipapers$cited_by_wikipedia_count),1:7]

#loop through the papers to populate the dataframe with Wikipedia edit details
i=1
while (i<=nrow(Wikipapers)){
  althtml <- read_html(paste0(toString(Wikipapers[i,"details_url"]), "/wikipedia"))
  
  #find the # of wikipedia mentions from the altmetric.com data
  num_W_edits<-(Wikipapers[i,"cited_by_wikipedia_count"])-1
  
  #find the obsevred # of wikipedia mentions
  TestCompliance <-althtml %>%
    html_nodes("h3") %>%
    html_text()

# if observed wikipedia mentions are larger than the database, add extra rows. (mention data is volatile)
  if (length(TestCompliance) > (num_W_edits+1)) {
    #print("noncomplaint") #for debugging
    #determine many rows to add rows
    Nrowstoadd<- length(TestCompliance)-(Wikipapers[i,"cited_by_wikipedia_count"])
    #add the rows
    j=1
    while(j<=Nrowstoadd){
      Wikipapers <- rbind(Wikipapers[1:i,],Wikipapers[i,],Wikipapers[-(1:i),])
      #print("new row") #for debugging
      j=j+1
    }
    #adjust the 'num edits' counter accordingly
    num_W_edits <-num_W_edits+Nrowstoadd
  }
  
  #need an altmetric license to get >5 mentions, so I'm looking for another way...
  if(Wikipapers[i,"cited_by_wikipedia_count"] < 5){  
  Wikipapers[i:(i+num_W_edits),"W_Pages"]<-althtml %>%
    html_nodes("h3") %>%
    html_text()
  #scrape for the authors
  Wikipapers[i:(i+num_W_edits),"W_Authors"] <-althtml %>%
    html_nodes("h4 a:nth-child(1)") %>%
    html_text()
  #scrape for edit times
  Wikipapers[i:(i+num_W_edits),"W_edit_times"] <-althtml %>%
    html_nodes("time") %>%
    html_text()
  }
  
  #increment i
  i=i+1+num_W_edits
  #print(i) #for debugging
}
save(Wikipapers,file="Wikipapers.Rda")
