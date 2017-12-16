library("tidyverse")
library("rvest")

#make some new rows to hold the new data
Wikipapers["WB_Authors"] <- NA
Wikipapers["WB_edit_times"] <- NA
Wikipapers["WB_ver_history"] <- NA

#Notes:
# -some people might not write in doi
# -preprints and alternative titles also confuse matters
#loop through the papers to populate the dataframe with Wikipedia edit details
for (k in 504:nrow(Wikipapers)) {
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
