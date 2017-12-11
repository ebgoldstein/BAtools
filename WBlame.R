library("tidyverse")
library("rvest")


article <- "enceladus"
needle <- "showman"
#needle can;t be DOI neccesarily, because some poeple ignore doi... so it needs to be author name?)


beginning <- "http://wikipedia.ramselehof.de/wikiblame.php?user_lang=en&lang=en&project=wikipedia&article=" 
after <- "&skipversions=0&ignorefirst=0&limit=500&offmon=10&offtag=16&offjahr=2017&searchmethod=int&order=asc&user="

site <-  paste0(beginning, article, "&needle=", needle, after)

wikiblame <- read_html(site)
  
#scrape for edit
spedit <- wikiblame %>%
  html_nodes("br~ a+ a") %>%
  html_attr('href')


# now follow the link to find the editor who added the 'needle': 

wiki <- read_html(spedit[2])

editor <- wiki %>%
  html_nodes("#mw-diff-ntitle2 bdi") %>%
  html_text()

# and determine the data that the editor added the needle
editdate <- wiki %>%
  html_nodes("#mw-diff-ntitle1 strong > a") %>%
  html_text()
