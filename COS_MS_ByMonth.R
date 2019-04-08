library(tidyverse)

#IMPORT THE DATA

services <- c("eartharxiv","engrxiv","lawarxiv","lissa","marxiv","mindrxiv","paleorxiv","psyarxiv","socarxiv")

for (name in services){
  #find hte data
  dpath <- (paste("COS Data/individual/",name,".log", sep = ""))
  #import the data
  df <- read_delim(dpath, ";", escape_double = FALSE, col_names = FALSE, 
  trim_ws = TRUE)
  #rename the whole dataframe for the service
  assign(name, df)
  #count submissions by month and save the results
  ByMonth  <- df %>%
    mutate(month = format(X5, "%m"), year = format(X5, "%Y")) %>%
    group_by(month, year) %>%
    count(month,year)
  #rename the ByMonth dataframe for the service
  assign(paste(name,"BM", sep = ""), ByMonth)
}

#plot cumulative submissions by month


