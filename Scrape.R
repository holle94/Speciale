library(tidyverse)
library(dplyr)
library(tidytext)
library(jsonlite)

# Scrap NYT ---------------------------------------------------------------


#################################################################################
####            function - search news article with API                      ####


#dt = nytime('donald trump',2018)
#xi = nytime('xi jinping',2018)

#write.csv(dt, "NYT news_donald trump.csv")
#write.csv(xi, "NYT news_xi jinping.csv")

#dt = read.csv("NYT news_donald trump.csv",header=T,stringsAsFactors = F)
#xi = read.csv("NYT news_xi jinping.csv",header=T,stringsAsFactors = F)


# Full search with facet --------------------------------------------------
api <- "bh4A7qupXodlVagYpZ0MHafg5BcrwgPC"
##FUyCcYanjFwHdztxQoYn060Knl8Ahmoq
## bh4A7qupXodlVagYpZ0MHafg5BcrwgPC
nytime1 = function (keyword,year) {
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0101&end_date=',year,'0330&api-key=',api,sep="")
  #get the total number of search results
  initialsearch = fromJSON(url,flatten = T)
  maxPages = round((initialsearch$response$meta$hits / 10)-1)
  
  #try with the max page limit at 10
  maxPages = ifelse(maxPages >= 10, 10, maxPages)
  
  #creat a empty data frame
  df = data.frame(id=as.numeric(),created_time=character(),snippet=character(),
                  headline=character())
  
  #save search results into data frame
  for(i in 0:maxPages){
    #get the search results of each page
    nytSearch = fromJSON(paste0(url, "&page=", i), flatten = T) 
    temp = data.frame(id=1:nrow(nytSearch$response$docs),
                      created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline.main)
    df=rbind(df,temp)
    Sys.sleep(6) #sleep for 5 second
  }
  return(df)
}

#test <- nytime1("dealbook",2018)

nyt_search1 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=section=',searchFQ,'&begin_date=',year,'0101&end_date=',year,'0131&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search2 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0201&end_date=',year,'0226&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search3 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0301&end_date=',year,'0331&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search4 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0401&end_date=',year,'0430&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search5 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0501&end_date=',year,'0531&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search6 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0601&end_date=',year,'0630&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search7 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0701&end_date=',year,'0731&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search8 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0801&end_date=',year,'0831&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search9 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'0901&end_date=',year,'0930&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search10 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'1001&end_date=',year,'1031&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search11 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'1101&end_date=',year,'1130&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_search12 = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=news_desk=',searchFQ,'&begin_date=',year,'1201&end_date=',year,'1231&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

nyt_searche = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,'&begin_date=',year,'0101&end_date=',year,'1231&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}
# Sidelights from Wall Street
# Financial and Business Sidelights of the Day
# Market Place
# section_name news_desk

# Business National World US Politics Foreign 


#write.csv(dt, "NYT news_donald trump.csv")
#write.csv(xi, "NYT news_xi jinping.csv")

# markets virker - tror søgninger med to ord giver problemer. 
# DealBook 


df1 = data.frame(created_time=character(),snippet=character(),headline=character())
t1 <- Sys.time()
for (i in 2011:2019) {
  tmp1 <-  nyt_search1('economy',i, "business")
  df1 <- rbind(df1,tmp1)
}
t2 <- Sys.time()
#write.csv(df1, "")
#
Sys.sleep(6)

df2 = data.frame(created_time=character(),snippet=character(),headline=character())
t3 <- Sys.time()
for (i in 2011:2019) {
  tmp2 <-  nyt_search2('',i, "business")
  df2 <- rbind(df2,tmp2)
}
t4 <- Sys.time()

#write.csv(df2, "f_s2.rdata")

rm(df12)

Sys.sleep(6)

df3 = data.frame(created_time=character(),snippet=character(),headline=character())
t5 <- Sys.time()
for (i in 2011:2019) {
  tmp3 <-  nyt_search3('',i, "business")
  df3 <- rbind(df3,tmp3)
}
t6 <- Sys.time()
#write.csv(df3, "")

Sys.sleep(6)

df4 = data.frame(created_time=character(),snippet=character(),headline=character())
t7 <- Sys.time()
for (i in 2011:2019) {
  tmp4 <-  nyt_search4('',i, "business")
  df4 <- rbind(df4,tmp4)
}

#write.csv(df4, "full_s4.rdata")

Sys.sleep(6)
t8 <- Sys.time()
df5 = data.frame(created_time=character(),snippet=character(),headline=character())

for (i in 2011:2019) {
  tmp5 <-  nyt_search5('',i, "business")
  df5 <- rbind(df5,tmp5)
}

#write.csv(df5, "ful_s5.rdata")

 Sys.sleep(6)

df6 = data.frame(created_time=character(),snippet=character(),headline=character())
t9 <- Sys.time()
for (i in 2011:2019) {
  tmp6 <-  nyt_search6('',i, "business")
  df6 <- rbind(df6,tmp6)
}

#write.csv(df6, "fulll_s6.rdata")

df7 = data.frame(created_time=character(),snippet=character(),headline=character())
t10 <- Sys.time()
for (i in 2011:2019) {
  tmp7 <-  nyt_search7('',i, "business")
  df7 <- rbind(df7,tmp7)
}


#write.csv(df7, "f_s7.rdata")
Sys.sleep(6)

df8 = data.frame(created_time=character(),snippet=character(),headline=character())
t11 <- Sys.time()
for (i in 2011:2019) {
  tmp8 <-  nyt_search8('',i, "business")
  df8 <- rbind(df8,tmp8)
}

#write.csv(df8, "F_s8.rdata")

Sys.sleep(6)

df9 = data.frame(created_time=character(),snippet=character(),headline=character())
t12 <- Sys.time()
for (i in 2011:2019) {
  tmp9 <-  nyt_search9('',i, "business")
  df9 <- rbind(df9,tmp9)
}

#write.csv(df9, "F_s9.rdata")

Sys.sleep(6)

df10 = data.frame(created_time=character(),snippet=character(),headline=character())
t13 <- Sys.time()
for (i in 2011:2019) {
  tmp10 <-  nyt_search10('',i, "business")
  df10 <- rbind(df10,tmp10)
}

#write.csv(df10, "F_s10.rdata")

Sys.sleep(6)

df11 = data.frame(created_time=character(),snippet=character(),headline=character())
t14 <- Sys.time()
for (i in 2011:2019) {
  tmp11 <-  nyt_search11('',i, "business")
  df11 <- rbind(df11,tmp11)
}

#write.csv(df11, "F_s11.rdata")

Sys.sleep(6)

df12 = data.frame(created_time=character(),snippet=character(),headline=character())
t15 <- Sys.time()
for (i in 2011:2019) {
  tmp12 <-  nyt_search12('',i, "business")
  df12 <- rbind(df12,tmp12)
}
t16 <- Sys.time()

#write.csv(df12, "")


# Econ --------------------------------------------------------------------

dfe = data.frame(created_time=character(),snippet=character(),headline=character())

econ_df <- nyt_searche("economy ", 2011)
for (i in 2007:2009) {
  tmpe <-  nyt_searche("economy ", 2011)
  dfe <- rbind(dfe,tmpe)
}

nyt_search_t = function (keyword, year, section){
  searchQ = URLencode(keyword)
  searchFQ = URLencode(section)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&fq=section_name=',searchFQ,'&begin_date=',year,'0101&end_date=',year,'1231&&api-key=',api,sep="")
  ##convert json to R object
  initialQuery = fromJSON(url,flatten=TRUE)
  maxPages = round((initialQuery$response$meta$hits / 10)-1)
  maxPages = ifelse(maxPages >= 99, 99, maxPages)
  
  #download all the data and transform into R obj
  df = data.frame(created_time=character(),snippet=character(),headline=character())
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(url, "&page=", i))
    temp = data.frame(created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline$main,
                      stringsAsFactors = F)
    df = rbind(df,temp)
    Sys.sleep(6) #sleep for 5 seconds
  }
  return(df)
}

test <- nyt_search_t("economy",2007,"business day")

