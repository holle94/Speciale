library(tidyverse)
library(dplyr)
library(tidytext)
library(jsonlite)

# Full search with facet --------------------------------------------------
api <- "FUyCcYanjFwHdztxQoYn060Knl8Ahmoq"
##FUyCcYanjFwHdztxQoYn060Knl8Ahmoq
## bh4A7qupXodlVagYpZ0MHafg5BcrwgPC
## dB3AHnoyLWn5lkIE9m5UaxAiSZkdS7u3
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

nyt_search1 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0101&end_date=',year,'0131&&api-key=',api,sep="")
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

nyt_search2 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0201&end_date=',year,'0226&&api-key=',api,sep="")
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

nyt_search3 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0301&end_date=',year,'0331&&api-key=',api,sep="")
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

nyt_search4 = function (keyword, year){
  searchQ = URLencode(keyword)

  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0401&end_date=',year,'0430&&api-key=',api,sep="")
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

nyt_search5 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0501&end_date=',year,'0531&&api-key=',api,sep="")
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

nyt_search6 = function (keyword, year){
  searchQ = URLencode(keyword)

  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0601&end_date=',year,'0630&&api-key=',api,sep="")
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

nyt_search7 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0701&end_date=',year,'0731&&api-key=',api,sep="")
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

nyt_search8 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0801&end_date=',year,'0831&&api-key=',api,sep="")
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

nyt_search9 = function (keyword, year){
  searchQ = URLencode(keyword)

  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0901&end_date=',year,'0930&&api-key=',api,sep="")
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

nyt_search10 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'1001&end_date=',year,'1031&&api-key=',api,sep="")
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

nyt_search11 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'1101&end_date=',year,'1130&&api-key=',api,sep="")
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

nyt_search12 = function (keyword, year){
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'1201&end_date=',year,'1231&&api-key=',api,sep="")
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
  tmp1 <-  nyt_search1('economy',i)
  df1 <- rbind(df1,tmp1)
}
   t2 <- Sys.time()
#write.csv(df1, "e_s1.rdata")

Sys.sleep(6)

df2 = data.frame(created_time=character(),snippet=character(),headline=character())
t3 <- Sys.time()
for (i in 2011:2019) {
  tmp2 <-  nyt_search2('economy',i)
  df2 <- rbind(df2,tmp2)
}
t4 <- Sys.time()

#write.csv(df2, "e_s2.rdata")

rm(df12)

Sys.sleep(6)

df3 = data.frame(created_time=character(),snippet=character(),headline=character())
t5 <- Sys.time()
for (i in 2011:2019) {
  tmp3 <-  nyt_search3('economy',i)
  df3 <- rbind(df3,tmp3)
}
t6 <- Sys.time()
#write.csv(df3, "e_s3.rdata")

Sys.sleep(6)

df4 = data.frame(created_time=character(),snippet=character(),headline=character())
t7 <- Sys.time()
for (i in 2011:2019) {
  tmp4 <-  nyt_search4('economy',i)
  df4 <- rbind(df4,tmp4)
}

#write.csv(df4, "e_s4.rdata")

Sys.sleep(6)
t8 <- Sys.time()
df5 = data.frame(created_time=character(),snippet=character(),headline=character())

for (i in 2011:2019) {
  tmp5 <-  nyt_search5('economy',i)
  df5 <- rbind(df5,tmp5)
}

#write.csv(df5, "e_s5.rdata")

Sys.sleep(6)

df6 = data.frame(created_time=character(),snippet=character(),headline=character())
t9 <- Sys.time()
for (i in 2011:2019) {
  tmp6 <-  nyt_search6('economy',i)
  df6 <- rbind(df6,tmp6)
}

#write.csv(df6, "e_s6.rdata")

df7 = data.frame(created_time=character(),snippet=character(),headline=character())
t10 <- Sys.time()
for (i in 2011:2019) {
  tmp7 <-  nyt_search7('economy',i)
  df7 <- rbind(df7,tmp7)
}

#write.csv(df7, "e_s7.rdata")

Sys.sleep(6)

df8 = data.frame(created_time=character(),snippet=character(),headline=character())
t11 <- Sys.time()
for (i in 2011:2019) {
  tmp8 <-  nyt_search8('economy',i)
  df8 <- rbind(df8,tmp8)
}

#write.csv(df8, "e_s8.rdata")

Sys.sleep(6)

df9 = data.frame(created_time=character(),snippet=character(),headline=character())
t12 <- Sys.time()
for (i in 2011:2019) {
  tmp9 <-  nyt_search9('economy',i)
  df9 <- rbind(df9,tmp9)
}

#write.csv(df9, "e_s9.rdata")

Sys.sleep(6)

df10 = data.frame(created_time=character(),snippet=character(),headline=character())
t13 <- Sys.time()
for (i in 2011:2019) {
  tmp10 <-  nyt_search10('economy',i)
  df10 <- rbind(df10,tmp10)
}

#write.csv(df10, "e_s10.rdata")

Sys.sleep(6)

df11 = data.frame(created_time=character(),snippet=character(),headline=character())
t14 <- Sys.time()
for (i in 2011:2019) {
  tmp11 <-  nyt_search11('economy',i)
  df11 <- rbind(df11,tmp11)
}

#write.csv(df11, "e_s11.rdata")

Sys.sleep(6)

df12 = data.frame(created_time=character(),snippet=character(),headline=character())
t15 <- Sys.time()
for (i in 2011:2019) {
  tmp12 <-  nyt_search12('economy',i)
  df12 <- rbind(df12,tmp12)
}
t16 <- Sys.time()

#write.csv(df12, "e_s12.rdata")

plot(dowJ_i$Volume)
