#library(Quandl)
library(tidyverse)
library(dplyr)
library(tidytext)
library(jsonlite)
#library(SentimentAnalysis) # https://cran.r-project.org/web/packages/SentimentAnalysis/SentimentAnalysis.pdf
library(readr)
library(httr)
library(magrittr)
library(fansi)
library(zoo)
library(textdata)
#library(tidyr)
library(lmtest)
library(aTSA)
library(ggthemes)
library(rugarch)
library(vars)
library(mltools)
#library(recipes)
library(fastDummies)
library(forecast)
library(FactoMineR)
library(factoextra)
library(lubridate)
library(tsbox)
library(aTSA)
library(sandwich)


# Data GI - Harvard
#data(DictionaryGI) 


#lexicon (nrc,bing,afinn) fra tidytext
#get_sentiments("afinn")




# Data --------------------------------------------------------------------

 
#df_test <- nytime1("economy",2018)
#df_test1 <- nyt_search1("",2018,"World")
 
dowJ <- read.csv("Yahoof_data_dowJ.csv")

df_market<-read.csv("Full_text.csv")
df_DB <- read.csv("df_DB")
df_econ <- read.csv("df_econ")

df_DB$text <- paste(df_DB$headline,df_DB$snippet)
df_econ$text <- paste(df_econ$headline,df_econ$snippet)
df_DB$created_time %<>% as.Date() 
df_econ$created_time %<>% as.Date() 
df_market$created_time %<>% as.Date() 


df_full <- rbind(df_market)
df_full<-df_full[!duplicated(df_full$text), ]

# NLP ---------------------------------------------------------------------
df_full$text %<>% as.character() 
 df_tidy <- df_full[,c(2,5)] %>% 
   unnest_tokens(output = word, input = text) %>% 
   mutate(weekday = weekdays(created_time)) %>% 
   mutate(created_time = if_else( weekday == "Saturday", created_time-1 , created_time)) %>% 
   mutate(created_time = if_else( weekday == "Sunday", created_time-2, created_time)) %>% 
   mutate(weekday = if_else( weekday == "Saturday", "Friday" , weekday)) %>% 
   mutate(weekday = if_else( weekday == "Sunday", "Friday" , weekday)) %>% 
   rename(Date = created_time) %>% 
   mutate(weekday = as.factor(weekday))
 
 df_tidy %>%
   count(word, sort = TRUE) %>%
   head(20)
 
 df_tidy %<>%
   anti_join(stop_words %>% bind_rows(stop_words), by = "word") 
 
 
 df_tidy %>%
   count(word, sort = TRUE) %>%
   head(10)
 
 df_tidy_c<- df_tidy %>% 
   group_by(Date) %>% 
   count() 
 
df_token <- df_tidy %>%    
inner_join(get_sentiments(lexicon = "loughran"), by = "word") %>% 
  count(sentiment) %>% 
  spread(sentiment,n, fill = 0)

df_token_p <- df_tidy %>%    
  inner_join(get_sentiments(lexicon = "loughran"), by = "word")

df_token_p %>% 
  count(word, sort = TRUE) %>%
  head(10)

df_tidy %>%    
  inner_join(get_sentiments(lexicon = "loughran"), by = "word") %>% 
  count(sentiment) %>% 
  spread(sentiment,n, fill = 0)

# Lexicon Loughran  --------------------------------------------------------

s_index<-df_tidy %>%    
  inner_join(get_sentiments(lexicon = "loughran"), by = "word") 

#%>% 
#mutate(weekday = weekdays(created_time)) %>% 
#  mutate(created_time = if_else( weekday == "Saturday", created_time-1 , created_time)) %>% 
#  mutate(created_time = if_else( weekday == "Sunday", created_time-2, created_time)) %>% 
#  mutate(weekday = if_else( weekday == "Saturday", "Friday" , weekday)) %>% 
#  mutate(weekday = if_else( weekday == "Sunday", "Friday" , weekday)) %>% 
#  rename(Date = created_time) %>% 
#  mutate(weekday = as.factor(weekday))

s_index_n <- s_index %>% 
  group_by(Date, weekday) %>% 
  filter(sentiment == "negative")%>% 
  count(sentiment) %>% 
  rename(n_score =n) 
 
s_index_p <- s_index %>% 
  group_by(Date, weekday) %>% 
  filter(sentiment == "positive")%>% 
  count(sentiment) %>% 
  rename(p_score =n) 

s_index_u <- s_index %>% 
  group_by(Date, weekday) %>% 
  filter(sentiment == "uncertainty")%>% 
  count(sentiment) %>% 
  rename(u_score =n) 

s_index_l <- s_index %>% 
  group_by(Date, weekday) %>% 
  filter(sentiment == "litigious")%>% 
  count(sentiment) %>% 
  rename(l_score =n) 

s_index_c <- s_index %>% 
  group_by(Date, weekday) %>% 
  filter(sentiment == "constraining")%>% 
  count(sentiment) %>% 
  rename(c_score =n) 

s_index_s <- s_index %>% 
  group_by(Date, weekday) %>% 
  filter(sentiment == "superfluous")%>% 
  count(sentiment) %>% 
  rename(s_score =n) 

s_index_f <- s_index_n[,-c(2,3)] %>% 
  left_join(s_index_p[,-c(2,3)], by = "Date") %>% 
  left_join(s_index_u[,-c(2,3)], by = "Date") %>% 
  left_join(s_index_l[,-c(2,3)], by = "Date") %>% 
  left_join(s_index_c[,-c(2,3)], by = "Date") %>% 
  left_join(s_index_s[,-c(2,3)], by = "Date") %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) %>% 
  group_by(Date)


  #s_index <- s_index[,c(1,4)]
 

# Lexicon AFINN  ----------------------------------------------------------------

s_index_A<-df_tidy %>%    
  inner_join(get_sentiments(lexicon = "afinn"), by = "word")

s_index_A %<>% 
  group_by(Date) %>% 
  summarise(AFINN=sum(value))


# Lexicon Bing ------------------------------------------------------------

s_index_B<-df_tidy %>%    
  inner_join(get_sentiments(lexicon = "bing"), by = "word")

s_index_n_B <- s_index_B %>% 
  group_by(Date) %>% 
  filter(sentiment == "negative")%>% 
  count(sentiment) %>% 
  rename(n_score_B =n) 

s_index_p_B <- s_index_B %>% 
  group_by(Date) %>% 
  filter(sentiment == "positive")%>% 
  count(sentiment) %>% 
  rename(p_score_B =n) 

s_index_f_B <- s_index_n_B[,-2] %>% 
  left_join(s_index_p_B[,-2], by = "Date") %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0))

# Dow Jones ---------------------------------------------------------------

dowJ$Date %<>% as.Date() 
dowJ %<>% 
  filter(Date >= "2011-01-01" & Date <= "2019-12-31")


dowJ_i<-dowJ %>% 
  left_join(s_index_f, by = "Date")

dowJ_i<-dowJ_i %>% 
  left_join(s_index_A, by = "Date")

dowJ_i<-dowJ_i %>% 
  left_join(s_index_f_B, by = "Date")

dowJ_i<-dowJ_i %>% 
  left_join(df_tidy_c, by = "Date")


dowJ_i %<>% mutate(score_n = n_score/n) %>% 
  mutate(score_p = p_score/n) %>% 
  mutate(score_c = c_score/n) %>% 
  mutate(score_l = l_score/n) %>% 
  mutate(score_u = u_score/n) %>% 
  mutate(score_s = s_score/n) %>% 
  mutate(score = (n_score-p_score)/n)%>% 
  mutate(score_AFINN = (AFINN)/n) %>% 
  mutate(score_n_B = n_score_B/n) %>% 
  mutate(score_p_B = p_score_B/n) %>% 
  mutate(score_B = (n_score_B-p_score_B)/n) %>% 
  mutate(weekday = weekdays(Date)) %>% 
  mutate(Volume = log(Volume)) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0))

pcr <- prcomp(x =  dowJ_i[,c("score_n", "score_p", "score_c", "score_l", "score_u", "score_s")], scale. = TRUE)

#pcr %>% 
#    fviz_screeplot(addlabels = TRUE, 
#                   ncp = 6, 
#                   ggtheme = theme_gray())
#  
#pcr %>%
#    fviz_pca_var(alpha.var = "cos2",
#                 col.var = "contrib",
#                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#                 repel = TRUE,
#                 ggtheme = theme_gray()) 
#
#  
  
pc <- pcr$x[,1]

#pc1<-pc/dowJ_i$n

dowJ_i <- cbind(dowJ_i,pc)

dowJ_i %<>% 
  arrange(desc(Date)) %>% 
  mutate(chg_price=c(-diff(Adj.Close)/Adj.Close[-1] *  100, NA)) %>% 
  mutate(Vlm=c(-diff(Volume), NA)) %>% 
  mutate(chg_log=c(-diff(log(Adj.Close)) *  100, NA))

dowJ_i$weekday <- as.factor(dowJ_i$weekday)

dowJ_i <- dowJ_i[-2264,]

dowJ_i %<>% dummy_cols( select_columns = c("weekday"),
                      remove_first_dummy = TRUE)

# Plots -------------------------------------------------------------------

dowJ_i %>% ggplot() +
  geom_line(aes(Date, chg_price)) +
  theme_classic()+
  labs(title = " Dow Jones Daily Returns", caption = "Data from: Yahoo Finance") +
  ylab("Daily Returns")

dowJ_i %>% ggplot() +
  geom_line(aes(Date, n_score)) +
  theme_classic() +
  labs(title = "Negative Index", caption = "Data from: New York Times") +
  ylab("Negative score value")

hist(dowJ_i$chg_price, breaks = 20, freq = FALSE, col = "grey")
curve(dnorm(x, mean=0, sd=1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

hist(dowJ_i$Volume, breaks = 20, freq = FALSE, col = "grey")

#ti<-dowJ_i[,1]
#
#  chg_price=  c(-diff(dowJ_i$Adj.Close)/dowJ_i$Adj.Close[-1] *  100, NA)
#
#df1<-as.data.frame(col(ti, chg_price)) %>% 
#  rename(Date = ti)
#

dowJ_i <- dowJ_i %>% 
  arrange(Date)

# adf.test ----------------------------------------------------------------

adf.test(dowJ_i$Volume, nlag = 5)

adf.test(dowJ_i$chg_price, nlag = 5)

adf.test(dowJ_i$Adj.Close, nlag = 5)

adf.test(dowJ_i$score, nlag = 5)

ts.plot(diff(dowJ_i$Volume))
# Granger-test ------------------------------------------------------------


grangertest(chg_price ~ score, order=5, na.action= na.omit , data=dowJ_i) 


# ARCH/GARCH --------------------------------------------------------------

auto.arima(dowJ_i$chg_price)

AA <- arima(x = dowJ_i$chg_price, order = c(3,0,2))


acf(resid(AA))
acf(resid(AA)^2)
pacf(resid(AA)^2)

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(1, 2), include.mean = TRUE), 
                             distribution.model = "norm")

garch <- ugarchfit(spec = spec, data = dowJ_i$chg_price, solver = "hybrid")

GARCH<-garch@fit$sigma

spec           <- getspec(gf11)
setfixed(spec) <- as.list(coef(gf11))
garchforecast1 <- ugarchforecast(spec, n.ahead = 1, n.roll = 2262, data = dowJ_i, out.sample = 2263)
# VAR ---------------------------------------------------------------------

#, weekday_Monday, weekday_Thursday, weekday_Tuesday, weekday_Wednesday

fVAR <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price, score_p, Volume) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 

#fVAR <- cbind(fVAR,GARCH)

fVAR1 <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price, Volume) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 


v=c(NA)
v1=c(NA)  
v2=c(NA) 


VARselect(fVAR)

OO <- VAR(fVAR, p =5, exogen = cbind(garch=GARCH,monday=dowJ_i$weekday_Monday, tuesday=dowJ_i$weekday_Tuesday, wednesday=dowJ_i$weekday_Wednesday, thursday=dowJ_i$weekday_Thursday))
summary(OO)


#for (i in 763) {
# monday= dowJ_i$weekday_Monday[-c(1:i)]
# tuesday=dowJ_i$weekday_Tuesday[-c(1:i)]
# wednesday=dowJ_i$weekday_Wednesday[-c(1:i)]
# thursday=dowJ_i$weekday_Thursday[-c(1:i)]
#}
#
# , tuesday=dowJ_i$weekday_Tuesday, wednesday=dowJ_i$weekday_Wednesday, thursday=dowJ_i$weekday_Thursday


for (i in 0:1500) {
    train = fVAR[1:(763+i), ]
    monday= dowJ_i$weekday_Monday[1:(763+i)]
    tuesday=dowJ_i$weekday_Tuesday[1:(763+i)]
    wednesday=dowJ_i$weekday_Wednesday[1:(763+i)]
    thursday=dowJ_i$weekday_Thursday[1:(763+i)]
    VARf <- VAR(train, p=5, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday))
    recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(763+i)], tuesday = tuesday[(763+i)], wednesday = wednesday[(763+i)], thursday = thursday[(763+i)]))
    fcst=recursive$fcst$chg_price[1,"fcst"]
    v[i+1]=fcst
  }

for (i in 0:1500) {
  train = fVAR[1:(763+i), ]
  VARf <- VAR(train, p=5)
  recursive = predict(VARf, n.ahead=1)
  fcst=recursive$fcst$chg_price[1,"fcst"]
  v1[i+1]=fcst
}

for (i in 0:1500) {
  train = fVAR1[1:(763+i), ]
  monday= dowJ_i$weekday_Monday[1:(763+i)]
  tuesday=dowJ_i$weekday_Tuesday[1:(763+i)]
  wednesday=dowJ_i$weekday_Wednesday[1:(763+i)]
  thursday=dowJ_i$weekday_Thursday[1:(763+i)]
  VARf <- VAR(train, p=5, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday))
  recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(763+i)], tuesday = tuesday[(763+i)], wednesday = wednesday[(763+i)], thursday = thursday[(763+i)]))
  fcst=recursive$fcst$chg_price[1,"fcst"]
  v2[i+1]=fcst
}

# Results / Comparison ----------------------------------------------------

com<- dowJ_i[763:2263,"chg_price"]

 MDirAcc <- function(Actual, Forecast, lag=1) {
   return( mean(sign(diff(Actual, lag=lag))==sign(diff(Forecast, lag=lag))) )
 }

 accuracy(v[-1501],com[-1501])
 accuracy(v1[-1501],com[-1501])
 accuracy(v2[-1501],com[-1501])
 
 DACTest(v[-1501],com[-1501])
 DACTest(v1[-1501],com[-1501])
 DACTest(v2[-1501],com[-1501])
 
 MDirAcc(v[-1501],com[-1501])
 MDirAcc(v1[-1501],com[-1501]) 
 MDirAcc(v2[-1501],com[-1501]) 

 mse(v,com)
 mse(v1,com)
 
kk <-VAR(fVAR, p= 5)

hat <- cbind(v,com)
