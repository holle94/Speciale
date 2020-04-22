library(mice)
library(rtweet)



## store api keys (these are fake example values; replace with your own keys)
#api_key <- "jKR7hlU6vsdAPcEfhoIvEY2HQ"
#api_secret_key <- "GrzBUufawPJN9TIRvYChnKbkO3Famk2bWE6PtaAc4pY0MkbZRF"
#access_token <- "1247593650932068353-5xjIOzy9NMzKocN2yGinYLTnWnX5bU"
#access_token_secret <- "YOysktEd2pyFR9YcULlizcU7vJDtWoE75VMS5UkwnvhwK"
#
### authenticate via web browser
#token <- create_token(
#  app = "rstatsjournalismresearch",
#  consumer_key = api_key,
#  consumer_secret = api_secret_key,
#  access_token = access_token,
#  access_secret = access_token_secret)
#
#get_token()


#r_dow <- search_tweets(
#   "#dowjones", n = 180000, include_rts = FALSE, retryonratelimit = TRUE
#)
#
#tweets <- get_timelines("Dow Jones", 
#                        n = 18000, 
#                        language = 'en',
#                        since = '2014-01-01', 
#                        until = '2014-12-31',
#                        token = token)
#
#
#test<-search_fullarchive(
#  "$AAPL",
#  n = 10,
#  fromDate = "2014-07-10",
#  toDate = "2014-08-12",
#  env_name = "Forecasting",
#  token = token
#)
#
#
#twitter <- read_csv("Twitter/twitter.csv")
#twitter2 <- read_csv("Twitter/twitter2.csv")
#twitter3 <- read_csv("Twitter/twitter3.csv", 
#                     col_names = FALSE) %>% 
#  rename("Id" = X1, "Time"= X2, "UserId" = X3, "Company" = X4, "Text" = X5)

#test <- read_csv(url("https://raw.githubusercontent.com/AndMu/Market-Wisdom/master/DataSets/Market/Twitter/resultsGoog.csv")) 
#
#test$Date %<>% dmy_hms()
#test$Date %<>% as.Date() 
#
#test %<>% group_by(Date) %>% 
#  summarise(sum(TotalWords))

mydata<-read_csv(url("https://raw.githubusercontent.com/AndMu/Market-Wisdom/master/DataSets/Market/Twitter/twitter_clean_text.001.csv"))
mydata2<-read_csv(url("https://raw.githubusercontent.com/AndMu/Market-Wisdom/master/DataSets/Market/Twitter/twitter_clean_text.002.csv"),
                  col_names = FALSE) %>% 
  rename("Id" = X1, "Time"= X2, "UserId" = X3, "Company" = X4, "Text" = X5)
mydata3<-read_csv(url("https://raw.githubusercontent.com/AndMu/Market-Wisdom/master/DataSets/Market/Twitter/twitter_clean_text.003.csv"),  
                                   col_names = FALSE) %>% 
  rename("Id" = X1, "Time"= X2, "UserId" = X3, "Company" = X4, "Text" = X5)

mydata_f<-read_csv(url("https://raw.githubusercontent.com/AndMu/Market-Wisdom/master/DataSets/Market/Twitter/psenti.csv"))

#Finalv %<>% 
#  mutate(mo = ifelse(Time>17, 1, 0 )) %>% 
#  filter(mo == 1)

twit <- rbind(mydata, 
              mydata2,
              mydata3)

#dowJ <- read.csv("Yahoof_data_dowJ.csv")

dowJ <- read_csv("Twitter/GOOG.csv")
twit <- twit %>% 
  filter(Company == "GOOG")

twit$Text %<>% as.character() 
twit$Time %<>% dmy_hms()
twit$Time %<>% as.Date()
df_tidy <- twit[,c(2,5)] %>% 
  unnest_tokens(output = word, input = Text) %>% 
  mutate(weekday = weekdays(Time)) %>% 
  mutate(Time = if_else( weekday == "Saturday", Time-1 , Time)) %>% 
  mutate(Time = if_else( weekday == "Sunday", Time-2, Time)) %>% 
  mutate(weekday = if_else( weekday == "Saturday", "Friday" , weekday)) %>% 
  mutate(weekday = if_else( weekday == "Sunday", "Friday" , weekday)) %>% 
  rename(Date = Time) %>% 
  mutate(weekday = as.factor(weekday))  %>% 
  mutate(Date = as.Date(Date))

df_tidy %<>%
  anti_join(stop_words %>% bind_rows(stop_words), by = "word") 

df_tidy_c<- df_tidy %>% 
  group_by(Date) %>% 
  count() 

df_token <- df_tidy %>%    
  inner_join(get_sentiments(lexicon = "loughran"), by = "word") %>% 
  count(sentiment) %>% 
  spread(sentiment,n, fill = 0)

df_token_p <- df_tidy %>%    
  inner_join(get_sentiments(lexicon = "loughran"), by = "word")

s_index<-df_tidy %>%    
  inner_join(get_sentiments(lexicon = "loughran"), by = "word") 


# 123 ---------------------------------------------------------------------

s_index_A<-df_tidy %>%    
  inner_join(get_sentiments(lexicon = "afinn"), by = "word")

s_index_A %<>% 
  group_by(Date) %>% 
  summarise(AFINN=sum(value))


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



dowJ$Date %<>% as.Date() 
#dowJ$Date %<>% dmy()
dowJ %<>% 
  filter(Date >= "2014-05-06" & Date <= "2015-02-01")


dowJ_i<-dowJ %>% 
  left_join(s_index_f, by = "Date")

dowJ_i<-dowJ_i %>% 
  left_join(s_index_A, by = "Date")

dowJ_i<-dowJ_i %>% 
  left_join(s_index_f_B, by = "Date")

dowJ_i<-dowJ_i %>% 
  left_join(df_tidy_c, by = "Date")


#dowJ_i<- dowJ_i %>% drop_na()
#dowJ_i[,c(8)] %<>% na_mean()
#dowJ_i[,c(9)] %<>% na_mean()
#dowJ_i[,c(10)] %<>% na_mean()
#dowJ_i[,c(11)] %<>% na_mean()
#dowJ_i[,c(12)] %<>% na_mean()
#dowJ_i[,c(13)] %<>% na_mean()
#dowJ_i[,c(14)] %<>% na_mean()
#dowJ_i[,c(15)] %<>% na_mean()
#dowJ_i[,c(16)] %<>% na_mean()
#dowJ_i[,c(17)] %<>% na_mean()



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

# "score_c", "score_l","score_s",

pcr <- prcomp(x =  dowJ_i[,c("score_n", "score_p","score_u", "score_l", "score_c", "score_s")], scale. = TRUE)


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
  mutate(chg_price=c(-diff(`Adj Close`)/`Adj Close`[-1] *  100, NA)) %>% 
  mutate(Vlm=c(-diff(Volume), NA)) %>% 
  mutate(chg_log=c(-diff(log(`Adj Close`)) *  100, NA))

dowJ_i$weekday <- as.factor(dowJ_i$weekday)

dowJ_i <- dowJ_i[-2264,]

dowJ_i %<>% dummy_cols( select_columns = c("weekday"),
                        remove_first_dummy = TRUE)
dowJ_i <- dowJ_i %>% 
  arrange(Date)

dowJ_i <- dowJ_i[-1,]

PL<-plsr(chg_price ~ lag(score_p) + lag(score_n) + lag(score_l) + lag(score_s) + lag(score_c) +lag(score_u), ncomp = 1, data = dowJ_i, validation = "CV")
PLSc<-  as.vector(rbind(0,PL$scores))

dowJ_i <- cbind(dowJ_i,PLSc)
# Granger -----------------------------------------------------------------


grangertest(chg_price ~ score_AFINN, order=5, na.action= na.omit , data=dowJ_i) 



spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                   distribution.model = "norm")


garch <- ugarchfit(spec = spec, data = dowJ_i$chg_price, solver = "hybrid")

GARCH<-garch@fit$sigma


fVAR <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price, pc) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 

fVAR1 <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price,Vlm) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 


#v=c(NA)
#v1=c(NA)  
#v2=c(NA) 
#
#
VARselect(fVAR)

OO <- VAR(fVAR, p =5, exogen = cbind(garch = GARCH,monday=dowJ_i$weekday_Monday, tuesday=dowJ_i$weekday_Tuesday, wednesday=dowJ_i$weekday_Wednesday, thursday=dowJ_i$weekday_Thursday))
summary(OO)
#residuals(plot(OO))
#
#vcov<-vcovHC(OO, type="HC1")
#NeweyWest(OO, lag = 5)
#
#NW <- coeftest(OO, vcov. = vcov)
#
#coeftest(OO,vcov. = NW)
#
#serial.test(OO)
#serial.test(OO, lags.pt=10, type="PT.asymptotic")

v=c(NA)
v1=c(NA)  
v2=c(NA)  

for (i in 0:86) {
  train = fVAR[1:(100+i), ]
  monday= dowJ_i$weekday_Monday[1:(100+i)]
  tuesday=dowJ_i$weekday_Tuesday[1:(100+i)]
  wednesday=dowJ_i$weekday_Wednesday[1:(100+i)]
  thursday=dowJ_i$weekday_Thursday[1:(100+i)]
  garch = ugarchfit(spec = spec, data = dowJ_i$chg_price[1:(100+i)], solver = "hybrid")
  GARCH=garch@fit$sigma[1:(100+i)]
  VARf <- VAR(train, p=5, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
  recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(100+i)], tuesday = tuesday[(100+i)], wednesday = wednesday[(100+i)], thursday = thursday[(100+i)], GARCH = GARCH[(100+i)]))
  fcst=recursive$fcst$chg_price[1,"fcst"]
  v[i+1]=fcst
}

for (i in 0:86) {
  train = fVAR1[1:(100+i), ]
  monday= dowJ_i$weekday_Monday[1:(100+i)]
  tuesday=dowJ_i$weekday_Tuesday[1:(100+i)]
  wednesday=dowJ_i$weekday_Wednesday[1:(100+i)]
  thursday=dowJ_i$weekday_Thursday[1:(100+i)]
  garch = ugarchfit(spec = spec, data = dowJ_i$chg_price[1:(100+i)], solver = "hybrid")
  GARCH=garch@fit$sigma[1:(100+i)]
  VARf <- VAR(train, p=2, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
  recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(100+i)], tuesday = tuesday[(100+i)], wednesday = wednesday[(100+i)], thursday = thursday[(100+i)], GARCH = GARCH[(100+i)]))
  fcst=recursive$fcst$chg_price[1,"fcst"]
  v1[i+1]=fcst
}

for (i in 0:1500) {
  train = dowJ_i$chg_price[1:(100+i)]
  monday= dowJ_i$weekday_Monday[1:(100+i)]
  tuesday=dowJ_i$weekday_Tuesday[1:(100+i)]
  wednesday=dowJ_i$weekday_Wednesday[1:(100+i)]
  thursday=dowJ_i$weekday_Thursday[1:(100+i)] 
  garch = ugarchfit(spec = spec, data = dowJ_i$chg_price[1:(100+i)], solver = "hybrid")
  GARCH=garch@fit$sigma[1:(100+i)]
  VARf <- arima(train, order = c(1, 0, 0), xreg = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
  recursive = predict(VARf, n.ahead=1, newxreg  = cbind(monday=monday[(100+i)], tuesday = tuesday[(100+i)], wednesday = wednesday[(100+i)], thursday = thursday[(100+i)], GARCH = GARCH[(100+i)]))
  fcst=recursive$pred
  v2[i+1]=fcst
}


com<- dowJ_i[100:186,"chg_price"]


#accuracy(v,com)
#accuracy(v1,com)

fVAR_b <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price, score_p, score_n, score_u,score_c,score_s,score_l) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 


DACTest(v,com)
DACTest(v1,com)
DACTest(v2,com)

library(BigVAR)

mod1<-constructModel(as.matrix(fVAR_b) ,p=5,"Basic",gran=c(150,10),RVAR=FALSE,h=1,cv="Rolling",MN=FALSE,verbose=FALSE,IC=TRUE)
results=cv.BigVAR(mod1)
results

#SparsityPlot.BigVAR.results(results)

coef(results)


data(Y)
VARX=list(k=2,s=2)
#returns k x (kp+ms+1) coefficient matrix
model=BigVAR.fit(Y,p=5,"Basic",lambda=1e-2,VARX=VARX)




for (i in 0:86) {
  CC<-plsr(chg_price[1:(100+i)] ~ lag(score_p[1:(100+i)]) + lag(score_n[1:(100+i)]) + lag(score_l[1:(100+i)]) + lag(score_s[1:(100+i)]) + lag(score_c[1:(100+i)]) +lag(score_u[1:(100+i)]), ncomp = 1, data = dowJ_i, validation = "CV")
CCs <- CC$score 

  }

trainz <-  dowJ_i[1:100,]

#CC<-plsr(lead(chg_price) ~ score_p + score_n + score_l + score_s + score_c +score_u, ncomp = 1, data = trainz, validation = "CV")
#CCs <- CC$score
#dat <- dowJ_i[100, c("score_p", "score_n", "score_l", "score_s", "score_c","score_u")]
#load <- CC$loading.weights[1:6]
#CCs_p <-as.numeric(dat) %*% load
#PLS1<-rbind(0,CCs)

v3=c(NA) 

fVAR2 <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 

for (i in 0:86) {
  trainz <-  dowJ_i[1:(100+i),]
  CC<-plsr(lead(chg_price) ~ score_p + score_n + score_l + score_s + score_c +score_u, ncomp = 1, data = trainz, validation = "CV")
  CCs <- CC$score
  PLS1<-rbind(0,CCs)
  train = fVAR1[1:(100+i), ]
  train = cbind(train, PLS1)
  monday= dowJ_i$weekday_Monday[1:(100+i)]
  tuesday=dowJ_i$weekday_Tuesday[1:(100+i)]
  wednesday=dowJ_i$weekday_Wednesday[1:(100+i)]
  thursday=dowJ_i$weekday_Thursday[1:(100+i)]
  garch = ugarchfit(spec = spec, data = dowJ_i$chg_price[1:(100+i)], solver = "hybrid")
  GARCH=garch@fit$sigma[1:(100+i)]
  VARf <- VAR(train, p=2, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
  recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(100+i)], tuesday = tuesday[(100+i)], wednesday = wednesday[(100+i)], thursday = thursday[(100+i)], GARCH = GARCH[(100+i)]))
  fcst=recursive$fcst$chg_price[1,"fcst"]
  v3[i+1]=fcst
}




