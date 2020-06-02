  library(tidyverse)
  library(dplyr)
  library(tidytext)
  library(jsonlite)
  library(readr)
  library(httr)
  library(magrittr)
  library(fansi)
  library(zoo)
  library(textdata)
  library(lmtest)
  library(aTSA)
  library(ggthemes)
  library(rugarch)
  library(vars)
  library(mltools)
  library(fastDummies)
  library(forecast)
  library(FactoMineR)
  library(factoextra)
  library(lubridate)
  #library(tsbox)
  library(aTSA)
  library(sandwich)
  library(pls) 
  #library(imputeTS)
  library(BigVAR)
  library(mice)
  library(rtweet)
  library(wordcloud)
  
  #library(SentimentAnalysis) # https://cran.r-project.org/web/packages/SentimentAnalysis/SentimentAnalysis.pdf
  #library(recipes)
  #library(tidyr)
  #library(Quandl)

  
  # Data --------------------------------------------------------------------
  
  f_s1 <- read_csv("full_s1.rdata")
  f_s2 <- read_csv("f_s2.rdata")
  f_s3 <- read_csv("full_s3.rdata")
  f_s4 <- read_csv("full_s4.rdata")
  f_s5 <- read_csv("ful_s5.rdata")
  f_s6 <- read_csv("full_s1.rdata")
  f_s7 <- read_csv("f_s7.rdata")
  f_s8 <- read_csv("F_s8.rdata")
  f_s9 <- read_csv("F_s9.rdata")
  f_s10 <- read_csv("F_s10.rdata")
  f_s11 <- read_csv("F_s11.rdata")
  f_s12 <- read_csv("f_s12.rdata")
  
  econf1  <- rbind(f_s1, 
                  f_s2, 
                  f_s3, 
                  f_s4, 
                  f_s5, 
                  f_s6, 
                  f_s7, 
                  f_s8, 
                  f_s9, 
                  f_s10,
                  f_s11,
                  f_s12) 
  
  econf1$text <- paste(econf1$headline,econf1$snippet)
  
  econf1$created_time %<>%  as.Date() 
  
  econf1<-econf1[!duplicated(econf1$text), ]
  
  econf1 %<>% rename(X = "X1") 
  
  e_s1 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s1.rdata")
  e_s2 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s2.rdata")
  e_s3 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s3.rdata")
  e_s4 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s4.rdata")
  e_s5 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s5.rdata")
  e_s6 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s6.rdata")
  e_s7 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s7.rdata")
  e_s8 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s8.rdata")
  e_s9 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s9.rdata")
  e_s10 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s10.rdata")
  e_s11 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s11.rdata")
  e_s12 <- read_csv("https://raw.githubusercontent.com/holle94/Speciale/master/e_s12.rdata")
  
  econf <- rbind(e_s1, 
                 e_s2, 
                 e_s3, 
                 e_s4, 
                 e_s5, 
                 e_s6, 
                 e_s7, 
                 e_s8, 
                 e_s9, 
                 e_s10,
                 e_s11,
                 e_s12)
  
  econf$text <- paste(econf$headline,econf$snippet)
  
  econf$created_time %<>%  as.Date() 
  
  econf<-econf[!duplicated(econf$text), ]

  econf %<>% rename(X = "X1") 
  
  dowJ <- read.csv("Yahoof_data_dowJ.csv")
  
  df_market<-read.csv("Full_text.csv")
  df_DB <- read.csv("df_DB")
  df_econ <- read.csv("df_econ")
  
  df_DB$text <- paste(df_DB$headline,df_DB$snippet)
  df_econ$text <- paste(df_econ$headline,df_econ$snippet)
  df_DB$created_time %<>% as.Date() 
  df_econ$created_time %<>% as.Date() 
  df_market$created_time %<>% as.Date() 

  
  df_full <- rbind(econf1)
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
  
  WC<-df_token_p %>% 
    count(word, sort = TRUE) 
  
  
  WCw <- WC$word
  WCn <- WC$n

  wordcloud(WCw, WCn, random.order = FALSE, max.words = 100, colors = brewer.pal(9,"RdYlBu"),rot.per=0.30,)
  
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
  
  
  dowJ_i %<>% mutate(score_n = n_score/n*100) %>% 
    mutate(score_p = p_score/n*100) %>% 
    mutate(score_c = c_score/n*100) %>% 
    mutate(score_l = l_score/n*100) %>% 
    mutate(score_u = u_score/n*100) %>% 
    mutate(score_s = s_score/n*100) %>% 
    mutate(score = (n_score-p_score)/n*100)%>% 
    mutate(score_AFINN = (AFINN)/n*100) %>% 
    mutate(score_n_B = n_score_B/n*100) %>% 
    mutate(score_p_B = p_score_B/n*100) %>% 
    mutate(score_B = (n_score_B-p_score_B)/n*100) %>% 
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
  pc2 <- pcr$x[,2]
  
  #pc1<-pc/dowJ_i$n
  
  dowJ_i <- cbind(dowJ_i,pc, pc2)
  
  dowJ_i %<>% 
    arrange(desc(Date)) %>% 
    mutate(chg_price=c(-diff(Adj.Close)/Adj.Close[-1] *  100, NA)) %>% 
    mutate(Vlm=c(-diff(Volume), NA)) %>% 
    mutate(chg_log=c(-diff(log(Adj.Close)) *  100, NA))
  
  dowJ_i$weekday <- as.factor(dowJ_i$weekday)
  
  
  dowJ_i %<>% dummy_cols( select_columns = c("weekday"),
                        remove_first_dummy = TRUE)
  
  # Plots -------------------------------------------------------------------
  
  dowJ_i %>% ggplot() +
    geom_line(aes(Date, chg_price)) +
    theme_classic()+
    labs(title = " Dow Jones Daily Returns", caption = "Data from: Thomson Reuters Eikon") +
    ylab("Daily Returns") +
    theme_economist()
  
  dowJ_i %>% ggplot() +
    geom_line(aes(Date, score_n)) +
    theme_classic() +
    labs(title = "Negative Index", caption = "Data from: New York Times") +
    ylab("Negative score value") +
    theme_economist()
  
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
  
  dowJ_i <- dowJ_i[-1,]
  
  PL<-plsr(chg_log ~ lag(score_p) + lag(score_n) + lag(score_l) + lag(score_s) + lag(score_c) +lag(score_u), ncomp = 1, data = dowJ_i, validation = "CV")
  PLSc<-  as.vector(rbind(0,PL$scores))
  
  dowJ_i <- cbind(dowJ_i,PLSc) 
  
  PL<-plsr(chg_log ~ lag(score_p) + lag(score_n) + lag(score_l) + lag(score_s) + lag(score_c) +lag(score_u), ncomp = 2, data = dowJ_i, validation = "CV")
  
  pcr$rotation
  PL$loadings
  
  # adf.test ----------------------------------------------------------------
  
  adf.test(dowJ_i$Volume, nlag = 5)
  
  adf.test(dowJ_i$chg_price, nlag = 5)
  
  adf.test(dowJ_i$Adj.Close, nlag = 5)
  
  adf.test(dowJ_i$score, nlag = 5)
  
  ts.plot(diff(dowJ_i$Volume))
  # Granger-test ------------------------------------------------------------
  
  
  
  
  
  grangertest(chg_log ~ pc, order=5, na.action= na.omit , data=dowJ_i) 
  
  
  # ARCH/GARCH --------------------------------------------------------------
  
  AA <- arima(x = dowJ_i$chg_price, order = c(5,0,0))
  
  acf(resid(AA))
  acf(resid(AA)^2)
  pacf(resid(AA)^2)
  
  Box.test(resid(AA)^2, lag = 10)
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                               mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                               distribution.model = "norm")
  
  garch <- ugarchfit(spec = spec, data = dowJ_i$chg_log, solver = "hybrid")
  
  GARCH<-garch@fit$sigma
  
  dowJ_i <- cbind(dowJ_i, GARCH)
  
 #spec           <- getspec(gf11)
 #setfixed(spec) <- as.list(coef(gf11))
 #garchforecast1 <- ugarchforecast(spec, n.ahead = 1, n.roll = 2262, data = dowJ_i, out.sample = 2263)
  

# HAC ---------------------------------------------------------------------

  OO <- VAR(fVAR, p =5, exogen = cbind(garch=GARCH,monday=dowJ_i$weekday_Monday, tuesday=dowJ_i$weekday_Tuesday, wednesday=dowJ_i$weekday_Wednesday, thursday=dowJ_i$weekday_Thursday))
  summary(OO)
  
  VARselect(fVAR)
  
  vcov<-vcovHC(OO, type="HC1")
  #
  coeftest(OO, vcov. = vcov)
  

  
  causality(OO, cause = "score_n")  
  
# VAR ---------------------------------------------------------------------
  
  
  fVAR <- dowJ_i %>% 
    arrange(Date) %>% 
    dplyr::select(chg_log,score_p) %>% 
    mutate_if(is.numeric,~replace(., is.na(.), 0)) 
  
  OO <- VAR(fVAR, p =5, exogen = cbind(garch=GARCH,monday=dowJ_i$weekday_Monday, tuesday=dowJ_i$weekday_Tuesday, wednesday=dowJ_i$weekday_Wednesday, thursday=dowJ_i$weekday_Thursday))
  summary(OO)
  
  NW<-NeweyWest(OO, lag=5)
  
  round(coeftest(OO, vcov = NW),digits = 3)
  
  
  fVAR1 <- dowJ_i %>% 
    arrange(Date) %>% 
    dplyr::select(chg_log, Vlm) %>% 
    mutate_if(is.numeric,~replace(., is.na(.), 0)) 
  

  fVAR2 <- dowJ_i %>% 
    arrange(Date) %>% 
    dplyr::select(chg_log) %>% 
    mutate_if(is.numeric,~replace(., is.na(.), 0)) 
  
# Empty df for fcst.  
  
  v=c(NA)
  v1=c(NA)  
  v2=c(NA) 
  v3=c(NA) 
  v4=c(NA) 
  v5=c(NA) 
  v6=c(NA) 
  
  
  mean(dowJ_i$score_n_B)
  mean(dowJ_i$score_p_B)
  
  mean(dowJ_i$score_n)
  mean(dowJ_i$score_p)
  
  mean(dowJ_i$chg_price)
  
  sd(dowJ_i$chg_price)
  

# Standard ----------------------------------------------------------------


  for (i in 0:1100) {
    train = fVAR[1:(1163+i), ]
    monday= dowJ_i$weekday_Monday[1:(1163+i)]
    tuesday=dowJ_i$weekday_Tuesday[1:(1163+i)]
    wednesday=dowJ_i$weekday_Wednesday[1:(1163+i)]
    thursday=dowJ_i$weekday_Thursday[1:(1163+i)]
    garch = ugarchfit(spec = spec, data = dowJ_i$chg_log[1:(1163+i)], solver = "hybrid")
    GARCH=garch@fit$sigma[1:(1163+i)]
    VARf <- VAR(train, p=5, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
    recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(1163+i)], tuesday = tuesday[(1163+i)], wednesday = wednesday[(1163+i)], thursday = thursday[(1163+i)], GARCH = GARCH[(1163+i)]))
    fcst=recursive$fcst$chg_log[1,"fcst"]
    v[i+1]=fcst
  }


# Vlm ---------------------------------------------------------------------
    
  for (i in 0:1100) {
    train = fVAR1[1:(1163+i), ]
    monday= dowJ_i$weekday_Monday[1:(1163+i)]
    tuesday=dowJ_i$weekday_Tuesday[1:(1163+i)]
    wednesday=dowJ_i$weekday_Wednesday[1:(1163+i)]
    thursday=dowJ_i$weekday_Thursday[1:(1163+i)]
    garch = ugarchfit(spec = spec, data = dowJ_i$chg_log[1:(1163+i)], solver = "hybrid")
    GARCH=garch@fit$sigma[1:(1163+i)]
    VARf <- VAR(train, p=5, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
    recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(1163+i)], tuesday = tuesday[(1163+i)], wednesday = wednesday[(1163+i)], thursday = thursday[(1163+i)], GARCH = GARCH[(1163+i)]))
    fcst=recursive$fcst$chg_log[1,"fcst"]
    v1[i+1]=fcst
  }
  

# AR(1) -------------------------------------------------------------------

  for (i in 0:1100) {
    train = dowJ_i$chg_price[1:(1163+i)]
    monday= dowJ_i$weekday_Monday[1:(1163+i)]
    tuesday=dowJ_i$weekday_Tuesday[1:(1163+i)]
    wednesday=dowJ_i$weekday_Wednesday[1:(1163+i)]
    thursday=dowJ_i$weekday_Thursday[1:(1163+i)]
    garch = ugarchfit(spec = spec, data = dowJ_i$chg_log[1:(1163+i)], solver = "hybrid")
    GARCH=garch@fit$sigma[1:(1163+i)]
    VARf <- arima(train, order = c(1, 0, 0), xreg = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
    recursive = predict(VARf, n.ahead=1, newxreg  = cbind(monday=monday[(1163+i)], tuesday = tuesday[(1163+i)], wednesday = wednesday[(1163+i)], thursday = thursday[(1163+i)], GARCH = GARCH[(1163+i)]))
    fcst=recursive$pred
    v2[i+1]=fcst
  }
  


  
  # PLS ---------------------------------------------------------------------
    
  for (i in 0:1100) {
    trainz <-  dowJ_i[1:(1163+i),]
    CC<-plsr(lead(chg_price) ~ score_p + score_n + score_l + score_s + score_c +score_u, ncomp = 1, data = trainz, validation = "CV")
    CCs <- CC$score
    PLS1<-rbind(0,CCs)
    train = fVAR2[1:(1163+i), ]
    train = cbind(train, PLS1)
    monday= dowJ_i$weekday_Monday[1:(1163+i)]
    tuesday=dowJ_i$weekday_Tuesday[1:(1163+i)]
    wednesday=dowJ_i$weekday_Wednesday[1:(1163+i)]
    thursday=dowJ_i$weekday_Thursday[1:(1163+i)]
    garch = ugarchfit(spec = spec, data = dowJ_i$chg_log[1:(1163+i)], solver = "hybrid")
    GARCH=garch@fit$sigma[1:(1163+i)]
    VARf <- VAR(train, p=1, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
    recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(1163+i)], tuesday = tuesday[(1163+i)], wednesday = wednesday[(1163+i)], thursday = thursday[(1163+i)], GARCH = GARCH[(1163+i)]))
    fcst=recursive$fcst$train[1,"fcst"]
    v3[i+1]=fcst
  }

  
  # Random Walk  ------------------------------------------------------------

  
  for (i in 0:1100) {
    train = dowJ_i$chg_log[1:(1163+i)]
    rw <- naive(train, h=1)
    fct<-rw$model$future
    v5[i+1]=fct
  }
  
  # BigVAR - Penalized regression (Lasso/Elasticnet) ------------------------
  
  
  fVAR3 <- dowJ_i %>% 
    arrange(Date) %>% 
    dplyr::select(chg_price,pc,score_p, score_n, score_l, score_u, score_c, weekday_Monday, weekday_Thursday, weekday_Tuesday, weekday_Wednesday, GARCH) %>% 
    mutate_if(is.numeric,~replace(., is.na(.), 0)) 
  
  fVAR6 <- dowJ_i %>% 
    arrange(Date) %>% 
    dplyr::select(chg_price, weekday_Monday, weekday_Thursday, weekday_Tuesday, weekday_Wednesday, GARCH) %>% 
    mutate_if(is.numeric,~replace(., is.na(.), 0)) 
  
  
  for (i in 0:1100) {
    trainz <-  dowJ_i[1:(1163+i),]
    CC<-plsr(lead(chg_price) ~ score_p + score_n + score_l + score_s + score_c +score_u, ncomp = 1, data = trainz, validation = "CV")
    CCs <- CC$score
    PLS1<-rbind(0,CCs)
    train = fVAR6[1:(1163+i), ]
    train = cbind(train, PLS1)
    VARf<-constructModel(as.matrix(train), p=5, "BasicEN",gran = c(5,5), VARX = list(k=2, s=1))
    VARf = cv.BigVAR(VARf)  
    recursive = BigVAR::predict(VARf, n.ahead = 1)
    v6[i+1]=recursive[1,1]
  }
  
  for (i in 0:1100){
    train = fVAR3[1:(1163+i), ]
    X = list(k=7, s=1)
    VARf<-constructModel(as.matrix(train), p=5, "BasicEN",gran = c(10,5), VARX=X)
    VARf = cv.BigVAR(VARf)  
    recursive = BigVAR::predict(VARf, n.ahead = 1)
    v4[i+1]=recursive[1,1]
  }
  
  
    plot(results)

  SparsityPlot.BigVAR.results(VARf)  
  VARf@OptimalLambda
  
  # Results / Comparison ----------------------------------------------------
  
  com<- dowJ_i[1164:2263,"chg_price"]
  com1<- dowJ_i[1501:2263,"chg_price"]
fe <- com-v[-1101]
fe2 <- com-v2[-1101]
fe3 <- com-v3[-1101]
fe4 <- com-v4[-1101]

  
  DACTest(v[-1101],com)  # VAR simple
  DACTest(v1[-1101],com) # VLM
  DACTest(v2[-1101],com) # AR(1)
  DACTest(v3[-1101],com) # PLS 
  DACTest(v5[-1101],com) # RW
  DACTest(v4[-1101],com) # BIGVAR
  DACTest(v6[-1101],com)
  
  mse(v[-1101],com) # VAR simple
  mse(v1[-1101],com) # VLM
  mse(v2[-1101],com) # AR(1)
  mse(v3[-1101],com) # PLS 
  mse(v4[-1101],com) # BIGVAR
  mse(v6[-1101],com)
  
  mse(v4[-1101],com)/mse(v2[-1101],com)
  mse(v6[-1101],com)/mse(v2[-1101],com)
  
  
  dm.test(fe,fe2, h=1, alternative = "less")
  dm.test(fe3,fe2, h=1, alternative = "less")
  
  dm.test(fe4,fe2, h=1, alternative = "less")


  qqnorm(dowJ_i$chg_log, pch =1 , frame =FALSE)
  qqline(dowJ_i$chg_log, col = "steelblue", lwd = 2)


# Manual selection of S_t lags --------------------------------------------
    
  fVAR <- dowJ_i %>% 
    arrange(Date) %>% 
    dplyr::select(chg_price,score_p) %>% 
    mutate_if(is.numeric,~replace(., is.na(.), 0))   
  
  for (i in 0:1500) {
    train = fVAR[1:(753+i), ]
    monday= dowJ_i$weekday_Monday[1:(753+i)]
    tuesday=dowJ_i$weekday_Tuesday[1:(753+i)]
    wednesday=dowJ_i$weekday_Wednesday[1:(753+i)]
    thursday=dowJ_i$weekday_Thursday[1:(753+i)]
    garch = ugarchfit(spec = spec, data = dowJ_i$chg_price[1:(753+i)], solver = "hybrid")
    GARCH=garch@fit$sigma[1:(753+i)]
    VARf <- VAR(train, p=5, exogen = cbind(monday=monday, tuesday = tuesday, wednesday = wednesday, thursday = thursday, GARCH = GARCH))
    resmat <- Bcoef(VARf)*0
    resmat[,] <- 1
    resmat[1,c(2,4)] <- 0
    VARf<-restrict(VARf, method="manual", resmat=resmat)
    recursive = predict(VARf, n.ahead=1, dumvar = cbind(monday=monday[(753+i)], tuesday = tuesday[(753+i)], wednesday = wednesday[(753+i)], thursday = thursday[(753+i)], GARCH = GARCH[(753+i)]))
    fcst=recursive$fcst$chg_price[1,"fcst"]
    v[i+1]=fcst
  }
  
  DACTest(v[-87],com)
