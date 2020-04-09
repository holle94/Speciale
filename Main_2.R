
#f_s1 <- read_csv("full_s1.rdata")
#f_s2 <- read_csv("f_s2.rdata")
#f_s3 <- read_csv("full_s3.rdata")
#f_s4 <- read_csv("full_s4.rdata")
#f_s5 <- read_csv("ful_s5.rdata")
#f_s6 <- read_csv("full_s1.rdata")
#f_s7 <- read_csv("f_s7.rdata")
#f_s8 <- read_csv("F_s8.rdata")
#f_s9 <- read_csv("F_s9.rdata")
#f_s10 <- read_csv("F_s10.rdata")
#f_s11 <- read_csv("F_s11.rdata")
#f_s12 <- read_csv("f_s12.rdata")
#
#Finalv <- rbind(f_s1, 
#                f_s2, 
#                f_s3, 
#                f_s4, 
#                f_s5, 
#                f_s6, 
#                f_s7, 
#                f_s8, 
#                f_s9, 
#                f_s10,
#                f_s11,
#                f_s12)
#

e_s1 <- read_csv("e_s1.rdata")
e_s2 <- read_csv("e_s2.rdata")
e_s3 <- read_csv("e_s3.rdata")
e_s4 <- read_csv("e_s4.rdata")
e_s5 <- read_csv("e_s5.rdata")
e_s6 <- read_csv("e_s6.rdata")
e_s7 <- read_csv("e_s7.rdata")
e_s8 <- read_csv("e_s8.rdata")
e_s9 <- read_csv("e_s9.rdata")
e_s10 <- read_csv("e_s10.rdata")
e_s11 <- read_csv("e_s11.rdata")
e_s12 <- read_csv("e_s12.rdata")

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

#write.csv(Finalv, "Finalv.rdata")

#Finalv <- read_csv("Finalv.rdata")
#Finalv <- Finalv[,-c(2)]

dowJ <- read.csv("Yahoof_data_dowJ.csv")

#Finalv$text <- paste(Finalv$headline,Finalv$snippet)
#
#Finalv$Time <- format(as.POSIXct(Finalv$created_time,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
#
#Finalv<-Finalv[!duplicated(Finalv$text), ]

econf$text <- paste(econf$headline,econf$snippet)

econf$Date = as_datetime(econf$created_time,tz = "America/New_York" )

econf$Time <- format(as.POSIXct(econf$Date,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")



econf<-econf[!duplicated(econf$text), ]



#Finalv %<>% 
#  mutate(mo = ifelse(Time>17, 1, 0 )) %>% 
#  filter(mo == 1)

econf$text %<>% as.character() 
df_tidy <- econf[,c(2,5)] %>% 
  unnest_tokens(output = word, input = text) %>% 
  mutate(weekday = weekdays(created_time)) %>% 
  mutate(created_time = if_else( weekday == "Saturday", created_time-1 , created_time)) %>% 
  mutate(created_time = if_else( weekday == "Sunday", created_time-2, created_time)) %>% 
  mutate(weekday = if_else( weekday == "Saturday", "Friday" , weekday)) %>% 
  mutate(weekday = if_else( weekday == "Sunday", "Friday" , weekday)) %>% 
  rename(Date = created_time) %>% 
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
dowJ %<>% 
  filter(Date >= "2011-01-01" & Date <= "2019-12-31")


dowJ_i<-dowJ %>% 
  left_join(s_index_f, by = "Date")

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
  mutate(score_n_B = n_score_B/n) %>% 
  mutate(score_p_B = p_score_B/n) %>% 
  mutate(score_B = (n_score_B-p_score_B)/n) %>% 
  mutate(weekday = weekdays(Date)) %>% 
  mutate(Volume = log(Volume)) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 

# "score_c", "score_l","score_s",

pcr <- prcomp(x =  dowJ_i[,c("score_n", "score_p","score_u")], scale. = TRUE)

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
dowJ_i <- dowJ_i %>% 
  arrange(Date)

grangertest(score_n ~ chg_price, order=5, na.action= na.omit , data=dowJ_i) 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                   distribution.model = "norm")

garch <- ugarchfit(spec = spec, data = dowJ_i$chg_price, solver = "hybrid")

GARCH<-garch@fit$sigma


fVAR <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price, score_p, Vlm) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 

fVAR1 <- dowJ_i %>% 
  arrange(Date) %>% 
  dplyr::select(chg_price, Volume) %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) 


v=c(NA)
v1=c(NA)  
v2=c(NA) 


VARselect(fVAR)

OO <- VAR(fVAR, p =5, exogen = cbind(garch = GARCH,monday=dowJ_i$weekday_Monday, tuesday=dowJ_i$weekday_Tuesday, wednesday=dowJ_i$weekday_Wednesday, thursday=dowJ_i$weekday_Thursday))
summary(OO)

vcov<-vcovHC(OO, type="HC1")

coeftest(OO, vcov. = vcov)

serial.test(OO)

acf(dowJ_i$chg_log[-1])
pacf(dowJ_i$chg_log[-1])
