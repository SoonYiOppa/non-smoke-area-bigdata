library(dplyr)
library(ggplot2)
library(showtext)
library(scales)
library(gridExtra)

df <- read.csv("C:/LJH/PYDATAexam/non-smoke-area-bigdata/data/10월_4주차_서울_생활인구(25개구).csv"
               ,header=T,encoding = "UTF-8")

df <- arrange(df,date)
df$time <- paste(df$time,"시",sep="")
age_20 <- df$m_20+df$m_25+df$f_20+df$f_25
day <- rep(c("월","화","수","목","금","토","일"),each=600)



df <- data.frame(df,age_20,day)
v_names <- names(df)

#텍스트 지정
font_add_google('Nanum Gothic',family = "NamumGothinc")
showtext_auto()

#색 지정 파레트 생성
palette1 <- c()


#palette2 <- c("0시"="#33658A", "1시"="#33658A", "2시"="#33658A", "3시"="#33658A", "4시"="#33658A", "5시"="#33658A",
#              "6시"="#2F4858","7시"="#2F4858","8시"="#2F4858","9시"="#2F4858","10시"="#2F4858","11시"="#2F4858",
#              "12시"="#F26419","13시"="#F26419","14시"="#F26419","15시"="#F26419","16시"="#F26419","17시"="#F26419",
#              "18시"="#2F4858","19시"="#2F4858","20시"="#2F4858","21시"="#2F4858","22시"="#2F4858","23시"="#2F4858")



#종로구 추출
df_jonglo <- subset(df,
                    select = v_names,
                    subset = (id==11110))

#종로구 요일별 그룹 평균
jonglo_day <-aggregate(age_20~day,data=df_jonglo,mean)

#종로구 요일별 그룹 평균 라인 그래프
jonglo_plot1 <- ggplot(jonglo_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 종로구 요일별 평균 생활인구")


#종로구 시간별 그룹 평균
jonglo_time <-aggregate(age_20~time,data=df_jonglo,mean)

#종로구 시간별 생활인구 라인+막대 그래프
jonglo_plot2 <- ggplot(jonglo_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 종로구 시간별 평균 생활인구")


#중구 추출
df_jung <- subset(df,
                    select = v_names,
                    subset = (id==11140))

#중구 요일별 그룹 평균
jung_day <-aggregate(age_20~day,data=df_jung,mean)

#중구 요일별 그룹 평균 라인 그래프
jung_plot1 <- ggplot(jung_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 중구 요일별 평균 생활인구")


#중구 시간별 그룹 평균
jung_time <-aggregate(age_20~time,data=df_jung,mean)

#중구 시간별 생활인구 라인+막대 그래프
jung_plot2 <- ggplot(jung_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 중구 시간별 평균 생활인구")



#용산구 추출
df_ys <- subset(df,
                select = v_names,
                subset = (id==11170))

#용산구 요일별 그룹 평균
ys_day <-aggregate(age_20~day,data=df_ys,mean)

#용산구 요일별 그룹 평균 라인 그래프
ys_plot1 <- ggplot(ys_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 용산구 요일별 평균 생활인구")


#용산구 시간별 그룹 평균
ys_time <-aggregate(age_20~time,data=df_ys,mean)

#용산구 시간별 생활인구 라인+막대 그래프
ys_plot2 <- ggplot(ys_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 용산구 시간별 평균 생활인구")


#성동구 추출
df_sd <- subset(df,
                     select = v_names,
                     subset = (id==11200))

#성동구 요일별 그룹 평균
sd_day <-aggregate(age_20~day,data=df_sd,mean)

#성동구 요일별 그룹 평균 라인 그래프
sd_plot1 <- ggplot(sd_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 성동구 요일별 평균 생활인구")


#성동구 시간별 그룹 평균
sd_time <-aggregate(age_20~time,data=df_sd,mean)

#성동구 시간별 생활인구 라인+막대 그래프
sd_plot2 <- ggplot(sd_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 성동구 시간별 평균 생활인구")

#광진구 추출
df_gj <- subset(df,
                     select = v_names,
                     subset = (id==11215))

#광진구 요일별 그룹 평균
gj_day <-aggregate(age_20~day,data=df_gj,mean)

#광진구 요일별 그룹 평균 라인 그래프
gj_plot1 <- ggplot(gj_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 광진구 요일별 평균 생활인구")


#광진구 시간별 그룹 평균
gj_time <-aggregate(age_20~time,data=df_sd,mean)

#광진구 시간별 생활인구 라인+막대 그래프
gj_plot2 <- ggplot(gj_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 광진구 시간별 평균 생활인구")


#동대문구 추출
df_ddm <- subset(df,
                select = v_names,
                subset = (id==11230))

#동대문구 요일별 그룹 평균
ddm_day <-aggregate(age_20~day,data=df_ddm,mean)

#동대문구 요일별 그룹 평균 라인 그래프
ddm_plot1 <- ggplot(ddm_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 동대문구 요일별 평균 생활인구")


#동대문구 시간별 그룹 평균
ddm_time <-aggregate(age_20~time,data=df_ddm,mean)

#동대문구 시간별 생활인구 라인+막대 그래프
ddm_plot2 <- ggplot(ddm_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 동대문구 시간별 평균 생활인구")



#중랑구 추출
df_jl <- subset(df,
                 select = v_names,
                 subset = (id==11230))

#중랑구 요일별 그룹 평균
jl_day <-aggregate(age_20~day,data=df_jl,mean)

#중랑구 요일별 그룹 평균 라인 그래프
jl_plot1 <- ggplot(jl_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 중랑구 요일별 평균 생활인구")


#중랑구 시간별 그룹 평균
jl_time <-aggregate(age_20~time,data=df_jl,mean)

#중랑구 시간별 생활인구 라인+막대 그래프
jl_plot2 <- ggplot(jl_time, aes(x=time,y=age_20,group=1,fill=time))+
  # geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 중랑구 시간별 평균 생활인구")



#성북구 추출
df_sb <- subset(df,
                 select = v_names,
                 subset = (id==11290))

#성북구 요일별 그룹 평균
sb_day <-aggregate(age_20~day,data=df_sb,mean)

#성북구 요일별 그룹 평균 라인 그래프
sb_plot1 <- ggplot(sb_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 성북구 요일별 평균 생활인구")


#성북구 시간별 그룹 평균
sb_time <-aggregate(age_20~time,data=df_sb,mean)

#성북구 시간별 생활인구 라인+막대 그래프
sb_plot2 <- ggplot(sb_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 성북구 시간별 평균 생활인구")




#강북구 추출
df_kb <- subset(df,
                 select = v_names,
                 subset = (id==11305))

#강북구 요일별 그룹 평균
kb_day <-aggregate(age_20~day,data=df_kb,mean)

#강북구 요일별 그룹 평균 라인 그래프
kb_plot1 <- ggplot(kb_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강북구 요일별 평균 생활인구")


#강북구 시간별 그룹 평균
kb_time <-aggregate(age_20~time,data=df_kb,mean)

#강북구 시간별 생활인구 라인+막대 그래프
kb_plot2 <- ggplot(kb_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강북구 시간별 평균 생활인구")


#도봉구 추출
df_db <- subset(df,
                 select = v_names,
                 subset = (id==11320))

#도봉구 요일별 그룹 평균
db_day <-aggregate(age_20~day,data=df_db,mean)

#도봉구 요일별 그룹 평균 라인 그래프
db_plot1 <- ggplot(db_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 도봉구 요일별 평균 생활인구")


#도봉구 시간별 그룹 평균
db_time <-aggregate(age_20~time,data=df_db,mean)

#도봉구 시간별 생활인구 라인+막대 그래프
db_plot2 <- ggplot(db_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 도봉구 시간별 평균 생활인구")

#노원구 추출
df_nw <- subset(df,
                select = v_names,
                subset = (id==11350))

#노원구 요일별 그룹 평균
nw_day <-aggregate(age_20~day,data=df_nw,mean)

#노원구 요일별 그룹 평균 라인 그래프
nw_plot1 <- ggplot(nw_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 노원구 요일별 평균 생활인구")


#노원구 시간별 그룹 평균
nw_time <-aggregate(age_20~time,data=df_nw,mean)

#노원구 시간별 생활인구 라인+막대 그래프
nw_plot2 <- ggplot(nw_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 노원구 시간별 평균 생활인구")



#은평구 추출
df_ep <- subset(df,
                select = v_names,
                subset = (id==11380))

#은평구 요일별 그룹 평균
ep_day <-aggregate(age_20~day,data=df_ep,mean)

#은평구 요일별 그룹 평균 라인 그래프
ep_plot1 <- ggplot(ep_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 은평구 요일별 평균 생활인구")


#은평구 시간별 그룹 평균
ep_time <-aggregate(age_20~time,data=df_ep,mean)

#은평구 시간별 생활인구 라인+막대 그래프
ep_plot2 <- ggplot(ep_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 은평구 시간별 평균 생활인구")


#서대문구 추출
df_sdm <- subset(df,
                select = v_names,
                subset = (id==11410))

#서대문구 요일별 그룹 평균
sdm_day <-aggregate(age_20~day,data=df_sdm,mean)

#서대문구 요일별 그룹 평균 라인 그래프
sdm_plot1 <- ggplot(sdm_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 서대문구 요일별 평균 생활인구")


#서대문구 시간별 그룹 평균
sdm_time <-aggregate(age_20~time,data=df_sdm,mean)

#서대문구 시간별 생활인구 라인+막대 그래프
sdm_plot2 <- ggplot(sdm_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 서대문구 시간별 평균 생활인구")


#마포구 추출
df_mp <- subset(df,
                 select = v_names,
                 subset = (id==11440))

#마포구 요일별 그룹 평균
mp_day <-aggregate(age_20~day,data=df_mp,mean)

#마포구 요일별 그룹 평균 라인 그래프
mp_plot1 <- ggplot(mp_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 마포구 요일별 평균 생활인구")


#마포구 시간별 그룹 평균
mp_time <-aggregate(age_20~time,data=df_mp,mean)

#마포구 시간별 생활인구 라인+막대 그래프
mp_plot2 <- ggplot(mp_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 마포구 시간별 평균 생활인구")





#서초구 추출

df_sc <- subset(df,
                    select = v_names,
                    subset = (id==11650))

#서초구 요일별 그룹 평균
sc_day <-aggregate(age_20~day,data=df_sc,mean)

#서초구 요일별 그룹 평균 라인 그래프
sc_plot1 <- ggplot(sc_day, aes(x=day,y=age_20,group=1))+
                geom_line(linetype="solid",size=1,colour="#F26419")+
                geom_point(size=3,shape=19,colour="#F26419")+
                scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
                labs(x="",
                     y="",
                     title="2020년 10월 4주차 서초구 요일별 평균 생활인구")





#양천구 추출
df_yc <- subset(df,
                select = v_names,
                subset = (id==11470))

#양천구 요일별 그룹 평균
yc_day <-aggregate(age_20~day,data=df_yc,mean)

#양천구 요일별 그룹 평균 라인 그래프
yc_plot1 <- ggplot(yc_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 양천구 요일별 평균 생활인구")


#양천구 시간별 그룹 평균
yc_time <-aggregate(age_20~time,data=df_yc,mean)

#양천구 시간별 생활인구 라인+막대 그래프
yc_plot2 <- ggplot(yc_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 양천구 시간별 평균 생활인구")



#강서구 추출
df_ks <- subset(df,
                select = v_names,
                subset = (id==11500))

#강서구 요일별 그룹 평균
ks_day <-aggregate(age_20~day,data=df_ks,mean)

#강서구 요일별 그룹 평균 라인 그래프
ks_plot1 <- ggplot(ks_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강서구 요일별 평균 생활인구")


#강서구 시간별 그룹 평균
ks_time <-aggregate(age_20~time,data=df_ks,mean)

#강서구 시간별 생활인구 라인+막대 그래프
ks_plot2 <- ggplot(ks_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강서구 시간별 평균 생활인구")

#구로구 추출
df_gr <- subset(df,
                select = v_names,
                subset = (id==11530))

#구로구 요일별 그룹 평균
gr_day <-aggregate(age_20~day,data=df_gr,mean)

#구로구 요일별 그룹 평균 라인 그래프
gr_plot1 <- ggplot(gr_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 구로구 요일별 평균 생활인구")


#구로구 시간별 그룹 평균
gr_time <-aggregate(age_20~time,data=df_gr,mean)

#강서구 시간별 생활인구 라인+막대 그래프
gr_plot2 <- ggplot(gr_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 구로구 시간별 평균 생활인구")


#금천구 추출
df_gc <- subset(df,
                select = v_names,
                subset = (id==11545))

#금천구 요일별 그룹 평균
gc_day <-aggregate(age_20~day,data=df_gc,mean)

#금천구 요일별 그룹 평균 라인 그래프
gc_plot1 <- ggplot(gc_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 금천구 요일별 평균 생활인구")


#금천구 시간별 그룹 평균
gc_time <-aggregate(age_20~time,data=df_gc,mean)

#금천구 시간별 생활인구 라인+막대 그래프
gc_plot2 <- ggplot(gc_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 금천구 시간별 평균 생활인구")




#영등포구 추출
df_ydp <- subset(df,
                select = v_names,
                subset = (id==11560))

#영등포구 요일별 그룹 평균
ydp_day <-aggregate(age_20~day,data=df_ydp,mean)

#영등포구 요일별 그룹 평균 라인 그래프
ydp_plot1 <- ggplot(ydp_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 영등포구 요일별 평균 생활인구")


#영등포구 시간별 그룹 평균
ydp_time <-aggregate(age_20~time,data=df_ydp,mean)

#영등포구 시간별 생활인구 라인+막대 그래프
ydp_plot2 <- ggplot(ydp_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 영등포구 시간별 평균 생활인구")



#동작구 추출
df_dj <- subset(df,
                 select = v_names,
                 subset = (id==11590))

#동작구 요일별 그룹 평균
dj_day <-aggregate(age_20~day,data=df_dj,mean)

#동작구 요일별 그룹 평균 라인 그래프
dj_plot1 <- ggplot(dj_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 동작구 요일별 평균 생활인구")


#동작구 시간별 그룹 평균
dj_time <-aggregate(age_20~time,data=df_dj,mean)

#동작구 시간별 생활인구 라인+막대 그래프
dj_plot2 <- ggplot(dj_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 동작구 시간별 평균 생활인구")


#관악구 추출
df_kw <- subset(df,
                select = v_names,
                subset = (id==11620))

#관악구 요일별 그룹 평균
kw_day <-aggregate(age_20~day,data=df_kw,mean)

#관악구 요일별 그룹 평균 라인 그래프
kw_plot1 <- ggplot(kw_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 관악구 요일별 평균 생활인구")


#관악구 시간별 그룹 평균
kw_time <-aggregate(age_20~time,data=df_kw,mean)

#관악구 시간별 생활인구 라인+막대 그래프
kw_plot2 <- ggplot(kw_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 관악구 시간별 평균 생활인구")





#서초구 추출

df_sc <- subset(df,
                select = v_names,
                subset = (id==11650))

#서초구 요일별 그룹 평균
sc_day <-aggregate(age_20~day,data=df_sc,mean)

#서초구 요일별 그룹 평균 라인 그래프
sc_plot1 <- ggplot(sc_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 서초구 요일별 평균 생활인구")

#서초구 시간별 그룹 평균
sc_time <-aggregate(age_20~time,data=df_sc,mean)

#서초구 시간별 생활인구 라인+막대 그래프
sc_plot2 <- ggplot(sc_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
               geom_line(linetype="solid",size=1,colour="black")+
               geom_point(size=3,shape=19,colour = "black")+
               theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
               scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                                           "7시","8시","9시","10시","11시","12시","13시",
                                           "14시","15시","16시","17시","18시","19시","20시",
                                           "21시","22시","23시"))+
               scale_y_continuous(labels = scales::comma)+
               labs(x="",
               y="",
               title="2020년 10월 4주차 서초구 시간별 평균 생활인구")

#강남구 추출
df_kn <- subset(df,
                select = v_names,
                subset = (id==11680))

#강남구 요일별 그룹 평균
kn_day <-aggregate(age_20~day,data=df_kn,mean)

#강남구 요일별 그룹 평균 라인 그래프
kn_plot1 <- ggplot(kn_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강남구 요일별 평균 생활인구")


#강남구 시간별 그룹 평균
kn_time <-aggregate(age_20~time,data=df_kn,mean)

#강남구 시간별 생활인구 라인+막대 그래프
kn_plot2 <- ggplot(kn_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강남구 시간별 평균 생활인구")

#송파구 추출
df_sp <- subset(df,
                select = v_names,
                subset = (id==11710))

#송파구 요일별 그룹 평균
sp_day <-aggregate(age_20~day,data=df_sp,mean)

#송파구 요일별 그룹 평균 라인 그래프
sp_plot1 <- ggplot(sp_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 송파구 요일별 평균 생활인구")


#송파구 시간별 그룹 평균
sp_time <-aggregate(age_20~time,data=df_sp,mean)

#송파구 시간별 생활인구 라인+막대 그래프
sp_plot2 <- ggplot(sp_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 송파구 시간별 평균 생활인구")


#강동구 추출
df_kd <- subset(df,
                select = v_names,
                subset = (id==11740))

#강동구 요일별 그룹 평균
kd_day <-aggregate(age_20~day,data=df_kd,mean)

#강동구 요일별 그룹 평균 라인 그래프
kd_plot1 <- ggplot(kd_day, aes(x=day,y=age_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강동구 요일별 평균 생활인구")


#강동구 시간별 그룹 평균
kd_time <-aggregate(age_20~time,data=df_kd,mean)

#강동구 시간별 생활인구 라인+막대 그래프
kd_plot2 <- ggplot(kd_time, aes(x=time,y=age_20,group=1,fill=time))+
  #geom_bar(stat = "identity")+
  geom_line(linetype="solid",size=1,colour="black")+
  geom_point(size=3,shape=19,colour = "black")+
  theme(legend.position = "none")+
  #scale_fill_manual(values=palette2)+
  scale_x_discrete(limits = c("0시","1시","2시","3시","4시","5시","6시",
                              "7시","8시","9시","10시","11시","12시","13시",
                              "14시","15시","16시","17시","18시","19시","20시",
                              "21시","22시","23시"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x="",
       y="",
       title="2020년 10월 4주차 강동구 시간별 평균 생활인구")

#지도 병렬
grid.arrange(jonglo_plot1,jung_plot1,ys_plot1,sd_plot1,gj_plot1,
               ddm_plot1,jl_plot1,sb_plot1,kb_plot1,db_plot1,
               nw_plot1,ep_plot1,sdm_plot1,mp_plot1,yc_plot1,
               ks_plot1,gr_plot1,gc_plot1,ydp_plot1,dj_plot1,
               kw_plot1,sc_plot1,kn_plot1,sp_plot1,kd_plot1,nrow=5,ncol=5)

grid.arrange(jonglo_plot2,jung_plot2,ys_plot2,sd_plot2,gj_plot2,
             ddm_plot2,jl_plot2,sb_plot2,kb_plot2,db_plot2,
             nw_plot2,ep_plot2,sdm_plot2,mp_plot2,yc_plot2,
             ks_plot2,gr_plot2,gc_plot2,ydp_plot2,dj_plot2,
             kw_plot2,sc_plot2,kn_plot2,sp_plot2,kd_plot2,nrow=5,ncol=5)

