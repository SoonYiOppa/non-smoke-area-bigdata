library(dplyr)
library(ggplot2)
library(showtext)
library(scales)
library(gridExtra)


df<-read.csv(file="C://LJH/PYDATAexam/non-smoke-area-bigdata/data/서초구_행정동_생활인구.csv",
             encoding="UTF-8")

df$time <- paste(df$time,"시",sep="")
age_20 <- df$m_20+df$m_25+df$f_20+df$f_25
df <- data.frame(df,age_20)

#서초구 추출
sc_data <- subset(df,
                  select = c("date","time","id","age_20"),
                  subset = (id>=11650510&id<=11650660))

day <- rep(c("월","화","수","목","금","토","일"),each=length(sc_data$date)/7)
time <- sc_data$time
id <- sc_data$id
sc_20 <- sc_data$age_20


sc_data <- data.frame(day,time,id,sc_20)

v_names <- names(sc_data)

#텍스트 지정
font_add_google('Nanum Gothic',family = "NamumGothinc")
showtext_auto()

#서초1동 추출 % 서초1동 요일별 평균 & 서초1동 시간별 평균
sc1 <- subset(sc_data,
              select = v_names,
              subset = (id==11650510))

sc1_day <-aggregate(sc_20~day,data=sc1,mean)

sc1_time <-aggregate(sc_20~time,data=sc1,mean)

#서초 1동 요일별 라인 그래프
sc1_plot1 <- ggplot(sc1_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="서초1동 요일별 평균 생활인구")

#서초1 시간별 생활인구 라인+막대 그래프
sc1_plot2 <- ggplot(sc1_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="서초1동 시간별 평균 생활인구")



#서초2동 추출 % 서초2동 요일별 평균 & 서초1동 시간별 평균
sc2 <- subset(sc_data,
              select = v_names,
              subset = (id==11650520))

sc2_day <-aggregate(sc_20~day,data=sc2,mean)

sc2_time <-aggregate(sc_20~time,data=sc2,mean)

#서초 2동 요일별 라인 그래프
sc2_plot1 <- ggplot(sc2_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="서초2동 요일별 평균 생활인구")

#서초2 시간별 생활인구 라인+막대 그래프
sc2_plot2 <- ggplot(sc2_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="서초2동 시간별 평균 생활인구")

#서초3동 추출 % 서초2동 요일별 평균 & 서초1동 시간별 평균
sc3 <- subset(sc_data,
              select = v_names,
              subset = (id==11650530))

sc3_day <-aggregate(sc_20~day,data=sc3,mean)

sc3_time <-aggregate(sc_20~time,data=sc3,mean)

#서초 3동 요일별 라인 그래프
sc3_plot1 <- ggplot(sc3_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="서초3동 요일별 평균 생활인구")

#서초3 시간별 생활인구 라인+막대 그래프
sc3_plot2 <- ggplot(sc3_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="서초3동 시간별 평균 생활인구")



#서초4동 추출 % 서초4동 요일별 평균 & 서초4동 시간별 평균
sc4 <- subset(sc_data,
              select = v_names,
              subset = (id==11650531))

sc4_day <-aggregate(sc_20~day,data=sc4,mean)

sc4_time <-aggregate(sc_20~time,data=sc4,mean)

#서초 4동 요일별 라인 그래프
sc4_plot1 <- ggplot(sc4_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="서초4동 요일별 평균 생활인구")

#서초4 시간별 생활인구 라인+막대 그래프
sc4_plot2 <- ggplot(sc4_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="서초4동 시간별 평균 생활인구")


#잠원동 추출 % 잠원동 요일별 평균 & 잠원동 시간별 평균
jw <- subset(sc_data,
              select = v_names,
              subset = (id==11650540))

jw_day <-aggregate(sc_20~day,data=jw,mean)

jw_time <-aggregate(sc_20~time,data=jw,mean)

#잠원 요일별 라인 그래프
jw_plot1 <- ggplot(jw_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="잠원동 요일별 평균 생활인구")

#잠원동 시간별 생활인구 라인+막대 그래프

jw_plot2 <- ggplot(jw_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="잠원동 시간별 평균 생활인구")

#반포본동 추출 % 요일별 평균 & 시간별 평균
bpb <- subset(sc_data,
             select = v_names,
             subset = (id==11650550))

bpb_day <-aggregate(sc_20~day,data=bpb,mean)

bpb_time <-aggregate(sc_20~time,data=bpb,mean)

#반포본동 요일별 라인 그래프
bpb_plot1 <- ggplot(bpb_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="반포본동 요일별 평균 생활인구")

#반포본동 시간별 생활인구 라인+막대 그래프

bpb_plot2 <- ggplot(bpb_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="반포본동 시간별 평균 생활인구")

#반포1동 추출 % 요일별 평균 & 시간별 평균
bp1 <- subset(sc_data,
              select = v_names,
              subset = (id==11650560))

bp1_day <-aggregate(sc_20~day,data=bp1,mean)

bp1_time <-aggregate(sc_20~time,data=bp1,mean)

#반포1동 요일별 라인 그래프
bp1_plot1 <- ggplot(bp1_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="반포1동 요일별 평균 생활인구")

#반포1동 시간별 생활인구 라인+막대 그래프

bp1_plot2 <- ggplot(bp1_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="반포1동 시간별 평균 생활인구")

#반포2동 추출 % 요일별 평균 & 시간별 평균
bp2 <- subset(sc_data,
              select = v_names,
              subset = (id==11650570))

bp2_day <-aggregate(sc_20~day,data=bp2,mean)

bp2_time <-aggregate(sc_20~time,data=bp2,mean)

#반포2동 요일별 라인 그래프
bp2_plot1 <- ggplot(bp2_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="반포2동 요일별 평균 생활인구")

#반포1동 시간별 생활인구 라인+막대 그래프

bp2_plot2 <- ggplot(bp2_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="반포2동 시간별 평균 생활인구")

#반포3동 추출 % 요일별 평균 & 시간별 평균
bp3 <- subset(sc_data,
              select = v_names,
              subset = (id==11650580))

bp3_day <-aggregate(sc_20~day,data=bp3,mean)

bp3_time <-aggregate(sc_20~time,data=bp3,mean)

#반포3동 요일별 라인 그래프
bp3_plot1 <- ggplot(bp3_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="반포3동 요일별 평균 생활인구")

#반포3동 시간별 생활인구 라인+막대 그래프

bp3_plot2 <- ggplot(bp3_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="반포3동 시간별 평균 생활인구")


#반포4동 추출 % 요일별 평균 & 시간별 평균
bp4 <- subset(sc_data,
              select = v_names,
              subset = (id==11650581))

bp4_day <-aggregate(sc_20~day,data=bp4,mean)

bp4_time <-aggregate(sc_20~time,data=bp4,mean)

#반포4동 요일별 라인 그래프
bp4_plot1 <- ggplot(bp4_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="반포4동 요일별 평균 생활인구")

#반포3동 시간별 생활인구 라인+막대 그래프

bp4_plot2 <- ggplot(bp4_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="반포4동 시간별 평균 생활인구")


#방배본동 추출 % 요일별 평균 & 시간별 평균
bbb <- subset(sc_data,
              select = v_names,
              subset = (id==11650590))

bbb_day <-aggregate(sc_20~day,data=bbb,mean)

bbb_time <-aggregate(sc_20~time,data=bbb,mean)

#방배본동 요일별 라인 그래프
bbb_plot1 <- ggplot(bbb_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="방배본동 요일별 평균 생활인구")

#방배본동 시간별 생활인구 라인+막대 그래프

bbb_plot2 <- ggplot(bbb_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="방배본동 시간별 평균 생활인구")

#방배1동 추출 % 요일별 평균 & 시간별 평균
bb1 <- subset(sc_data,
              select = v_names,
              subset = (id==11650600))

bb1_day <-aggregate(sc_20~day,data=bb1,mean)

bb1_time <-aggregate(sc_20~time,data=bb1,mean)

#방배1동 요일별 라인 그래프
bb1_plot1 <- ggplot(bb1_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="방배1동 요일별 평균 생활인구")

#방배1동 시간별 생활인구 라인+막대 그래프

bb1_plot2 <- ggplot(bb1_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="방배1동 시간별 평균 생활인구")


#방배2동 추출 % 요일별 평균 & 시간별 평균
bb2 <- subset(sc_data,
              select = v_names,
              subset = (id==11650610))

bb2_day <-aggregate(sc_20~day,data=bb2,mean)

bb2_time <-aggregate(sc_20~time,data=bb2,mean)

#방배2동 요일별 라인 그래프
bb2_plot1 <- ggplot(bb2_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="방배2동 요일별 평균 생활인구")

#방배2동 시간별 생활인구 라인+막대 그래프

bb2_plot2 <- ggplot(bb2_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="방배2동 시간별 평균 생활인구")


#방배3동 추출 % 요일별 평균 & 시간별 평균
bb3 <- subset(sc_data,
              select = v_names,
              subset = (id==11650620))

bb3_day <-aggregate(sc_20~day,data=bb3,mean)

bb3_time <-aggregate(sc_20~time,data=bb3,mean)

#방배3동 요일별 라인 그래프
bb3_plot1 <- ggplot(bb3_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="방배3동 요일별 평균 생활인구")

#방배3동 시간별 생활인구 라인+막대 그래프

bb3_plot2 <- ggplot(bb3_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="방배3동 시간별 평균 생활인구")

#방배4동 추출 % 요일별 평균 & 시간별 평균
bb4 <- subset(sc_data,
              select = v_names,
              subset = (id==11650621))

bb4_day <-aggregate(sc_20~day,data=bb4,mean)

bb4_time <-aggregate(sc_20~time,data=bb4,mean)

#방배4동 요일별 라인 그래프
bb4_plot1 <- ggplot(bb4_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="방배4동 요일별 평균 생활인구")

#방배4동 시간별 생활인구 라인+막대 그래프

bb4_plot2 <- ggplot(bb4_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="방배4동 시간별 평균 생활인구")

#양재1동 추출 % 요일별 평균 & 시간별 평균
yj1 <- subset(sc_data,
              select = v_names,
              subset = (id==11650651))

yj1_day <-aggregate(sc_20~day,data=yj1,mean)

yj1_time <-aggregate(sc_20~time,data=yj1,mean)

#양재1동 요일별 라인 그래프
yj1_plot1 <- ggplot(yj1_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="양재1동 요일별 평균 생활인구")

#양재1동 시간별 생활인구 라인+막대 그래프

yj1_plot2 <- ggplot(yj1_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="양재1동 시간별 평균 생활인구")


#양재2동 추출 % 요일별 평균 & 시간별 평균
yj2 <- subset(sc_data,
              select = v_names,
              subset = (id==11650652))

yj2_day <-aggregate(sc_20~day,data=yj2,mean)

yj2_time <-aggregate(sc_20~time,data=yj2,mean)

#양재2동 요일별 라인 그래프
yj2_plot1 <- ggplot(yj2_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="양재2동 요일별 평균 생활인구")

#양재2동 시간별 생활인구 라인+막대 그래프

yj2_plot2 <- ggplot(yj2_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="양재2동 시간별 평균 생활인구")

#내곡동 추출 % 요일별 평균 & 시간별 평균
ng <- subset(sc_data,
              select = v_names,
              subset = (id==11650660))

ng_day <-aggregate(sc_20~day,data=ng,mean)

ng_time <-aggregate(sc_20~time,data=ng,mean)

#내곡동 요일별 라인 그래프
ng_plot1 <- ggplot(ng_day, aes(x=day,y=sc_20,group=1))+
  geom_line(linetype="solid",size=1,colour="#F26419")+
  geom_point(size=3,shape=19,colour="#F26419")+
  scale_x_discrete(limits = c("월","화","수","목","금","토","일"))+
  labs(x="",
       y="",
       title="내곡동 요일별 평균 생활인구")

#내곡동 시간별 생활인구 라인+막대 그래프

ng_plot2 <- ggplot(ng_time, aes(x=time,y=sc_20,group=1,fill=time))+
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
       title="내곡동 시간별 평균 생활인구")

#지도 병렬

grid.arrange(sc1_plot1,sc2_plot1,sc3_plot1,sc4_plot1,
             jw_plot1,
             bpb_plot1,bp1_plot1,bp2_plot1,bp3_plot1,bp4_plot1,
             bbb_plot1,bb1_plot1,bb2_plot1,bb3_plot1,bb4_plot1,
             yj1_plot1,yj2_plot1,ng_plot1,nrow=4,ncol=5)

grid.arrange(sc1_plot2,sc2_plot2,sc3_plot2,sc4_plot2,
             jw_plot2,
             bpb_plot2,bp1_plot2,bp2_plot2,bp3_plot2,bp4_plot2,
             bbb_plot2,bb1_plot2,bb2_plot2,bb3_plot2,bb4_plot2,
             yj1_plot2,yj2_plot2,ng_plot2,nrow=4,ncol=5)
