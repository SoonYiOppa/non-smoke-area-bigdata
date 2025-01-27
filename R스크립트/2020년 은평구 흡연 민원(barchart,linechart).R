library(ggplot2)
library(showtext)

df <- read.csv("C://LJH/PYDATAexam/non-smoke-area-bigdata/data/2020년_은평구_흡연민원.csv",header=T,encoding="UTF-8")



complaints <- df$complaints
caught <-df$caught
sum <- complaints+caught



shape <- rep(c("compaints","caught","합계"),each=12)
month <- paste(rep(c(1:12),3),"월",sep="")

freq <- c(complaints,caught,sum)

df1 <- data.frame(month, shape, freq)
df2<- df1[1:24,1:3]


#글꼴 지정
font_add_google('Nanum Gothic',family = "NamumGothinc")
showtext_auto()


# 막대 그래프 그리기

ggplot(data=df2,
       aes(x=month,y=freq,fill=shape))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=freq, label=freq), position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_manual(values =c("#F26419","#33658A"),
    name=c(""),
    labels=c("현장적발","민원처리"))+
  scale_x_discrete(limits = c("1월","2월","3월","4월","5월","6월",
                              "7월","8월","9월","10월","11월","12월"))+
  labs(x="",
       y="",
       title="2020년 은평구 흡연민원 현황")

# 라인 그래프 그리기

ggplot(df1, aes(month,y=freq,group=shape,color=shape))+
  geom_line(linetype="solid",size=1)+
  geom_point(size=3,shape=17)+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("#33658A","#2F4858","#F26419"),
                     name=c(""),
                     labels=c("현장적발","민원처리","합계"))+
  scale_x_discrete(limits = c("1월","2월","3월","4월","5월","6월",
                              "7월","8월","9월","10월","11월","12월"))+
  labs(x="",
       y="",
       title="2020년 은평구 흡연민원 현황")



