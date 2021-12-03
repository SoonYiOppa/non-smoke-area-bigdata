library(dplyr)
library(plyr)
library(ggplot2)
library(showtext)

df <- read.csv("C:/LJH/PYDATAexam/non-smoke-area-bigdata/data/서울특별시_금연구역_정보.csv",
               header= T, encoding = "ANsI")

df_count <- count(df,"sgg_nm")
df_count <- arrange(df_count, desc(freq))
df_count <- filter(df_count,freq >= 100)


df_area <- count(df,"area_detail")
df_area <- arrange(df_area,desc(freq))
df_area <- filter(df_area, freq >= 30)

#폰트 지정
font_add_google('Nanum Gothic',family = "NamumGothinc")
showtext_auto()

#색지정
palette1 <- c("구로구"="#F26419", "노원구"="#F26419","마포구"="#2F4858","송파구"="#2F4858","강서구"="#2F4858",
              "강북구"="#2F4858","강남구" ="#2F4858","금천구" ="#2F4858","양천구" ="#2F4858",
              "동작구"="#2F4858","영등포구"="#2F4858","관악구"="#33658A","종로구"="#33658A","은평구"="#33658A",
              "중랑구"="#33658A","중구"="#33658A","서대문구"="#33658A","동대문구"="#33658A","서초구"="#33658A","성동구"="#33658A")

palette2 <- c("유치원 및 어린이집"="#F26419", "버스정류장"="#F26419", "어린이 놀이시설"="#F26419","의료기관"="#F26419",
              "공원"="#F6AE2D","학교 절대 보호구역"="#F6AE2D","실외금연구역"="#F6AE2D","가로변 버스정류장"="#F6AE2D",
              "지하철역"="#2F4858","어린이운송용승합차"="#2F4858","초등학교"="#2F4858","관공서"="#2F4858","복합건물"="#2F4858",
              "도서관"="#2F4858","금연거리"="#2F4858","체육시설"="#2F4858","중학교"="#2F4858",
              "고등학교"="#33658A","가스 충전소 및 주유소"="#33658A","지하철 출입구"="#33658A","사회복지시설"="#33658A",
              "택시승차장"="#33658A","목욕탕"="#33658A","대규모점포"="#33658A","공동주택"="#33658A")



#서울 20개구 금연시설 박대 그래프
ggplot(data = df_count,
       aes(x=reorder(sgg_nm,freq),y=freq,fill=sgg_nm))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(y=freq,label=freq),position = position_dodge(1))+
  scale_fill_manual(values=palette1)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="",
       y="",
       title="서울 20개구 금연구역 지정 개수")
  
# 서울시 25개구 금연구역 시설 분포
  ggplot(data = df_area,
         aes(x=reorder(area_detail,freq),y=freq,fill=area_detail))+
    geom_bar(stat="identity")+
    coord_flip()+
    geom_text(aes(y=freq,label=freq),position = position_dodge(1))+
    scale_fill_manual(values=palette2)+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x="",
       y="",
       title="금연구역 및 시설 유형")