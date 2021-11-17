
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(plyr)
library(reshape)

#데이터 불러 오기
df <- read.csv("C://LJH/non_smoking_area/data/2018년_서울시_흡연_적발건수.csv", header= T, encoding="ANSI")

long <-c(126.978061,126.995227,126.979489,127.041285,127.085592,
         127.053258,127.093171,127.017674,127.010241,127.033412,
         127.075085,126.925604,126.937978,126.907785,126.854823,
         126.821907,126.856060,126.899865,126.909517,126.951095,
         126.944660,127.030290,127.063453,127.115178,127.147103)
lat  <-c(37.595678,37.559762,37.530907,37.550784,37.545238,
         37.581976,37.597683,37.606227,37.643523,37.668783,
         37.652035,37.619306,37.577736,37.559885,37.524367,
         37.561258,37.494527,37.460746,37.522797,37.499043,
         37.467817,37.473710,37.496490,37.504737,37.550271)


sgg_nm <- df$sgg_nm
smoking_freq <- df$smoking_freq
smoking_freq <- paste(smoking_freq,"건",sep="")


gu_name <- data.frame(sgg_nm,smoking_freq,long,lat)


# 서울 지도 만들기

map <- shapefile("C://LJH/Suwon_Security/SIG_201703/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map<-fortify(map, region = 'SIG_CD')
new_map$id <-as.numeric(new_map$id)
seoul_map<-new_map[new_map$id<=11740,]

m_merge<-merge(seoul_map,df,by='id')

ggplot()+
  geom_polygon(data=m_merge,
               aes(x=long,y=lat,group=group,fill=smoking_freq),color="white")+
  scale_fill_gradient(low="#ECD6AF",high = "#FF6900",space="lab",guide="colourbar")+
  labs(fill="서울특별시 흡연 적발 건수")+
  theme_void()+
  theme(legend.position = c(.15, .85))+
  geom_text(data=gu_name,
            aes(x=long, y=lat,label=paste(sgg_nm,smoking_freq,sep="\n")))