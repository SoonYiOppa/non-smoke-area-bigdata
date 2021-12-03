df <- read.csv(file="C://LJH/PYDATAexam/non-smoke-area-bigdata/AL_11650_D199_20210712.csv",header = T,
               encoding="ANSI")


address1 <- df$address1
address2 <- df$code

address <- paste(address1,address2)
purpose1 <- df$purpose1
purpose2 <- df$purpose2

seocho <- data.frame(address,purpose1,purpose2)

#geocoding 좌표 추출
 library(ggmap)

ggmap::register_google(key="") #본인 구글  API key 사용

seocho$address <- enc2utf8(seocho$address)
geocode_output <-ggmap::mutate_geocode(data = seocho, location = address, source = 'google')

write.csv(geocode_output,"서초구_건물용도.csv",fileEncoding='UTF-8')