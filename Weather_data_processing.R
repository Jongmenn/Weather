#--------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf","RColorBrewer","lubridate","lmtest","readxl","survival",
               "splines","data.table","stringr","tidyr","extrafont","scales","gridExtra","Epi","tsModel")


#기상청 원시자료 
me<-read.csv("D:\\EUMC\\데이터관리\\기상청\\기상자료\\wea2000_2020.csv")

me$year=year(me$date)

me$k=paste0(me$mecode,"-",me$date)

me<-me %>% left_join(adad) %>% select(mecode:date,year,meantemp:meanatp,totsun)


#종관관측소 지점코드 포함
me.index <-read.csv("D:\\EUMC\\데이터관리\\기상청\\기상자료\\weather_index.csv")
me.index2<-subset(me.index,포함유무==T) %>% dplyr:: select(시도,지점코드,geocode)

names(me.index2)[1:2]=c("KOR_SIDO","mecode")

me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="경기도"  ,"경기",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="경상남도","경남",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="경상북도","경북",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="전라남도","전남",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="전라북도","전북",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="충청남도","충남",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="충청북도","충북",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="제주도"  ,"제주",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="강원도"  ,"강원",me.index2$KOR_SIDO)

#사용할 지점코드만 추출할 경우 -R
me.sub<-subset(me,mecode %in% subset(me.index,포함유무=="T")$지점코드)

me.index$포함유무 %>% table

#사용할 지점코드만 추출할 경우 -SQL
#sqldf("select * from me where mecode in (select mecode from `me.index2`)")

#지역명 부여 
me.sub2<-merge(me.sub,me.index2,by="mecode")

#지역별 평균 자료로 산출

avg.me<-sqldf("select KOR_SIDO, geocode, date,avg(mintemp) as mintemp,avg(meantemp) as meantemp, avg(maxtemp) as maxtemp,
              avg(meandew) as meandew, avg(meanhumi) as meanhumi, avg(meanprec) as meanprec, avg(meanwindsp) as meanwindsp,
              avg(meanatp) as meanatp from `me.sub2` group by KOR_SIDO, date")

avg.me$date=ymd(avg.me$date)
avg.me$KOR_SIDO=factor(avg.me$KOR_SIDO,level=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원",
                                               "충북","충남","전북","전남","경북","경남","제주"))

avg.me<-tibble(avg.me) %>% arrange(KOR_SIDO)

avg.me$year =year(avg.me$date)
avg.me$month=month(avg.me$date)
avg.me$day  =day(avg.me$date)
avg.me$dow  =weekdays(avg.me$date)

avg.me<- avg.me %>% select(KOR_SIDO :date,year:dow,mintemp:meanatp)

# write.csv(avg.me,file="wea_2000_2020_rev.csv",row.names=F,na="")

