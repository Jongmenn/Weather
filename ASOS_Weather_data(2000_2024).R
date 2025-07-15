#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf",
               "RColorBrewer","lubridate","lmtest","readxl")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd("D:\\EUMC\\데이터관리\\기상청\\기상자료\\일별자료")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
lf<-list.files()
list.files()
d<-read.csv(lf[[1]],fileEncoding = "euc-kr")

dat.list=NULL
for(i in 1:length(lf)){
  d<-read.csv(lf[[i]],fileEncoding = "euc-kr")  
  names(d)=c("station_code","station","date","meantemp","mintemp","maxtemp","precip","meanwindsp",
             "meandew","meanhumi","meanatp","mean_local_pressure","mean_sealevel_pressure")
  
  dat.list[[i]]<-d
  print(i)
}

#2000~2024년 ASOS 지점별 일별 자료
dat<-do.call(rbind,dat.list)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#종관관측소 지점코드 포함
me.index <-read_excel("D:\\EUMC\\데이터관리\\기상청\\기상자료\\기상관측지점_update250715.xlsx",sheet=1)
me.index2<-subset(me.index,포함유무=="T") %>% dplyr:: select(시도,지점코드,geocode)

names(me.index2)[1:2]=c("KOR_SIDO","station_code")

me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="경기도"  ,"경기",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="경상남도","경남",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="경상북도","경북",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="전라남도","전남",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="전라북도","전북",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="충청남도","충남",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="충청북도","충북",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="제주도"  ,"제주",me.index2$KOR_SIDO)
me.index2$KOR_SIDO=ifelse(me.index2$KOR_SIDO=="강원도"  ,"강원",me.index2$KOR_SIDO)

#지역명 부여 
dat2<-merge(dat,me.index2,by="station_code")

#지역별 평균 자료로 산출
head(dat2)
avg.me<-sqldf("select KOR_SIDO, geocode, date,
               avg(mintemp) as mintemp,
               avg(meantemp) as meantemp,
               avg(maxtemp) as maxtemp,
               avg(meandew) as meandew,
               avg(meanhumi) as meanhumi,
               avg(precip) as precip, 
               avg(meanwindsp) as meanwindsp,
              avg(meanatp) as meanatp,
              avg(mean_local_pressure) as mean_local_pressure,
              avg(mean_sealevel_pressure) as mean_sealevel_pressure
              from `dat2` group by KOR_SIDO, date")

avg.me$date=ymd(avg.me$date)
avg.me$KOR_SIDO=factor(avg.me$KOR_SIDO,level=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원",
                                               "충북","충남","전북","전남","경북","경남","제주"))

avg.me<-tibble(avg.me) %>% arrange(KOR_SIDO)

avg.me$year =year(avg.me$date)
avg.me$month=month(avg.me$date)
avg.me$day  =day(avg.me$date)
avg.me$dow  =weekdays(avg.me$date)

avg.me<- avg.me %>% dplyr:: select(KOR_SIDO :date,year:dow,mintemp:mean_sealevel_pressure)

View(avg.me)

setwd("D:\\EUMC\\데이터관리\\기상청\\기상자료")
write.csv(avg.me,file="wea2000_2024_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")
