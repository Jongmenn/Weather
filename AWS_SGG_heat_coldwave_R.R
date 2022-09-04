#일일 폭염/한파 모니터링 자료
#연도별 시군구별 폭염일수/한파일수 
#폭염: 2일 이상 지속
#한파: 최저기온 -12도 이하
#연도별로 시군구 250개로 맞춰져있음 

#R library
pacman::p_load("lubridate","dplyr","readxl")

#working directory
setwd("D:\\SNU\\KEI\\자료\\KEI_서울대_건강자료(보안)")

#load data
wea_sgg<-read_excel("aws20102019.xlsx") %>% as.data.frame

wea_sgg$date2=ymd(wea_sgg$date)
wea_sgg$year =year(wea_sgg$date2)
wea_sgg$month=month(wea_sgg$date2)
wea_sgg$day  =day(wea_sgg$date2)

unique_sgg<-unique(wea_sgg$sgg2019)


seq(as.Date("2010-01-01"),as.Date("2019-12-31"),1) %>% length
3652*250
nrow(wea_sgg) #일부 측정 안된날들 존재 

wea_sgg_list=NULL
for(i in unique_sgg){
  subdat<-subset(wea_sgg,sgg2019==i)
  
  #일최고기온 33도 이상
  subdat$maxtemp33_lag0=with(subdat,ifelse(maxtemp>=33,1,0))
  subdat$maxtemp33_lag1=lag(subdat$maxtemp33)

  #폭염 정의, 일최고기온 33도 이상 이틀이상 지속
  subdat$heatwave=ifelse(apply(subdat %>% select(maxtemp33_lag0:maxtemp33_lag1),1,sum,na.rm=T)>=2,1,0)
  
  #한파 정의, 일최저기온 -12도 이하
  subdat$coldwave=with(subdat,ifelse(mintemp<=-12,1,0))
  
  wea_sgg_list[[i]]<-subdat %>% select(-c(maxtemp33_lag0,maxtemp33_lag1))
  print(i)
}

wea_sgg2<-as.data.frame(do.call(rbind,wea_sgg_list))
write.csv(wea_sgg2,file="wea_sgg2.csv",row.names=F,na="")


#연도별 시군구별 합계
hw_yr_sgg<-wea_sgg2 %>% group_by(sgg2019,year) %>% summarise(hw=sum(heatwave,na.rm=T))
cw_yr_sgg<-wea_sgg2 %>% group_by(sgg2019,year) %>% summarise(cw=sum(coldwave,na.rm=T))


#연도별, 월별 시군구별 합계 
hw_yr_mon_sgg<-wea_sgg2 %>% group_by(sgg2019,year,month) %>% summarise(hw=sum(heatwave,na.rm=T))
cw_yr_mon_sgg<-wea_sgg2 %>% group_by(sgg2019,year,month) %>% summarise(cw=sum(coldwave,na.rm=T))


sgg_yr_hw.list=NULL
sgg_yr_cw.list=NULL
sgg_yr_mon.hw.list=NULL
sgg_yr_mon.cw.list=NULL

for(i in unique_sgg){

  #연도별, 시군구별 합계
  t_hw<-t(subset(hw_yr_sgg,sgg2019==i)[,2:3])
  t_cw<-t(subset(cw_yr_sgg,sgg2019==i)[,2:3])
  
  colnames(t_hw)=paste0("year",t_d[1,])
  colnames(t_cw)=paste0("year",t_d[1,])
  
  t_hw2<-as.data.frame(t(t_hw[2,]))
  t_cw2<-as.data.frame(t(t_cw[2,]))
  
  t_hw2$sgg=i
  t_cw2$sgg=i
  
  #연도별,월별 시군구별 합계
  hw2010<-subset(hw_yr_mon_sgg,sgg2019==i & year==2010);hw2011<-subset(hw_yr_mon_sgg,sgg2019==i & year==2011)
  hw2012<-subset(hw_yr_mon_sgg,sgg2019==i & year==2012);hw2013<-subset(hw_yr_mon_sgg,sgg2019==i & year==2013)
  hw2014<-subset(hw_yr_mon_sgg,sgg2019==i & year==2014);hw2015<-subset(hw_yr_mon_sgg,sgg2019==i & year==2015)
  hw2016<-subset(hw_yr_mon_sgg,sgg2019==i & year==2016);hw2017<-subset(hw_yr_mon_sgg,sgg2019==i & year==2017)
  hw2018<-subset(hw_yr_mon_sgg,sgg2019==i & year==2018);hw2019<-subset(hw_yr_mon_sgg,sgg2019==i & year==2019)
  
  cw2010<-subset(cw_yr_mon_sgg,sgg2019==i & year==2010);cw2011<-subset(cw_yr_mon_sgg,sgg2019==i & year==2011)
  cw2012<-subset(cw_yr_mon_sgg,sgg2019==i & year==2012);cw2013<-subset(cw_yr_mon_sgg,sgg2019==i & year==2013)
  cw2014<-subset(cw_yr_mon_sgg,sgg2019==i & year==2014);cw2015<-subset(cw_yr_mon_sgg,sgg2019==i & year==2015)
  cw2016<-subset(cw_yr_mon_sgg,sgg2019==i & year==2016);cw2017<-subset(cw_yr_mon_sgg,sgg2019==i & year==2017)
  cw2018<-subset(cw_yr_mon_sgg,sgg2019==i & year==2018);cw2019<-subset(cw_yr_mon_sgg,sgg2019==i & year==2019)
  
  hw_yr_mon<-as.data.frame(cbind(yr2010=hw2010$hw,yr2011=hw2011$hw,yr2012=hw2012$hw,yr2013=hw2013$hw,yr2014=hw2014$hw,
                                 yr2015=hw2015$hw,yr2016=hw2016$hw,yr2017=hw2017$hw,yr2018=hw2018$hw,yr2019=hw2019$hw))
  
  cw_yr_mon<-as.data.frame(cbind(yr2010=cw2010$cw,yr2011=cw2011$cw,yr2012=cw2012$cw,yr2013=cw2013$cw,yr2014=cw2014$cw,
                                 yr2015=cw2015$cw,yr2016=cw2016$cw,yr2017=cw2017$cw,yr2018=cw2018$cw,yr2019=cw2019$cw))
  
  hw_yr_mon$sgg=i
  cw_yr_mon$sgg=i
  hw_yr_mon$month=row.names(hw_yr_mon)
  cw_yr_mon$month=row.names(cw_yr_mon)
  
  #연도별 시군구별 폭염/한파 자료 리스트 저장 
  sgg_yr_hw.list[[i]]<-t_hw2
  sgg_yr_cw.list[[i]]<-t_cw2
  
  #연도별,월별 시군구별 폭염/한파 자료 리스트 저장 
  sgg_yr_mon.hw.list[[i]]<-hw_yr_mon
  sgg_yr_mon.cw.list[[i]]<-cw_yr_mon
}

#연도별,시군구별 폭염 일수
sgg_yr_hw<-bind_rows(sgg_yr_hw.list) %>% as.data.frame
sgg_yr_cw<-bind_rows(sgg_yr_cw.list) %>% as.data.frame

sgg_yr_mon.hw<-bind_rows(sgg_yr_mon.hw.list) %>% as.data.frame
sgg_yr_mon.cw<-bind_rows(sgg_yr_mon.cw.list) %>% as.data.frame

write.csv(sgg_yr_hw,file="sgg_yr_hw.csv",row.names=F,na="")
write.csv(sgg_yr_cw,file="sgg_yr_cw.csv",row.names=F,na="")

write.csv(sgg_yr_mon.hw,file="sgg_yr_mon.hw.csv",row.names=F,na="")
write.csv(sgg_yr_mon.cw,file="sgg_yr_mon.cw.csv",row.names=F,na="")



