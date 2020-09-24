rm(list=ls())
library(rio)
library(car)
library(moments)

demo=import("train.csv")
colnames(demo)=tolower(make.names(colnames(demo)))
attach(demo)
country_region=as.factor(country_region)
us=subset(demo, country_region=="US")
us$time=seq(1:nrow(us))
plot(us$time,us$confirmedcases,pch=19,type="l")
province_state=as.factor(province_state)

demo$jul=as.numeric(format(as.Date(demo[,4]),"%j"))-21


demo$month=format(as.Date(demo[,4]),"%m")
demo$month=as.numeric(demo$month)
demo$day=format(as.Date(demo[,4]),"%d")
demo$day=as.numeric(demo$day)
for(i in 1:nrow(demo)){
  if(demo$month[i]==1){
    demo$time[i]=demo$day[i]-21
  } else if(demo$month[i]==2){demo$time[i]=demo$day[i]+31-21}
  else if(demo$month[i]==3){demo$time[i]=demo$day[i]+31+29-21}
  else if(demo$month[i]==4){demo$time[i]=demo$day[i]+31+29+31-21}
}
reg1=lm(confirmedcases~time+I(time^2)+I(time^3)+country_region+province_state,data=demo)
summary(reg1)
plot(confirmedcases~time,pch=19,type="l",data=subset(demo, country_region=="Afghanistan"))
reg2=lm(fatalities~time+confirmedcases+country_region+province_state,data=demo)
summary(reg2)
test=import("test.csv")
colnames(test)=tolower(make.names(colnames(test)))
test1=test[,c("province_state","country_region")]
test1$time=as.numeric(format(as.Date(test[,4]),"%j"))-21

res=data.frame(predict(reg1,test1,interval="predict"))
test1$confirmedcases=res$fit
res2=data.frame(predict(reg2,test1,interval="predict"))
