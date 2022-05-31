library(rms)
library(foreign)
library(survival)




source("stdca.R") #一定要把stdca.R放在之前设定的起始目录中。
Srv=Surv(dev$follow,dev$death)
coxmod=coxph(formula = Surv(TIMES, status == 0) ~ Age + T..stage + size + 
       M..stage + Surgery, data = fib1)



froc<-f11 <- coxph(Surv(TIMES,status==0)~Age+size+M..stage+Surgery+T..stage,x=TRUE,y=TRUE,data=fib1) 
froctmn<-f2 <- coxph(Surv(TIMES,status==0)~T..stage+M..stage+N..stage,x=TRUE,y=TRUE,data=fib1) 

#以死亡率为连续变量进行ROC曲线分析
fib1$one.years.death.Probabilitynew = c(1- (summary(survfit(froc,newdata=fib1),times=12)$surv))  #计算1年死亡率
fib1$tmnone.years.death.Probabilitynew = c(1- (summary(survfit(froctmn,newdata=fib1),times=12)$surv))  #计算1年死亡率

froc<- cph(Surv(TIMES,status==0)~Age+size+M..stage+Surgery+T..stage,x=TRUE,y=TRUE,data=fib1,surv=TRUE,time.inc=36)
froctmn<-cph(Surv(TIMES,status==0)~ T..stage+M..stage+N..stage,x=T,y=T,data=fib1,surv=T,time.inc=36)

fib1$three.years.death.Probabilitynew = c(1- (summary(survfit(froc,newdata=fib1),times=36)$surv))  #计算3年死亡率
fib1$tmnthree.years.death.Probabilitynew = c(1- (summary(survfit(froctmn,newdata=fib1),times=36)$surv))  #计算3年死亡率

fib1$five.years.death.Probabilitynew = c(1- (summary(survfit(froc,newdata=fib1),times=60)$surv))  #计算5年死亡率
fib1$tmnfive.years.death.Probabilitynew = c(1- (summary(survfit(froctmn,newdata=fib1),times=60)$surv))  #计算5年死亡率

attach(fib1)

stdca(data=fib1,outcome=fib1$status==0,ttoutcome=fib1$TIMES,timepoint=36,predictors=fib1$three.years.death.Probabilitynew,xstop=0.9,smooth=TRUE)
stdca(data=fib1,outcome="status",ttoutcome="TIMES",timepoint=12,predictors="one.years.death.Probabilitynew",xstop=0.9,smooth=TRUE)

