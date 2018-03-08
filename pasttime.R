library(sqldf)
stopline<-400#停车线位置
ti<-c()#定义一个空向量存储过停车线时间
cycle<-90#上一个交叉口的周期

accum<-function(array,x){
  return(length(array[array<x])/length(array))
}

#选取自由通过数据获取通过停车线时间
#freepass自由通过车辆的数据
freepass<-read.table("C:/Users/10993/Desktop/上创/ForR/freepass.csv",header=T,sep=",")
distinct_vec_no<-sqldf("select distinct vec_no from freepass")#选取通过停车线的所有车辆编号
for (x in distinct_vec_no[,1]){
  tra<-freepass[which(freepass$vec_no==x),]#选取每一辆车的轨迹
  n<-nrow(tra)
  tra<-sqldf("select t as time,len from tra order by len desc")#按行驶距离降序排序
  for(j in 1:(n-1)){
    if (tra[j,2]>=stopline && tra[j+1,2]<stopline){
      ti[length(ti)+1]<-(tra[j+1,1]-tra[j,1])/(tra[j+1,2]-tra[j,2])*(stopline-tra[j,2])+tra[j,1]
    }
  }
}

#选取停车数据获取通过停车线时间
#stoppass停车通过的样本
stoppass<-read.table("C:/Users/10993/Desktop/上创/ForR/stoppass.csv",header=T,sep=",")
ti<-c(ti,as.array((stopline-stoppass$y)/stoppass$f_k+stoppass$f_t))

#累计频率曲线
ti<-ti%%cycle

#per周期中每一秒的累计频率
per<-c()
for(x in 0:cycle){
  per[x+1]<-accum(ti,x)
}
plot(0:cycle,per)

#计算将来某个时刻的排队长度
#now为当前时刻
#lat为计算时刻
#accum为计算累计百分比的函数
#vol_len为流量与单位车辆停车长度的乘积
#spe为进口道车速
#ini是初始排队长度，默认为0
queue<-function(now,lat,vol_len,spe,accum,ti,ini=0){
  chang<-function(ti,now,lat,spe,ini,lal,vol_len) (accum(ti,lat+lal/spe)-accum(ti,now+ini/spe))*vol_len-(lal-ini)
  return(uniroot(chang,c(200*min(now-lat,lat-now),200*max(now-lat,lat-now)),ti=ti,
                 now=now,lat=lat,spe=spe,ini=ini,vol_len=vol_len,tol=1))
}

#排队一整个周期的排队曲线
len<-c()
len[1]<-0
for(x in 1:cycle){
  per[x+1]<-queue(0,x,60,30,accum,ti)$root
}
plot(0:cycle,per)