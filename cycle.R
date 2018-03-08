#这种算周期的方法尚有两个问题需要解决：
#1.如果碰到消散点刚好都在x=0前后事情会很麻烦；
#2.排队过长的时候长排队的后端会被割为两部分。解决方案：只计6m/s*一个红灯时间内的排队样本。

library(sqldf)
ti<-c()#定义一个空向量存储过停车线时间
cycle<-90#上一个交叉口的周期
pasttime_code<-"C:/Users/10993/Desktop/上创/ForR/pasttime.R"
direction<-1#区分数据方向的参数，车辆轨迹数据从小到大为1

#选取停车数据获取通过停车线时间，并备份一份不会被修改的rawstoppass数据
#stoppass停车通过的样本
#redtime是红灯时间，取98%分位的stoppass的停车时间
rawstoppass<-read.table("C:/Users/10993/Desktop/上创/ForR/stoppass.csv",header=T,sep=",")
stoppass<-rawstoppass
stoppass$stoptime<-stoppass$l_t-stoppass$f_t#计算停车时间
redtime<-quantile(stoppass$stoptime,0.98)/0.98#假设为均匀到达，那么取0.98分位数就还应该方法1/0.98倍。
stopline<-max(stoppass$y)#停车线位置

stoppass<-stoppass[which(stoppass$y<6*redtime),]#这一句注意改改

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

#rsq为消散波线性拟合R方
#greenshot为切换绿灯时间
#dep为排队消散速度(depart)
#cycle为周期长度
rsq<-0
for(x in ceiling(redtime):600){
  lim<-summary(lm(stoppass$y~(stoppass$l_t%%x)))
  rsqua<-lim$adj.r.squared
  GreenShot<--(lim$coefficients[1,1]-stopline)/lim$coefficients[2,1]
  ticyc<-ti%%x
  green<-GreenShot-0.1*redtime
  red<-0.8*redtime
  if (green>red) y<-length(ticyc[ticyc>green|ticyc<(green-red)])
  if (green<red) y<-length(ticyc[ticyc>green&ticyc<(green-red+x)])
  y<-rsqua/(1+max(0.95*length(ticyc)-y,0))
  if (rsq<y) {rsq<-y;dep<-lim$coefficients[2,1];cycle<-x;greenshot<-GreenShot}
}

#样本按周期分组
stoppass$lt<-stoppass$l_t-greenshot+0.5*cycle
stoppass$cycleno<-stoppass$lt%/%cycle+1

#随机抽取部分样本
pr=0.05
fcd<-stoppass[sample(nrow(stoppass),ceiling(nrow(stoppass)*pr)),]

#cyc为不同周期，这个数据框中cycno是唯一属性
cyc<-data.frame(no=1:max(fcd$cycleno))
#这个是确定每个周期的红灯开始时间
cyc$red<-round((cyc$no-1)*cycle+greenshot-redtime,0)
#这个是确定每个周期的绿灯开始时间
cyc$gre<-round((cyc$no-1)*cycle+greenshot,0)
#红灯特征点的纵坐标
cyc$y<-stopline

#special_point为集聚特征点，f_t为横坐标，y为纵坐标，cycleno为周期编号
special_point<-subset(fcd,select=c(f_t,y,cycleno))
special_point<-rbind(special_point,data.frame(f_t=cyc$red,y=cyc$y,cycleno=cyc$no))
special_point<-special_point[order(special_point$f_t),]

source(pasttime_code)

#接下来这部分要算流量与单位车辆停车长度的乘积len_vol
cyc$len_vol<-NA
for(x in 1:max(fcd$cycleno)){
  if(nrow(special_point[which(special_point$cycleno==x),])>1){
    maxx<-sqldf(paste("select max(f_t) from special_point where cycleno=",x))
    maxy<-sqldf(paste("select max(y) from special_point where cycleno=",x))
    minx<-sqldf(paste("select min(f_t) from special_point where cycleno=",x))
    miny<-sqldf(paste("select min(y) from special_point where cycleno=",x))
    cyc[x,5]<-(maxy-miny)/(accum(ti,as.numeric(maxx%%cycle))-accum(ti,as.numeric(minx%%cycle)))
  }
}

#剔除异常len_vol（很粗暴的处理方法，这段代码必须改）
normal<-quantile(cyc$len_vol,.75,na.rm=T)
for(x in 1:max(fcd$cycleno)){
  if(!is.na(cyc[x,5])){
    if(cyc[x,5]>normal) cyc[x,5]<-normal    
  }
}

#挑最近的周期去替换确实的len_vol数据
for(x in 1:max(fcd$cycleno)){
  if(is.na(cyc[x,5])){
    min<-max(fcd$cycleno)
    minmark<-1
    for(y in 1:max(fcd$cycleno)){
      if(!is.na(cyc[y,5])){if(abs(y-x)<min) {minmark<-y;min<-abs(y-x)}}
    }
    cyc[x,5]<-cyc[minmark,5]
  }
}

#排队一整个周期的排队曲线
#ti用于计算累计频率曲线向量
spe<-mean(stoppass$f_k)
len<-c()
len[1]<-0
for(x in 1:cycle){
  per[x+1]<-queue(0,x,cyc[1,5],spe,accum,ti)$root
}
plot(0:cycle,per,type="l")

#建立空的坐标系
plot(0,0,xlim=c(0,500),ylim=c(min(stoppass$y)-100,max(stoppass$y)+100))
#按每周期添加排队曲线
for(x in 1:max(fcd$cycleno)){
  former_x<-cyc[x,2];former_y<-stopline;
  latter_x<-cyc[x,2]+1;latter_y<-0;
  #如果所绘制的点与绿灯切换点的连线斜率小于消散波斜率，则绘制消散波曲线并终止循环
  while(!(latter_x>cyc[x,3]&abs(latter_y-stopline)/(latter_x-cyc[x,3])<abs(dep))){
    latter_y<-queue((cyc[x,2]+0.5)%%cycle,latter_x%%cycle,cyc[x,5],spe,accum,ti)$root
    latter_y<-stopline-direction*latter_y
    lines(c(former_x,latter_x),c(former_y,latter_y),type="l")
    #数据迭代
    former_x<-latter_x;former_y<-latter_y;latter_x<-latter_x+1;
  }
  lines(c(cyc[x,3],latter_x),c(stopline,latter_y),type="l")
}