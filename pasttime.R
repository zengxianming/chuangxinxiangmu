library(sqldf)
stopline<-400#ͣ����λ��
ti<-c()#����һ���������洢��ͣ����ʱ��
cycle<-90#��һ������ڵ�����

accum<-function(array,x){
  return(length(array[array<x])/length(array))
}

#ѡȡ����ͨ�����ݻ�ȡͨ��ͣ����ʱ��
#freepass����ͨ������������
freepass<-read.table("C:/Users/10993/Desktop/�ϴ�/ForR/freepass.csv",header=T,sep=",")
distinct_vec_no<-sqldf("select distinct vec_no from freepass")#ѡȡͨ��ͣ���ߵ����г������
for (x in distinct_vec_no[,1]){
  tra<-freepass[which(freepass$vec_no==x),]#ѡȡÿһ�����Ĺ켣
  n<-nrow(tra)
  tra<-sqldf("select t as time,len from tra order by len desc")#����ʻ���뽵������
  for(j in 1:(n-1)){
    if (tra[j,2]>=stopline && tra[j+1,2]<stopline){
      ti[length(ti)+1]<-(tra[j+1,1]-tra[j,1])/(tra[j+1,2]-tra[j,2])*(stopline-tra[j,2])+tra[j,1]
    }
  }
}

#ѡȡͣ�����ݻ�ȡͨ��ͣ����ʱ��
#stoppassͣ��ͨ��������
stoppass<-read.table("C:/Users/10993/Desktop/�ϴ�/ForR/stoppass.csv",header=T,sep=",")
ti<-c(ti,as.array((stopline-stoppass$y)/stoppass$f_k+stoppass$f_t))

#�ۼ�Ƶ������
ti<-ti%%cycle

#per������ÿһ����ۼ�Ƶ��
per<-c()
for(x in 0:cycle){
  per[x+1]<-accum(ti,x)
}
plot(0:cycle,per)

#���㽫��ĳ��ʱ�̵��Ŷӳ���
#nowΪ��ǰʱ��
#latΪ����ʱ��
#accumΪ�����ۼưٷֱȵĺ���
#vol_lenΪ�����뵥λ����ͣ�����ȵĳ˻�
#speΪ���ڵ�����
#ini�ǳ�ʼ�Ŷӳ��ȣ�Ĭ��Ϊ0
queue<-function(now,lat,vol_len,spe,accum,ti,ini=0){
  chang<-function(ti,now,lat,spe,ini,lal,vol_len) (accum(ti,lat+lal/spe)-accum(ti,now+ini/spe))*vol_len-(lal-ini)
  return(uniroot(chang,c(200*min(now-lat,lat-now),200*max(now-lat,lat-now)),ti=ti,
                 now=now,lat=lat,spe=spe,ini=ini,vol_len=vol_len,tol=1))
}

#�Ŷ�һ�������ڵ��Ŷ�����
len<-c()
len[1]<-0
for(x in 1:cycle){
  per[x+1]<-queue(0,x,60,30,accum,ti)$root
}
plot(0:cycle,per)