rt<-read.csv('D:/texlive/texstudio/sample.csv',head=TRUE);
rt<-rt[-1,];rt<-rt[,-1]
rt[c(32,60,73,190),7]<-c('否','否','否','否')  #数据预处理
for (i in c(2,3,4,5,6,10,11,9)){         #将数据类型转为数值型
  rt[,i]<-as.numeric(rt[,i])}
weight<-rt[,5]; 
FPG<-rt[,9]; 
SBP<-rt[,3]; DBP<-rt[,4]; TG<-rt[,10]; HDLC<-rt[,11];
hight<-rt[,6]
hight[is.na(hight)]<-mean(hight,na.rm=T)    #缺失数据用均值填充
weight[is.na(weight)]<-mean(weight,na.rm=T)
SBP[is.na(SBP)]<-mean(SBP,na.rm=T) 
DBP[is.na(DBP)]<-mean(DBP,na.rm=T) 
BMI<-weight*10000/hight^2    #计算BMI
X<-matrix(c(BMI,FPG,SBP,DBP,TG,HDLC),ncol=6)
cov<-cov(X)     #计算样本协方差阵，相关系数矩阵，样本均值。
cor<-cor(X)
Xmean<-c(mean(BMI),mean(FPG),mean(SBP),mean(DBP),mean(TG),mean(HDLC))
male1<-0
male<-0
female1<-0
female<-0
smoke<-0;smoke0<-0;smoketosick<-0;smokenosick<-0
smoke0tosick<-0;smoke0nosick<-0
drunk<-0;drunk0<-0;drunktosick<-0;drunknosick<-0
drunk0tosick<-0;drunk0nosick<-0
for (i in 1:270){
  count<-0
  if (rt[i,1]=='男'){
    male=male+1
    if (BMI[i]>=25)
      count=count+1
    if (FPG[i]>=6.1)
      count=count+1
    if (SBP[i]>=140 | DBP[i]>=90)
      count=count+1
    if (TG[i]>=1.7 | HDLC[i]<0.9)
      count=count+1 }
      if (count>=3){
        male1=male1+1
        rt[i,12]<-1}  #患病标记为-1
      }
for (i in 1:270){
  count<-0
  if (rt[i,1]=='女'){
    female=female+1
    if (BMI[i]>=25)
      count=count+1
    if (FPG[i]>=6.1)
      count=count+1
    if (SBP[i]>=140 | DBP[i]>=90)
      count=count+1
    if (TG[i]>=1.7 | HDLC[i]<1)
      count=count+1 }
      if (count>=3){
        female1=female1+1
        rt[i,12]<-1}
  }
for (i in 1:270){
  if (rt[i,7]=='是'){
    smoke=smoke+1
    if (is.na(rt[i,12]))
      smokenosick<-smokenosick+1
    else
      smoketosick<-smoketosick+1}
  if (rt[i,7]=='否'){
    smoke0=smoke0+1
    if (is.na(rt[i,12]))
      smoke0nosick<-smoke0nosick+1
    else
      smoke0tosick<-smoke0tosick+1}
}
for (i in 1:270){
  if (rt[i,8]=='是'){
    drunk=drunk+1
    if (is.na(rt[i,12]))
      drunknosick<-drunknosick+1
    else
      drunktosick<-drunktosick+1}
  if (rt[i,8]=='无'|rt[i,8]=='否'){
    drunk0=drunk0+1
    if (is.na(rt[i,12]))
      drunk0nosick<-drunk0nosick+1
    else
      drunk0tosick<-drunk0tosick+1}
}
young<-c(2)   #找出年龄在20-30的数据组成一个新矩阵Xyoung
for (i in 3:270){
  if (is.na(rt[i,2]))
    next
  else
    if (20<=rt[i,2]&rt[i,2]<=30)
      young<-c(young,i)}
Xyoung<-X[young,]
source("outline.R")
outline(Xyoung)  #轮廓图
source("unison.R")
unison(Xyoung)   #调和曲线图
stars(Xyoung)   #雷达图