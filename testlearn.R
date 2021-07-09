mm<-read.csv('D:/texlive/texstudio/sample.csv',head=TRUE);
rt<-rt[-1,];rt<-rt[,-1]
rt[c(32,60,73,190),7]<-c('??','??','??','??')  #????Ԥ????
for (i in c(2,3,4,5,6,10,11,9)){         #??????????תΪ??ֵ??
  rt[,i]<-as.numeric(rt[,i])}
weight<-rt[,5]; 
FPG<-rt[,9]; 
SBP<-rt[,3]; DBP<-rt[,4]; TG<-rt[,10]; HDLC<-rt[,11];
hight<-rt[,6]
hight[is.na(hight)]<-mean(hight,na.rm=T)    #ȱʧ?????þ?ֵ????
weight[is.na(weight)]<-mean(weight,na.rm=T)
SBP[is.na(SBP)]<-mean(SBP,na.rm=T) 
DBP[is.na(DBP)]<-mean(DBP,na.rm=T) 
BMI<-weight*10000/hight^2    #????BMI
X<-matrix(c(BMI,FPG,SBP,DBP,TG,HDLC),ncol=6)
cov<-cov(X)     #????????Э????????????ϵ??????????????ֵ??
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
  if (rt[i,1]=='??'){
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
        rt[i,12]<-1}  #????????Ϊ-1
      }
test 3