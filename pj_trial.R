n=100
ZA=as.vector(c(1,2,3,4)%*%rmultinom(n, size = 1, prob = c(0.15,0.2,0.25,0.4)))
ZB=as.vector(c(1,2,3,4)%*%rmultinom(n, size = 1, prob = c(0.15,0.2,0.25,0.4)))


ZAB=double(n)
ZBA=double(n)

fa1=sum(ZA==1)
fa2=sum(ZA==2)
fa3=sum(ZA==3)
fa4=sum(ZA==4)

fb1=sum(ZB==1)
fb2=sum(ZB==2)
fb3=sum(ZB==3)
fb4=sum(ZB==4)

fa=c(fa1,fa2,fa3,fa4)
fb=c(fb1,fb2,fb3,fb4)


for(j in 1:n)
{
  if(ZA[j]==1)
  {
    ZAB[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.075/.15,0.025/.15,0.025/.15,0.025/.15))
  }
  else if(ZA[j]==2)
  {
    ZAB[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.025/.2,0.1/.2,0.0375/.2,0.0375/.2))
  }
  else if(ZA[j]==3)
  {
    ZAB[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.025/.25,0.025/.25,0.15/.25,0.05/.25))
  }
  else if(ZA[j]==4)
  {
    ZAB[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.025/.4,0.05/.4,0.0375/.4,0.2875/.4))
  }
  
  if(ZB[j]==1)
  {
    ZBA[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.075/.15,0.025/.15,0.025/.15,0.025/.15))
  }
  else if(ZB[j]==2)
  {
    ZBA[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.025/.2,0.1/.2,0.0375/.2,0.0375/.2))
  }
  else if(ZB[j]==3)
  {
    ZBA[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.025/.25,0.025/.25,0.15/.25,0.05/.25))
  }
  else if(ZB[j]==4)
  {
    ZBA[j]=c(1,2,3,4)%*%rmultinom(1,1,prob=c(0.025/.4,0.05/.4,0.0375/.4,0.2875/.4))
  }
  
}

fab1=sum(ZAB==1)
fab2=sum(ZAB==2)
fab3=sum(ZAB==3)
fab4=sum(ZAB==4)

fba1=sum(ZBA==1)
fba2=sum(ZBA==2)
fba3=sum(ZBA==3)
fba4=sum(ZBA==4)

fab=c(fab1,fab2,fab3,fab4)
fba=c(fba1,fba2,fba3,fba4)



pa.1<-(fa1+1/4)/(n+1);pa.2<-(fa2+1/4)/(n+1);pa.3<-(fa3+1/4)/(n+1);pa.4<-(fa4+1/4)/(n+1)    #p_A (j)
pb.1<-(fb1+1/4)/(n+1);pb.2<-(fb2+1/4)/(n+1);pb.3<-(fb3+1/4)/(n+1);pb.4<-(fb4+1/4)/(n+1)    #p_B (j)

pa=c(pa.1,pa.2,pa.3,pa.4)
pa
pb=c(pb.1,pb.2,pb.3,pb.4)
pb



pab.1<-(fab1+1/4)/(n+1);pab.2<-(fab2+1/4)/(n+1);pab.3<-(fab3+1/4)/(n+1);pab.4<-(fab4+1/4)/(n+1)    #p_AB (j)
pba.1<-(fba1+1/4)/(n+1);pba.2<-(fba2+1/4)/(n+1);pba.3<-(fba3+1/4)/(n+1);pba.4<-(fba4+1/4)/(n+1)    #p_BA (j)

pab=c(pab.1,pab.2,pab.3,pab.4)

pab

pba=c(pba.1,pba.2,pba.3,pba.4)

pba



