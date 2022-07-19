n= 50

alpha = 0.25
alpha.a = 0.2
alpha.b = 0.1

#taking log(s+1)
gamma.null.log<- function(x){
  g = exp(alpha + log(x))/(1 + exp(alpha + log(x)))
  return(g)
}

gamma.A.log<- function(x){
  g = exp(alpha.a + log(x))/(1 + exp(alpha.a + log(x)))
  return(g)
}

gamma.B.log<- function(x){
  g = exp(alpha.b + log(x))/(1 + exp(alpha.b + log(x)))
  return(g)
}

pie.A.1 = gamma.A.log(1)
pie.A.2 = gamma.A.log(2) - gamma.A.log(1)
pie.A.3 = gamma.A.log(3) - gamma.A.log(2)

pie.B.1 = gamma.B.log(1)
pie.B.2 = gamma.B.log(2) - gamma.B.log(1)
pie.B.3 = gamma.B.log(3) - gamma.B.log(2)

#for 3 categorical response
ZA=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.A.1, pie.A.2, pie.A.3)))
ZB=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.B.1, pie.B.2, pie.B.3)))


fa1=sum(ZA==1)
fa2=sum(ZA==2)
fa3=sum(ZA==3)


fb1=sum(ZB==1)
fb2=sum(ZB==2)
fb3=sum(ZB==3)


fa=c(fa1,fa2,fa3)
fb=c(fb1,fb2,fb3)


ZAB=double(n)
ZBA=double(n)


for(j in 1:n)
{
  if(ZA[j]==1)
  {
    ZAB[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(0.060/0.25, 0.15/0.25, 0.040/0.25))
  }
  else if(ZA[j]==2)
  {
    ZAB[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(0.125/0.5,0.25/0.5, 0.125/0.5))
  }
  else if(ZA[j]==3)
  {
    ZAB[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(0.060/0.25, 0.15/0.25, 0.040/0.25))
  }
  
  
  if(ZB[j]==1)
  {
    ZBA[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(0.060/0.25, 0.15/0.25, 0.040/0.2))
  }
  else if(ZB[j]==2)
  {
    ZBA[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(0.125/0.5,0.25/0.5, 0.125/0.5))
  }
  else if(ZB[j]==3)
  {
    ZBA[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(0.060/0.25, 0.15/0.25, 0.040/0.2))
  }
}

fab1=sum(ZAB==1)
fab2=sum(ZAB==2)
fab3=sum(ZAB==3)


fba1=sum(ZBA==1)
fba2=sum(ZBA==2)
fba3=sum(ZBA==3)


fab=c(fab1,fab2,fab3)
fba=c(fba1,fba2,fba3)



pa.1<-(fa1+1/4)/(n+1);pa.2<-(fa2+1/4)/(n+1);pa.3<-(fa3+1/4)/(n+1)    #p_A (j)
pb.1<-(fb1+1/4)/(n+1);pb.2<-(fb2+1/4)/(n+1);pb.3<-(fb3+1/4)/(n+1)    #p_B (j)

pab.1<-(fab1+1/4)/(n+1);pab.2<-(fab2+1/4)/(n+1);pab.3<-(fab3+1/4)/(n+1)   #p_AB (j)
pba.1<-(fba1+1/4)/(n+1);pba.2<-(fba2+1/4)/(n+1);pba.3<-(fba3+1/4)/(n+1)    #p_BA (j)


p1.1<-(pa.1+pb.1)/2;p1.2<-(pa.2+pb.2)/2;p1.3<-(pa.3+pb.3)/2
p2.1<-(pab.1+pba.1)/2;p2.2<-(pab.2+pba.2)/2;p2.3<-(pab.3+pba.3)/2


ca1=(sum(ZA==1))/n
ca2=(sum(ZA==2))/n
ca3=(sum(ZA==3))/n

cb1=(sum(ZB==1))/n
cb2=(sum(ZB==2))/n
cb3=(sum(ZB==3))/n

cab1 = fab1/n
cab2 = fab2/n
cab3 = fab3/n

cba1 = fba1/n
cba2 = fba2/n
cba3 = fba3/n


g.func<- function(x){
  func_value = log(x)/(1-log(x))
  return(func_value)
}

sum.1 = sum((g.func(ca1)-(g.func(cb1))), (g.func(ca2)-(g.func(cb2))),(g.func(ca3)-(g.func(cb3))) )

#cum.pa1 = 0.25
#cum.pa2 = cum.pa1 + 0.5
#cum.pa3 = cum.pa2 + 0.25


#cum.pb1 = 0.25
#cum.pb2 = cum.pb1 + 0.5
#cum.pb3 = cum.pb2 + 0.25

sigma_xy<- function(x,y){
  sig_xy = 1/((1-x)*y)
  return(sig_xy)
}

sig.11 = sigma_xy(gamma.null.log(1), gamma.null.log(1))
sig.12 = sigma_xy(gamma.null.log(1), gamma.null.log(2))
#sig.13 = sigma_xy(cum.pa1, cum.pa3)
sig.21 = sigma_xy(gamma.null.log(1), gamma.null.log(2))
sig.22 = sigma_xy(gamma.null.log(2), gamma.null.log(2))
#sig.23 = sigma_xy(cum.pa2, cum.pa3)
#sig.31 = sigma_xy(cum.pa1, cum.pa3)
#sig.32 = sigma_xy(cum.pa2, cum.pa3)
#sig.33 = sigma_xy(cum.pa3, cum.pa3)


SIGMA_1 = sum(sig.11, sig.12,  sig.21, sig.22)

tau_1 = sum.1/sqrt((2*SIGMA_1)/n)  #test_statistic under H0


tau_1





