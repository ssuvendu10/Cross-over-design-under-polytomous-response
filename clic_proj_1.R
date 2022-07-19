n= 50

alpha = 0.15
alpha.a = 0.9
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

g.func<- function(x){
  func_value = log(x)/(1-log(x))
  return(func_value)
}

sigma_xy<- function(x,y){
  sig_xy = 1/((1-x)*y)
  return(sig_xy)
}

pie.1 = gamma.null.log(1)
pie.2 = gamma.null.log(2) - gamma.null.log(1)
pie.3 = gamma.null.log(3) - gamma.null.log(2)

pie.A.1 = gamma.A.log(1)
pie.A.2 = gamma.A.log(2) - gamma.A.log(1)
pie.A.3 = gamma.A.log(3) - gamma.A.log(2)

pie.B.1 = gamma.B.log(1)
pie.B.2 = gamma.B.log(2) - gamma.B.log(1)
pie.B.3 = gamma.B.log(3) - gamma.B.log(2)

tau.1 = NULL
tau.1.null = NULL

#calculating empirical p-value
for(i in 1:10000){
  #for 3 categorical response
  ZA.null = as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.1, pie.2, pie.3)))
  ZB.null = as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.1, pie.2, pie.3)))
  ZA=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.A.1, pie.A.2, pie.A.3)))
  ZB=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.B.1, pie.B.2, pie.B.3)))
  
  fa1.null=sum(ZA.null==1)
  fa2.null=sum(ZA.null==2)
  fa3.null=sum(ZA.null==3)
  
  
  fb1.null=sum(ZB.null==1)
  fb2.null=sum(ZB.null==2)
  fb3.null=sum(ZB.null==3)
  
  
  ca1.null=(sum(ZA.null==1))/n
  ca2.null=(sum(ZA.null==2))/n
  ca3.null=(sum(ZA.null==3))/n
  
  cb1.null=(sum(ZB.null==1))/n
  cb2.null=(sum(ZB.null==2))/n
  cb3.null=(sum(ZB.null==3))/n
  
  sum.1.null = sum((g.func(ca1.null)-(g.func(cb1.null))), (g.func(ca2.null)-(g.func(cb2.null))),(g.func(ca3.null)-(g.func(cb3.null))) )
  
  fa1=sum(ZA==1)
  fa2=sum(ZA==2)
  fa3=sum(ZA==3)
  
  
  fb1=sum(ZB==1)
  fb2=sum(ZB==2)
  fb3=sum(ZB==3)
  
  
  fa=c(fa1,fa2,fa3)
  fb=c(fb1,fb2,fb3)
  
  ca1=(sum(ZA==1))/n
  ca2=(sum(ZA==2))/n
  ca3=(sum(ZA==3))/n
  
  cb1=(sum(ZB==1))/n
  cb2=(sum(ZB==2))/n
  cb3=(sum(ZB==3))/n
  
  sum.1 = sum((g.func(ca1)-(g.func(cb1))), (g.func(ca2)-(g.func(cb2))),(g.func(ca3)-(g.func(cb3))) )
  
  sig.11 = sigma_xy(gamma.null.log(1), gamma.null.log(1))
  sig.12 = sigma_xy(gamma.null.log(1), gamma.null.log(2))
  sig.21 = sigma_xy(gamma.null.log(1), gamma.null.log(2))
  sig.22 = sigma_xy(gamma.null.log(2), gamma.null.log(2))
  
  SIGMA_1 = sum(sig.11, sig.12,  sig.21, sig.22)
  
  tau_1 = sum.1/sqrt((2*SIGMA_1)/n)  #test_statistic under H0
  tau_1.null = sum.1.null/sqrt((2*SIGMA_1)/n)  #test_statistic under H0
  
  
  tau.1 = c(tau.1,tau_1)
  tau.1.null = c(tau.1.null, tau_1.null)
}

emp.power.1 = sum(which(tau.1>1.645))/10000
emp.power.1
emp.size = sum(which(tau.1.null>1.645))/10000
emp.size
