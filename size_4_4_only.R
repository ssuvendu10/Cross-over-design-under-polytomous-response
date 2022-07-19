n= 50

alpha = 0.45
gamma.null.log<- function(x){
  g = exp(alpha + log(x))/(1 + exp(alpha + log(x)))
  return(g)
}
g.func<- function(x){
  func_value = log(x/(1-x))
  return(func_value)
}
sigma_xy<- function(x,y){
  sig_xy = 1/((1-x)*y)
  return(sig_xy)
}
pie.1 = gamma.null.log(1)
pie.2 = gamma.null.log(2) - gamma.null.log(1)
pie.3 = 1 - pie.1 - pie.2

tau.1.null = NULL


#for not same alpha cases:
alpha.a = 0.6
alpha.b = 0.3
gamma.A.log<- function(x){
  g = exp(alpha.a + log(x))/(1 + exp(alpha.a + log(x)))
  return(g)
}

gamma.B.log<- function(x){
  g = exp(alpha.b + log(x))/(1 + exp(alpha.b + log(x)))
  return(g)
}
pie.1 = gamma.null.log(1)
pie.2 = gamma.null.log(2) - gamma.null.log(1)
pie.3 = 1 - pie.1 - pie.2

pie.A.1 = gamma.A.log(1)
pie.A.2 = gamma.A.log(2) - gamma.A.log(1)
pie.A.3 = 1 - pie.A.1 - pie.A.2

pie.B.1 = gamma.B.log(1)
pie.B.2 = gamma.B.log(2) - gamma.B.log(1)
pie.B.3 = 1 - pie.B.1 - pie.B.2

tau.1 = NULL

for(i in 1:10000){
  #for 3 categorical response
  ZA.null = as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.1, pie.2, pie.3)))
  ZB.null = as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.1, pie.2, pie.3)))
  
  #alpha_a and alpha_b equal case:
  fa1.null=sum(ZA.null==1)
  fa2.null=sum(ZA.null==2)
  fa3.null=sum(ZA.null==3)
  
  
  fb1.null=sum(ZB.null==1)
  fb2.null=sum(ZB.null==2)
  fb3.null=sum(ZB.null==3)
  
  
  ca1.null=(fa1.null)/n
  ca2.null=(fa1.null + fa2.null)/n
  ca3.null=(fa1.null + fa2.null + fa3.null)/n
  
  cb1.null=(fb1.null)/n
  cb2.null=(fb1.null + fb2.null)/n
  cb3.null=(fb1.null + fb2.null + fb3.null)/n
  
  sum.1.null = sum((g.func(ca1.null)-(g.func(cb1.null))), (g.func(ca2.null)-(g.func(cb2.null))))
  
  
  #estiamting gamma
  gamma.est.A.1.null = fa1.null/n
  gamma.est.A.2.null = fa1.null/n + fa2.null/n
  gamma.est.A.3.null = fa1.null/n + fa2.null/n +fa3.null/n
  
  gamma.est.B.1.null = fb1.null/n
  gamma.est.B.2.null = fb1.null/n + fb2.null/n
  gamma.est.B.3.null = fb1.null/n + fb2.null/n +fb3.null/n
  
  sig.11.A.null = sigma_xy(gamma.est.A.1.null, gamma.est.A.1.null)
  sig.12.A.null = sigma_xy(gamma.est.A.1.null, gamma.est.A.2.null)
  sig.21.A.null = sigma_xy(gamma.est.A.1.null, gamma.est.A.2.null)
  sig.22.A.null = sigma_xy(gamma.est.A.2.null, gamma.est.A.2.null)
  
  SIGMA_1.A.null = matrix(c(sig.11.A.null, sig.12.A.null,  sig.21.A.null, sig.22.A.null) , nrow = 2)
  
  sig.11.B.null = sigma_xy(gamma.est.B.1.null, gamma.est.B.1.null)
  sig.12.B.null = sigma_xy(gamma.est.B.1.null, gamma.est.B.2.null)
  sig.21.B.null = sigma_xy(gamma.est.B.1.null, gamma.est.B.2.null)
  sig.22.B.null = sigma_xy(gamma.est.B.2.null, gamma.est.B.2.null)
  
  SIGMA_1.B.null = matrix(c(sig.11.B.null, sig.12.B.null,  sig.21.B.null, sig.22.B.null) , nrow = 2)
  SIGMA_1.null.hat = matrix(c(mean(sig.11.A.null,sig.11.B.null), mean(sig.12.A.null,sig.12.B.null), mean(sig.21.A.null,sig.21.B.null), mean(sig.22.A.null,sig.22.B.null)), nrow = 2)
 
  tau_1.null = sum.1.null/sqrt((2*sum( SIGMA_1.null.hat))/n)
  
  tau.1.null = c(tau.1.null, tau_1.null)
  
  
  
  #case when alpha_a and alpha_b are not equal
  #alpha not equal case
  ZA=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.A.1, pie.A.2, pie.A.3)))
  ZB=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.B.1, pie.B.2, pie.B.3)))
  
  fa1=sum(ZA==1)
  fa2=sum(ZA==2)
  fa3=sum(ZA==3)
  
  
  fb1=sum(ZB==1)
  fb2=sum(ZB==2)
  fb3=sum(ZB==3)
  
  #calculating Z-bar:
  ca1=(fa1)/n
  ca2=(fa1 + fa2)/n
  ca3=(fa1 + fa2 + fa3)/n
  
  cb1=(fb1)/n
  cb2=(fb1 + fb2)/n
  cb3=(fb1 + fb2 + fb3)/n
  
  gamma.est.A.1 = fa1/n
  gamma.est.A.2 = fa1/n + fa2/n
  gamma.est.A.3 = fa1/n + fa2/n +fa3/n
  
  gamma.est.B.1 = fb1/n
  gamma.est.B.2 = fb1/n + fb2/n
  gamma.est.B.3 = fb1/n + fb2/n +fb3/n
  
  sum.1 = sum((g.func(ca1)-(g.func(cb1))), (g.func(ca2)-(g.func(cb2))))
  
  sig.11.A = sigma_xy(gamma.est.A.1, gamma.est.A.1)
  sig.12.A = sigma_xy(gamma.est.A.1, gamma.est.A.2)
  sig.21.A = sigma_xy(gamma.est.A.1, gamma.est.A.2)
  sig.22.A = sigma_xy(gamma.est.A.2, gamma.est.A.2)
  
  SIGMA_1.A = matrix(c(sig.11.A, sig.12.A,  sig.21.A, sig.22.A) , nrow = 2)
  
  sig.11.B = sigma_xy(gamma.est.B.1, gamma.est.B.1)
  sig.12.B = sigma_xy(gamma.est.B.1, gamma.est.B.2)
  sig.21.B = sigma_xy(gamma.est.B.1, gamma.est.B.2)
  sig.22.B = sigma_xy(gamma.est.B.2, gamma.est.B.2)
  
  SIGMA_1.B = matrix(c(sig.11.B, sig.12.B,  sig.21.B, sig.22.B) , nrow = 2)
  SIGMA_1.hat = matrix(c(mean(sig.11.A,sig.11.B), mean(sig.12.A,sig.12.B), mean(sig.21.A,sig.21.B), mean(sig.22.A,sig.22.B)), nrow = 2)
  
  tau_1 = sum.1/sqrt((2*sum(SIGMA_1.hat))/n)  #test_statistic 
  
  tau.1 = c(tau.1,tau_1)
  
  
}

emp.size = sum((tau.1.null>1.645))/10000
emp.size 
emp.power.1 = sum(which(tau.1>1.645))/10000
emp.power.1