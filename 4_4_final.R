n = 500
t = 10000
alpha_a = 0.6
alpha_b = 0.3
tau.1 = NULL
tau.2 = NULL
gamma.log<- function(x,a){
  g = exp(a + log(x))/(1 + exp(a + log(x)))
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

pie.A.1 = gamma.log(1,alpha_a)
pie.A.2 = gamma.log(2,alpha_a) - gamma.log(1,alpha_a)
pie.A.3 = 1 - pie.A.1 - pie.A.2

pie.B.1 = gamma.log(1,alpha_b)
pie.B.2 = gamma.log(2,alpha_b) - gamma.log(1,alpha_b)
pie.B.3 = 1 - pie.B.1 - pie.B.2

indicator.func <- function(z,i,s){
  z = c(rep(0,n))
  if(z[i] == s){
    x = 1
  }
  else
    x = 0
  return(x)
}

#2nd stage 

#consider differnet alpha for getting different gamma
#for log(s)
pie.AB.1 = gamma.log(1, alpha_a)
pie.AB.2 = gamma.log(2, alpha_a) - gamma.log(1, alpha_a)
pie.AB.3 = 1 - pie.AB.1 - pie.AB.2

pie.BA.1 = gamma.log(1, alpha_b)
pie.BA.2 = gamma.log(2, alpha_b) - gamma.log(1, alpha_b)
pie.BA.3 = 1 - pie.BA.1 - pie.BA.2

#conditional probability
cond.prob.AB.1 = (1/(1+alpha_b))*(pie.AB.1 + (alpha_b/3)*((pie.AB.1 - pie.A.1)+ indicator.func(ZA,1,1)) + (alpha_b/3)*((pie.AB.1 - pie.A.2)+ indicator.func(ZA,1,2)) + (alpha_b/3)*((pie.AB.1 - pie.A.3)+ indicator.func(ZA,1,3)))
cond.prob.AB.2 = (1/(1+alpha_b))*(pie.AB.2 + (alpha_b/3)*((pie.AB.2 - pie.A.1)+ indicator.func(ZA,1,1)) + (alpha_b/3)*((pie.AB.2 - pie.A.2)+ indicator.func(ZA,1,2)) + (alpha_b/3)*((pie.AB.2 - pie.A.3)+ indicator.func(ZA,1,3)))
cond.prob.AB.3 = 1 - cond.prob.AB.1 - cond.prob.AB.2

cond.prob.BA.1 = (1/(1+alpha_a))*(pie.BA.1 + (alpha_a/3)*((pie.BA.1 - pie.B.1)+ indicator.func(ZB,1,1)) + (alpha_a/3)*((pie.BA.1 - pie.B.2)+ indicator.func(ZB,1,2)) + (alpha_a/3)*((pie.BA.1 - pie.B.3)+ indicator.func(ZB,1,3)))
cond.prob.BA.2 = (1/(1+alpha_a))*(pie.BA.2 + (alpha_a/3)*((pie.BA.2 - pie.B.1)+ indicator.func(ZB,1,1)) + (alpha_a/3)*((pie.BA.2 - pie.B.2)+ indicator.func(ZB,1,2)) + (alpha_a/3)*((pie.BA.2 - pie.B.3)+ indicator.func(ZB,1,3)))
cond.prob.BA.3 = 1 - cond.prob.BA.1 - cond.prob.BA.2

ZAB=double(n)
ZBA=double(n)

#loop for emperical size and power:


for(i in 1:t){
  ZA=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.A.1, pie.A.2, pie.A.3)))
  ZB=as.vector(c(1,2,3)%*%rmultinom(n, size = 1, prob = c(pie.B.1, pie.B.2, pie.B.3)))
  for(j in 1:n)
  {
    if(ZA[j]==1)
    {
      ZAB[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(cond.prob.AB.1, cond.prob.AB.2, cond.prob.AB.3))
    }
    else if(ZA[j]==2)
    {
      ZAB[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(cond.prob.AB.1, cond.prob.AB.2, cond.prob.AB.3))
    }
    else if(ZA[j]==3)
    {
      ZAB[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(cond.prob.AB.1, cond.prob.AB.2, cond.prob.AB.3))
    }
    
    if(ZB[j]==1)
    {
      ZBA[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(cond.prob.BA.1, cond.prob.BA.2, cond.prob.BA.3))
    }
    else if(ZB[j]==2)
    {
      ZBA[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(cond.prob.BA.1, cond.prob.BA.2, cond.prob.BA.3))
    }
    else if(ZB[j]==3)
    {
      ZBA[j]=c(1,2,3)%*%rmultinom(1,1,prob=c(cond.prob.BA.1, cond.prob.BA.2, cond.prob.BA.3))
    }
  }
  
  #calculation stage 1
  
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
  
  
  #for stage 2
  fab1=sum(ZAB==1)
  fab2=sum(ZAB==2)
  fab3=sum(ZAB==3)

  
  fba1=sum(ZBA==1)
  fba2=sum(ZBA==2)
  fba3=sum(ZBA==3)

  
  fab=c(fab1,fab2,fab3)
  fba=c(fba1,fba2,fba3)
  
  #calculating Z-barfor 2nd stage :
  cab1=(fab1)/n
  cab2=(fab1 + fab2)/n
  cab3=(fab1 + fab2 + fab3)/n
  
  cba1=(fba1)/n
  cba2=(fba1 + fba2)/n
  cba3=(fba1 + fba2 + fba3)/n
  
  gamma.est.AB.1 = fa1/n
  gamma.est.AB.2 = fa1/n + fa2/n
  gamma.est.AB.3 = fa1/n + fa2/n +fa3/n
  
  gamma.est.BA.1 = fba1/n
  gamma.est.BA.2 = fba1/n + fba2/n
  gamma.est.BA.3 = fba1/n + fba2/n +fba3/n
  
  sum.2 = sum((g.func(cba1)-(g.func(cab1))), (g.func(cba2)-(g.func(cab2))))
  
  sig.11.AB = sigma_xy(gamma.est.AB.1, gamma.est.AB.1)
  sig.12.AB = sigma_xy(gamma.est.AB.1, gamma.est.AB.2)
  sig.21.AB = sigma_xy(gamma.est.AB.1, gamma.est.AB.2)
  sig.22.AB = sigma_xy(gamma.est.AB.2, gamma.est.AB.2)
  
  SIGMA_1.AB = matrix(c(sig.11.AB, sig.12.AB,  sig.21.AB, sig.22.AB) , nrow = 2)
  
  sig.11.BA = sigma_xy(gamma.est.BA.1, gamma.est.BA.1)
  sig.12.BA = sigma_xy(gamma.est.BA.1, gamma.est.BA.2)
  sig.21.BA = sigma_xy(gamma.est.BA.1, gamma.est.BA.2)
  sig.22.BA = sigma_xy(gamma.est.BA.2, gamma.est.BA.2)
  
  SIGMA_1.BA = matrix(c(sig.11.BA, sig.12.BA,  sig.21.BA, sig.22.BA) , nrow = 2)
  SIGMA_2.hat = matrix(c(mean(sig.11.AB,sig.11.BA), mean(sig.12.AB,sig.12.BA), mean(sig.21.AB,sig.21.BA), mean(sig.22.AB,sig.22.BA)), nrow = 2)
  tau_2 = sum.2/sqrt((2*sum(SIGMA_2.hat))/n)  #test_statistic 
  
  tau.2 = c(tau.2,tau_2)
}
prob.test.stat = sum((tau.1>1.645))/10000
prob.test.stat 

prob.test.stat2 = sum((tau.2>1.645))/10000
prob.test.stat2 

#for test stat
u = NULL
tau_0 = cbind(tau.1,tau.2)
mean_tau = apply(tau_0,2,mean)

SIGMA_hat_0 = cov(tau_0)
for(i in 1:t){
  u[i] = (t(tau_0[i,] - mean_tau))%*%solve(SIGMA_hat_0)%*%((tau_0[i,] - mean_tau))
}
chi_sq_2 = rchisq(t,2)
#KS test:
#install.packages("dgof")
library("dgof")
plot(ecdf(u),
     xlim = range(c(u, chi_sq_2)),
     col = "blue")
plot(ecdf(chi_sq_2),
     add = TRUE,
     lty = "dashed",
     col = "red")
ks.test(u, chi_sq_2)
#qq plot
qqplot(u,chi_sq_2)

#Test statistic of Chaterjee;
Q = NULL
cor_coef = NULL

cor_coef.1 = cor(tau.1[c(1:10)], tau.2[c(1:10)])
for(i in 1:t){
  if(i<=10){
    cor_coef[i] = cor_coef.1
  }
  else{
    cor_coef[i] = cor(tau.1[c(1:i)], tau.2[c(1:i)])
  }
}

for(i in 1:t){
  if(tau.1[i] > 0 && tau.2[i] > 0){
    Q[i] = sqrt((((tau.1[i])^2) + ((tau.2[i])^2) - (2*cor_coef[i]*tau.1[i]*tau.2[i]))/(1 - (cor_coef[i])^2) )
  }
  else if(tau.2[i] > tau.1[i] && tau.1[i] < 0){
    Q[i] = (tau.2[i] - (cor_coef[i]*tau.1[i]))/(1 - (cor_coef[i])^2)
  }
  else if(tau.2[i] < tau.1[i] && tau.2[i] < 0){
    Q[i] = (tau.1[i] - (cor_coef[i]*tau.2[i]))/(1 - (cor_coef[i])^2)
  }
  else{
    Q[i] = 0
  }
}
#chaterjee stat statistic:
chi.value = qchisq(0.05,2,lower.tail = TRUE)
emperical_power_Q = sum((Q > chi.value))/t #chi_square(0.05,2)


#mean test stat
mean_tau = (tau.1 + tau.2)/2
mean_tau_order = sort(mean_tau, decreasing = FALSE)

emperical_power_mean = sum((mean_tau > 1.13343))/t #chi_square(0.05,2)

mean_tau_star = mean_tau/(sqrt((1+cor_coef)/2))
#maximum tau
max_tau = NULL
max_tau = pmax(tau.1, tau.2)
max_tau_order = sort(max_tau, decreasing = FALSE)

emperical_power_max = sum((max_tau > 1.917603))/t #chi_square(0.05,2)




