library(ggplot2)
df= read.csv("physical1.csv")

#Question 1
ggplot(data=df,aes(x=X))+
  geom_line(aes(y=Z),color="red")+
  geom_line(aes(y=Y),color="blue")


lambda = 2

#Question 2


#Krzysztof code - Changed

y_functionloglik<-function(x, z, y, lambda_k, n){(sum(x*z) + lambda_k + sum(x*y))/4*n}

EM<-function(x,y,z,lambda, eps, kmax){ #To fix
  Zobs <- Z[!is.na(Z)]
  Zmiss <- Z[is.na(Z)]
  n <- length(c(Zobs, Zmiss))
  r <- length(Zobs)
  
  k<-1
  
  llvalprev<-floglik(Yobs,muk,sigma2k,r);
  llvalcurr<-llvalprev+10+100*eps
  print(c(muk,sigma2k,llvalcurr))
  
  while ((abs(llvalprev-llvalcurr)>eps) && (k<(kmax+1))){
    llvalprev<-llvalcurr
    ## E-step
    EY<-sum(Yobs)+(n-r)*muk
    EY2<-sum(Yobs^2)+(n-r)*(muk^2+sigma2k)
    
    ## M-step
    muk<-EY/n
    sigma2k<-EY2/n-muk^2
    
    ## Compute log-likelihood
    llvalcurr<-floglik(Yobs,muk,sigma2k,r)
    k<-k+1
    
    print(c(muk,sigma2k,llvalcurr))
  }
}