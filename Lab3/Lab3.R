#1.1
library(ggplot2)

f_x <- function(x,c){
  val = c*((sqrt(2*pi))**-1)*exp((-c**2)/(2*x))*x**(-3/2)
  return (val)
}

fp_x <- function(x,t_min,alpha){
  val = c()
  for (i in 1:length(x)){
    if (t_min <= x[i]){
      val[i] = ((alpha-1)/t_min)*(x[i]/t_min)**-alpha
    } 
    else{
      val[i] = 0
    }
  }
  return (val)
}

#c = 1.2
#t_min = 1.5
#alpha = 1.25



t_min=0.8
c=1.5
alpha=1.15

library(plotly)
x = seq(0.01,100,0.01)
#g1 = dgamma(x,shape=24,scale=0.0625)
#x_2 = seq(0.01,t_min,0.01)
#unif= dunif(x, min = 0, max = t_min)
df = data.frame(x,f_x(x,c),fp_x(x,t_min,alpha),unif)
colnames(df) = c("x", "fx","fpx","unif")


p <- ggplot(df, aes(x=x)) +
  geom_line(aes(y=fx, color="f(x)"))+
  geom_line(aes(y=fpx, color="fp(x)"))+
  scale_color_manual(name = "Functions",
                     values=c("f(x)" = "red",
                              "fp(x)" = "blue"))+
  ylab("Y Values")+
  ggtitle("Y values over x")+
  xlim(0,100)
p
ggplotly(p)
  

#Visualization type of solution to get:


#Can the power{law distribution be used just by itself or is
#there a problem at any place of the support Explain what the problem is

#A: Missing values at fp(x) between to t_min, which cant generate values for these
#values.


#how can it be taken care of
#A:??? Find similar distribution? 

#Provide values of the power{law distribution's parameters that can be
#used in the acceptance{rejection algorithm.


#A: For example:
#t_min=0.8
#c=1.5
#alpha=1.15
#This makes the supported majorizing density directly under the target density.
#Later on, it will be multiplied by a majorizing constant to envelop the target density.
#but when the values goes towards infinity, the majorizing density is higher than target density.
#The only value that needs to be found is the between 0 to Tmin to have a fully completed majorizing density.

#Q: Derive and implement a majorizing density.
#A: The majorizing density lack support from (0,Tmin)
#To get values for corresponding range, a uniform distribution will be used
#with alpha = 0 and beta = t_min. 



x = seq(0.01,100,0.01)

df = data.frame(x,f_x(x,c),fp_x(x,t_min,alpha),rep(((alpha-1)/t_min)*(t_min/t_min)**-alpha,))
colnames(df) = c("x", "fx","fpx")


p <- ggplot(df, aes(x=x)) +
  geom_line(aes(y=fx, color="f(x)"))+
  geom_line(aes(y=fpx, color="fp(x)"))+
  scale_color_manual(name = "Functions",
                     values=c("f(x)" = "red",
                              "fp(x)" = "blue"))+
  ylab("Y Values")+
  ggtitle("Y values over x")+
  xlim(0,100)
p
ggplotly(p)


#2.
#To find majorizing constant, for (0,Tmin) and (Tmin, infinity)
#It can be found by c =  target density / majorizing constant.
#Which will give 2 different c values.

fp_x <- function(x,t_min,alpha,c1,c2){
  val = c()
  for (i in 1:length(x)){
    if (t_min <= x[i]){
      val[i] = (((alpha-1)/t_min)*(x[i]/t_min)**-alpha)*c1
    } 
    else{
      val[i] = (((alpha-1)/t_min)*(t_min/t_min)**-alpha)*c2
    }
  }
  return (val)
}



df2 = df %>% filter(x <= t_min)
mconstant = max(df$fx/df$fpx)
mconstant2 = max(df2$fx/df2$fpx)



fgenonesided<-function(n){
  results = c()
  num.reject<-0
  for(i in 1:n){
    x<-NA
    while (is.na(x)){
      y<-fp_x(runif(1), t_min, alpha, 1, 1)
      u<-runif(1)
      if (u<=f_x(y, c)/fp_x(y, t_min, alpha, mconstant, mconstant2)){
        x<-y
        results=append(results, x)
      }
      else{
        num.reject<-num.reject+1
      }
    }  
  }
  return(c(results,num.reject))
}

tmp = fgenonesided(2000)
#p_tmp = ggplot() + geom_histogram(aes(tmp),binwidth=1)
hist(tmp,col="black",breaks=100,xlab="",ylab="",freq=FALSE,main="")
#hist(tmp,col=gray(0.8),breaks=100,xlab="",ylab="",freq=FALSE,main="",add=TRUE)

mbetas1<-sapply(rep(4,10000),fgenonesided)


