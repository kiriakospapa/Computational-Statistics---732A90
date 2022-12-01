#Question 2. Gibbs sampling

#1. 
load("chemical.RData")

library(ggplot2)

data= data.frame(X,Y)
ggplot(data, aes(x=X,y=Y))+
  geom_point()

#2.

