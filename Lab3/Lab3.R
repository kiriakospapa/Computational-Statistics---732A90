library(poweRlaw)
library(ggplot2)
library(gridExtra)


target_function <- function(x, c){
  res = c * sqrt(2 * pi) ^ (-1) * exp((-c^2) / (2*x)) * x ^ (-3/2)
}

majority_function <- function(x, a, t_min){
  res <- c()
  
  for(i in 1:length(x)){
    if(x[i] <= t_min){
      res[i] = ((a - 1) / t_min) * (t_min/t_min) ^ (-a)
    }
    else{
      res[i] <- ((a - 1) / t_min) * (x[i]/t_min) ^ (-a)
    }
  }
  
  return(res)
}

t_min=1.5
c=1.5
alpha=1.5

ggplot() +
  xlim(0.1, 10) +
  geom_function(fun = target_function, args = list(c = c), colour = "red") +
  geom_function(fun = majority_function, colour = "black", args = list(a=alpha,t_min=t_min))


x <- seq(0.001, 10, 0.001)

prob <- sum(x <= t_min) / (sum(x))

# For x bigger than t_min
maj_c1 <- max(target_function(x[x>t_min],c) / majority_function(x[x>t_min], alpha, t_min))

# For x smaller than t_min

maj_c2 <- max(target_function(x[x<=t_min],c) / majority_function(x[x<=t_min], alpha, t_min))



n = 5000

accept_reject <- function(n,c){
  values <- c()
  rejects <- 0
  for(i in 1:n){
    x <- NA
    
    while(is.na(x)){
      prob2 <- sample(0:1, 1, prob = c(prob, 1-prob))
      # For 0 is x <= tmin
      # For 1 is x > tmin
      u <- runif(1)
      
      
      if(prob2 == 0){
        y <- runif(1, 0, t_min)
        
        if(u <= target_function(y, c) / (maj_c2 * majority_function(y, alpha, t_min))){
          x <- y
          values <- append(values, x)
        }else{
          rejects <- rejects + 1
        }
      }
      else{
        y <- rplcon(1, xmin = t_min, alpha = alpha)
        
        if(u <= target_function(y, c) / (maj_c1 * majority_function(y, alpha, t_min))){
          x <- y
          values <- append(values, x)
        }else{
          rejects <- rejects + 1
        }
      }
      
    }
    
  }
  return(c(values,rejects))
}


hist(values[values < 200], col="green", breaks=70, xlab="", ylab="sample density", freq=FALSE, main="")

c_list = c(1,2,3,4,5,6)
results = data.frame(matrix(NA,ncol=length(c_list),nrow=n))
summarized_results = data.frame(matrix(NA,ncol=length(c_list),nrow=3))
plots = c()
df = data.frame()
for (i in 1:length(c_list)) {
  var <- c_list[i]
  answer = accept_reject(n,var)
  rejects = answer[n+1]
  #values = answer[1:n]
  results[i] = answer[1:n]
  summarized_results[1,i] = mean(answer[1:n])
  summarized_results[2,i] = var(answer[1:n])
  summarized_results[3,i] = rejects/n
  df = data.frame(results)
  #plots[i] <- hist(results[i][results[i] < 200], col="green", breaks=70, xlab="", ylab="sample density", freq=FALSE, main="")
}
colnames(results) = c("c1","c2","c3","c4","c5","c6")
colnames(df) = c("c1","c2","c3","c4","c5","c6")
colnames(summarized_results) = c("c1","c2","c3","c4","c5","c6")
rownames(summarized_results) = c("Mean","Variance","Rejection Rate")


create_plot <- function(i){
  title = paste("c = ", i, sep = "")
  
  hist(results[i][results[i] < 200], col="green", breaks=70, xlab="", ylab="sample density", freq=FALSE, main=title)
}


for(i in 1:6){
  create_plot(i)
}

# Code to show all the graphs
par(mfrow=c(3,2))
sapply(1:6, create_plot)

#What is the mean and variance and how do they depend on c?
#Study the rejection rate.



