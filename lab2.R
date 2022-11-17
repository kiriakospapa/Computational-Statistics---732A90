# Question 1
f <- function (x, a0, a1, a2){
  out <- a0 + a1*x + a2*(x^2)
  return(out)
}

squarred_error <- function(params, data, f1){
  print(params)
  y <- 0
  for(x in data){
    y = y + (f(x, params[1], params[2], params[3])- f1(x)) ^ 2
  }
  return(y)
}

find_alphas <- function(f, data){
 alphas <- optim(c(1,1,1), fn=squarred_error, f1=f, data = data)
 return(alphas)
}

x <- seq(0,1, by = 0.0001)

tmp <- function(n, f1){
  x <- seq(0,1, by=0.0001)
  p <- round(length(x) / n)
  percent <- round(length(x) / n)
  for(i in 0:(p-1)){
    data2 <- x[((i * p) + 1 ): ((i * percent) + percent)]
    first_point <- data2[1]
    last_point <- data2[length(data2)]
    middle_point <- data2[round(length(data2)/2)]
    print(f1(first_point))
    print(f1(last_point))
    print(f1(middle_point))
    
    if ((f1(middle_point)> f1(last_point)) || (f1(middle_point)> f1(first_point))){
      stop("it's wrong")
    }
  }
}

# Question 3
f1 <- function(x){
 return(-x*(1-x)) 
}

f2 <- function(x){
  return(-x*sin(10*pi*x))
}

loss_function <- function(x, a){
  out <- f(a)
  return()
}




#======================================

# Question 1
load("data.RData")


# Question 2
n = length(data)
ll_mean_value = (1/n) * sum(data)
ll_variance = sqrt((1 / n) * sum((data - mean(data)) ^ 2))


# Question 3
loglikelihood <- function(params, data){
  mean_value = params[1]
  sigma_squared = params[2]
  n = length(data)
  loglikelihood = -(n / 2) * log( 2 * pi) - (n / 2) * log(abs(sigma_squared)) - ((sum((data - mean_value)^2))/(2*sigma_squared))
  return(-loglikelihood)
}

gradient <- function(params, data){
  
  n <- length(data)
  mean_value <- (sum(x) - 2 * n * params[1]) / (params[2] ^ 2)
  sigma_squared <- (-n/params[2]) + (1/params[3]^3) * (sum(data - params[2]))^2
  print(mean_value)
  
  return(c(mean_value,sigma_squared))
}

result1 = optim(c(0, 1), fn=loglikelihood, data=data, method = "BFGS") 
result1$par

result2 = optim(c(0, 1), fn=loglikelihood, data=data, method = "CG") 
result2$par


result3 = optim(c(0, 1), fn=loglikelihood, data=data, method = "BFGS", gr = gradient) 
result3$par

result4 = optim(c(0, 1), fn=loglikelihood, data=data, method = "CG", gr = gradient) 
result4$par

# We have to answer question 4 as well

