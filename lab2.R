f <- function (x, a0, a1, a2){
  out <- a0 + a1*x + a2*(x^2)
  return(out)
}

loss_function <- function(x, a){
  out <- f(a)
  return()
}

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


result1 = optim(c(0, 1), fn=loglikelihood, data=data, method = "BFGS") 
result1$par

result2 = optim(c(0, 1), fn=loglikelihood, data=data, method = "CG") 
result2$par