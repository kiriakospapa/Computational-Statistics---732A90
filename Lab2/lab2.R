# Question 1
f <- function (x, a0, a1, a2){
  out <- a0 + a1*x + a2*(x^2)
  return(out)
}

loss_function <- function(params, data, f1){
  y <- 0
  y <- (f(data[1], params[1], params[2], params[3]) - f1(data[1])) ^ 2
  y <- y + (f(data[2], params[1], params[2], params[3]) - f1(data[2])) ^ 2
  y <- y + (f(data[3], params[1], params[2], params[3]) - f1(data[3])) ^ 2
  return(y)
}

find_alphas <- function(f, x1,x2,x3){
 alphas <- optim(c(1,1,1), fn=loss_function, f1=f, data = c(x1, x2, x3))
 return(alphas)
}


approximate <- function(n, f1){
  x <- seq(0,1, by=(1/n))
  
  i = 1
  points <- c()
  while(i < length(x)){
    
     first_value <- x[i]
     last_value <- x[i+1]
     mid_value <- (first_value + last_value) / 2
     result <- find_alphas(f1, first_value,  mid_value, last_value)
     out <- f(x[i], result$par[1], result$par[2], result$par[3])
     points <- append(points, out)
     i <- i + 1
  }
  return(points)
}


# Question 3
f1 <- function(x){
 return(-x*(1-x)) 
}

f2 <- function(x){
  return(-x*sin(10*pi*x))
}


plot_the_difference <- function(n, f1){
  points <- approximate(n, f1)
  x<-seq (0,1, by=(1/n))
  points2 <- f1(x)
  dataf <- data.frame(points, points2[-1])
  dataf['i'] <- 1:n
  p <- ggplot(dataf, aes(i, points)) + geom_point() + geom_line(data= dataf, 
                                                                aes(x=i, points2..1., color = "red"))
  p
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
  # Here we took the partial derivatives from loglikehood with respect to mean and sigma
  n <- length(data)
  
  mean_value <- (sum(data) - 2 * n * params[1]) / (params[2] ^ 2)
  sigma_squared <- (-n/params[2]) + (1/params[3]^3) * (sum(data - params[2]))^2
  print(mean_value)
  
  return(c(mean_value,sigma_squared))
}

data = 1:100
result1 = optim(c(0, 1), fn=loglikelihood, data=data, method = "BFGS") 
result1$par

result2 = optim(c(0, 1), fn=loglikelihood, data=data, method = "CG") 
result2$par


result3 = optim(c(0, 1), fn=loglikelihood, data=data, method = "BFGS", gr = gradient) 
result3$par

result4 = optim(c(0, 1), fn=loglikelihood, data=data, method = "CG", gr = gradient) 
result4$par

# Question: Why it is a bad idea to maximize likelihood rather than maximizing 
#log–likelihood

# Answer: Because maximasing the likelihood there is chance to face an overflow 
# problem. While with the log-likehood the value of likehood will be much smaller
# as it will be in log.

# We have to answer question 4 as well

optimal_mi= c(result1$par[1], result2$par[1], result3$par[1], result4$par[1])
optimal_sigma = c(result1$par[2], result2$par[2], result3$par[2], result4$par[2])
functions_required = c(result1$counts["function"], result2$counts["function"],
                     result3$counts["function"], result4$counts["function"])
gradient_evaluations_required = c(result1$counts["gradient"], result2$counts["gradient"],
                                  result3$counts["gradient"], result4$counts["gradient"])
converged = c("Yes", "Yes", "Yes", "Yes")
table = data.frame(optimal_mi, optimal_sigma, functions_required 
                   ,gradient_evaluations_required, converged)


  
  
  
  
  
  
  
  
  
  
  


