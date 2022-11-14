library(plotly)
library(ggplot2)

#Question 1: Be careful when comparing

x1 <-  1/3
x2 <-  1/4
if(x1 - x2==1/12){
  print("Subtraction is correct")
  }else{
  print("Subtraction is wrong")
}

x1 <- 1 
x2 <- 1/2
if(x1 - x2==1/2){
  print("Subtraction is correct")
  }else{
  print("Subtraction is wrong")
}

x1 <-  1/3
x2 <-  1/4
#1.2
if(isTRUE(all.equal(x1-x2,1/12))){
  print("Subtraction is correct")
}else{
  print("Subtraction is wrong")
}


#Question 2

derivative <- function(x){
  out =(x + 10^-15 - x)/10^-15
  return(out)
}
x =  1
print(derivative(x))

#2.3
#What values did you obtain? What are the true values? Explain the reasons behind the
#discovered differences.
#We got 1.110223 and 0, the real value should be 1 as the x cancel eachother,
#And the error/error = 1
# The issue is due to bytes, there are too many values which makes the program delete
# the error.


#Question 3
myvar <- function(x){
  n = length(x)
  result = ((1/(n-1))*(sum(x^2)- (1/n)*(sum(x)^2)))
  return (result)
}

t = rnorm(10000, 10^8, 1)


test = c()
for(i in 1:length(t)){
  a = myvar(t[1:i]) - var(t[1:i])
  test = append(test, a)
}

data = data.frame("Values" = test)
data["N"] = 1:length(t)
data[1,1] <- 0

p <- ggplot(data, aes(x=N, y=as.numeric(Values))) +
  geom_point()
ggplotly(p)


better_myvar <- function(x){
  sum((x- mean(x))^2) / (length(x) - 1)
}

test2 = c()
for(i in 1:length(t)){
  a = better_myvar(t[1:i]) - var(t[1:i])
  test2 = append(test2, a)
}

data = data.frame("Values" = test2)
data["N"] = 1:length(t)
data[1,1] <- 0


# We have to zoom out here
p <- ggplot(data, aes(x=N, y=as.numeric(Values))) +
  geom_point()
plot(p)
#ggplotly(p)

n = 50
k = 50

bf1 <- function(n, k){
  out <- prod ( 1 : n ) / (prod ( 1 : k ) * prod ( 1 : ( n - k ) ) )
  return(out)
}

bf2 <- function(n, k){
  out <- prod( ( k+1) : n ) / prod ( 1 : ( n - k ) )
  return(out)
}

bf3 <- function(n, k){
  out <- prod ( ( ( k+1) : n ) / ( 1 : ( n - k ) ) )
  return(out)
}

# Question 4.1
# 1. If n = k then the demoninator will be 0 so the value that it will return
# is infinitive

# Question 4.2

n_values = c(110, 120, 130, 140, 150, 160, 170, 180, 190, 200)
k_values = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
results1 = c()
results2 = c()
results3 = c()

for(i in 1:length(  k_values)){
  results1[i] <- bf1(n_values[i], k_values[i])
  results2[i] <- bf2(n_values[i], k_values[i])
  results3[i] <- bf3(n_values[i], k_values[i])
}


frame = data.frame(results1, results2, results3, n_values, k_values)
colnames(frame) = c("df1", "df2", "df3", "N")


results1
results2
results3

# Probably gives NaN when it's Inf/Inf
# When I run bf2 seperately it gives me NaN while in the loop it gives me Inf

p <- ggplot() + geom_line(data = frame, aes(x=n_values, y= results3), color="red") + geom_line(data = frame, aes(x=n_values, y= results2), color="blue")


plot(p)

# The first 2 functions overflow. The third function just goes to inf. That's it
# because the devide becomes inside the prod so the result is Inf compared to
# the previous functions as it goes inf/inf that gives NA



