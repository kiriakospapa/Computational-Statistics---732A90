library(ggplot2)
library(gridExtra)


## Question 2
# 1
de <- function(x){
  y <- (1/2) * exp(-abs(x))
  return(y)
}

# https://www.tutorialspoint.com/statistics/laplace_distribution.htm to find the 
# cdf

x <- runif(10000)

invcdf <- function(x){
  out <- 0 - 1 * sign(x - 0.5) * log(1-2*abs(x - 0.5), base = exp(1))
}

set.seed(12345)
x <- runif(10000, min=0, max= 1)
y1 = de(x)
y2 = invcdf(x)

df = data.frame(y1, y2, x)
df[is.na(df)] = 0

p <- ggplot(df, aes(x=y2)) + geom_histogram(color= "black"), binwidth = 0.01)
p1 <- ggplot(df, aes(x = x, y=y1)) + geom_line()


# 2

# Write my notes to markdown after.
c = (2/sqrt(2*pi)) * exp(0.5)
n = 2000

counter <- 0
results <- c()

for(i in 1:n){
  
  continue <- TRUE
  
  while(continue){
      counter <- counter + 1
      y <- invcdf(runif(1))
      u <- runif(1)
      print(i)
      if( u <= (dnorm(y,mean =  0, sd = 1) / (c * de(y)))){
        results <- append(results, y)
        continue = FALSE
      }
  }

}

normald = rnorm(2000, 0, 1)

df = data.frame(results)
p <- ggplot(df, aes(x=results)) + geom_histogram(color="black", binwidth = 0.1,
                                                 fill = "blue") + ggtitle("Acceptance Rejectance Method") +
                                                 xlab("") + ylab("")

norm = rnorm(2000,0, 1)
df2 = data.frame(norm)
p1 <- ggplot(df2, aes(x=norm)) + geom_histogram(color="black", binwidth = 0.1,
                                                  fill = "red") + ggtitle("Normal Distribution") +
                                                  xlab("") + ylab("")

rejection_rate <- n / counter
expected_rejection_rate <- 1/c

grid.arrange(p, p1, ncol=2)
