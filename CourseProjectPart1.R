## Course Project Part 1.

# Number of simulations
nosim<-1000
# Number in each simulations
n<-40
# Rate in exponentional distribution
lambda<-0.2

library(ggplot2)

## The sample mean
set.seed(1286)
cfunc <- function(x, n) sqrt(n) * (mean(x) - 1/lambda) / (1/lambda)
dat <- data.frame( x = apply(matrix(rexp(n*nosim,lambda),nosim), 1, cfunc, n) )
g <- ggplot(dat, aes(x = x)) +
        geom_histogram(alpha = .20, binwidth=.5,
                       colour = "darkgreen", fill = "blue", aes(y = ..density..)
                       ) 
g <- g + stat_function(fun = dnorm, size = 1,colour="red")
g<-g +geom_vline(xintercept = 0, colour="red") # The theoretical mean 1/lambda
g<-g +geom_vline(xintercept = mean(dat$x)) # The sample mean 
g<-g+coord_cartesian(xlim = c(-4,4))
g

mean(dat$x) # sample mean, almost equal to 0.
var(dat$x) # almost equal to 1 as a standard normal should have

## The sample variance
set.seed(1286)
cfunc2 <- function(x, n) sqrt(n) * (sd(x) - 1/lambda) / (1/lambda)
datvar <- data.frame( x = apply(matrix(rexp(n*nosim,lambda),nosim), 1, cfunc2, n) )
gvar <- ggplot(datvar, aes(x = x)) +
        geom_histogram(alpha = .20, binwidth=.5,
                       colour = "darkgreen", fill = "blue", aes(y = ..density..)
        ) 
gvar <- gvar + stat_function(fun = dnorm, size = 1,colour="red")
#g<-g +geom_vline(xintercept = 0, colour="red") # The theoretical mean 1/lambda
#g<-g +geom_vline(xintercept = mean(dat$x)) # The sample mean 
#g<-g+coord_cartesian(xlim = c(-4,4))
gvar


## Is it Normal
dat1 <- data.frame( x = c(
        
        apply(matrix(rexp(n*nosim,lambda),nosim), 1, mean),
        rexp(nosim,lambda) 
        ),
        type = factor(rep(c("avg", "non-avg"), rep(nosim, 2)))
        )
        
g1 <- ggplot(dat1, aes(x = x, fill = type)) +
        geom_histogram(alpha = .20, binwidth=.5,
                       colour = "darkgreen", aes(y = ..density..)
        ) 
g1 + facet_grid(. ~ type)

mean(dat2$x) # sample mean
var(dat2$x) 

##

##