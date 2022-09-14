#1. b)i) Creating a function to compute the #probability that the lifetime of a 
#satellite exceeds 15 years


calc_lifetime <- function(t) {
  return(0.2*exp(-0.1*t)-0.2*exp(-0.2*t))
}
#when t = 30
calc_lifetime(30)


#b)ii)repeating the steps 10,000 times by simulating 10,000 draws
#from the distribution of T
#using replicate function
t_10k = replicate(10000,max(rexp(n=1, rate =0.1),rexp(n=1, rate =0.1)))
t_10k

#b)iii) Plotting a histogram to superimpose the density function mentioned above 
#using hist (without curve)
hist(t_10k, col="yellow",prob=TRUE)

#superimposing it with a 'curve'
curve(calc_lifetime, col='red',add=TRUE)

#b)iv) estimating the expected value(mean)
mean(t_10k)

#b)v)estimating the probability that the satellite lasts more than 15 years.
1-pexp(15, rate=1/mean(t_10k))

#b)vi) estimating the probability 4 more times
#TEST 1
t_10k = replicate(10000,max(rexp(n=1,rate=0.1),rexp(n=1,rate=0.1)))
hist(t_10k,col="yellow")
1-pexp(15,rate=1/mean(t_10k))
mean(t_10k)

#TEST 2
t_10k = replicate(10000,max(rexp(n=1,rate=0.1),rexp(n=1,rate=0.1)))
hist(t_10k,col="yellow")
1-pexp(15,rate=1/mean(t_10k))
mean(t_10k)

#TEST 3
t_10k = replicate(10000,max(rexp(n=1,rate=0.1),rexp(n=1,rate=0.1)))
hist(t_10k,col="yellow")
1-pexp(15,rate=1/mean(t_10k))
mean(t_10k)

#TEST 4
t_10k = replicate(10000,max(rexp(n=1,rate=0.1),rexp(n=1,rate=0.1)))
hist(t_10k,col="yellow")
1-pexp(15,rate=1/mean(t_10k))
mean(t_10k)



