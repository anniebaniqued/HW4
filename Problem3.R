############ PROBLEM 3(a) Density Function
x=seq(-20,20,length=20000)
n1 <- 1/sqrt(2*pi*25)*exp(-(x-1)^2/(2*25))
n2 <- 1/sqrt(2*pi*1)*exp(-(x+2)^2/(2*1))
n3 <- 1/sqrt(2*pi*4)*exp(-(x-3)^2/(2*2))
y <- 0.2*n1 + 0.3*n2 + 0.5*n3
plot(x,y,type="l",lwd=2,col="red",xlim=c(-25,25))


############ PROBLEM 3(b) Direct Sampling
y <- 0
nsim <- 500
U <- runif(nsim)
first <- as.numeric(U < .2)
second <- as.numeric(.2 <= U & U < .5)
third <- as.numeric(.5 <= U & U < 1)
y <- first*rnorm(nsim,mean=1,sd=5)+second*rnorm(nsim,mean=-2,sd=1)+third*rnorm(nsim,mean=3,sd=2)

hist(y,breaks=50,main="500 samples drawn from the mixture of Gaussians",col="yellow", freq=FALSE,xlim=c(-25,25))


############ PROBLEM 3(c) Rejection sampling
