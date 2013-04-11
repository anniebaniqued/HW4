############ PROBLEM 3(a) Density Function
x=seq(-40,40,length=40000)
n1 <- 1/sqrt(2*pi*25)*exp(-(x-1)^2/(2*25))
n2 <- 1/sqrt(2*pi*1)*exp(-(x+2)^2/(2*1))
n3 <- 1/sqrt(2*pi*4)*exp(-(x-3)^2/(2*2))
p <- 0.2*n1 + 0.3*n2 + 0.5*n3
plot(x,p,type="l",lwd=2,col="red",xlim=c(-25,25),main="Mixture of Gaussians PDF")


############ PROBLEM 3(b) Direct Sampling
y <- 0
nsim <- 500
U <- runif(nsim)
first <- as.numeric(U < .2)
second <- as.numeric(.2 <= U & U < .5)
third <- as.numeric(.5 <= U & U < 1)
y <- first*rnorm(nsim,mean=1,sd=5)+second*rnorm(nsim,mean=-2,sd=1)+third*rnorm(nsim,mean=3,sd=2)

hist(y,breaks=50,main="500 samples drawn from the mixture of Gaussians",col="lightblue", freq=FALSE,xlim=c(-25,25))


############ PROBLEM 3(c) Rejection sampling

# Just comparing the 2 distributions
plot(x,p,type="l",lwd=2,col="red",xlim=c(-25,25),main="Mixture of Gaussians PDF")

q <- 2.75/sqrt(2*pi*30)*exp(-(x-1.5)^2/(2*30))
par(new=TRUE)
lines(x,q,col="blue")

# # Implementing the rejection sampling
# sample.x = runif(10,-2,2)
# accept = c()
# 
# for(i in 1:length(sample.x)){
#  U = dnorm(sample.x[i], mean = 1.5, sd = sqrt(30))
#  n1 <- 1/sqrt(2*pi*25)*exp(-(sample.x[i]-1)^2/(2*25))
#  n2 <- 1/sqrt(2*pi*1)*exp(-(sample.x[i]+2)^2/(2*1))
#  n3 <- 1/sqrt(2*pi*4)*exp(-(sample.x[i]-3)^2/(2*2))
#  p <- 0.2*n1 + 0.3*n2 + 0.5*n3
#  if(dunif(sample.x[i], min=0, max=1)*2.75*U <= p) {
#  	accept[i] <- 'Yes'
#  }
#  else if (dunif(sample.x[i], min=0, max=1)*2.75*U > p) {
#  	accept[i] <- 'No'
#  }
# }

# Implementing the rejection sampling
sample.x = rnorm(10000,mean=1.5,sd=sqrt(30))
accept = c()

for(i in 1:length(sample.x)){
 n1 <- 1/sqrt(2*pi*25)*exp(-(sample.x[i]-1)^2/(2*25))
 n2 <- 1/sqrt(2*pi*1)*exp(-(sample.x[i]+2)^2/(2*1))
 n3 <- 1/sqrt(2*pi*4)*exp(-(sample.x[i]-3)^2/(2*2))
 p <- 0.2*n1 + 0.3*n2 + 0.5*n3
 if(runif(1, min=0, max=2.75*dnorm(sample.x[i],mean=1.5,sd=sqrt(30))) <= p) {
 	accept[i] <- 'Yes'
 }
 else if(runif(1, min=0, max=2.75*dnorm(sample.x[i],mean=1.5,sd=sqrt(30))) > p) {
 	accept[i] <- 'No'
 }
}

yes <- 0
no <- 0 
for (i in 1:length(accept)){
	if(!is.na(accept[i])){
		if(accept[i]=="Yes"){
			yes <- yes + 1
		}
		else {
			no <- no + 1
		}
		if(yes==500){
			break
		}	
	}
}
i
yes
no

T = data.frame(sample.x, accept = factor(accept, levels= c('Yes','No')))


# Plotting the results along with the true distribution
par("new")
hist(T[,1][T$accept=='Yes'], breaks = 50, freq = FALSE, main = 'Histogram of X', xlab = 'X')

x=seq(-40,40,length=40000)
n1 <- 1/sqrt(2*pi*25)*exp(-(x-1)^2/(2*25))
n2 <- 1/sqrt(2*pi*1)*exp(-(x+2)^2/(2*1))
n3 <- 1/sqrt(2*pi*4)*exp(-(x-3)^2/(2*2))
p <- 0.2*n1 + 0.3*n2 + 0.5*n3
lines(x,p,type="l",col="red",xlim=c(-25,25),main="Mixture of Gaussians PDF")




