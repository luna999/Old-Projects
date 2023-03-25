library("quantmod")

ftse100<-new.env()
getSymbols("^FTSE",env=  ftse100,src="yahoo",
           from=as.Date("2007-01-01"),to=as.Date("2017-12-19"))
## [1] "FTSE" 
FTSE<-ftse100$FTSE
FTSE <- na.omit(FTSE)
head(FTSE)

chartSeries(FTSE)
candleChart(FTSE,multi.col=TRUE,theme='white')
zoomChart("2007::")
## Add volumes to it
chartSeries(FTSE, theme="white", 
            TA="addVo();addBBands();addCCI()") 

returns = diff(log(FTSE$FTSE.Close))
returns = returns[2:length(returns)] 
names(returns)=c("FTSE logReturn") 
head(returns)

class(returns)

print(returns["2017-12-01/"])

plot(returns["2007-01-03/2017-12-19"], ylim=c(-0.12,0.12), main="log-returns 2007-2017 FTSE100")
plot(returns["2017"], ylim=c(-0.025,0.025), main="log-returns 2017 FTSE100")

returns.mat = coredata(returns)
dates = index(returns)
class(returns.mat)

class(dates)

qqnorm(returns)
qqline(returns, col='red')


##### SP500 ######
sp500<-new.env()
getSymbols("^GSPC",env=  sp500,src="yahoo",
           from=as.Date("2007-01-01"),to=as.Date("2017-12-20"))
GSPC<-sp500$GSPC
GSPC <- na.omit(GSPC)
head(GSPC)

chartSeries(GSPC)
candleChart(GSPC, multi.col=TRUE, theme="white")
addBBands()
## Add volumes to it
chartSeries(GSPC, theme="white", 
            TA="addVo();addBBands();addCCI()") 

returns2 = diff(log(GSPC$GSPC.Close))
returns2 = returns2[2:length(returns2)] 
names(returns2)=c("GSPC logReturn") 
head(returns2)

class(returns2)

print(returns2["2017-12-01/"])

plot(returns2["2007/2017"], ylim=c(-0.12,0.12), main="log-returns 2007-2017 SP500")
plot(returns2["2017"], ylim=c(-0.025,0.025), main="log-returns 2017 SP500")

returns2.mat = coredata(returns2)
dates2 = index(returns2)
class(returns2.mat)

class(dates2)

qqnorm(returns2)
qqline(returns2, col='red')



#### Question 2 #####Assessed

### Part 1

# Create an xts object y over June and July 2016 and plot the corresponding log-returns
y <- returns["2017"]
plot(y, ylim = c(-0.025,0.025), main='Log-returns in 2017',ylab='log-returns',xlab='time')
#modify functions
g = function(y,sigma){
  return(1/sqrt(2*pi*sigma^2)*exp(-as.vector(y)^2/(2*sigma^2)))
} #or could have used dnorm

alpha_recursion = function(y,mu,A,sigma){
  K <- length(mu)
  T1 <- length(y)
  alpha <- matrix(0,nrow=T1,ncol=K)
  for (j in 1:K) alpha[1,j] <- g(y[1],sigma[j])*sum(A[,j]*mu)
  for (t in 2:T1) for(j in 1:K) alpha[t,j] = g(y[t],sigma[j])*sum(A[,j]*alpha[t-1,])
  return(alpha)
}

beta_recursion = function(y,mu,A,sigma){
  K = length(mu)
  T1 = length(y)
  beta = matrix(0,nrow=T1,ncol=K)
  for (j in 1:K) beta[T1,j] = 1
  for (t in T1:2) for (i in 1:K) beta[t-1,i] = sum(g(y[t],sigma)*A[i,]*beta[t,])
  return(beta)
}
#Calculation
A = matrix(c(0.99,0.01,0.01,0.99),2,2)
mu = c(0.5,0.5)
sigma = c(0.01,0.05)
alpha <- alpha_recursion(y,mu,A,sigma)
beta <- beta_recursion(y,mu,A,sigma)

filtering.pmf <- matrix(0,nrow=length(y),ncol=length(mu))
for (i in 1:length(mu)){
  for (t in 1:length(y)){
    filtering.pmf.f[t,i] <- alpha[t,i]/sum(alpha[t,])
  }
}

smoothing.pmf <- matrix(0,nrow=length(y),ncol=length(mu))
for (i in 1:length(mu)){
  for (t in 1:length(y)){
    smoothing.pmf[t,i] <- alpha[t,i]*beta[t,i]/sum(alpha[t,]*beta[t,])
  }
}
#Add columns
y.new <- cbind(y,filtering.pmf[,2],smoothing.pmf[,2])
names(y.new) = c("FTSE logReturn","Filtering","Smoothing")
#Plot
plot(y.new[,2], main="Probability of High Volatility in 2017")


#### S&P500 #####Assessed

### Part 1

# Create an xts object y over June and July 2016 and plot the corresponding log-returns
y2 <- returns2["2017/"]

#modify functions
g = function(y,sigma){
  return(1/sqrt(2*pi*sigma^2)*exp(-as.vector(y)^2/(2*sigma^2)))
} #or could have used dnorm

alpha_recursion = function(y,mu,A,sigma){
  K <- length(mu)
  T1 <- length(y)
  alpha <- matrix(0,nrow=T1,ncol=K)
  for (j in 1:K) alpha[1,j] <- g(y[1],sigma[j])*sum(A[,j]*mu)
  for (t in 2:T1) for(j in 1:K) alpha[t,j] = g(y[t],sigma[j])*sum(A[,j]*alpha[t-1,])
  return(alpha)
}

beta_recursion = function(y,mu,A,sigma){
  K = length(mu)
  T1 = length(y)
  beta = matrix(0,nrow=T1,ncol=K)
  for (j in 1:K) beta[T1,j] = 1
  for (t in T1:2) for (i in 1:K) beta[t-1,i] = sum(g(y[t],sigma)*A[i,]*beta[t,])
  return(beta)
}
#Calculation
A = matrix(c(0.99,0.01,0.01,0.99),2,2)
mu = c(0.5,0.5)
sigma = c(0.01,0.05)
alpha <- alpha_recursion(y2,mu,A,sigma)
beta <- beta_recursion(y2,mu,A,sigma)

filtering.pmf <- matrix(0,nrow=length(y2),ncol=length(mu))
for (i in 1:length(mu)){
  for (t in 1:length(y2)){
    filtering.pmf[t,i] <- alpha[t,i]/sum(alpha[t,])
  }
}

smoothing.pmf <- matrix(0,nrow=length(y2),ncol=length(mu))
for (i in 1:length(mu)){
  for (t in 1:length(y2)){
    smoothing.pmf[t,i] <- alpha[t,i]*beta[t,i]/sum(alpha[t,]*beta[t,])
  }
}
#Add columns
y2.new <- cbind(y2,filtering.pmf[,2],smoothing.pmf[,2])
names(y2.new) = c("S&P logReturn","Filtering","Smoothing")
#Plot
plot(y2.new[,2], main="Probability of High Volatility given data up to time t")











#???????????????hannah new code plot volatility
alpha2 <- alpha_recursion(returns,mu,A,sigma)
filtering.pmf2 <- matrix(0,nrow=length(returns),ncol=length(mu))
for (i in 1:length(mu)){
  for (t in 1:length(returns)){
    filtering.pmf2[t,i] <- alpha2[t,i]/sum(alpha2[t,])
  }
}
filtering.pmf2[1:300,]
#alpha2 reach infinity at t=223, since pdf multiply to become very large
#hence filtering pmf has no values after t=223
alpha2_recursion = function(y,mu,A,sigma){
  K <- length(mu)
  T1 <- length(y)
  alpha <- matrix(0,nrow=T1,ncol=K)
  for (j in 1:K) alpha[1,j] <- g(y[1],sigma[j])*sum(A[,j]*mu)
  for (t in 2:T1) for(j in 1:K) alpha[t,j] = g(y[t],sigma[j])*sum(A[,j]*alpha[t-1,]/sum(alpha[t-1,]))
  pmf <- matrix(0,nrow=T1,ncol=K)
  for (t in 1:T1) for(i in 1:K) pmf[t,i] = alpha[t,i]/sum(alpha[t,])
  return(pmf)
}

alpha_y <- alpha2_recursion(y,mu,A,sigma)
alpha_y - filtering.pmf #very small value close to 0
max(abs(alpha_y - filtering.pmf))
alpha2 <- alpha2_recursion(returns,mu,A,sigma)
returns.f <- cbind(returns,alpha2[,2])

#plot whole dataset
plot(returns.f[,2],main="Probability of High Volatility given data up to time t")


### Part 2

# Run alpha on the whole dataset and calculate pfiltering
alpha_whole<-alpha_recursion(returns, mu, A, sigma)
alpha_whole[1:300,]
pfiltering_whole = matrix(0, nrow=length(returns), ncol=length(mu))
for (t in 1:(length(returns))) for (j in 1:(length(mu))) {
  pfiltering_whole[t,j] <- alpha_whole[t,j]/(sum(alpha_whole[t,]))
}
pfiltering_whole[1:300,]

# Create a novel function 
alpha2_recursion <- function(y, mu, A, sigma)
{
  alpha = matrix(0, nrow=length(y), ncol=length(mu))
  for (j in 1:(length(mu))) alpha[1,j] = g(y[1], sigma[j])*sum(A[,j]*mu)
  for (t in 2:(length(y))) for (j in 1:(length(mu))) alpha[t,j] = g(y[t], sigma[j])*sum(A[,j]*alpha[t-1,]/sum(alpha[t-1,]))
  filtering = matrix(0, nrow=length(y), ncol=length(mu))
  for (t in 1:(length(y))) for (i in 1:(length(mu)))
    filtering[t,i] = alpha[t,i]/(sum(alpha[t,]))
  return(filtering)
}

# Run the function on y and compare it with pfiltering
alpha2_recursion(y, mu, A, sigma)- pfiltering

# Run the function over the whole set
alpha2_whole<-alpha2_recursion(returns, mu, A, sigma)
return2 = cbind(returns, alpha2_whole[,2])
plot(returns2[,2], main = "Probability of high volatility given log-returns up to t")

plot(returns2["2007-01-01/2017-01-31",2], main = "Probability of high volatility given log-returns up to t in 2017 January")
