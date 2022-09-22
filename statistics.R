## The Central Limit Theorem (for a Poisson)
m <- 5

set.seed(23)
n <- 40
S <- rpois(n, m)
table(S)
hist(S, breaks=(-0.5:20.5), xlab="Claims")

xs <- 0:20
lines(xs, n*dpois(xs, m), type="o", col="blue")

xbars <- rep(0,1000)

set.seed(23)
for (k in 1:1000) {
  xbars[k] <- mean(rpois(n,m))
}
hist(xbars, prob=TRUE, ylim=c(0,1.2), xlab="sample means", main="")
range(xbars)
xvals <- seq(3,7, by=0.01)
lines(xvals, dnorm(xvals, m, sqrt(m/n)), type="l", lwd=2, col="red")

hist(xbars, prob=TRUE, ylim=c(0,1.2), xlab="sample means", main="")
range(xbars)
xvals <- seq(3,7, by=0.01)
curve(dnorm(x, m, sqrt(m/n)), 3,7, add=TRUE, lwd=2, col="red")

### Varying the sample size
par(mfrow=c(2,2))

set.seed(23)
xbars <- rep(0, 1000)

for (n in c(5, 10, 50, 100)) {
  for (k in 1:1000) {
    xbars[k] <- mean(rpois(n,m))
  }
  xvals <- seq(3,7, by=0.01)
  hist(xbars, prob=TRUE, ylim=c(0,2), xlab="sample means", main=paste("n =", n))
  lines(xvals, dnorm(xvals, m, sqrt(m/n)), type="l", lwd=2, col="red")
}
par(mfrow=c(1,1))

### Empirical v theoretical i.e. central limit probabilities
m <- 5
set.seed(23)
xbars <- rep(0,1000)
n <- 40
for (k in 1:1000) {
  xbars[k] <- mean(rpois(n,m))
}

#P(xbars > 5.5), xbars ~ Norm(m, m/n)
c(length(xbars[xbars > 5.5])/length(xbars), 1 - pnorm(5.5, m, sqrt(m/n)))

### Empirical v theoretical i.e. central limit moments
c(mean(xbars), m)
c(var(xbars), m/n)


### Empirical v theoretical i.e. central limit quantiles
c(median(xbars), qnorm(0.5, m, sqrt(m/n)))
c(quantile(xbars, c(0.25, 0.75)),qnorm(c(0.25, 0.75), m, sqrt(m/n)))

#type 4 is n/4 and 3n/4
#type 5 is (n+2)/4 and (3n+2)/4
#type 6 is (n+1)/4 and (3n+3)/4
#type 7 is (n+3)/4 and (3n+1)/4 (default)

summary(xbars)

qqnorm(xbars)
qqline(xbars, lty=2, col="red", lwd=2)
#shorter tail on -ve & longer tail +ve = +ve skew

## Normal approx to binomial 
##(used when finding confidece interval for binom by hand)
n <- 10
p <- 0.5
xs <- 0:n
barplot(dbinom(xs, n, p), names=xs)
# can't superimpose normal pdf on barplot so use hist
set.seed(25)
S <- rbinom(1000,n,p)
table(S)
hist(S, prob=TRUE, breaks=(-0.5:10.5), ylim=c(0,0.25))
#now we can superimpose
xvals <- seq(0,10, by=0.01)
lines(xvals, dnorm(xvals, n*p, sqrt(n * p * (1 - p))), 
      type="l", lwd=2, col="red")

### Computing empirical probabilities
#P(X > 5), P(X > 5.5) with continuity correction
c(length(S[S>5])/length(S), 1 - pnorm(5.5, n*p, sqrt(n * p * (1 - p))))

### Comparing quantiles
qqnorm(S)
qqline(S, lty=2, col="red", lwd=2)

par(mfrow = c(2,2))
set.seed(25)
p <- 0.2

for (n in c(10, 25)) {
    S <- rbinom(1000,n,p)
    table(S)
    hist(S, breaks=(-0.5:n+0.5), prob=TRUE, 
         main=paste("p=0.2, n=", n), ylim=c(0,0.35))
    
    xvals <- seq(-0.5, 8.5, by=0.01)
    lines(xvals, dnorm(xvals, n*p, sqrt(n * p * (1 -p))), type="l", lwd=2, col="red")
    
    qqnorm(S)
    qqline(S, lty=2, col="red", lwd=2)
}

par(mfrow=c(1,1))

# Standardising the Central Limit Theorem
m <- 5
set.seed(23)
xbar <- rep(0,1000)
n <- 40
for (k in 1:1000) {
  xbar[k] <- mean(rpois(n,m))
}

#CLT => xbar ~ N(m, m/n) hence Z ~ N(0,1)
Z <- (xbar - m)/sqrt(m/n)

hist(Z, prob=TRUE, xlab = "standardised sample means", main="")
lines(density(Z), col="blue")

xvals <- seq(-4, 4, by=0.01)
lines(xvals, dnorm(xvals, 0, 1), lwd=2, lty=2, col="red")

#Moments
c(mean(Z), 0)
c(var(Z), 1)

#Quantiles
c(median(Z), 0)

c(quantile(Z, c(0.25, 0.75)), qnorm(c(0.25, 0.75), 0, 1))

qqnorm(Z)
qqline(Z, lty=2, col="red", lwd=2)

#Probabilities
c(length(Z[Z > 1.5])/length(Z), 1 - pnorm(1.5, 0, 1))

#-------------------------
# Fitting a discrete distribution
#--------------------------
C <- read.table("children.csv")
table(C)
hist(C)
str(C)
c <- C$V1
str(c)


# Empirical moments
mean(c)
var(c)
sd(c)

skew <- sum((c - mean(c))^3)/length(c)
skew/sd(c)^3

#Use method of moments to estimate parameters for fitted distributions
hist(c, breaks=c(-0.5:7.5), ylim=c(0,30), xlim=c(0,10),
     xlab="no. of children", main="Children in 80 families")
#Poisson
m <- mean(c)
m
xs <- 0:10
lines(xs, length(c)*dpois(xs,m), type="o", col="blue")

#Binomial
p <- (mean(c) - var(c))/mean(c)
p

n <- mean(c)^2 / (mean(c) - var(c))
n
round(n)
lines(xs, length(c)*dbinom(xs,round(n),p), type="o", col="green")

#Negative Binomial 2
k <- mean(c)^2 / (var(c) - mean(c))
k

#Comparing CDFs
plot(ecdf(c))

xs <- 0:8
lines(xs, ppois(xs, m), type="s", col="blue")
lines(xs, pbinom(xs, round(n),p), type="s", col="green")

#-------------------------
# Fitting a continuous distribution
#--------------------------

#Comparing quantiles
xlnorm <- rlnorm(1000, m, s)
xgamma <- rgamma(1000, a, l)

qqplot(xlnorm, w, xlab="logN quantiles", ylab="sample quantiles")
abline(0, 1, col="red", lty=2)

qqplot(xgamma, w, xlab="logN quantiles", ylab="sample quantiles")
abline(0, 1, col="red", lty=2)

#
# Non-parametric bootstrap method
#

data <- c(0.61, 6.47, 2.56, 5.44, 2.72, 0.87, 2.77, 6.00, 0.14, 0.75)

set.seed(47)
est <- rep(0,1000)
for (k in 1:1000) {
  x <- sample(data, replace=TRUE)
  est[k] <- 1/mean(x)
}
est

set.seed(47)
est <- replicate(1000, 1/mean(sample(data, replace=TRUE)))
est

hist(est, prob=TRUE, ylim=c(0,5))
lines(density(est), col="blue")

mean(est)
sd(est)

#
# Parametric bootstrap method
#

set.seed(47)
param.est <- replicate(1000, 1/mean(rexp(10, 1/mean(data))))


hist(param.est, prob=TRUE, ylim=c(0,3.5))
lines(density(param.est), col="blue")

mean(param.est)
sd(param.est)


#
# Confidence intervals
#
#2-sided CI for mean, known population variance
xs <- replicate(50, 132)
n <- length(xs)
sigma <- 20
alpha <- 1 - 0.95
mean(xs) + c(-1, 1)*qnorm(1-alpha/2,0,1) * sigma/sqrt(n)

#2-sided CI for mean, unknown population variance
xs <- c(124, 122, 130, 125, 132)
n <- length(xs)
alpha <- 1 - 0.95
mean(xs) + c(-1, 1)*qt(1-alpha/2,n-1) * sd(xs)/sqrt(n)

t.test(xs, conf=0.95)

chickwts
chickwts$feed
sun <- chickwts[chickwts$feed=="sunflower", 1]
sun
t.test(sun)

#Exploring t.test
ci <- t.test(sun)
names(ci)
ci$conf.int
c(ci$conf.int[1], ci$conf.int[2])

#
# CI for variance
#
#Data <- read.table("file.csv")
#data <- Data$V1
data <- c(124, 122, 130, 125, 132)
n <- length(data)
alpha <- 0.05

#Two sided ci for var
l <- (n-1) * var(data)/qchisq(1-alpha/2, df= n - 1)
r <- (n-1) * var(data)/qchisq(alpha/2, df= n - 1)
c(l,r)
#Two sided ci for sd
sqrt(c(l,r))
#One-sided CI for sd
r <- (n-1) * var(data)/qchisq(alpha, df= n - 1)
sqrt(c(0,r))

n <- length(sun)
l <- (n-1) * var(sun)/qchisq(1-alpha/2, df= n - 1)
r <- (n-1) * var(sun)/qchisq(alpha/2, df= n - 1)
c(l,r)

#
# Bootstrap method for CI
#
#2-sided CI for mean, unknown population variance
xs <- c(124, 122, 130, 125, 132)

# Parametric
set.seed(17)
bm <- replicate(1000, mean(rnorm(n, mean(xs), sd(xs))))
hist(bm, xlab="sample mean", main="histogram of sample means")
c(quantile(bm, c(0.025, 0.975)), c(t.test(xs)$conf[1], t.test(xs)$conf[2]))

# Non-parametric
set.seed(17)
bm <- replicate(1000, mean(sample(xs, replace=TRUE)))
hist(bm, xlab="sample mean", main="histogram of sample means")
c(quantile(bm, c(0.025, 0.975)), c(t.test(xs)$conf[1], t.test(xs)$conf[2]))

# Parametric - variance
#Recall
xs <- c(124, 122, 130, 125, 132)
l <- (n-1) * var(data)/qchisq(1-alpha/2, df= n - 1)
r <- (n-1) * var(data)/qchisq(alpha/2, df= n - 1)
c(l,r)

set.seed(17)
bv <- replicate(1000, var(rnorm(n, mean(xs), sd(xs))))
hist(bv, xlab="sample variances", main="histogram of sample variances")
c(quantile(bv, c(0.025, 0.975)), c(l,r))

# Non-parametric
set.seed(17)
bv <- replicate(1000, var(sample(xs, replace=TRUE)))
hist(bv, xlab="sample mean", main="histogram of sample means")
c(quantile(bv, c(0.025, 0.975)), c(l,r))

#
# CI for parameter p of a binomial
#

#Given a value of 1 from Bin(20, p), construct 95% symmetrical CI for p
x <- 1
n <- 20
binom.test(x, n, conf=0.95)

#compare with norm approx.
x <- 45
n <- 250
p_hat <- x/n
alpha <- 0.1
ci <- binom.test(x, n, conf=1-alpha)$conf
p_hat+c(-1,1)*qnorm(1-alpha/2)*sqrt(p_hat*(1-p_hat)/n)
c(ci[1], ci[2])

#
# CI for parameter l of Poisson
#
x <- 1
T <- 1 #default time base
poisson.test(x, T, conf=0.9)

#compare with norm approx.
x <- 800 #claims
n <- 5000 #policies
l_hat <- x/n
alpha <- 0.1
l_hat+c(-1,1)*qnorm(1-alpha/2)*sqrt(l_hat/n)
ci <- poisson.test(x, n, r=1, conf=1-alpha)$conf
c(ci[1], ci[2])

#Test that unknown average claim frequency r < 0.175
x <- 800 #claims
n <- 5000 #policies
alpha <- 0.05

# if test$p.value < 0.05 then reject test$null.value (H0 : r >= 0.175)
test <- poisson.test(x, n, r=0.175, alt="less", conf=1-alpha)
test
names(test)
test$alternative
test$estimate #MLE

#
# CI for difference of two means, variance known
#
xs1 <- replicate(50, 132)
xs2 <- replicate(40, 128)

n1 <- length(xs1)
n2 <- length(xs2)

sigma1 <- 20
sigma2 <- 25

alpha <- 0.05

(mean(xs1)-mean(xs2)) + c(-1,1) * qnorm(1-alpha/2) * 
  sqrt(sigma1^2/n1 + sigma2^2/n2)

## unknown equal variances
A <- c(31.4, 29.9, 33.2, 34.4, 32.0, 28.7, 26.1, 30.3)
C <- c(27.0, 32.2, 30.4, 28.0, 26.5, 25.5, 29.6, 27.2)

t.test(A,C, var.equal=TRUE)

sxx <- 37.51367
b <- 0.8153798
alpha <- 1 - 0.95
shatsq <- 0.18095344
b + c(-1, 1) * qt(1-alpha/2, 198) * sqrt(shatsq/sxx)
# (b – t_198_0.05 * sqrt(s^2/sxx, b + t_198_0.05 * sqrt(s^2/sxx)
1580.5686 + c(-1,1) * qt(1-alpha/2, 198) * 147.85994821018852
se <- sqrt((1+1/200 + (1930 - 7.598)^2/30.58789)*0.18095)
se
1580.5686 + c(-1,1) * qt(1-alpha/2, 198) * se
