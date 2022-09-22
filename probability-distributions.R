# Probability distributions

## The discrete uniform distribution

### Rolling a die
D <- seq(1,6, by=1) # = seq(1,6) = 1:6 = c(1,2,3,4,5,6)

### Computing theoretical probabilities
#### Probability <= 4
length(D[D<=4])/length(D)

#### Probability == 3
length(D[D==3])/length(D)

#### Probability > 2
length(D[D>2])/length(D)

#### Probability <2 or >5
length(D[D<2 | D>5])/length(D)

#### Probability =2 and = 3
length(D[D==2 & D==3])/length(D)

### Computing theoretical moments

### Simulating outcomes
set.seed(77)
S <- sample(D, size=100, replace=TRUE)
S
table(S)
hist(S, breaks=(0.5:6.5))


### Computing empirical probabilities
#### Probability <= 4
length(S[S<=4])/length(S)

#### Probability == 3
length(S[S==3])/length(S)

#### Probability > 2
length(S[S>2])/length(S)

#### Probability <2 or >5
length(S[S<2 | S>5])/length(S)

#### Probability =2 and = 3
length(S[S==2 & S==3])/length(S)

### Computing empirical moments
mean(S)

median(S)

var(S)

sd(S)

skew <- sum((S - mean(S))^3)/length(S)
skew

coef_skew <- skew/(sd(S)^3)
coef_skew

## The binomial distribution

### X = "the number of sixes obtained when a fair die is thrown twelve times"
n <- 12
p <- 1/6

### Computing theoretical probabilities

#### P(X=4)
x <- 4
dbinom(x, n, p)

#### P(X=4 or X = 6)
sum(dbinom(c(4,6), n, p))

#### P(X <= 4)
sum(dbinom(0:4, n, p))
pbinom(4,n,p)

#### P(X < 4) = P(X <= 3)
pbinom(3,n,p)

#### P(X > 2)
sum(dbinom(3:12, n, p))
1 - pbinom(2, n, p)
pbinom(2, n, p, lower.tail=FALSE)

#### P(X >= 9) = 1 - P(X < 9) = 1 - P(X <= 8)
1 - pbinom(8, n, p)
pbinom(8, n, p, lower.tail = FALSE)

### Visualizing the distribution
#### varying x, constant p
xs <- 0:12
barplot(dbinom(xs,n,p),
        names=xs,
        xlab="number of sixes rolled",
        ylab="probability",
        main="Results from 12 die rolls",
        col="blue")

barplot(pbinom(xs,n,p),
        names=xs,
        xlab="number of sixes rolled",
        ylab="cumulative probability",
        main="Results from 12 die rolls",
        col="blue")

#### Varying x, varying p
par(mfrow=c(2,2))  #top left, top right, bot left, bot right

p <- 0.3
barplot(dbinom(xs,n,p),
        names=xs,
        xlab="number of sixes rolled",
        ylab="probability",
        main="Results from 12 die rolls",
        col="blue")

p <- 0.5
barplot(dbinom(xs,n,p),
        names=xs,
        xlab="number of sixes rolled",
        ylab="probability",
        main="Results from 12 die rolls",
        col="blue")

p <- 0.7
barplot(dbinom(xs,n,p),
        names=xs,
        xlab="number of sixes rolled",
        ylab="probability",
        main="Results from 12 die rolls",
        col="blue")

p <- 1
barplot(dbinom(xs,n,p),
        names=xs,
        xlab="number of sixes rolled",
        ylab="probability",
        main="Results from 12 die rolls",
        col="blue")
par(mfrow=c(1,1))

### Computing theoretical moments
#### Computing the mean
n <- 12
p <- 1/6
xs <- 0:12
sum(xs * dbinom(xs, n, p))

### Computing quantiles
n <- 12
p <- 1/6

#### P(X <= 4)
pbinom(4, n, p)
#### Find x such that P(X <= x) = 0.96365
qbinom(0.9636499, n, p)

#### Find x such that P(X <= x) = 0.9
qbinom(0.899, n, p)
#### check
c(pbinom(3, n, p), pbinom(4, n, p))

#### Find x such that P(X > x) = 0.6
qbinom(1-0.599, n, p)
qbinom(0.599, n, p, lower.tail = FALSE)
#### check
c(pbinom(1, n, p), pbinom(2, n, p))

#### Computing the median i.e. find x : P(X <= x) = 0.5
qbinom(0.499, n, p)
c(pbinom(1, n, p), pbinom(2, n, p))

#### Find the interquartile range i.e. difference between upper and lower quartile)
qbinom(0.7499, n, p) - qbinom(0.2499, n, p)

### Quantile step graph
probs <- seq(0, 1, by=0.01)
probs
plot(probs,
     qbinom(probs, n, p),
     type="s",
     xlab="cumulative probablity",
     ylab="number of sixes")

### Simulating outcomes
n <- 12
p <- 1/6

set.seed(31)
trials <- 100 # sample size sampN?
S <- rbinom(trials, n, p)
S
table(S)
hist(S, breaks=(-0.5:12.5))
xs <- 0:12
lines(xs, trials * dbinom(xs, n, p), type="o", col="blue")

### Computing empirical probabilities
#### P(X = x)
table(S) / length(S)
#### check
round(dbinom(0:12, n, p), 2)

#### P(X <= 2)
c(length(S[S<=2]) / length(S), pbinom(2, n, p))

#### P(X > 4)
c(length(S[S>4]) / length(S), 1-pbinom(4, n, p))

### Computing empirical moments

#### The mean
c(mean(S), n * p)

#### The median
c(median(S), qbinom(0.499, n, p))

#### The interquartile range
c(quantile(S, 0.75)-quantile(S, 0.25), qbinom(0.7499, n, p)-qbinom(0.2499, n, p))

#### The variance
c(var(S), n * p * (1 - p))

#### The coefficient of skewness (+ve skewed as tail is in +ve direction)
skew <- sum((S - mean(S))^3) / length(S) # third central moment
c(skew / sd(S)^3, (1 - 2 * p) / sqrt( n * p * (1 - p)))

### Long-term trend of the mean
n <- 12
p <- 1/6

set.seed(31)
S <- rbinom(2500, n, p)
table(S)

avg <- rep(0, 2500)
for (k in 1:2500) {
  avg[k] <- mean(S[1:k])
}

xs <- 1:2500
plot(xs, avg[1:2500])
abline(h = 2, col="red", lty = 2, lwd = 2)

## The geometric distribution

# X = "the total number of phone calls made to obtain the first sale"
# X ~ Geo1(p) where p is the probability that a phone call leads to a sale.
p <- 0.25
x <- 3

# P(X = 3)
(1 - p)^(x - 1) * p

# Y = the number of phone calls before the first sale
# Y ~ Geo2(p)
# Y = X - 1
y <- x - 1

# P(X = 3) = P(X = 2 + 1) = P(X - 1 = 2) = P(Y = 2)
(1 - p)^y * p
dgeom(y, p)

### Visualization
ys <- 0:20
p <- 0.25

barplot(dgeom(ys, p),
        names=ys,
        xlab="number of failures",
        ylab="probability",
        main="Failures before first success",
        col="blue")

barplot(dgeom(ys+1, p),
        names=ys+1,
        xlab="number of trials i.e. phone calls",
        ylab="probability",
        main="Trials until first success",
        col="blue")

#### Varying x, varying p
par(mfrow=c(2,2))  #top left, top right, bot left, bot right

for (k in c(0.2, 0.4, 0.6, 0.8)) {
  barplot(dgeom(ys, p=k),
          names=ys,
          xlab="number of failures",
          ylab="probability",
          main=k,
          col="blue")
}

par(mfrow=c(1,1))

plot(ys, pgeom(ys, p=0.25), type="s")

### Computing theoretical probabilities
p <- 0.25

# P(1 <= X <= 3) = P(0 <= Y <= 2)
pgeom(2,p)

# P(Y > 4)
1 - pgeom(4,p)

### Computing theoretical moments
p <- 0.25

# E(Y)
ys <- 0:100
c(sum(ys * dgeom(ys,p)), (1 - p)/p)

### Computing quantiles
p <- 0.25

#Find y such that P(Y <= y) = 0.7
qgeom(0.699, p)
#check
c(pgeom(3,p), pgeom(4,p))

#Find the median
c(pgeom(1,p), pgeom(qgeom(0.499, p), p), pgeom(2,p))

### Simulations
p <- 0.25

set.seed(43)
S <- rgeom(500, p)
S
table(S)
hist(S, breaks = (-0.5:21.5))
ys <- 0:21
lines(ys, 500*dgeom(ys,p), type="o", col="blue")

### Computing empirical probabilities
p <- 0.25

#P(Y=y)
table(S)/length(S)
round(dgeom(0:21, p), 3)

#P(Y <= 2)
c(length(S[S<=2])/length(S), pgeom(2,p))

#P(Y > 4)
c(length(S[S>4])/length(S), 1-pgeom(4,p))

### Computing empirical moments
p <- 0.25

#The mean
c(mean(S), (1 - p)/p)

#The upper quartile
c(quantile(S, 0.75), qgeom(0.7499, p))

### The long-term trend of the mean
p <- 0.25

avg <- rep(0, 500)

xs <- 1:500

for (k in xs) {
  avg[k] <- mean(S[1:k])
}


plot(xs, avg)
abline(h=(1 - p)/p, col="red", lty=2, lwd=2)

## The negative binomial distribution
# X = "the total number of phone calls made to obtain the first sale"
# X ~ Geo1(p) where p is the probability that a phone call leads to 1 sale.
# X = "the total number of phone calls made to obtain the k sale"
# X ~ NBin1(k, p) 
# Geo1(p) = NBin1(1, p)
p <- 0.25
k <- 3

# Y = the number of failures made to obtain the k sales
# Y ~ NBin2(k, p)
# Y = X - k


#P(X = 5) = P(X = 2 + 3) = P(X - 3 = 2) = P(Y = 2)
x <- 5
y <- x - k
dnbinom(y, k, p)

### Visualization
ys <- 0:32
k <- 3
p <- 0.25

barplot(dnbinom(ys, k, p),
        names=ys,
        xlab="number of failures",
        ylab="probability",
        main="Failures before third success",
        col="blue")

#### Varying y, varying p
par(mfrow=c(2,2))  #top left, top right, bot left, bot right

for (j in c(0.2, 0.4, 0.6, 0.8)) {
  barplot(dnbinom(ys, k, p=j),
          names=ys,
          xlab="number of failures",
          ylab="probability",
          main=j,
          col="blue")
}

par(mfrow=c(1,1))

#Step graph
plot(ys, pnbinom(ys, k, p), type="s")

### Computing theoretical probabilities
k <- 3
p <- 0.25

# P(3 <= X <= 5) = P(0 <= Y <= 2)
pnbinom(2, k, p)

# P(Y > 5)
1 - pnbinom(5, k, p)

### Computing theoretical quantiles
#Find y: P(Y <= y) = 0.6 == least y : P(Y <= y) >= 0.6 (for nbinom is discrete)
qnbinom(0.599, k, p)
#check
c(pnbinom(8, k, p), pnbinom(qnbinom(0.599, k, p), k, p), pnbinom(9, k, p))

#Find x: P(X > x) = 0.1
#== x: P(X <=x) = 1 - 0.1
#== least x : P(X <= x) >= 1 - 0.1 (for nbinom is discrete)
qnbinom(1 - 0.1, k, p)
#check
c(pnbinom(16, k, p), pnbinom(qnbinom(1 - 0.1, k, p), k, p), pnbinom(17, k, p))

### Running simulations
k <- 3
p <- 0.25

set.seed(53)
S <- rnbinom(1000, k, p)
table(S)
hist(S, breaks = (-0.5:39.5))

ys <- 0:40
lines(ys, 1000*dnbinom(ys, k, p), type="o", col="blue")

### Computing empirical probabilities

#P(Y <= 2)
c(length(S[S<=2])/length(S), pnbinom(2, k,p))

#P(Y > 5)
c(length(S[S>5])/length(S), 1-pnbinom(5, k,p))

### Computing empirical quantiles
#x: P(X<=x) = 0.6
c(quantile(S, 0.6), qnbinom(0.599, k, p))

#x: P(X > x) = 0.1
c(quantile(S, 1-0.1), qnbinom(1-0.1, k, p))

### Computing empirical moments
c(mean(S), k * (1 - p)/p)

### (Sum j : 1 < j < k: Geo2(p)) = NBin(k, p)
set.seed(53)
SG <- rep(0, 1000)
for (j in 1:1000) {
  SG[j] <- sum(rgeom(k, p))
}
table(SG)
hist(SG, breaks = -0.5:56.5)

ys <- 0:60
lines(ys, 1000*dnbinom(ys, k, p), type="l", col="blue")

## The hypergeometric distribution

#A box contains k milk chocolates and N - k plain chocolates.
#n chocolates are eaten.
#Let X be the number of milk chocolates eaten.
# X ~ Hyp(x, k, N - k, n)


k <- 12
N <- 20
n <- 2

#P(X = 1)
x <- 1
dhyper(x, k, N - k, n)

### Visualizing the distribution
k <- 12
N <- 20
n <- 6

xs <- 0:6
barplot(dhyper(xs, k, N - k, n),
        names=xs,
        xlab="number of milk chocolates eaten",
        ylab="probability",
        main="Milk chocolates eaten",
        col="blue")

par(mfrow=c(2,2))  #top left, top right, bot left, bot right

for (j in c(4, 8, 12, 16)) {
  barplot(dhyper(xs, j, N - j, n),
          names=xs,
          xlab="number of milk chocolates",
          ylab="probability",
          main=j,
          col="blue")
}

par(mfrow=c(1,1))

#Step graph
plot(xs, phyper(xs, k, N - k, n), type="s")

### Computing theoretical probabilities
k <- 12
N <- 20
n <- 6

#P(X <= 2)
phyper(2, k, N - k, n)

#P(X > 4)
1 - phyper(4, k, N - k, n)

### Computing theoretical quantiles
# Find x : P(X <= x) = 0.8 == x : P(X <= x) >= 0.8
x <- qhyper(0.799, k, N - k, n)
x
#check
c(phyper(x - 1, k, N - k, n), 0.8, phyper(x, k, N - k, n))

### Running simulations
set.seed(63)
S <- rhyper(1000, k, N - k, n)
table(S)
hist(S, breaks=(0.5:6.5), ylim=c(0,350))
xs <- 0:6
lines(xs, 1000*dhyper(xs, k, N - k, n), type="o", col="blue")

### Computing empirical probabilities

#P(X = x)
table(S) / length(S)
round(dhyper(xs, k, N - k, n),  3)

#P(X <= 2)
c(length(S[S <= 2])/length(S), phyper(2, k, N - k, n))

#P(X > 4)
c(length(S[S > 4])/length(S), 1 - phyper(4, k, N - k, n))

### Computing empirical quantiles
# Find x : P(X <= x) = 0.8 == x : P(X <= x) >= 0.8
c(quantile(S, 0.8), qhyper(0.799, k, N - k, n))

### Computing empirical moments
c(mean(S), n * k/N)

### Hyp(k, N - k, n) ~ Bin(n, k/N) for large N
k <- 12
N <- 20

p <- k/N

xs <- 0:6
plot(xs, dbinom(xs, n, p),
     xlab="no. of successes",
     ylab="probability",
     type = "o",
     ylim=c(0,0.4))

lines(xs, dhyper(xs, k, N - k, n), type="o", col="red")

#### increase N 
N <- N * 50/20
k <- k * 50/20

p <- k/N

lines(xs, dhyper(xs, k, N - k, n), type="o", col="green")

#### increase N , must increase k to keep k/N constant
N <- N * 2
k <- k * 2

p <- k/N

lines(xs, dhyper(xs, k, N - k, n), type="o", col="blue")

### The poisson distribution

# The rate at which goals are scored in a football match is on average m.
#Let X be the number of goals scored in a match.
# X ~ Poi(m)

m <- 3

#P(X = 2)
x <- 2
dpois(x, m)

## Visualizing the distribution
xs <- 0:20
barplot(dpois(xs,m),
        names=xs,
        xlab="No. of goals",
        ylab="probability",
        main="Goals scored in a match",
        col="blue")

## Computing theoretical probabilities
#P(0 <= X <= 4)
ppois(4, m)

#P(X > 2)
1 -ppois(2,m)

## Computing theoretical quantiles
#Find x : P(X <= x) = 0.9
#= { discrete distribution }
#Find least x: 0.9 <= P(X <= x)
qpois(0.9, m)
#check
c(ppois(4,m), 0.9, ppois(5,m))


#Find x : P(X > x) = 0.4
#= { discrete distribution }
#Find least x: 0.4 <= P(X > x)
#=
#Find least x: 1-0.4 <= P(X <= x)
qpois(1-0.4,m)
qpois(0.9, m)

## Running simulations
set.seed(73)
S <- rpois(1000, m)
table(S)
hist(S, breaks=-0.5:10.5)
xs <- 0:10
lines(xs, 1000*dpois(xs,m), type="o", col="blue")

## Computing empirical probabilities
#P(X = x)
table(S)/length(S)
round(dpois(0:10, m), 3)

#P(X <= 4)
c(length(S[S<=4])/length(S), ppois(4,m))

#P(X > 2)
c(length(S[S>2])/length(S), 1-ppois(2,m))

### Computing empirical quantiles
c(quantile(S, 0.9), qpois(0.9,m))

### Computing empirical moments
c(mean(S), m)
c(var(S), m)

### Bin(n, p) = Poi(n * p) for large n and small p
xs <- 0:15
plot(xs, dpois(xs,m),
     xlab="no. of successes",
     ylab="probability",
     type="o",
     ylim=c(0,0.4))

#for (k in c(10,50,100)) {
  #Bin(k, m/k)
  k <- 10
  lines(xs, dbinom(xs, k, m/k),
        type="o",
        col="red")
  
  k <- 50
  lines(xs, dbinom(xs, k, m/k),
        type="o",
        col="green")
  
  k <- 100
  lines(xs, dbinom(xs, k, m/k),
        type="o",
        col="blue")
#}

## The exponential distribution
  
# The rate at which goals are scored in a football match is on average m.
#Let X be the number of goals scored in a match.
# X ~ Poi(m)
  
l <- 2

#pdf (Exp(l)) 4
x <- 4
dexp(x, l)

### Visualizing the pdf
xs <- seq(0, 10, by=0.01)

par(mfrow=c(2,2))

plot(xs, dexp(xs,l), type="l", col="blue", ylab="f(x)", main="PDF of Exp(2)")

#This doesn't work!!!
curve(dexp(xs, rate = l), 0, 10, col="green", ylab="f(x)", main="PDF of Exp(2)")

plot(xs, type="n", xlim=c(0,10), ylim=c(0,2), xlab="x", ylab="f(x)", main="PDF of Exp(2)")
lines(xs, dexp(xs,l), type="l", col="purple")

par(mfrow=c(1,1)) 

#### Varying lambda
l <- 2
plot(xs, type="n", xlim=c(0,10), ylim=c(0,2), xlab="x", ylab="f(x)", main="PDF of Exp(2)")
lines(xs, dexp(xs,l), type="l", col="purple")

l <- 1
lines(xs, dexp(xs,l), type="l", col="blue", lty=2)

l <- 0.5
lines(xs, dexp(xs,l), type="l", col="blue", lty=3)

legend("topright",
       title="PDF of Exp(l)",
       c("l=2", "l=1", "l=0.5"),
       lty=c(1,2,3),
       col=c("purple", "blue", "red"))

### Computing theoretical probabilities

#P(X <= 4) = P(X < 4) as distribution continuous
l <- 2
pexp(4, l)

#P(X > 4)
1 - pexp(4, l)

#P(2 < X <= 3) = P(X <= 3) - P(X < 2)
pexp(3,l) - pexp(2, l)

### Visualizing the cdf
xs <- seq(0, 10, by=0.01)

par(mfrow=c(2,2))

plot(xs, pexp(xs,l), type="l", col="blue", ylab="F(x)", main="CDF of Exp(2)")

#This doesn't work!!!
curve(pexp(xs, rate = l), 0, 10, col="green", ylab="F(x)", main="CDF of Exp(2)")

plot(xs, type="n", xlim=c(0,10), ylim=c(0,1),
     xlab="xs", ylab="F(x)", main="PDF of Exp(2)")
lines(xs, pexp(xs,l), type="l", col="purple")

par(mfrow=c(1,1)) 

### Computing theoretical quantiles
l <- 2
x <- 4
c(pexp(4, l), qexp(0.9996645, l))

#Find x: P(X <= x) = 0.9
qexp(0.9, l)
#check
pexp(1.151293, l)

#Find x: P(X > x) = 0.6
qexp(1-0.6, l)
#check
pexp(0.2554128, l)

ys <- seq(0, 1, by=0.01)
plot(ys, qexp(ys,l), type="l", xlab="cumulative probability", ylab="x")


#The median
c(qexp(0.5, l), pexp(0.3465736,l))

#The interquartile range
qexp(0.75, l)-qexp(0.25,l)

### Running simulations

#Suppose our random number is u.
#We want x : cdf (Exp(l)) = u
#A solution to the above equation is x := -log(1 - u)/l = qexp(u, l)
l <- 2
u <- 0.384
qexp(u,l) 

set.seed(31)
n <- 1000
S <- rexp(n, l)
#table(S) unhelpful as continuous
hist(S, prob=TRUE, xlab="simulated value", main="simulations from Exp(2)")
xs <- seq(0, 3.5, by=0.01)
lines(xs, dexp(xs, l), type="l", col="blue")


### Computing empirical probabilities
#P(X <= 2)
c(length(S[S<=2])/length(S), pexp(2,l))

#P(X > 1)
c(length(S[S>1])/length(S), 1 -pexp(1,l))

### Computing empirical moments
c(mean(S), 1/l)
c(var(S), 1/l^2)

skew <- sum((S-mean(S))^3)/length(S)
c(skew/sd(S)^3, 2)

### Computing empirical quantiles
c(median(S), qexp(0.5,l))

c(quantile(S, 0.75)-quantile(S,0.25),
  qexp(0.75, l)-qexp(0.25,l))


### Long-term trend of the mean
lam <- 2
set.seed(31)
S <- rexp(5000, lam)
avgs <- rep(0, 5000)
for (k in 1:5000) {
  avgs[k] <- mean(S[1:k])
}

xs <- 1:5000
plot(xs, avgs[1:5000])
abline(h=1/lam, col="red", lty=2, lwd=2)
  
## The gamma distribution
a <- 3
l <- 2
x <- 4
#pdf (Gam(a,l)) x
dgamma(x, a, l)


### Visualizing the pdf
xs <- seq(0, 10, by=0.01)

par(mfrow=c(2,2))

plot(xs, dgamma(xs,a, l), type="l", col="blue", ylab="f(x)", main="PDF of Gam(3,2)")

#This doesn't work!!!
curve(dgamma(xs, a, l), 0, 10, col="green", ylab="f(x)", main="PDF of Gam(3,2)")

plot(xs, type="n",
     xlim=c(0,10), ylim=c(0,0.6),
     xlab="x", ylab="f(x)", main="PDF of Gam(3,2)")
lines(xs, dgamma(xs,a,l), type="l", col="purple")

par(mfrow=c(1,1)) 

### Varying the parameter(s)

l <- 2
xs <- seq(0, 10, 0.01)

plot(xs, type="n",
     xlim=c(0,10), ylim=c(0,0.6),
     xlab="x", ylab="f(x)", main="PDF of Gam(3,2)")

a <- 3
lines(xs, dgamma(xs, a, l), type="l", col="purple", lty=1)

a <- 5
lines(xs, dgamma(xs, a, l), type="l", col="blue", lty=2)

a <- 10
lines(xs, dgamma(xs, a, l), type="l", col="red", lty=3)

a <- 0.5
lines(xs, dgamma(xs, a, l), type="l", col="green", lty=4)


### Visualizing the cdf
a <- 3
l <- 2
xs <- seq(0, 10, by=0.01)
plot(xs, pgamma(xs, a,l), type="l", col="blue",
     ylab="cdf (Gam(a,l)) x", main="CDF of Gamma(3,2)")

### Computing theoretical probabilities
a <- 3
l <- 2

#calculate P(X < 1.5)
pgamma(1.5, a, l)

#calculate P(X > 3)
1 - pgamma(3, a, l)

### Computing theoretical quantiles
#Find x such that P(X <= x) = 0.5768099
qgamma(0.5768099, a, l)

#Find x such that P(X > x) = 0.3, check answer
c(qgamma(1-0.3, a, l), 1 - pgamma(1.807784, a,l))

#The median
c(qgamma(0.5, a, l), pgamma(1.33703, a,l))

### Running simulations
set.seed(43)
n <- 500
S <- rgamma(n, a, l)

hist(S, prob=TRUE, ylim=c(0,0.6),
     xlab="simulated value", main="simulations from a Gamma(3,2))")
xs <- seq(0, 10, by=0.01)
lines(xs, dgamma(xs,a,l), type="l", col="blue")

### Computing empirical probabilities
#P(<1.5)
c(length(S[S < 1.5])/length(S), pgamma(1.5, a,l))

#P(>3)
c(length(S[S > 3])/length(S), 1-pgamma(3, a,l))


### Computing empirical quantiles
c(quantile(S,0.75), qgamma(0.75, a,l))
### Computing empirical moments
c(mean(S), a/l)

### Long-term trend of the mean
xbars <- rep(0,500)

for (k in 1:500) {
  xbars[k] <- mean(S[1:k])
}

xs <- 1:500
plot(xs, xbars)
abline(h = a/l, col="red", lty=2, lwd=2)

## The chi-square distrbution
#Chi(v) = Gamma(v/2, 0.5)
v <- 4
a <- v/2
l <- 1/2

x <- 4
c(dchisq(x,v), dgamma(x, v/2, 1/2))

### Visualizing the pdf
v <- 4
xs <- seq(0,20, by=0.01)
plot(xs, dchisq(xs,v), type="l", col="blue",
     ylab="pdf (Chi(v)) x", main="PDF of Chi(4)")

v <- 6
lines(xs, dchisq(xs, v), type="l", col="purple", lty=2)

v <- 10
lines(xs, dchisq(xs, v), type="l", col="red", lty=2)

# v = 2 => Chi(v) = Gamma(1, 0.5) =  Exp(0.5)
v <- 2
lines(xs, dchisq(xs, v), type="l", col="green", lty=2)

### Visualizing the cdf
### Computing theoretical probabilities
v <- 4

#P(X < 1.5)
pchisq(1.5, v)

#P(X > 3)
1 - pchisq(3,v)

### Computing theoretical quantiles
# x: P(X <= x) = 0.1733585
c(qchisq(0.1733585, v), pchisq(1.5, v))

#x : P(X <= x) = 0.5 i.e. median
c(qchisq(0.5, v), pchisq(3.356694, v))

### Running simulations
set.seed(47)

n <- 1000
S <- rchisq(n,v)

hist(S, prob=TRUE, ylim=c(0,0.2),
     xlab="Simulated value", main="simulations from Chi(4)")

range(S)
xs <- seq(0, 25, by=0.01)
lines(xs, dchisq(xs, v), type="l", col="blue")

### Computing empirical probabilities
#P(X < 1.5)
c(length(S[S < 1.5])/length(S), pchisq(1.5, v))

### Computing empirical quantiles
### Computing empirical moments
c(mean(S), v)

c(var(S) , 2 * v)

### X ~ Gam(a, l) == 2 * l * x = Chi(2 * a)
a <- 2
l <- 1.5
x <- 4
#P(X > 4) = P(2 * l * X > 12)
c(1 - pgamma(x, a, l), 1 - pchisq(2 * l * x, 2 * a))

## The continuous uniform distribution
a <- 0
b <- 2
#pdf (Uni(a,b)) x = 1/(b-a)
c(dunif(0.6, a, b), 1/(b-a))

### Visualizing the pdf
### Varying the parameter(s)
### Visualizing the cdf
### Computing theoretical probabilities
#P(X < 1.2)
a <- 0
b <- 2
punif(1.2, a, b)

#P(X > 0.3)
1 - punif(0.3,a,b)

### Computing theoretical quantiles
#Find x : P(X <= x) = 0.6
c(qunif(0.6,a,b), punif(1.2,a,b))

### Running simulations
#If we generate a random number u from Uni(0,1)
#we can simulate x from distribution with cdf F 
#by solving x:  u = F.x

#Simulate 1000 values from an Exp(0.5) distribution.
set.seed(53)
u <- runif(1000, 0, 1)
#Solve x: u = cdf (Exp(0.5)) = 1 - exp(-0.5x)
# x = -2 * ln(1 - u)
x <- -2 * log(1 - u)
hist(x, prob=TRUE)
range(x)
xs <- seq(0, 15, by=0.1)
lines(xs, dexp(xs, 0.5), type="l", col="blue")

### Computing empirical probabilities
### Computing empirical quantiles
### Computing empirical moments
### Long-term trend of the mean

## The beta distribution
a <- 2
b <- 3
x <- 0.6
c(dbeta(x, a, b), 0, 0)

### Visualizing the pdf
xs <- seq(0,1, by=0.01)
plot(xs, dbeta(xs, a, b), type="l", col="blue",
     ylab="pdf x", main="PDF of Beta(2,3)")

### Varying the parameter(s)
### Visualizing the cdf

### Computing theoretical probabilities
a <- 2
b <- 3

#P(X < 0.8)
pbeta(0.8, a, b)

#P(X > 0.3)
1 - pbeta(0.3, a, b)

### Computing theoretical quantiles
#Find x: P(X <= x) = 0.6
c(qbeta(0.6, a, b), pbeta(0.4445, a,b))

### Running simulations
set.seed(53)

n <- 1000
S <- rbeta(n, a, b)

hist(S, prob=TRUE, ylim=c(0,1.8),
     xlab="simulated value", main="simulations from a Beta(a,b)")

range(S)
xs <- seq(0, 1, by = 0.01)
lines(xs, dbeta(xs, a, b), type="l", col="blue")

### Computing empirical probabilities
a <- 2
b <- 3

#P(X > 0.3)
c(length(S[S>0.3])/length(S), 1 - pbeta(0.3,a,b))

### Computing empirical quantiles
#x : P(X <= x) = 0.5 
c(median(S), qbeta(0.5, a, b))

### Computing empirical moments
c(mean(S), a/(a+b))
### Long-term trend of the mean

## The normal distribution
m <- 10
s <- 2

x <- 8
dnorm(x, m, s)

### Visualizations
xs <- seq(3, 17, by=0.01)
plot(xs, dnorm(xs, m, s), type="l", col="blue",
     ylab="pdf (Norm(m, s^2)) x", main="PDF of N(10,2^2)")


### Varying the parameter(s)
### Visualizing the cdf
### Computing theoretical probabilities
#P(X < 12)
pnorm(12, m, s)

#P(X > 9)
1-pnorm(9, m, s)

### Computing theoretical quantiles
#Find x: P(X <= x) = 0.8, check
c(qnorm(0.8, m, s), pnorm(11.68324,m, s))

### Running simulations
set.seed(59)

n <- 1000
S <- rnorm(n,m,s)
hist(S, prob=TRUE, xlab="simulated value", main="simulations from N(10, 2^2)")
range(S)
xs <- seq(3,16, by=0.01)
lines(xs, dnorm(xs, m, s), type="l", col="blue")

### Computing empirical probabilities
#P(X > 9)
c(length(S[S>9])/length(S), 1-pnorm(9, m, s))

### Computing empirical quantiles
#x: P(X < x) = 0.8
c(quantile(S,0.8), qnorm(0.8, m, s))

### Computing empirical moments
c(mean(S), m)
c(sd(S), s)

### Generating random simulations with exact mean and sd
set.seed(59)

n <- 1000
Z <- rnorm(n, 0, 1)
c(mean(Z), sd(Z))

Z <- (Z - mean(Z))/sd(Z)
c(mean(Z), sd(Z))

S <- Z*s + m
c(mean(S), sd(S))

## The lognormal distribution
m <- 5
s <- 2

x <- 50
dlnorm(x, m, s)

### Visualizing 
xs <- seq(0, 100, by=0.1)
plot(xs, dlnorm(xs, m, s), type="l", col="blue",
     ylab="pdf (LogN(m, s^2)) x", main="PDF of LogN(5, 2^2)")
### Varying the parameter(s)
### Visualizing the cdf

### Computing theoretical probabilities
#P(X <  100)
plnorm(100, m, s)

#P(X > 500)
1 - plnorm(500, m, s)

### Computing theoretical quantiles
# x : P(X <= x) = 0.9
qlnorm(0.9, m, s)

### Running simulations
set.seed(59)

n <- 1000
S <- rlnorm(n, m, s)
hist(S, prob=TRUE, 
     xlab="simulated value", main="simulations from LogN(5, 2^2)")
round(range(S,1))
xs <- seq(0, 55000, by=1)
lines(xs, dlnorm(xs, m, s), type="l", col="blue")

### Computing empirical probabilities
#P(X < 100)
c(length(S[S<100])/length(S), plnorm(100, m, s))

### Computing empirical quantiles
c(quantile(S, 0.9), qlnorm(0.9,m,s))

### Computing empirical moments
c(mean(S), exp(m+0.5 * s^2))
c(var(S), exp(m*2 + s^2) * exp(s^2)-1) #difference warrants using larger n in rlnorm

### Computing simulations with exact mean and sd

## The t distribution
v <- 10

x <- 2
dt(x, v
   )
### Visualizing the pdf
xs <- seq(-4, 4, by=0.01)
plot(xs, dt(xs,v), type="l", col="blue",
     ylab="pdf x", main="PDF of Stu(10)")

### Varying the parameter(s)
### Visualizing the cdf
### Computing theoretical probabilities
#P(X < -0.5)
pt(-0.5, v)

#P(X > 1.5)
1 - pt(1.5, v)

### Computing theoretical quantiles
# x: P(X <= x) = 0.7, check
c(qt(0.7, v), pt(0.541528, v))

### Running simulations
set.seed(61)

n <- 1000
S <- rt(n, v)
hist(S, prob=TRUE, ylim=c(0,0.4), 
     xlab="simulated value", main="simulations from Stu(10)")
round(range(S,1))
xs <- seq(-5, 4, by=0.01)
lines(xs, dt(xs, v), type="l", col="blue")

### Computing empirical probabilities
#P(X < -0.5)
c(length(S[S < -0.5])/length(S), pt(-0.5, v))

### Computing empirical quantiles
c(quantile(S, 0.7), qt(0.7, v))

### Computing empirical moments
c(mean(S))
c(var(S))


## The Fisher "F" distribution
v <- 2
w <- 3

x <- 2
df(x, v, w)

### Visualizing the pdf
xs <- seq(0, 4, by=0.01)
plot(xs, df(xs,v, w), type="l", col="blue",
     ylab="pdf x", main="PDF of Fisher(2,3)")

### Varying the parameter(s)
### Visualizing the cdf

### Computing theoretical probabilities
#P(X < 0.8)
pf(0.8, v, w)

#P(X > 2)
1 - pf(2, v,w)

### Computing theoretical quantiles
# x: P(X <= x) = 0.6, check
c(qf(0.6, v, w), pf(1.263024, v, w))

### Running simulations
set.seed(63)

n <- 10000
S <- rf(n, v, w)
hist(S, prob=TRUE,
     xlab="simulated value", main="simulations from Fisher(2,3)")
round(range(S,1))
xs <- seq(0, 511, by=0.01)
lines(xs, df(xs, v, w), type="l", col="blue")

### Computing empirical probabilities
#P(X > 2)
c(length(S[S > 2])/length(S), 1 - pf(2, v,w))

### Computing empirical quantiles
c(quantile(S, 0.6), qf(0.6, v, w))

## Conditional to unconditional moments
#Annual claim numbers, X, come from a Poission distribution
#with mean M, M ~ Gamma(a,b)

#Find the empirical mean and variance for the annual claims.
a <- 5
b <- 2

set.seed(71)
n <- 10000
xs <- rep(0,n)
for (k in 1:n) {
  m <- rgamma(1, a, b)
  xs[k] <- rpois(1,m)
}

# E.X
#= 
# E(E(X|M))
#= X|M ~ Poi(M), 
# E(Poi(M))
#= E(Poi(M))=M
# E(M)
#= M ~ Gamma(a,b)
# E(Gamma(a,b))
#= tables
# a/b
c(mean(xs), a/b)

# V.X
#=
# E(V.(X|M)) + V(E(X|M))
#= 
# E(V(Poi(M))) + V(E(Poi.M))
#=
# E.M + V.M
#= M ~ Gamma(a,b)
# a/b + a/b^2
c(var(xs), a/b + a/b^2)

### Visualizing the conditional distribution
range(xs)
hist(xs, prob=TRUE, breaks=c(-0.5:14.5))

#X ~ Poi(Gamma(a,b)) = Nbin2(a, b/(b+1))
lines(0:14, dnbinom(0:14, a, b/(b+1)), type="o", col="blue")

c(length(xs[xs <= 6])/length(xs), pnbinom(6, a, b/(b+1)))


## 
### Visualizing the pdf
### Varying the parameter(s)
### Visualizing the cdf
### Computing theoretical probabilities
### Computing theoretical quantiles
### Running simulations
### Computing empirical probabilities
### Computing empirical quantiles
### Computing empirical moments
### Long-term trend of the mean