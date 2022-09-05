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
