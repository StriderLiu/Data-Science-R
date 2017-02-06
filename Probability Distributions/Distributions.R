# 1.Beta
# Distribution Function
x <- seq(0, 1, length = 50)
dis1 <- pbeta(x, 3, 2)
df.dis1 <- data.frame(X = x, Beta = dis1)
ggplot(data = df.dis1, aes(x = X, y = Beta)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dbeta(x, 3, 2)
df.den1 <- data.frame(X = x, Beta = den1)
ggplot(data = df.den1, aes(x = X, y = Beta)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (shape1 and shape2)
den2 <- dbeta(x, 10, 1)
df.den2 <- data.frame(X = x, Beta = den2)
ggplot(data = df.den2, aes(x = X, y = Beta)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Use case: 1.time allocation in project management;
# 2.heterogeneity in the probability of HIV transmission

# 2.Binomial
# Distribution Function
dis1 <- pbinom(35:65, 100, 0.5)
df.dis1 <- data.frame(X = 1:length(dis1), Binomial = dis1)
ggplot(data = df.dis1, aes(x = X, y = Binomial)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dbinom(35:65, 100, 0.5)
df.den1 <- data.frame(X = 1:length(den1), Binomial = den1)
ggplot(data = df.den1, aes(x = X, y = Binomial)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (n and p)
den2 <- dbinom(30:40, 200, 0.3)
df.den2 <- data.frame(X = 1:length(den2), Binomial = den2)
ggplot(data = df.den2, aes(x = X, y = Binomial)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: binomial test; statistical significance
# If the sampling is carried out without replacement, the draws are not independent and so the resulting distribution is a hypergeometric distribution
# One-time experiment equals to Bernoulli trial
# if n is very large and p is very small, binomial trends to become Possion

# 3.Chi-Squared
# Distribution Function
dis1 <- pchisq(10, df = 1:30)
df.dis1 <- data.frame(X = 1:length(dis1), ChiSquare = dis1)
ggplot(data = df.dis1, aes(x = X, y = ChiSquare)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dchisq(10, df = 1:30)
df.den1 <- data.frame(X = 1:length(den1), ChiSquare = den1)
ggplot(data = df.den1, aes(x = X, y = ChiSquare)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (x and df)
den2 <- dchisq(40, df = 1:50, log = T)
df.den2 <- data.frame(X = 1:length(den2), ChiSquare = den2)
ggplot(data = df.den2, aes(x = X, y = ChiSquare)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: 1.hypothesis testing;
# 2.confidence intervals

# 4.Exponential
# Distribution Function
x <- seq(0, 5, length = 50)
dis1 <- pexp(x)
df.dis1 <- data.frame(X = x, Exponential = dis1)
ggplot(data = df.dis1, aes(x = X, y = Exponential)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dexp(x)
df.den1 <- data.frame(X = x, Exponential = den1)
ggplot(data = df.den1, aes(x = X, y = Exponential)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (rate)
den2 <- dexp(x, rate = 3)
df.den2 <- data.frame(X = x, Exponential = den2)
ggplot(data = df.den2, aes(x = X, y = Exponential)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: 1.analysis of Poisson processes;
# 2.counting life of a bulb (using memorylessness)

# 5.Gamma
# Distribution Function
x <- seq(0, 20, length = 50)
dis1 <- pgamma(x, shape = 10, scale = 1)
df.dis1 <- data.frame(X = x, Gamma = dis1)
ggplot(data = df.dis1, aes(x = X, y = Gamma)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dgamma(x, shape = 10, scale = 1)
df.den1 <- data.frame(X = x, Gamma = den1)
ggplot(data = df.den1, aes(x = X, y = Gamma)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (shape and scale)
den2 <- dgamma(x, shape = 5, scale = 3)
df.den2 <- data.frame(X = x, Gamma = den2)
ggplot(data = df.den2, aes(x = X, y = Gamma)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: modeling the size of insurance claims and rainfalls

# 6.Geometric
# Distribution Function
x <- seq(0, 1, length = 10)
dis1 <- pgeom(x, prob = .3)
df.dis1 <- data.frame(X = x, Geometric = dis1)
ggplot(data = df.dis1, aes(x = X, y = Geometric)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dgeom(x, prob = .3)
df.den1 <- data.frame(X = x, Geometric = den1)
ggplot(data = df.den1, aes(x = X, y = Geometric)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (prob)
den2 <- dgeom(x, prob = .8)
df.den2 <- data.frame(X = x, Geometric = den2)
ggplot(data = df.den2, aes(x = X, y = Geometric)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: 1.fetching balls of 2 colors from a bag, 
# 2.get the probability of getting 3 black balls before the first red ball.

# 7.Log Normal
# Distribution Function
x <- seq(0, 5, length = 50)
dis1 <- plnorm(x)
df.dis1 <- data.frame(X = x, LogNormal = dis1)
ggplot(data = df.dis1, aes(x = X, y = LogNormal)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dlnorm(x)
df.den1 <- data.frame(X = x, LogNormal = den1)
ggplot(data = df.den1, aes(x = X, y = LogNormal)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (meanlog and sdlog)
den2 <- dlnorm(x, meanlog = 2, sdlog = 1)
df.den2 <- data.frame(X = x, LogNormal = den2)
ggplot(data = df.den2, aes(x = X, y = LogNormal)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: 1.The length of comments posted in Internet discussion forums follows a log-normal distribution;
# 2.Measures of size of living tissue (length, skin area, weight)

# 8.Negative Binomial
# Distribution Function
dis1 <- pnbinom(35:65, 100, 0.5)
df.dis1 <- data.frame(X = 1:length(dis1), NegativeBinomial = dis1)
ggplot(data = df.dis1, aes(x = X, y = NegativeBinomial)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dnbinom(35:65, 100, 0.5)
df.den1 <- data.frame(X = 1:length(den1), NegativeBinomial = den1)
ggplot(data = df.den1, aes(x = X, y = NegativeBinomial)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (size and probability)
den2 <- dnbinom(30:40, 200, 0.3)
df.den2 <- data.frame(X = 1:length(den2), NegativeBinomial = den2)
ggplot(data = df.den2, aes(x = X, y = NegativeBinomial)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: if we define a "1" as failure, all non-"1"s as successes,
# and we throw a die repeatedly until the third time ???1??? appears (r = three failures),
# then the probability distribution of the number of non-???1???s that had appeared will be a negative binomial.

# 9.Normal
# Distribution Function
x <- seq(0, 10, length = 50)
dis1 <- pnorm(x)
df.dis1 <- data.frame(X = x, Normal = dis1)
ggplot(data = df.dis1, aes(x = X, y = Normal)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dnorm(x)
df.den1 <- data.frame(X = x, Normal = den1)
ggplot(data = df.den1, aes(x = X, y = Normal)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (mean and sd)
den2 <- dnorm(x, mean = 2.5, sd = 2)
df.den2 <- data.frame(X = x, Normal = den2)
ggplot(data = df.den2, aes(x = X, y = Normal)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: 1.Black???Scholes model;
# 2.Measurement errors;
# 3.standardized testing

# 10. Hypergeometric
# Distribution Function
x <- 0:9
dis1 <- phyper(x, 10, 7, 8)
df.dis1 <- data.frame(X = x, Hypergeometric = dis1)
ggplot(data = df.dis1, aes(x = X, y = Hypergeometric)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dhyper(x, 10, 7, 8)
df.den1 <- data.frame(X = x, Hypergeometric = den1)
ggplot(data = df.den1, aes(x = X, y = Hypergeometric)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (m, n and k)
den2 <- dhyper(x, 9, 12, 8)
df.den2 <- data.frame(X = x, Hypergeometric = den2)
ggplot(data = df.den2, aes(x = X, y = Hypergeometric)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: calculating probabilities when sampling without replacement

# 11.Poisson
# Distribution Function
x <- 0:10
dis1 <- ppois(x, 1)
df.dis1 <- data.frame(X = x, Poisson = dis1)
ggplot(data = df.dis1, aes(x = X, y = Poisson)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dpois(x, 1)
df.den1 <- data.frame(X = x, Poisson = den1)
ggplot(data = df.den1, aes(x = X, y = Poisson)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (lambda)
den2 <- dpois(x, lambda = 3)
df.den2 <- data.frame(X = x, Poisson = den2)
ggplot(data = df.den2, aes(x = X, y = Poisson)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: 1.telephone calls arriving in a system;
# 2.number of mutations on a strand of DNA per unit length;
# 3.number of Losses/Claims occurring in a given period of Time

# 12.Student t
x <- 0:10
dis1 <- pt(x, 1)
df.dis1 <- data.frame(X = x, Student_t = dis1)
ggplot(data = df.dis1, aes(x = X, y = Student_t)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dt(x, 1)
df.den1 <- data.frame(X = x, Student_t = den1)
ggplot(data = df.den1, aes(x = X, y = Student_t)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (df)
den2 <- dt(x, df = 3)
df.den2 <- data.frame(X = x, Student_t = den2)
ggplot(data = df.den2, aes(x = X, y = Student_t)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: Student's t-test; Hypothesis testing; Bayesian analysis 

# 13.Uniform
x <- seq(0, 2, by = 0.05)
dis1 <- punif(x)
df.dis1 <- data.frame(X = x, Uniform = dis1)
ggplot(data = df.dis1, aes(x = X, y = Uniform)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dunif(x)
df.den1 <- data.frame(X = x, Uniform = den1)
ggplot(data = df.den1, aes(x = X, y = Uniform)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (max and min)
den2 <- dunif(x, max = 2, min = 1)
df.den2 <- data.frame(X = x, Uniform = den2)
ggplot(data = df.den2, aes(x = X, y = Uniform)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: generate pseudo-random numbers

# 14.Weibull
x <- seq(0, 40, by = 1)
dis1 <- pweibull(x, shape = 1, scale = pi)
df.dis1 <- data.frame(X = x, Weibull = dis1)
ggplot(data = df.dis1, aes(x = X, y = Weibull)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
# when shape is 1 Weibull is equal to exponential distribution
all.equal(dweibull(x, shape = 1), dexp(x))
den1 <- dweibull(x, shape = 1, scale = pi)
df.den1 <- data.frame(X = x, Weibull = den1)
ggplot(data = df.den1, aes(x = X, y = Weibull)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (shape and scale)
den2 <- dweibull(x, shape = 3, scale = 6)
df.den2 <- data.frame(X = x, Weibull = den2)
ggplot(data = df.den2, aes(x = X, y = Weibull)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: 1.Weather forecast
# 2.The cost of an unplanned failure for a component,
# subject to a wear out failure mode, is twenty times the cost of a planned replacement. 
# What is the optimal replacement interval?
# 3.A state Air Resources Board requires a fleet recall when any part in the emissions system exceeds a 4% failure rate during the warranty period.
# Based on the warranty data, which parts will exceed the 4% rate and on what date?

# 15.Bernoulli
install.packages("LaplacesDemon")
library(LaplacesDemon)
x <- seq(0, 2, by = 0.05)
dis1 <- pbern(x, .5)
df.dis1 <- data.frame(X = x, Bernoulli = dis1)
ggplot(data = df.dis1, aes(x = X, y = Bernoulli)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue')
# Density Function
den1 <- dbern(x, .5)
df.den1 <- data.frame(X = x, Bernoulli = den1)
ggplot(data = df.den1, aes(x = X, y = Bernoulli)) + 
  geom_line(colour = 'magenta') + 
  geom_point(colour = 'blue') 
# Let's try different parameters (prob)
den2 <- dbern(x, prob = .1)
df.den2 <- data.frame(X = x, Bernoulli = den2)
ggplot(data = df.den2, aes(x = X, y = Bernoulli)) + 
  geom_line(colour = 'brown') + 
  geom_point(colour = 'yellow')
# Usage: tossing a coin; base case of many famous distributions