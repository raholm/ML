## 1

## ---- assign2-init
data <- read.csv("../data/machines.csv", sep=",", header=TRUE)
data$Length <- as.numeric(data$Length)
## ---- end-of-assign2-init

## 2

## ---- assign2-2-init
length_histogram <- hist(data$Length, plot=FALSE)
multiplier <- length_histogram$counts / length_histogram$density
multiplier <- max(multiplier[which(!is.nan(multiplier))])
length_density <- density(data$Length)
length_density$y <- length_density$y * multiplier

log_likelihood <- function(x, theta) {
    log(theta * exp(-theta * x))
}

thetas <- seq(0.1, 5, by=0.1)

log_likelihoods <- sapply(thetas, function(x) {
    sum(log_likelihood(x=data$Length, theta=x))
})

best_theta <- thetas[which.max(log_likelihoods)]
## ---- end-of-assign2-2-init

## ---- assign2-2-plot-distribution
plot(length_histogram, col="orange", main="Distribution",
     xlab="Lifetime", ylab="Frequency", xlim=c(0, 5))
lines(length_density, col="blue", lwd=2)
## ---- end-of-assign2-2-plot-distribution

## ---- assign2-2-plot-likelihoods
plot(thetas, log_likelihoods, main="Log-Likelihood",
     xlab="Theta", ylab="Log-Likelihood", type="l", lwd=2)
## ---- end-of-assign2-2-plot-likelihoods

## 3
## ---- assign2-3-init
log_likelihoods_6 <- sapply(thetas, function(x) {
    sum(log_likelihood(x=data$Length[1:6], theta=x))
})

ylim <- c(min(min(log_likelihoods), min(log_likelihoods_6)),
          max(max(log_likelihoods), max(log_likelihoods_6)))
## ---- end-ofassign2-3-init

## ---- assign2-3-plot-likelihoods
plot(thetas, log_likelihoods, col="orange",
     main="Log-Likelihood", xlab="Theta", ylab="Log-Likelihood",
     type="l", ylim=ylim, lwd=2)
lines(thetas, log_likelihoods_6, col="blue", lwd=2)
## ---- end-of-assign2-3-plot-likelihoods

## ---- assign2-3-plot-likelihoods-6
plot(thetas, log_likelihoods_6, type="l")
## ---- end-of-assign2-3-plot-likelihoods-6

## 4
## ---- assign2-4-init
prior <- function(theta, lambda=10) {
    lambda * exp(-lambda * theta)
}

log_posteriors <- sapply(1:length(thetas), function(i) {
    log_likelihoods[i] + log(prior(thetas[i]))
})
## ---- end-of-assign2-4-init

## ---- assign2-4-plot-posteriors
plot(thetas, log_posteriors, col="orange",
     main="Log-Posterior", xlab="Theta", ylab="Log-Posterior",
     type="l", lwd=2)
## ---- end-of-assign2-4-plot-posteriors

## 5
## ---- assign2-5-init
set.seed(12345)
new_data <- rexp(50, best_theta)
## ---- end-of-assign2-5-init

## ---- assign2-5-plot-distribution
par(mfrow=c(1, 2))
hist(new_data)
hist(data$Length)
## ---- end-of-assign2-5-plot-distribution
