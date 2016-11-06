## 1
data <- read.csv("../../data/machines.csv", sep=",", header=TRUE)

## 2
length_histogram <- hist(data$Length)
multiplier <- length_histogram$counts / length_histogram$density
multiplier[which(multiplier == NaN)] <- mean(multiplier[which(multiplier != NaN)])
length_density <- density(data$Length)
length_density$y <- length_density$y * multiplier

plot(length_histogram, col="orange", main="Distribution",
     xlab="Lifetime", ylab="Frequency", xlim=c(0, 5))
lines(length_density, col="blue", lwd=2)

log_likelihood <- function(x, theta) {
    log(theta * exp(-theta * x))
}


thetas <- seq(0.1, 5, by=0.1)

log_likelihoods <- sapply(thetas, function(x) {
    sum(log_likelihood(x=data$Length, theta=x))
})

plot(thetas, log_likelihoods, main="Log-Likelihood", xlab="Theta", ylab="Log-Likelihood", type="l")

## 3
log_likelihoods_6 <- sapply(thetas, function(x) {
    sum(log_likelihood(x=data$Length[1:6], theta=x))
})

ylim <- c(min(min(log_likelihoods), min(log_likelihoods_6)), max(max(log_likelihoods), max(log_likelihoods_6)))

plot(thetas, log_likelihoods, col="orange",
     main="Log-Likelihood", xlab="Theta", ylab="Log-Likelihood", type="l", ylim=ylim)
lines(thetas, log_likelihoods_6, col="blue")

plot(thetas, log_likelihoods_6, type="l")

## 4
prior <- function(theta, lambda=10) {
    lambda * exp(-lambda * theta)
}

likelihood <- function(x, theta) {
    theta * exp(-theta * x)
}

log_posterior <- function(x, theta) {
    log(likelihood(x, theta) * prior(theta))
}

log_posteriors <- sapply(thetas, function(x) {
    sum(log_posterior(x=data$Length, theta=x))
})

plot(thetas, log_posteriors, col="orange",
     main="Log-Posterior", xlab="Theta", ylab="Log-Posterior", type="l")

## 5
set.seed(12345)
theta <- 1
new_data <- rexp(50, theta)

par(mfrow=c(1, 2))
hist(new_data)
hist(data$Length)
