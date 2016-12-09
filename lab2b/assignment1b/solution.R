## ---- assign1b-init
x_given_mu <- function(x, mu) {
    x_mu <- matrix(1, nrow=nrow(x), ncol=nrow(mu))

    for (n in 1:N) {
        for (k in 1:K) {
            for (i in 1:D) {
                prob <-  mu[k, i]^x[n, i] * (1 - mu[k, i])^(1 - x[n, i])
                x_mu[n, k] <- x_mu[n, k] * prob
            }
        }
    }

    x_mu
}

expectation.step <- function(x, x_given_mu, pi) {
    z <- matrix(nrow=nrow(x), ncol=length(pi))

    for (n in 1:N) {
        denominator <- sum(pi * x_given_mu[n,])

        for (k in 1:K) {
            nominator <- pi[k] * x_given_mu[n, k]

            z[n, k] <- nominator / denominator
        }
    }

    z
}

loglikelihood <- function(x, x_given_mu, pi) {
    llik <- 0
    for (n in 1:N) {
        inner_summation <- 0
        for (k in 1:K) {
            inner_summation <- inner_summation + pi[k] * x_given_mu[n, k]
        }
        llik <- llik + log(inner_summation)
    }

    llik
}


maximization.step <- function(x, z) {
    pi <- vector(length=ncol(z))
    mu <- matrix(nrow=ncol(z), ncol=ncol(x))
    
    for (k in 1:K) {
        pi[k] <- sum(z[, k]) / nrow(x)
    }

    for (k in 1:K) {
        denominator <- sum(z[, k])
        for (i in 1:D) {
            nominator <- sum(x[, i] * z[, k])
            mu[k, i] <- nominator / denominator
        }
    }

    list(pi=pi, mu=mu)
}

EM <- function(N, D, K, max_it, min_change, true_pi, true_mu) {

    ## Producing the training data
    x <- matrix(nrow=N, ncol=D)

    for(n in 1:N) {
        k <- sample(1:3, 1, prob=true_pi)
        for(d in 1:D) {
            x[n, d] <- rbinom(1, 1, true_mu[k, d])
        }
    }

    z <- matrix(nrow=N, ncol=K) # fractional component assignments
    pi <- vector(length=K) # mixing coefficients
    mu <- matrix(nrow=K, ncol=D) # conditional distributions
    llik <- vector(length=max_it) # log likelihood of the EM iterations

    ## Random initialization of the paramters
    pi <- runif(K, 0.49, 0.51)
    pi <- pi / sum(pi)
    for(k in 1:K) {
        mu[k,] <- runif(D, 0.49, 0.51)
    }

    for(it in 1:max_it) {
        x_mu <- x_given_mu(x, mu)

        ## E-step: Computation of the fractional component assignments
        z <- expectation.step(x, x_mu, pi)

        ## Log likelihood computation.
        llik[it] <- loglikelihood(x, x_mu, pi)

        ## Stop if the lok likelihood has not changed significantly
        if (it > 1 && abs(llik[it] - llik[it-1]) < min_change) break

        ## M-step: ML parameter estimation from the data and fractional component assignments
        result <- maximization.step(x, z)
        pi <- result$pi
        mu <- result$mu
    }

    list(pi=pi, mu=mu, llik=llik, it=it)
}

max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations

N <- 1000 # number of training points
D <- 10 # number of dimensions
K <- 3 # number of guessed components

## true mixing coefficients
true_pi <- vector(length=3)
true_pi <- c(1/3, 1/3, 1/3)

## true conditional distributions
true_mu <- matrix(nrow=3, ncol=D)
true_mu[1,] <- c(0.5, 0.6, 0.4, 0.7, 0.3, 0.8, 0.2, 0.9, 0.1, 1)
true_mu[2,] <- c(0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8, 0.1, 0.9, 0)
true_mu[3,] <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
## ---- end-of-assign1b-init

## ----- assign1b-plot-truemu
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1),
     xlab="Class", ylab="Probability")
axis(side=1, at=c(1:D))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
## ----- end-of-assign1b-plot-truemu

## ---- assign1b-EM-K2
set.seed(1234567890)

K <- 2
result <- EM(N, D, K, max_it, min_change, true_pi, true_mu)
mu <- result$mu
pi <- result$pi
llik <- result$llik
it <- result$it
## ---- end-of-assign1b-EM-K2

## ----- assign1b-EM-estimatepi2
pi
## ----- end-of-assign1b-EM-estimatepi2

## ----- assign1b-plot-estimatemu2
plot(mu[1,], type="o", col="blue", ylim=c(0,1),
     xlab="Class", ylab="Probability")
axis(side=1, at=c(1:D))
points(mu[2,], type="o", col="red")
## ----- end-of-assign1b-plot-estimatemu2

## ----- assign1b-plot-llik2
plot(llik[1:it], type="o", xlab="Iterations",
     ylab="Log-Likelihood")
## ----- end-of-assign1b-plot-llik2

## ---- assign1b-EM-K3
set.seed(1234567890)

K <- 3
result <- EM(N, D, K, max_it, min_change, true_pi, true_mu)
mu <- result$mu
pi <- result$pi
llik <- result$llik
it <- result$it
## ---- end-of-assign1b-EM-K3

## ----- assign1b-EM-estimatepi3
pi
## ----- end-of-assign1b-EM-estimatepi3

## ----- assign1b-plot-estimatemu3
plot(mu[1,], type="o", col="blue", ylim=c(0,1),
     xlab="Class", ylab="Probability")
axis(side=1, at=c(1:D))
points(mu[2,], type="o", col="red")
points(mu[3,], type="o", col="green")
## ----- end-of-assign1b-plot-estimatemu3

## ----- assign1b-plot-llik3
plot(llik[1:it], type="o", xlab="Iterations",
     ylab="Log-Likelihood")
## ----- end-of-assign1b-plot-llik3

## ---- assign1b-EM-K4
set.seed(1234567890)

K <- 4
result <- EM(N, D, K, max_it, min_change, true_pi, true_mu)
mu <- result$mu
pi <- result$pi
llik <- result$llik
it <- result$it
## ---- end-of-assign1b-EM-K4

## ----- assign1b-EM-estimatepi4
pi
## ----- end-of-assign1b-EM-estimatepi4

## ----- assign1b-plot-estimatemu4
plot(mu[1,], type="o", col="blue", ylim=c(0,1),
     xlab="Class", ylab="Probability")
axis(side=1, at=c(1:D))
points(mu[2,], type="o", col="red")
points(mu[3,], type="o", col="green")
points(mu[4,], type="o", col="orange")
## ----- end-of-assign1b-plot-estimatemu4

## ----- assign1b-plot-llik4
plot(llik[1:it], type="o", xlab="Iterations",
     ylab="Log-Likelihood")
## ----- end-of-assign1b-plot-llik4
