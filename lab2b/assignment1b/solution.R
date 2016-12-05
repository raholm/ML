## ---- assign1b-init
set.seed(1234567890)

max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations

N <- 1000 # number of training points
D <- 10 # number of dimensions

## true mixing coefficients
true_pi <- vector(length=3)
true_pi <- c(1/3, 1/3, 1/3)

## true conditional distributions
true_mu <- matrix(nrow=3, ncol=D)
true_mu[1,] <- c(0.5, 0.6, 0.4, 0.7, 0.3, 0.8, 0.2, 0.9, 0.1, 1)
true_mu[2,] <- c(0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8, 0.1, 0.9, 0)
true_mu[3,] <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")

## Producing the training data
x <- matrix(nrow=N, ncol=D)

for(n in 1:N) {
    k <- sample(1:3, 1, prob=true_pi)
    for(d in 1:D) {
        x[n, d] <- rbinom(1, 1, true_mu[k, d])
    }
}

K <- 3 # number of guessed components
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
pi
mu

for(it in 1:max_it) {
    plot(mu[1,], type="o", col="blue", ylim=c(0,1))
    points(mu[2,], type="o", col="red")
    points(mu[3,], type="o", col="green")
    ## points(mu[4,], type="o", col="yellow")
    Sys.sleep(0.5)

    ## E-step: Computation of the fractional component assignments
    ## Your code here
    cond_joint <- matrix(1, nrow=N, ncol=D)
    for (n in 1:N) {
        for (k in 1:K) {
            for (i in 1:D) {
                cond_joint[n, i] <- cond_joint[n, i] * mu[k, i]^x[n, i] * (1 - mu[k, i])^x[n, i]
            }

            nominator <- pi[k] * cond_joint[n, k]
            denominator <- sum(cond_joint[n,])

            z[n, k] <- nominator / denominator
        }
    }

    ## Log likelihood computation.
    ## Your code here
    llik[it] <- 0
    for (n in 1:N) {
        for (k in 1:K) {
            summation <- 0
            for (i in 1:D) {
                summation <- summation + x[n, i] * log(mu[k, i]) + (1 - x[n, i]) * log(1 - mu[k, i])
            }
            llik[it] <- llik[it] + z[n, k] * (log(pi[k]) + summation)
        }
    }

    cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
    flush.console()

    ## Stop if the lok likelihood has not changed significantly
    ## Your code here
    if (it > 1 && abs(llik[it] - llik[it-1]) < min_change) break

    ## M-step: ML parameter estimation from the data and fractional component assignments
    ## Your code here
    for (k in 1:K) {
        pi[k] <- sum(z[, k]) / N
    }

    for (k in 1:K) {
        denominator <- sum(z[, k])
        for (i in 1:D) {
            nominator <- sum(x[, i] * z[, k])

            ## for (n in 1:N) {
            ##     nominator <- nominator + x[n, i] * z[n, k]
            ## }

            mu[k, i] <- nominator / denominator
        }
    }
}
pi
mu

plot(llik[1:it], type="o")
## ---- end-of-assign1b-init
