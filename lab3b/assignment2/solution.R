## ---- assign2-init
set.seed(1234567890)
spam <- read.csv2("../data/spambase.csv")

ind <- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)]
spam$Spam <- 2 * spam$Spam - 1

gaussian_k <- function(x, h) {
    ## Gaussian kernel
    exp(-(x / h)^2)
}

euclidean_d <- function(x, xi) {
    as.numeric(apply(x, 1, function(x) {
        sqrt(sum((x - xi)^2))
    }))
}

SVM <- function(sv, i) { # SVM on point i with support vectors sv
    ## Your code here
    ## Note that the labels in spambase.csv are 0/1 and SVMs need -1/+1. Then, use 2*label-1
    ## to convert from 0/1 to -1/+1
    ## Do not include the labels when computing the Euclidean distance between the point i
    ## and each of the support vectors. This is the distance to use in the kernel function
    ## You can use dist() to compute the Euclidean distance

    h <- 1
    b <- 0
    x <- sv[, -ncol(sv)]
    t <- sv[, ncol(sv)]

    predicted <- sum(t * gaussian_k(euclidean_d(x, i), h)) + b
    predicted
}

sv.least_important <- function(sv) {
    which.max(lapply(sv, function(m) {
        obs <- spam[m,]
        x <- obs[, -ncol(obs)]
        t <- obs$Spam
        y <- SVM(spam[sv,], x)
        h <- 1
        k <- gaussian_k(euclidean_d(x, x), h)
        t * (y - t * k)
    }))
}

h <- 1
beta <- -0.0 # Your value here
M <- 20 # Your value here
N <- 500 # number of training points

errors <- 1
errorrate <- vector(length = N)
errorrate[1] <- 1
sv <- c(1)

for(i in 2:N) {
    predicted <- SVM(spam[sv,], spam[i, -ncol(spam)])

    if (spam[i, "Spam"] * predicted < beta) {
        sv <- c(sv, i)
        errors <- errors + 1

        if (length(sv) > M) {
            sv <- sv[-sv.least_important(sv)]
        }
    }

    errorrate[i] <- errors / i
}

print(errorrate)
plot(errorrate)

plot(errorrate[seq(from=1, to=N, by=10)], ylim=c(0.2,0.4), type="o")

length(sv)
errorrate[N]
## ---- end-of-assign2-init
