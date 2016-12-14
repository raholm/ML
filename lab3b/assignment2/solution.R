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

SVM <- function(sv, i) {
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

run_BOSVM <- function(data, beta, M) {
    N <- 500

    errors <- 1
    errorrate <- vector(length = N)
    errorrate[1] <- 1
    sv <- c(1)

    for(i in 2:N) {
        predicted <- SVM(data[sv,], data[i, -ncol(data)])

        if (data[i, "Spam"] * predicted < beta) {
            sv <- c(sv, i)
            errors <- errors + 1

            if (length(sv) > M) {
                sv <- sv[-sv.least_important(sv)]
            }
        }

        errorrate[i] <- errors / i
    }
    errorrate
}

errorrate <- run_BOSVM(spam, beta=0, M=500)
plot(errorrate[seq(from=1, to=N, by=10)],
     type="o", main="Beta=0, M=500")

errorrate <- run_BOSVM(spam, beta=-0.05, M=500)
plot(errorrate[seq(from=1, to=N, by=10)],
     type="o", main="Beta=-0.05, M=500")

errorrate <- run_BOSVM(spam, beta=0, M=20)
plot(errorrate[seq(from=1, to=N, by=10)],
     type="o", main="Beta=0, M=20")

errorrate <- run_BOSVM(spam, beta=-0.05, M=20)
plot(errorrate[seq(from=1, to=N, by=10)],
     type="o", main="Beta=-0.05, M=20")
## ---- end-of-assign2-init
