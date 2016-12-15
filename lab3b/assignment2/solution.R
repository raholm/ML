## ---- assign2-init
library(ggplot2)
library(reshape2)

set.seed(1234567890)
spam <- read.csv2("../data/spambase.csv")

ind <- sample(1:nrow(spam))
spam <- spam[ind, c(1:48,58)]
spam$Spam <- 2 * spam$Spam - 1

gaussian_k <- function(x, h) {
    exp(-(x / 2 * h))
}

euclideansq_d <- function(x, xi) {
    x <- t(as.matrix(x))
    xi <- as.numeric(xi)
    colSums((x - xi)^2)
}

SVM <- function(sv, xi) {
    h <- 1
    b <- 0

    x <- sv[, -ncol(sv)]
    t <- sv[, ncol(sv)]

    k <- gaussian_k(euclideansq_d(x, xi), h)

    sum(t * k) + b
}

sv.least_important <- function(sv) {
    which.max(lapply(sv, function(m) {
        obs <- spam[m,]
        x <- obs[, -ncol(obs)]
        t <- obs[, ncol(obs)]
        y <- SVM(spam[sv,], x)
        h <- 1
        k <- gaussian_k(euclideansq_d(x, x), h)
        t * (y - t * k)
    }))
}

run_BOSVM <- function(data, beta, M, N) {
    errors <- 1
    errorrate <- vector(length = N)
    errorrate[1] <- 1
    sv <- c(1)

    for(i in 2:N) {
        predicted <- SVM(data[sv,], data[i, -ncol(data)])

        if (data[i, "Spam"] * predicted <= beta) {
            sv <- c(sv, i)

            if (length(sv) > M) {
                sv <- sv[-sv.least_important(sv)]
            }
        }

        if (data[i, "Spam"] * predicted < 0) {
            errors <- errors + 1
        }

        errorrate[i] <- errors / i
    }
    list(errorrate=errorrate, sv=sv)
}

N <- 500
## ---- end-of-assign2-init

## ---- assign2-run1
result1 <- run_BOSVM(data=spam, beta=0, M=500, N=N)
cat("Beta = 0, M = 500")
cat(paste("Number of Support Vectors:", length(result1$sv)))
## ---- end-of-assign2-run1

## ---- assign2-run2
result2 <- run_BOSVM(data=spam, beta=-0.05, M=500, N=N)
cat("Beta = -0.05, M = 500")
cat(paste("Number of Support Vectors:", length(result2$sv)))
## ---- end-of-assign2-run2

## ---- assign2-run3
result3 <- run_BOSVM(data=spam, beta=0, M=20, N=N)
cat("Beta = 0, M = 20")
cat(paste("Number of Support Vectors:", length(result3$sv)))
## ---- end-of-assign2-run3

## ---- assign2-run4
result4 <- run_BOSVM(data=spam, beta=-0.05, M=20, N=N)
cat("Beta = -0.05, M = 20")
cat(paste("Number of Support Vectors:", length(result4$sv)))
## ---- end-of-assign2-run4

## ---- assign2-plot
plot_data <- data.frame(
    x=1:length(seq(from=1, to=N, by=10)),
    B0M500=result1$errorrate[seq(from=1, to=N, by=10)],
    B5M500=result2$errorrate[seq(from=1, to=N, by=10)],
    B0M20=result3$errorrate[seq(from=1, to=N, by=10)],
    B5M20=result4$errorrate[seq(from=1, to=N, by=10)])

plot_data <- melt(plot_data, id="x", value.name="ErrorRate",
                  variable.name="Res")

ggplot(plot_data) +
    geom_line(aes(x=x, y=ErrorRate, color=Res))
## ---- end-of-assign2-plot
