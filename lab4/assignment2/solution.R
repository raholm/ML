## ---- assign2-init
library(ggplot2)
library(fastICA)
library(pcaMethods)

data <- read.csv2("../data/NIRSpectra.csv")
## ---- end-of.assign2-init

## 1
pca <- prcomp(data)

## Eigenvalues
lambda <- pca$sdev^2

sprintf("%2.3f",lambda / sum(lambda) * 100)
screeplot(pca)

var99_comp_count <- which.max(cumsum(lambda / sum(lambda) * 100) > 99)
components <- as.data.frame(pca$x[, 1:var99_comp_count])

ggplot(components) +
    geom_point(aes(x=PC1, y=PC2))

## 2
U <- pca$rotation

plot(U[, 1], main="Traceplot, PC1")
plot(U[, 2], main="Traceplot, PC2")

## 3
set.seed(12345)
ica <- fastICA(data, var99_comp_count, alg.typ = "parallel", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)
?fastICA

W_prime <- ica$K %*% ica$W

plot(W_prime[, 1], main="Traceplot, PC1")
plot(W_prime[, 2], main="Traceplot, PC2")

pccomps <- t(t(ica$K) %*% t(ica$X))
components <- as.data.frame(pccomps)

ggplot(components) +
    geom_point(aes(x=V1, y=V2))

## 4
set.seed(12345)
ppcafit <- pca(as.matrix(data), method="ppca", cv="q2")
ppcafit

?pca

         
