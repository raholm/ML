## ---- assign2-init
library(ggplot2)
library(fastICA)
library(pls)
library(reshape2)

data <- read.csv2("../data/NIRSpectra.csv")

X <- scale(data[, -ncol(data)])
y <- data[, ncol(data)]

traceplot <- function(n, pc1, pc2) {
    plot_data <- data.frame(x=1:n, PC1=pc1, PC2=pc2)
    plot_data <- melt(plot_data, id="x")
    names(plot_data) <- c("Index", "Component", "Value")
    xlimits <- seq(0, n, by=10)

    ggplot(plot_data) +
        geom_line(aes(x=Index, y=Value, color=Component), show.legend=FALSE) +
        scale_x_discrete(limits=xlimits) +
        facet_grid(Component ~ ., scales="free")
}
## ---- end-of-assign2-init

## 1
## ---- assign2-1
pca <- prcomp(X)

## Eigenvalues
lambda <- pca$sdev^2
variances <- lambda / sum(lambda)

var99_comp_count <- which.max(cumsum(variances * 100) > 99)
components <- as.data.frame(pca$x[, 1:var99_comp_count])
## ---- end-of-assign2-1

## ---- assign2-1-variance
sprintf("%2.3f", variances * 100)
sprintf("%2.3f", cumsum(variances))
## ---- end-of-assign2-1-variance

## ---- assign2-1-variance-plot
pc_comps <- 1:10
plot_data <- data.frame(x=pc_comps, Variance=variances[pc_comps])

ggplot(plot_data, aes(x=x, y=Variance)) +
    geom_bar(stat="identity") +
    scale_x_discrete(limits=pc_comps, labels=as.numeric(pc_comps)) +
    xlab("Principal Component")
## ---- end-of-assign2-1-variance-plot

## ---- assign2-1-score
ggplot(components) +
    geom_point(aes(x=PC1, y=PC2))
## ---- end-of-assign2-1-score

## 2
## ---- assign2-2
U <- pca$rotation
## ---- end-of-assign2-2

## ---- assign2-2-trace
traceplot(nrow(U), U[, 1], U[, 2])
## ---- end-of-assign2-2-trace

## 3
## ---- assign2-3
set.seed(12345)
ica <- fastICA(X, var99_comp_count, alg.typ = "parallel", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200, tol = 1e-06, verbose = FALSE)

W_prime <- ica$K %*% ica$W
components <- as.data.frame(ica$S)
## ---- end-of-assign2-3

## ---- assign2-3-trace
traceplot(nrow(W_prime), W_prime[, 1], W_prime[, 2])
## ---- end-of-assign2-3-trace

## ---- assign2-3-score
ggplot(components) +
    geom_point(aes(x=V1, y=V2))
## ---- end-of-assign2-3-score

## 4
## ---- assign2-4
set.seed(12345)
pcrfit <- pcr(Viscosity ~ ., data=data, scale=TRUE)
cvpcrfit <- crossval(pcrfit, segments=10, segment.type="random")
## ---- end-of-assign2-4

## ---- assign2-4-MSEP
cv_scores <- t(matrix(MSEP(cvpcrfit)$val, nrow=2))
plot_data <- data.frame(cbind(1:ncol(data), cv_scores))
colnames(plot_data) <- c("Components", "CV", "adjCV")
plot_data <- melt(plot_data, id="Components", variable_name="Measure")
names(plot_data)[ncol(plot_data)] <- "Score"
xlimits <- seq(0, ncol(data), by=5)
ylimits <- seq(0, max(plot_data$Score) + 0.05, by=0.05)

ggplot(plot_data) +
    geom_line(aes(x=Components, y=Score, color=Measure), size=1) +
    scale_x_discrete(limits=xlimits) +
    scale_y_continuous(breaks=ylimits, labels=ylimits, limits=c(0, max(plot_data$Score)))
## ---- end-of-assign2-4-MSEP
