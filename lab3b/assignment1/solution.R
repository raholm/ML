## ---- assign1-init
library(pamr)
library(glmnet)
library(kernlab)
library(ggplot2)

data <- read.csv("../data/data.csv", sep=";", header=TRUE,
                 stringsAsFactors=FALSE, encoding="latin1")
rownames(data) <- 1:nrow(data)

set.seed(12345)
train_idx <- sample(nrow(data), size=floor(nrow(data) * 7 / 10))
train <- data[train_idx,]
test <- data[-train_idx,]

x <- t(train[, -ncol(data)])
y <- train[, ncol(data)]

x_test <- t(test[, -ncol(data)])
y_test <- test[, ncol(data)]
## ---- end-of-assign1-init

## 1
## ---- assign1-1-nsc
set.seed(12345)

nsc_data <- list(x=x, y=as.factor(y), geneid=as.character(1:nrow(x)), genenames=rownames(x))
model <- pamr.train(nsc_data, threshold=seq(0,4, 0.1))
cvmodel <- pamr.cv(model, nsc_data)

optimal_threshold <- cvmodel$threshold[which.min(cvmodel$error)]
optimal_size <- cvmodel$size[which.min(cvmodel$error)]

class_error <- 1 - (sum(pamr.predict(model, x_test,
                                     threshold=optimal_threshold) == y_test) /
                    length(y_test))

optimal_threshold
optimal_size
class_error

pamr.plotcen(model, nsc_data, threshold=1)
pamr.plotcen(model, nsc_data, threshold=2.5)
pamr.plotcen(model, nsc_data, threshold=optimal_threshold)


a <- pamr.listgenes(model, nsc_data, threshold=2.5)
cat(paste(colnames(data)[as.numeric(a[,1])], collapse='\n' ) )

a <- pamr.listgenes(model, nsc_data, threshold=optimal_threshold)
cat(paste(colnames(data)[as.numeric(a[,1])][1:10], collapse='\n' ) )

print(cvmodel)
pamr.plotcv(cvmodel)
## ---- end-of-assign1-1-nsc


## 2
## a
## ---- assign1-2-elasticnet
set.seed(12345)

alpha <- 0.5
fit <- cv.glmnet(x=t(x), y=y, alpha=alpha, family="binomial")

optimal_lambda <- fit$lambda[which.min(fit$cvm)]
optimal_size <- fit$nzero[which.min(fit$cvm)]
class_error <- 1 - (sum(predict(fit, t(x_test), type="class") == y_test) / length(y_test))

optimal_lambda
optimal_size
class_error
## ---- end-of-assign1-2-elasticnet

## b
## ---- assign1-2-svm
set.seed(12345)

fit <- ksvm(x=t(x), y=y, kernel="vanilladot",
            type="C-svc", cross=10, scale=FALSE)

optimal_size <- fit@nSV
class_error <- 1 - (sum(predict(fit, t(x_test)) == y_test) / length(y_test))

optimal_size
class_error
## ---- end-of-assign1-2-svm

## 3
## ---- assign1-3-benjhoch
benjamini_hochberg <- function(x, y, alpha) {
    pvalues <- apply(x, 2, function(feature) t.test(feature ~ y, alternative="two.sided")$p.value)
    m <- length(pvalues)

    sorted <- sort(pvalues)
    values <- 1:m * alpha / m

    L <- which.min(sorted < values) - 1
    mask <- sorted <= sorted[L]
    list(mask=mask, pvalues=sorted, features=colnames(x)[order(pvalues)][mask])
}

result <- benjamini_hochberg(x=data[,-ncol(data)], y=data[, ncol(data)], alpha=0.05)
## ---- end-of-assign1-3-benjhoch

## ---- assign1-3-features
result$features
## ---- end-of-assign1-3-features

## ---- assign1-3-plot
ggplot() +
    geom_point(data=data.frame(x=1:length(result$features),
                               y=result$pvalues[result$mask]),
               aes(x=x, y=y), col="red") +
    geom_point(data=data.frame(x=((length(result$features) + 1):(ncol(data) -1)),
                               y=result$pvalues[!result$mask]),
               aes(x=x, y=y), col="blue")
## ---- end-of-assign1-3-plot
