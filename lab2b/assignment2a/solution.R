## ---- assign2a-init
library(tree)

data <- read.csv2("../data/bodyfatregression.csv")
names(data) <- c("Waist", "Weight", "Bodyfat")
## ---- end-of-assign2a-init

## 1
## ---- assign2a-1-lowerbound
set.seed(1234567890)
train_idx <- sample(nrow(data), floor(nrow(data) * (2 / 3)))
train <- data[train_idx,]
test <- data[-train_idx,]

set.seed(1234567890)

tree_count <- 100
test_errors <- rep(0, tree_count)

for (i in 1:tree_count) {
    newdata <- train[sample(nrow(train), replace=TRUE),]
    fit <- tree(Bodyfat ~ ., data=newdata, split="deviance")
    test_error <- sum(predict(fit, test) - test$Bodyfat)
    test_errors[i] <- test_error
}

mean((test_errors / length(test_errors))^2)
## ---- end-of-assign2a-1-lowerbound


## ---- assign2a-1-upperbound
set.seed(1234567890)
train_idx <- sample(nrow(data), floor(nrow(data) * (2 / 3)))
train <- data[train_idx,]
test <- data[-train_idx,]

set.seed(1234567890)

tree_count <- 100
test_errors <- rep(0, tree_count)

for (i in 1:tree_count) {
    newdata <- train[sample(nrow(train), replace=TRUE),]
    fit <- tree(Bodyfat ~ ., data=newdata, split="deviance")

    test_error <- mean((predict(fit, test) - test$Bodyfat)^2)
    test_errors[i] <- test_error
}

mean(test_errors)
## ---- end-of-assign2a-1-upperbound

## 2
## ---- assign2a-2-upperbound
tree_count <- 100
fold_count <- 3
test_errors <- matrix(0, nrow=tree_count, ncol=fold_count)

set.seed(1234567890)

folds <- suppressWarnings(split(nrow(data), f=1:fold_count))

for (j in 1:fold_count) {
    train <- data[-folds[[j]],]
    test <- data[folds[[j]],]

    for (i in 1:tree_count) {
        newdata <- train[sample(nrow(train), replace=TRUE),]
        fit <- tree(Bodyfat ~ ., data=newdata, split="deviance")

        test_error <- mean((predict(fit, test) - test$Bodyfat)^2)
        test_errors[i, j] <- test_error
    }
}

mean(test_errors)
## ---- end-of-assign2a-2-upperbound

## 3
## ---- assign2a-3-bag
bagging.regtrees <- function(formula, data, newdata, b) {
    predictions <- matrix(0, nrow=nrow(newdata), ncol=b)
    trees <- list()

    for (i in 1:b) {
        bootstrap_sample <- data[sample(nrow(data), replace=TRUE),]
        fit <- tree(formula, data=bootstrap_sample, split="deviance")
        trees[[i]] <- fit
        predictions[, i] <- predict(fit, newdata)
    }

    list(trees=trees, predictions=rowMeans(predictions))
}
## ---- end-of-assign2a-3-bag

## ---- assign2a-3-bagcv
cv.regtrees <- function(formula, data, newdata, b, k) {
    predictions <- matrix(0, nrow(nrow(newdata)), ncol=b*k)

    
    for (i in 1:tree_count) {
        bootstrap_sample <- data[sample(nrow(data), replace=TRUE),]
        datasets <- suppressWarnings(split(bootstrap_sample, 1:k))

        train1 <- rbind(datasets[[1]], datasets[[2]])
        test1 <- datasets[[3]]

        train2 <- rbind(datasets[[1]], datasets[[3]])
        test2 <- datasets[[2]]

        train3 <- rbind(datasets[[2]], datasets[[3]])
        test3 <- datasets[[1]]

        fit1 <- tree(Bodyfat ~ ., data=train1, split="deviance")
        prediction1 <- predict(fit1, newdata)

        fit2 <- tree(Bodyfat ~ ., data=train2, split="deviance")
        prediction2 <- predict(fit2, newdata)

        fit3 <- tree(Bodyfat ~ ., data=train3, split="deviance")
        prediction2 <- predict(fit2, newdata)

        predictions[, (i - 1) * k + 1] <- predcition1
        predictions[, (i - 1) * k + 2] <- prediction2
        predictions[, (i - 1) * k + 3] <- prediction3
    }

    rowMeans(predictions)
}
## ---- end-of-assign2a-3-bagcv
