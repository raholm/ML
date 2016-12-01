## ---- assign2-init
library(tree)

data <- read.csv2("../data/bodyfatregression.csv")
names(data) <- c("Waist", "Weight", "Bodyfat")

set.seed(1234567890)
train_idx <- sample(nrow(data), floor(nrow(data) * (2 / 3)))
train <- data[train_idx,]
test <- data[-train_idx,]
## ---- end-of-assign2-init

## 1
## Lower Bound
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

# Upper Bound
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

## 2
## Upper Bound
set.seed(1234567890)

tree_count <- 100
k <- 3
errors <- rep(0, tree_count * k)

for (i in 1:tree_count) {
    newdata <- data[sample(nrow(data), replace=TRUE),]
    datasets <- suppressWarnings(split(newdata, 1:k))

    train1 <- rbind(datasets[[1]], datasets[[2]])
    test1 <- datasets[[3]]

    train2 <- rbind(datasets[[1]], datasets[[3]])
    test2 <- datasets[[2]]

    train3 <- rbind(datasets[[2]], datasets[[3]])
    test3 <- datasets[[1]]
    
    fit1 <- tree(Bodyfat ~ ., data=train1, split="deviance")
    error1 <- mean((predict(fit1, test1) - test1$Bodyfat)^2)

    fit2 <- tree(Bodyfat ~ ., data=train2, split="deviance")
    error2 <- mean((predict(fit2, test2) - test2$Bodyfat)^2)

    fit3 <- tree(Bodyfat ~ ., data=train3, split="deviance")
    error3 <- mean((predict(fit3, test3) - test3$Bodyfat)^2)

    errors[(i - 1) * k + 1] <- error1
    errors[(i - 1) * k + 2] <- error2
    errors[(i - 1) * k + 3] <- error3
}

mean(errors)

## 3

## Bagging Regression Trees
bagging.regtrees <- function(formula, data, newdata, b) {
    predictions <- matrix(0, nrow=nrow(newdata), ncol=b)

    for (i in 1:k) {
        bootstrap_sample <- data[sample(nrow(data), replace=TRUE),]
        fit <- tree(formula, data=bootstrap_sample, split="deviance")
        predictions[, i] <- predict(fit, newdata)
    }

    rowMeans(predictions)
}

## Bagging CV Regression Trees
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
