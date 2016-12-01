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
set.seed(1234567890)

tree_count <- 100
test_errors <- rep(0, tree_count)

for (i in 1:tree_count) {
    newdata <- train[sample(nrow(train), replace=TRUE),]
    fit <- tree(Bodyfat ~ ., data=newdata, split="deviance")
    test_error <- sum(predict(fit, test) - test$Bodyfat)
    test_errors[i] <- test_error
}

mean((test_errors / tree_count)^2)

## 2
set.seed(1234567890)

tree_count <- 100
errors <- rep(0, tree_count)

for (i in 1:tree_count) {
    newdata <- data[sample(nrow(data), replace=TRUE),]
    fit <- tree(Bodyfat ~ ., data=newdata, split="deviance")
    fit.cv <- cv.tree(fit, K=3)

    optimal_size <- fit.cv$size[which.min(fit.cv$dev)]
    fit <- prune.tree(fit, best=optimal_size)
    
    error <- sum(predict(fit, data) - data$Bodyfat)
    errors[i] <- error
}

mean((errors / tree_count)^2)

## 3

## Bagging Regression Trees
set.seed(1234567890)
trees <- list()

for (i in 1:tree_count) {
    newdata <- data[sample(nrow(data), replace=TRUE),]
    fit <- tree(Bodyfat ~ ., data=newdata, split="deviance")
    trees[[i]] <- fit
}

length(trees)

## Bagging CV Regression Trees
set.seed(1234567890)
trees <- list()

for (i in 1:tree_count) {
    newdata <- data[sample(nrow(data), replace=TRUE),]
    fit <- tree(Bodyfat ~ ., data=newdata, split="deviance")
    fit.cv <- cv.tree(fit, K=3)

    optimal_size <- fit.cv$size[which.min(fit.cv$dev)]
    fit <- prune.tree(fit, best=optimal_size)
    trees[[i]] <- fit
}

