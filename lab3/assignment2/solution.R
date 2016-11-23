## ---- assign2-init
library(gdata)
library(tree)
library(partykit)
library(ggplot2)
library(reshape2)
library(e1071)

data_division <- function(n, train, test, validation) {
    indices <- 1:n

    train <- sample(indices, floor(n * train))
    test <- sample(indices[-train], floor(n * test))
    validation <- indices[-c(train, test)]

    list(train=train, test=test, validation=validation)
}

data <- read.xls("../data/creditscoring.xls")
## ---- end-of-assign2-init

## 1
## ---- assign2-1
set.seed(12345)
indices <- data_division(n=nrow(data), train=0.5, test=0.25, validation=0.25)

train <- data[indices$train,]
test <- data[indices$test,]
validation <- data[indices$validation,]
## ---- end-of-assign2-1

## 2
## ---- assign2-2
prediction.tree <- function(model, X, y) {
    predicted <- predict(model, X)
    predicted <- factor(ifelse(predicted[, 1] > predicted[, 2], 0, 1),
                        levels=c(0, 1), labels=c("bad", "good"))

    confusion_matrix <- table(pred=predicted, true=y)
    list(confusion_matrix=confusion_matrix,
         misclassification_rate= 1 - sum(diag(confusion_matrix)) / sum (confusion_matrix))
}

dtreefit <- tree(good_bad ~ ., data=train, split="deviance")
gtreefit <- tree(good_bad ~ ., data=train, split="gini")
## ---- end-of-assign2-2

## ---- assign2-2-dtree-plot
plot(dtreefit)
## ---- end-of-assign2-2-dtree-plot

## ---- assign2-2-dtree-train
prediction.tree(dtreefit, train[, -ncol(train)], train$good_bad)
## ---- end-of-assign2-2-dtree-train

## ---- assign2-2-dtree-test
prediction.tree(dtreefit, test[, -ncol(test)], test$good_bad)
## ---- end-of-assign2-2-dtree-test

## ---- assign2-2-gtree-plot
plot(gtreefit)
## ---- end-of-assign2-2-gtree-plot

## ---- assign2-2-gtree-train
prediction.tree(gtreefit, train[, -ncol(train)], train$good_bad)
## ---- end-of-assign2-2-gtree-train

## ---- assign2-2-gtree-test
prediction.tree(gtreefit, test[, -ncol(test)], test$good_bad)
## ---- end-of-assign2-2-gtree-test

## 3
## ---- assign2-3
leaves <- 2:summary(dtreefit)[4]$size
train_score <- rep(0, max(leaves))
validation_score <- rep(0, max(leaves))

for(i in leaves) {
    prunedTree <- prune.tree(dtreefit, best=i)
    pred <- predict(prunedTree, newdata=validation, type="tree")
    train_score[i] <- deviance(prunedTree)
    validation_score[i] <- deviance(pred)
}
## ---- end-of-assign2-3

## ---- assign2-3-score
plot_data <- data.frame(Leaves=leaves,
                        Train=train_score[leaves],
                        Validation=validation_score[leaves])
plot_data <- melt(plot_data, id="Leaves", value.name="Deviance", variable.name="Set")

ggplot() +
    geom_line(data=plot_data, aes(x=Leaves, y=Deviance, color=Set)) +
    scale_x_continuous(breaks=leaves)
## ---- end-of-assign2-3-score

## ---- assign2-3-optimal
optimal_leaves <- which.min(validation_score[leaves]) + 1
optimal_tree <- prune.tree(dtreefit, best=optimal_leaves)
prediction.tree(optimal_tree, test[, -ncol(test)], test$good_bad)
## ---- end-of-assign2-3-optimal

## ---- assign2-3-plot
plot(optimal_tree)
text(optimal_tree, pretty=0)
## ---- end-of-assign2-3-plot

## 4
## ---- assign2-4
prediction.bayes <- function(model, X, y, loss) {
    predicted <- predict(model, X, type="raw")
    predicted <- factor(ifelse(predicted[, 1] / predicted[, 2] > loss, 0, 1),
                        levels=c(0, 1), labels=c("bad", "good"))

    confusion_matrix <- table(pred=predicted, true=y)
    list(confusion_matrix=confusion_matrix,
         misclassification_rate= 1 - sum(diag(confusion_matrix)) / sum (confusion_matrix))
}

bayesfit <- naiveBayes(good_bad ~ ., data=train)
## ---- end-of-assign2-4

## ---- assign2-4-train
prediction.bayes(bayesfit, train[, -ncol(train)], train$good_bad, 1)
## ---- end-of-assign2-4-train

## ---- assign2-4-test
prediction.bayes(bayesfit, test[, -ncol(test)], test$good_bad, 1)
## ---- end-of-assign2-4-test

## 5
## ---- assign2-5
bayesfit <- naiveBayes(good_bad ~ ., data=train)
## ---- end-of-assign2-5

## ---- assign2-5-train
prediction.bayes(bayesfit, train[, -ncol(train)], train$good_bad,  1 / 10)
## ---- end-of-assign2-5-train

## ---- assign2-5-test
prediction.bayes(bayesfit, test[, -ncol(test)], test$good_bad, 1 / 10)
## ---- end-of-assign2-5-test
