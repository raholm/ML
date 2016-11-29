## ---- assign4-init
library(mboost)
library(randomForest)
library(ggplot2)
library(reshape2)

data <- read.csv2("../data/spambase.csv")
data$Spam <- as.factor(data$Spam)

set.seed(1234567890)
train_idx <- sample(nrow(data), floor(nrow(data) * (2 / 3)))
train <- data[train_idx,]
test <- data[-train_idx,]
## ---- end-of-assign4-init

## ---- assign4-tree
tree_counts <- seq(10, 100, by=10)
test_errors <- rep(0, length(tree_counts))
train_errors <- rep(0, length(tree_counts))

for (i in 1:length(tree_counts)) {
    fit <- blackboost(Spam ~ ., data=train, family=AdaExp(),
                      control=boost_control(mstop=tree_counts[i]))
    test_error <- 1 - sum(predict(fit, test, type="class") == test$Spam) / nrow(test)
    train_error <- 1 - sum(predict(fit, type="class") == train$Spam) / nrow(train)
    test_errors[i] <- test_error
    train_errors[i] <- train_error
}
## ---- end-of-assign4-tree

## ---- assign4-tree-plot
plot_data <- data.frame(Trees=tree_counts, test=test_errors, train=train_errors)
plot_data <- melt(plot_data, id="Trees", value.name="Error", variable.name="Data")

ggplot(plot_data) +
    xlab("Number of Regression Trees") +
    ylab("Misclassification Rate") +
    geom_line(aes(x=Trees, y=Error, color=Data)) +
    scale_x_discrete(limits=tree_counts)
## ---- end-of-assign4-tree-plot

## ---- assign4-forest
test_errors <- rep(0, length(tree_counts))
train_errors <- rep(0, length(tree_counts))

for (i in 1:length(tree_counts)) {
    fit <- randomForest(Spam ~ ., data=train, ntree=tree_counts[i])
    test_error <- 1 - sum(predict(fit, test, type="class") == test$Spam) / nrow(test)
    train_error <- 1 - sum(predict(fit, type="class") == train$Spam) / nrow(train)
    test_errors[i] <- test_error
    train_errors[i] <- train_error
}
## ---- end-of-assign4-forest

## ---- assign4-forest-plot
plot_data <- data.frame(Trees=tree_counts, test=test_errors, train=train_errors)
plot_data <- melt(plot_data, id="Trees", value.name="Error", variable.name="Data")

ggplot(plot_data) +
    xlab("Number of Regression Trees") +
    ylab("Misclassification Rate") +
    geom_line(aes(x=Trees, y=Error, color=Data)) +
    scale_x_discrete(limits=tree_counts)
## ---- end-of-assign4-forest-plot
