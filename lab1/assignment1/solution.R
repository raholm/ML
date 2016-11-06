## ---- assign1-init
library(kknn)
library(caret)
library(ggplot2)
library(reshape)

data <- read.csv("../data/spambase.csv", sep=",", header=TRUE)

n <- nrow(data)
set.seed(12345)
id <- sample(1:n, floor(n * 0.5))

train <- data[id,]
test <- data[-id,]

train_labels <- factor(train[, ncol(train)], levels=c(0, 1), labels=c("non-spam", "spam"))
test_labels <- factor(test[, ncol(test)], levels=c(0, 1), labels=c("non-spam", "spam"))

single_threshold <- 0.5

knearest <- function(data, k, newdata) {
    stopifnot(k > 0)
    
    train_labels <- data[, ncol(data)]
    train <- as.matrix(data[, -ncol(data)])
    train <- train / sqrt(rowSums(train^2))

    test_labels <- newdata[, ncol(newdata)]
    test <- as.matrix(newdata[, -ncol(newdata)])
    test <- test / sqrt(rowSums(test^2))

    cosine_sim <- train %*% t(test)
    cosine_dis <- 1 - cosine_sim

    ordering <- as.matrix(t(apply(cosine_dis, 2, order))[, 1:k])

    predicted <- as.matrix(apply(ordering, 1, function(x) {
        mean(train_labels[x])
    }))

    predicted
}

predicted_result <- function(predicted, actual, threshold) {
    predicted <- as.numeric(predicted > threshold)
    predicted <- factor(predicted, levels=c(0, 1), labels=c("non-spam", "spam"))
    table(actual, predicted)
}
## ---- end-of-assign1-init

## Testing that the implementation seems to be correct
## ---- assign1-test
predicted <- knearest(train, 1, train)
predicted_result(predicted, train_labels, single_threshold)

predicted <- knearest(test, 1, test)
predicted_result(predicted, test_labels, single_threshold)
## ---- end-of-assign1-test

## 3
## ---- assign1-3-table
predicted <- knearest(train, 5, test)
predicted_result(predicted, test_labels, single_threshold)
## ---- end-of-assign1-3-table

## 4
## ---- assign1-4-table
predicted <- knearest(train, 1, test)
predicted_result(predicted, test_labels, single_threshold)
## ---- end-of-assign1-4-table

## 5
## ---- assign1-5-table
kknn.fit <- kknn(Spam ~ ., train=train, test=test, distance=2, k=5)
predicted <- fitted(kknn.fit)
predicted_result(predicted, test_labels, single_threshold)
## ---- end-of-assign1-5-table

## 6
## ---- assign1-6-init
threshold <- seq(0.05, 0.95, by=0.05)

predicted_knearest <- knearest(train, 5, test)

kknn.fit <- kknn(Spam ~ ., train=train, test=test, distance=2, k=5)
predicted_kknn <- fitted(kknn.fit)

knearest_sensitivity <- rep(0, length(threshold))
knearest_specificity <- rep(0, length(threshold))

kknn_sensitivity <- rep(0, length(threshold))
kknn_specificity <- rep(0, length(threshold))

for (i in 1:length(threshold)) {
    knearest_prediction <- predicted_result(predicted_knearest, test_labels, threshold[i])
    knearest_sensitivity[i] <- sensitivity(knearest_prediction)
    knearest_specificity[i] <- specificity(knearest_prediction)

    kknn_prediction <- predicted_result(predicted_kknn, test_labels, threshold[i])
    kknn_sensitivity[i] <- sensitivity(kknn_prediction)
    kknn_specificity[i] <- specificity(kknn_prediction)
}

knearest_x <- c(0, rev(1 - knearest_specificity), 1)
knearest_y <- c(0, rev(knearest_sensitivity), 1)

kknn_x <- c(0, rev(1 - kknn_specificity), 1)
kknn_y <- c(0, rev(kknn_sensitivity), 1)

knearest_data <- data.frame(x=knearest_x, y=knearest_y,
                            label=rep("knearest", length(knearest_x)))

kknn_data <- data.frame(x=kknn_x, y=kknn_y,
                        label=rep("kknn", length(kknn_x)))

reference_line <- data.frame(x=seq(0, 1, 0.05), y=seq(0, 1, 0.05),
                             label=rep("random", length(knearest_x)))


complete_data <- melt(rbind(knearest_data, kknn_data, reference_line), id=c("x", "y"))
names(complete_data)[4] <- "Algorithm"
## ---- end-of-assign1-6-init

## ---- assign1-6-ROC
ggplot() + ggtitle("ROC Curve") +
    xlab("False Positive Rate (1 - specificity)") +
    ylab("True Positive Rate (sensitivity)") +
    geom_line(data=complete_data, aes(x=x, y=y, color=Algorithm), size=1) +
    scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits=c(0, 1)) +
    theme(plot.title=element_text(hjust=0.5))
## ---- end-of-assign1-6-ROC
