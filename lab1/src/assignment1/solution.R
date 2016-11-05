library(kknn)
library(caret)
library(ggplot2)

## 1
data <- read.csv("../../data/spambase.csv", sep=",", header=TRUE)

n <- nrow(data)
set.seed(12345)
id <- sample(1:n, floor(n * 0.5))

train <- data[id,]
test <- data[-id,]

train_labels <- factor(train[, ncol(train)], levels=c(0, 1), labels=c("non-spam", "spam"))
test_labels <- factor(test[, ncol(test)], levels=c(0, 1), labels=c("non-spam", "spam"))

threshold <- 0.5

## 2
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

## Testing that the implementation seems to be correct
predicted <- knearest(train, 1, train)
predicted_result(predicted, train_labels, threshold)

predicted <- knearest(test, 1, test)
predicted_result(predicted, test_labels, threshold)

## 3
predicted <- knearest(train, 5, test)
predicted_result(predicted, test_labels, threshold)

## 4
predicted <- knearest(train, 1, test)
predicted_result(predicted, test_labels, threshold)

## 5
kknn.fit <- kknn(Spam ~ ., train=train, test=test, distance=2, k=5)
predicted <- fitted(kknn.fit)
predicted_result(predicted, test_labels, threshold)

## 6
threshold <- seq(0.05, 0.95, by=0.05)

predicted_knearest <- knearest(train, 5, test)

kknn.fit <- kknn(Spam ~ ., train=train, test=test, distance=2, k=5)
predicted_kknn<- fitted(kknn.fit)

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

1- kknn_specificity
kknn_sensitivity

knearest_data <- data.frame(x=(1 - knearest_specificity), y=knearest_sensitivity,
                            label=as.character(threshold))
knearest_data

kknn_data <- data.frame(x=(1 - kknn_specificity), y=kknn_sensitivity,
                        label=as.character(threshold))
kknn_data

x1 <- cumsum(kknn_data$x)
x1 <- x1 / max(x1)
x1

y1 <- cumsum(kknn_data$y)
y1 <- y1 / max(y1)
y1


x2 <- cumsum(knearest_data$x)
x2 <- x2 / max(x2)
x2

y2 <- cumsum(knearest_data$y)
y2 <- y2 / max(y2)
y2


data <- data.frame(x1=c(0, x1), y1=c(0, y1),
                   x2=c(0, x2), y2=c(0, y2),
                   x3=seq(0, 0.95, 0.05), y3=seq(0, 0.95, 0.05))

ggplot() +
    geom_line(data=data, aes(x1, y1), color="green") +
    geom_line(data=data, aes(x2, y2), color="blue") +
    geom_line(data=data, aes(x3, y3), color="red") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits=c(0, 1))

ggplot() + ggtitle("ROC Curve") +
    xlab("False Positive Rate (1 - specificity)") +
    ylab("True Positive Rate (sensitivity)") +
    geom_point(data=knearest_data, aes(x, y), color="red") +
    geom_point(data=kknn_data, aes(x, y), color="blue") +
    scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits=c(0, 1)) +
    theme(plot.title=element_text(hjust=0.5))
