library(kknn)

data <- read.csv("../../data/spambase.csv", sep=",", header=TRUE)

n <- nrow(data)
set.seed(12345)
id <- sample(1:n, floor(n * 0.5))
train <- data[id,]
test <- data[-id,]

knearest <- function(data, k, newdata) {
    stopifnot(k > 0)
    
    train_labels <- data[, ncol(data)]
    train <- as.matrix(data[, -ncol(data)])
    train <- train / sqrt(rowSums(train^2))

    test_labels <- newdata[, ncol(newdata)]
    test <- as.matrix(newdata[, -ncol(newdata)])
    test <- test / sqrt(rowSums(test^2))

    cosine_sim <- train %*% t(test)
    distance_mat <- 1 - cosine_sim


    
    ordering <- as.matrix(apply(distance_mat, 2, order)[1:k,])
    print(head(ordering))
    
    predicted_labels <- as.matrix(apply(ordering, 2, function(x) {
        mean(train_labels[x])
    }))
    predicted_labels
}

threshold <- 0.5

predicted_labels <- knearest(train, 5, test)
predicted_labels <- as.numeric(predicted_labels > threshold)
sum(predicted_labels)
predicted_labels <- factor(predicted_labels, levels=c(0, 1), labels=c("non-spam", "spam"))
table(predicted_labels, test[, ncol(test)])
table(predicted_labels, train[, ncol(train)])

predicted_labels <- knearest(train, 1, test)
predicted_labels <- as.numeric(predicted_labels > threshold)
predicted_labels <- factor(predicted_labels, levels=c(0, 1), labels=c("non-spam", "spam"))
table(predicted_labels, test[, ncol(test)])


predicted_labels <- knearest(train, 1, train)
predicted_labels

predicted_labels <- as.numeric(predicted_labels > threshold)
predicted_labels <- factor(predicted_labels, levels=c(0, 1), labels=c("non-spam", "spam"))

predicted_labels
table(predicted_labels, train[, ncol(train)])



?kknn
