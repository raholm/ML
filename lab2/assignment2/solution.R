## ---- assign2-init
library(MASS)
library(glmnet)
library(readxl)
library(ggplot2)
library(Matrix)
library(reshape2)

data <- read_excel("../data/tecator.xlsx")
## ---- end-of-assign2-init

## 1
## ---- assign2-2-plot
ggplot(data) +
    geom_point(aes(x=Protein, y=Moisture))
## ---- end-of-assign2-2-plot

## 2

## 3
## ---- assign2-3
set.seed(12345)
n <- nrow(data)
training_idx <- sample(1:n, size=floor(n * 0.5))

train <- data[training_idx,]
test <- data[-training_idx,]

power <- 6

train_mse <- rep(0, power)
test_mse <- rep(0, power)

for (i in 1:power) {
    model <- lm(Moisture ~ poly(Protein, i), data=train)

    train_mse[i] <- mean((train$Moisture - predict(model, train))^2)
    test_mse[i] <- mean((test$Moisture - predict(model, test))^2)
}

plot_data <- data.frame(x=1:power, Train=train_mse, Test=test_mse)
plot_data <- melt(plot_data, id="x", variable.name="Set")

ggplot(plot_data) +
    geom_line(aes(x=x, y=value, color=Set), lwd=2) +
    geom_point(aes(x=x, y=value, color=Set), size=4) +
    ggtitle("Polynomial Function") +
    xlab("Polynomial Terms") + ylab("Mean Squared Error") +
    scale_x_continuous(breaks=1:power)
## ---- end-of-assign2-3

## 4
## ---- assign2-4
linear_model <- lm(Fat ~ . - Protein - Moisture - Sample, data=data)
aic <- stepAIC(linear_model, direction="both", trace=FALSE)
feature_selection_count <- length(aic$coefficients) - 1
## ---- end-of-assign2-4

## 5
## ---- assign2-5
response <- as.matrix(data[, setdiff(names(data), c("Sample", "Protein",
                                                    "Moisture", "Fat"))])
target <- data[, "Fat"]

## Ridge Regression
ridge_model <- glmnet(x=response, y=target, alpha=0, nlambda=100)
coefficients <- coef(ridge_model)
coefficients <- as.matrix(coefficients[-1,])
colnames(coefficients) <- ridge_model$lambda

plot_data <- melt(coefficients, id=rownames, varnames=c("feature", "lambda"))

ggplot(plot_data, aes(x=log(lambda), y=value, colour=feature)) +
    geom_line(show.legend=FALSE) +
    xlab(expression(log(lambda)))
## ---- end-of-assign2-5

## 6
## ---- assign2-6
## Lasso Regression
lasso_model <- glmnet(x=response, y=target, alpha=1, nlambda=100)
coefficients <- coef(lasso_model)
colnames(coefficients) <- lasso_model$lambda
coefficients <- as.matrix(coefficients[-1,])

plot_data <- melt(coefficients, id=rownames, varnames=c("feature", "lambda"))

ggplot(plot_data, aes(x=log(lambda), y=value, colour=feature)) +
    geom_line(show.legend=FALSE) +
    xlab(expression(log(lambda)))
## ---- end-of-assign2-6

## 7
## ---- assign2-7
lasso_model_cv <- cv.glmnet(response, target, alpha=1)
optimal_lambda <- lasso_model_cv$lambda.min
feature_selection_count <- sum(as.matrix(coef(lasso_model_cv)) != 0) - 1

plot_data <- data.frame(x=lasso_model_cv$lambda, y=lasso_model_cv$cvm)

ggplot(plot_data, aes(x=log(x), y=y)) + geom_point() +
    ggtitle("Cross-validation Scores") +
    xlab(expression(log(lambda))) +
    ylab("Mean Squared Error")
## ---- end-of-assign2-7
