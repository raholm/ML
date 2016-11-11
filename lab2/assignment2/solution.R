library(MASS)
library(glmnet)
library(readxl)
library(ggplot2)
library(Matrix)
library(reshape2)

data <- read_excel("../data/tecator.xlsx")
names(data)

## 1
ggplot(data) +
    geom_point(aes(x=Protein, y=Moisture))

## 2

## 3
set.seed(12345)
n <- nrow(data)
training_idx <- sample(1:n, size=floor(n * 0.5))

train <- data[training_idx,]
test <- data[-training_idx,]

## 4
linear_model <- lm(Fat ~ . - Protein - Moisture - Sample, data=data)
aic <- stepAIC(linear_model, direction="both", trace=FALSE)

## 5
lambdas <- c(0.001, 0.01, 0.1, 0, 1, 10, 100)

response <- as.matrix(data[, setdiff(names(data), c("Sample", "Protein", "Moisture", "Fat"))])
target <- data[, "Fat"]

## Ridge Regression
ridge_model <- glmnet(x=response, y=target, alpha=0, lambda=lambdas)
coefficients <- coef(ridge_model)
coefficients <- as.matrix(coefficients[-1,])
colnames(coefficients) <- ridge_model$lambda

plot_data <- melt(coefficients, id=rownames, varnames=c("feature", "lambda"))

ggplot(plot_data, aes(x=lambda, y=value, colour=feature)) + geom_line(show.legend=FALSE)

## 6
## Lasso Regression
lasso_model <- glmnet(x=response, y=target, alpha=1, nlambda=100)
coefficients <- coef(lasso_model)
colnames(coefficients) <- lasso_model$lambda
coefficients <- as.matrix(coefficients[-1,])

plot_data <- melt(coefficients, id=rownames, varnames=c("feature", "lambda"))

ggplot(plot_data, aes(x=lambda, y=value, colour=feature)) + geom_line(show.legend=FALSE)

## 7
lasso_model_cv <- cv.glmnet(response, target, alpha=1)
lasso_model_cv$lambda
lasso_model_cv$glmnet.fit
lasso_model_cv$lambda.min
lasso_model_cv$name

## 8

