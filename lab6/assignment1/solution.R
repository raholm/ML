library(neuralnet)
set.seed(1234567890)

n <- 50
m <- 10

Var <- runif(n, 0, 10)
data <- data.frame(Var, Sin=sin(Var))

train <- data[1:(n / 2),] # Training
validation <- data[(n / 2 + 1):n,] # Validation

## Random initializaiton of the weights in the interval [-1, 1]
winit <- runif(3 * m + 1, -1, 1)

errors <- rep(0, 10)

for(i in 1:10) {
    nn <- neuralnet(Sin ~ Var, data=train, threshold=i / 1000,
                    hidden=10, startweights=winit)
    predicted <- compute(nn, validation$Var)$net.result
    errors[i] <- sum((predicted - validation$Sin)^2) / nrow(validation)

    if (i > 1 && errors[i] > errors[i - 1]) break
}

plot(errors)

## plot(nn <- neuralnet())

prediction(nn)

##  Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1)
points(data, col = "red")

?neuralnet
