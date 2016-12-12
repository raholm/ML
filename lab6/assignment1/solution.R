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

for(it in 1:10) {
    nn <- neuralnet(Sin ~ Var, data=train, threshold=it / 1000,
                    hidden=10, startweights=winit)
    predicted <- compute(nn, validation$Var)$net.result
    errors[it] <- sum((predicted - validation$Sin)^2) / nrow(validation)

    if (it > 1 && errors[it] > errors[it - 1]) break
}

optimal_threshold <- it / 1000

nn <- neuralnet(Sin ~ Var, data=data,
                threshold=optimal_threshold,
                hidden=10, startweights=winit)

plot(errors[1:it])
plot(nn)

##  Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1)
points(data, col = "red")
