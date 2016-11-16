## ---- assign2-init
library(ggplot2)
library(readxl)
library(reshape2)
library(mgcv)

data <- read_excel("../data/Influenza.xlsx")
## ---- end-of-assign2-init

## 1
## ---- assign2-1
plot_data <- melt(data[, c("Time", "Mortality", "Influenza")], id="Time")
ggplot(plot_data) +
    geom_line(aes(x=Time, y=value, color=variable))
## ---- end-of-assign2-1

## 2
## ---- assign2-2
gamfit <- gam(Mortality ~ Year + s(Week), family=gaussian(), data=data, method="GCV.Cp")
coefficients <- coef(gamfit)
## ---- end-of-assign2-2

## 3
## ---- assign2-3-plot
yhat <- predict(gamfit, data)

plot_data <- data.frame(Time=data$Time, Actual=data$Mortality, Estimate=as.numeric(yhat))
plot_data <- melt(plot_data, id="Time", value.name="Mortality", variable.name="Data")

ggplot(plot_data) +
    geom_line(aes(x=Time, y=Mortality, color=Data))
## ---- end-of-assign2-3-plot

summary(gamfit)
plot(gamfit)
coef(gamfit)

## 4
k <- length(unique(data$Week)) - 1

gamfit1 <- gam(Mortality ~ Year + s(Week, k=k, sp=100000000),
              family=gaussian, data=data, method="GCV.Cp")
gamfit2 <- gam(Mortality ~ Year + s(Week, k=k, sp=0),
              family=gaussian, data=data, method="GCV.Cp")

summary(gamfit1)
summary(gamfit2)

plot_data <- data.frame(Time=data$Time, Actual=data$Mortality,
                        EstimateHigh=gamfit1$fitted.values, EstimateLow=gamfit2$fitted.values)
plot_data <- melt(plot_data, id="Time", value.name="Mortality", variable.name="Data")

ggplot(plot_data) +
    geom_line(aes(x=Time, y=Mortality, color=Data))


plot(gamfit, residuals=TRUE)

## 5
## ---- assign2-5
residuals <- resid(gamfit)
plot_data <- data.frame(Time=data$Time, Influenza=data$Influenza, Residuals=residuals)
plot_data <- melt(plot_data, id="Time", value.name="Value", variable.name="Data")

ggplot(plot_data) +
    geom_line(aes(x=Time, y=Value, color=Data))
## ---- end-of-assign2-5

## 6
gamfit <- gam(Mortality ~ s(Year) + s(Week) + s(Influenza), data=data)

data$Influenza
data$Year
