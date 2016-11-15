library(ggplot2)
library(readxl)
library(reshape2)
library(mgcv)

data <- read_excel("../data/Influenza.xlsx")

## 1
plot_data <- melt(data[, c("Time", "Mortality", "Influenza")], id="Time")
ggplot(plot_data) +
    geom_line(aes(x=Time, y=value, color=variable))

## 2
gamfit <- gam(Mortality ~ Year + s(Week), family=gaussian(), data=data, method="GCV.Cp")
coefficients <- coef(gamfit)

## 3
yhat <- predict(gamfit, data)

plot_data <- data.frame(Time=data$Time, Actual=data$Mortality, Estimate=as.numeric(yhat))
plot_data <- melt(plot_data, id="Time", value.name="Mortality", variable.name="Data")

ggplot(plot_data) +
    geom_line(aes(x=Time, y=Mortality, color=Data))


## gamfit$sp
## gamfit$smooth
## coef(gamfit)
## gamfit$terms

## 4


## 5


## 6

