## ---- assign2-init
library(ggplot2)
library(readxl)
library(reshape2)
library(mgcv)
library(grid)
library(gridExtra)

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
gamfit <- gam(Mortality ~ Year + s(Week), family=gaussian, data=data, method="GCV.Cp")
## ---- end-of-assign2-2

## 3
## ---- assign2-3-plot
yhat <- predict(gamfit, data)

plot_data <- data.frame(Time=data$Time, Actual=data$Mortality, Estimate=as.numeric(yhat))
plot_data <- melt(plot_data, id="Time", value.name="Mortality", variable.name="Data")

ggplot(plot_data) +
    geom_line(aes(x=Time, y=Mortality, color=Data))
## ---- end-of-assign2-3-plot

## ---- assign2-3-summary
summary(gamfit)
## ---- end-of-assign2-3-summary

## ---- assign2-3-spline
plot(gamfit)
## ---- end-of-assign2-3-spline

## 4
## ---- assign2-4
k <- length(unique(data$Week)) - 1
penalty_values <- c(0, 10, 1000, 100000)

plots <- list()

for (i in 1:length(penalty_values)) {
    fit <- gam(Mortality ~ Year + s(Week, k=k, sp=penalty_values[i]),
               family=gaussian, data=data, method="GCV.Cp")

    title <- paste("Params (k=", k, ",sp=", penalty_values[i], ")", sep="")

    plot_data <- data.frame(Time=data$Time, Actual=data$Mortality, Estimate=fitted(fit))
    plot_data <- melt(plot_data, id="Time", value.name="Mortality", variable.name="Data")

    plots[[i]] <- ggplot(plot_data) +
        geom_line(aes(x=Time, y=Mortality, color=Data), show.legend=FALSE) +
        ggtitle(title) +
        theme(axis.text=element_blank())
}

do.call(grid.arrange, c(plots, list(ncol=2)))
## ---- end-of-assign2-4

## 5
## ---- assign2-5
gamfit <- gam(Mortality ~ Year + s(Week), family=gaussian, data=data, method="GCV.Cp")
residuals <- resid(gamfit)
plot_data <- data.frame(Time=data$Time, Residuals=residuals, Influenza=data$Influenza)
plot_data <- melt(plot_data, id="Time", value.name="Value", variable.name="Data")

ggplot(plot_data) +
    geom_line(aes(x=Time, y=Value, color=Data))
## ---- end-of-assign2-5

## 6
## ---- assign2-6
gamfit <- gam(Mortality ~ s(Year, k=length(unique(data$Year)) - 1) +
                  s(Week, k=length(unique(data$Week)) - 1) +
                  s(Influenza, k=length(unique(data$Influenza))),
              data=data)
summary(gamfit)

plot_data <- data.frame(Time=data$Time, Actual=data$Mortality, Estimate=fitted(gamfit))
plot_data <- melt(plot_data, id="Time", value.name="Value", variable.name="Data")

ggplot(plot_data) +
    geom_line(aes(x=Time, y=Value, color=Data))
## ---- end-of-assign2-6
