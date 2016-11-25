## ---- assign1-init
library(ggplot2)
library(tree)
library(reshape2)
library(boot)

data <- read.csv2("../data/State.csv", header=TRUE, sep=";")
data <- data[order(data$MET),]
## ---- end-of-assign1-init

## 1
## ---- assign1-1
ggplot(data) +
    geom_point(aes(x=MET, y=EX))
## ---- end-of-assign1-1

## 2
## ---- assign1-2
treefit <- tree(EX ~ MET, data=data,
                control=tree.control(nobs=nrow(data), minsize=8))

set.seed(12345)
treefit.cv <- cv.tree(treefit, FUN=prune.tree, K=10)
optimal_leaf_count <- treefit.cv$size[which.min(treefit.cv$dev)]

optimal_tree <- prune.tree(treefit, best=optimal_leaf_count)
## ---- end-of-assign1-2

## ---- assign1-2-tree-plot
plot(optimal_tree)
text(optimal_tree, pretty=0)
## ---- end-of-assign1-2-tree-plotr

## ---- assign1-2-tree-fit
predicted <- predict(optimal_tree, data)
plot_data <- data.frame(MET=data$MET, Actual=data$EX, Estimate=predicted)
plot_data <- melt(plot_data, id="MET", variable.name="Data", value.name="EX")

ggplot(plot_data) +
    geom_point(aes(x=MET, y=EX, color=Data))
## ---- end-of-assign1-2-tree-fit

## ---- assign1-2-tree-resid
residuals <- resid(optimal_tree)

plot_data <- data.frame(x=1:length(residuals), y=residuals)

ggplot(plot_data) +
    xlab("Index") +
    ylab("Residual") +
    geom_point(aes(x=x, y=y))
## ---- end-of-assign1-2-tree-resid

## 3
estimate <- function(formula, original_data, leaves){
    formula <- formula
    original_data <- original_data
    leaves <- leaves

    function(data, ind) {
        sample <- data[ind,]
        fit <- tree(formula, data=sample, split="deviance")
        fit <- prune.tree(fit, best=leaves)
        prediction <- predict(fit, newdata=original_data)
        return(prediction)
    }
}

f <- estimate(formula=EX ~ MET, original_data=data, leaves=optimal_leaf_count)

fit <- boot(data, f, R=1000)
e <- envelope(fit)

plot_data <- data.frame(x=data$MET, CBL=e$point[1, ], CBU=e$point[2, ])
plot_data <- melt(plot_data, id="x")

predicted <- predict(optimal_tree, data)
plot_data2 <- data.frame(MET=data$MET, Observed=data$EX, Estimate=predicted)
plot_data2 <- melt(plot_data2, id="MET", variable.name="Data", value.name="EX")

ggplot() +
    geom_line(data=plot_data, aes(x=x, y=value, color=variable)) +
    geom_point(data=plot_data2, aes(x=MET, y=EX, color=Data))

## 4

## 5
