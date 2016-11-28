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
treefit <- tree(EX ~ MET, data=data, split="deviance",
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
plot_data <- data.frame(resid=residuals)

ggplot(plot_data) +
    xlab("Residual") +
    ylab("Frequency") +
    geom_histogram(aes(resid), bins=10)
## ---- end-of-assign1-2-tree-resid

## 3
## ---- assign1-3
nonparametric.estimate <- function(formula, original_data, leaves){
    formula <- formula
    original_data <- original_data
    leaves <- leaves

    function(data, idx) {
        sample <- data[idx,]
        fit <- tree(formula, data=sample, split="deviance",
                    control=tree.control(nobs=nrow(original_data), minsize=8))
        fit <- prune.tree(fit, best=leaves)
        prediction <- predict(fit, newdata=original_data)
        prediction
    }
}

f <- nonparametric.estimate(formula=EX ~ MET, original_data=data, leaves=optimal_leaf_count)

set.seed(12345)
fit <- boot(data, f, R=1000)
confidence_bands <- envelope(fit, level=0.95)
## ---- end-of-assign1-3

## ---- assign1-3-confbounds
predicted <- predict(optimal_tree, data)
plot_data_est <- data.frame(MET=data$MET, Observed=data$EX, Estimate=predicted)
plot_data_est <- melt(plot_data_est, id="MET", variable.name="Data", value.name="EX")

plot_data_CB <- data.frame(MET=data$MET, CBU=confidence_bands$point[1,],
                          CBL=confidence_bands$point[2,])

ggplot() +
    geom_point(data=plot_data_est, aes(x=MET, y=EX, color=Data)) +
    geom_ribbon(data=plot_data_CB, aes(x=MET, ymin=CBL, ymax=CBU), color="red", alpha=0.1, fill="red")
## ---- end-of-assign1-3-confbounds

## 4
## ---- assign1-4
rng <- function(data, model) {
    n <- nrow(data)
    newdata <- data.frame(MET=data$MET, EX=data$EX)
    newdata$EX <- rnorm(n, predict(model, newdata=newdata),
                        sd(resid(model)))
    newdata
}

parametric.estimate.cb <- function(formula, original_data, leaves){
    formula <- formula
    original_data <- original_data
    leaves <- leaves

    function(data) {
        fit <- tree(formula, data=data, split="deviance",
                    control=tree.control(nobs=nrow(original_data), minsize=8))
        fit <- prune.tree(fit, best=leaves)
        prediction <- predict(fit, newdata=original_data)
        prediction
    }
}

parametric.estimate.pb <- function(formula, original_data, leaves){
    formula <- formula
    original_data <- original_data
    leaves <- leaves

    function(data) {
        fit <- tree(formula, data=data, split="deviance",
                    control=tree.control(nobs=nrow(original_data), minsize=8))
        fit <- prune.tree(fit, best=leaves)
        prediction <- predict(fit, newdata=original_data)
        rnorm(nrow(data), prediction, sd(resid(fit)))
    }
}

set.seed(12345)
f.cb <- parametric.estimate.cb(formula=EX ~ MET, original_data=data, leaves=optimal_leaf_count)
fit  <- boot(data, statistic=f.cb, R=1000,
             mle=optimal_tree, ran.gen=rng, sim="parametric")
confidence_bands <- envelope(fit, level=0.95)

set.seed(12345)
f.pb <- parametric.estimate.pb(formula=EX ~ MET, original_data=data, leaves=optimal_leaf_count)
fit  <- boot(data, statistic=f.pb, R=1000,
             mle=optimal_tree, ran.gen=rng, sim="parametric")
prediction_bands <- envelope(fit, level=0.95)
## ---- end-of-assign1-4

## ---- assign1-4-confbounds
predicted <- predict(optimal_tree, data)
plot_data_est <- data.frame(MET=data$MET, Observed=data$EX, Estimate=predicted)
plot_data_est <- melt(plot_data_est, id="MET", variable.name="Data", value.name="EX")

plot_data_CB <- data.frame(MET=data$MET, CBU=confidence_bands$point[1,],
                          CBL=confidence_bands$point[2,])

plot_data_PB <- data.frame(MET=data$MET, PBU=prediction_bands$point[1,],
                          PBL=prediction_bands$point[2,])

ggplot() +
    geom_point(data=plot_data_est, aes(x=MET, y=EX, color=Data)) +
    geom_ribbon(data=plot_data_CB, aes(x=MET, ymin=CBL, ymax=CBU), color="red", alpha=0.1, fill="red") +
    geom_ribbon(data=plot_data_PB, aes(x=MET, ymin=PBL, ymax=PBU), color="blue", alpha=0.1, fill="blue")
## ---- end-of-assign1-4-confbounds

## 5
