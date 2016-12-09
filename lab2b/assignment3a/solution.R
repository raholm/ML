## ---- assign3a-init
library(mboost)

data <- read.csv2("../data/bodyfatregression.csv")

set.seed(1234567890)
fit <- blackboost(Bodyfat_percent ~ Waist_cm + Weight_kg, data=data)

cvf <- cv(model.weights(fit), type="kfold")
cvm <- cvrisk(fit, folds=cvf, grid=1:100)
## ---- end-of-assign3a-init

## ---- assign3a-plot
plot(cvm)
## ---- end-of-assign3a-plot

## ---- assign3a-2
set.seed(1234567890)
train_idx <- sample(nrow(data), floor(nrow(data) * (2 / 3)))
train <- data[train_idx,]
test <- data[-train_idx,]

fit <- blackboost(Bodyfat_percent ~ Waist_cm + Weight_kg, data=train,
                  control=boost_control(mstop=mstop(cvm)))
test_error <- sum((predict(fit, test) - test$Bodyfat_percent)^2)
train_error <- sum((predict(fit, train) - train$Bodyfat_percent)^2)

cat(paste("Test Error:", test_error, "\n"))
cat(paste("Train Error:", train_error, "\n"))
## ---- end-of-assign3a-2
