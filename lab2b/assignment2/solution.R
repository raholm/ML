## ---- assign2-init
library(tree)

data <- read.csv2("../data/bodyfatregression.csv")
names(data) <- c("Waist", "Weight", "Bodyfat")
## ---- end-of-assign2-init

## 1
fit <- tree(Bodyfat ~ Waist + Weight, data=data)

## 2

## 3



